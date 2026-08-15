module Fixen.LSP.Analysis (
  analyzeDocument,
  changeText,
) where

import Control.Monad.IO.Class (liftIO)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Prettyprinter (
  defaultLayoutOptions,
  layoutPretty,
  unAnnotate,
 )
import Prettyprinter.Render.Text (renderStrict)
import System.IO (hPutStrLn, stderr)

import Fixen.Diagnostics qualified as Fixen
import Fixen.IR.AST qualified as AST
import Fixen.Monad (runFixenM)
import Fixen.Monad.Env.Symbol qualified as Symbol
import Fixen.Pipeline (pipelineWithSymbols)

import Fixen.LSP.Diagnostics (
  sendDocumentDiagnostics,
  toLspDiagnostic,
 )
import Fixen.LSP.State (
  ServerState,
  storeDocument,
 )
import Fixen.LSP.Types (
  Config,
  DocumentAnalysis (..),
  HoverInfo (..),
 )

changeText :: TextDocumentContentChangeEvent -> Text.Text
changeText (TextDocumentContentChangeEvent (InR wholeDocument)) =
  let TextDocumentContentChangeWholeDocument contents = wholeDocument
   in contents
changeText (TextDocumentContentChangeEvent (InL partialChange)) =
  let TextDocumentContentChangePartial _range _rangeLength contents = partialChange
   in contents

programRelationNames :: AST.Program -> [Text.Text]
programRelationNames program = map relationName (AST.programRelationDeclarations program)
  where
    relationName = AST.simpleIdentifier . AST.relationLikeName

renderType :: AST.Type -> Text.Text
renderType =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . unAnnotate
    . AST.prettyType

renderTypeLattice :: Symbol.TypeLattice -> Text.Text
renderTypeLattice Symbol.Dynamic = "Dynamic"
renderTypeLattice (Symbol.ActualType actualType _evidence) = renderType actualType
renderTypeLattice Symbol.Bottom = "<type error>"

relationHoverInfo :: AST.RelationDeclaration -> HoverInfo
relationHoverInfo relation =
  HoverInfo
    { hoverName = relationName
    , hoverRange = Nothing
    , hoverContents = signature
    }
  where
    relationName = AST.simpleIdentifier (AST.relationLikeName relation)

    parameterTypes = map renderParameterType (AST.relationLikeArgs relation)
    renderParameterType = renderType . AST.relationParameterType

    signature =
      relationName
        <> " : "
        <> Text.intercalate " -> " (parameterTypes <> ["Relation"])

programRelationHoverInfo :: AST.Program -> [HoverInfo]
programRelationHoverInfo program = map relationHoverInfo (AST.programRelationDeclarations program)

ruleParameterLine :: (Text.Text, Symbol.RuleParameterInfo) -> Text.Text
ruleParameterLine (name, parameterInfo) =
  name <> " : " <> renderTypeLattice (Symbol._ruleParamType parameterInfo)

ruleParameterHoverInfo :: (Text.Text, Symbol.RuleParameterInfo) -> HoverInfo
ruleParameterHoverInfo parameter@(name, _parameterInfo) =
  HoverInfo
    { hoverName = name
    , hoverRange = Nothing
    , hoverContents = ruleParameterLine parameter
    }

ruleInfoHoverInfo :: Symbol.RuleInfo -> [HoverInfo]
ruleInfoHoverInfo ruleInfo = ruleNameEntries <> parameterEntries
  where
    rule = Symbol._ruleDeclaration ruleInfo
    parameters = Map.toList (Symbol._ruleBoundVars ruleInfo)
    parameterEntries = map ruleParameterHoverInfo parameters
    parameterLines = map (("  " <>) . ruleParameterLine) parameters
    ruleNameEntries =
      case AST.ruleName rule of
        Nothing -> []
        Just ruleIdentifier ->
          [ HoverInfo
              { hoverName = name
              , hoverRange = Nothing
              , hoverContents = Text.unlines (("rule " <> name) : parameterLines)
              }
          ]
          where
            name = AST.simpleIdentifier ruleIdentifier

programHoverInfo :: AST.Program -> Symbol.SymbolEnv -> [HoverInfo]
programHoverInfo program symbolEnvironment = relationEntries <> ruleEntries
  where
    relationEntries = programRelationHoverInfo program
    ruleEntries = concatMap ruleInfoHoverInfo (IntMap.elems (Symbol._ruleMap symbolEnvironment))

analyzeDocument :: ServerState -> Uri -> Text.Text -> LspM Config ()
analyzeDocument state uri contents =
  case uriToFilePath uri of
    Nothing -> liftIO $ hPutStrLn stderr ("cannot analyze non-file URI: " <> show uri)
    Just filePath -> do
      liftIO $ hPutStrLn stderr ("analyzing: " <> filePath)

      result <-
        liftIO $
          runFixenM $
            pipelineWithSymbols
              filePath
              (Text.unpack contents)
              (\_warning -> pure ())

      case result of
        Left compilerDiagnostics -> do
          let lspDiagnostics = map toLspDiagnostic (Fixen.toFixenDiagnostics compilerDiagnostics)
              message = "analysis failed with " <> show (length lspDiagnostics) <> " diagnostic(s)"
          liftIO $ hPutStrLn stderr message
          sendDocumentDiagnostics uri lspDiagnostics
        Right
          ( program
            , symbolEnvironment
            , _ruleForests
            , _representation
            , _generatedCode
            ) -> do
            let analysis =
                  DocumentAnalysis
                    { analysisContents = contents
                    , analysisRelationNames = programRelationNames program
                    , analysisHoverInfo = programHoverInfo program symbolEnvironment
                    }

            liftIO $ do
              storeDocument state uri analysis
              hPutStrLn stderr "analysis succeeded"
              hPutStrLn stderr ("relations: " <> show (analysisRelationNames analysis))

            sendDocumentDiagnostics uri []
