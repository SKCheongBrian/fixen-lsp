module Fixen.LSP.Analysis (
  analyzeDocument,
  changeText,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.IO (hPutStrLn, stderr)

import Fixen.Diagnostics qualified as Fixen
import Fixen.IR.AST qualified as AST
import Fixen.Monad (runFixenM)
import Fixen.Pipeline (pipeline)

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
 )

changeText :: TextDocumentContentChangeEvent -> Text.Text
changeText (TextDocumentContentChangeEvent (InR wholeDocument)) =
  let TextDocumentContentChangeWholeDocument contents =
        wholeDocument
   in contents
changeText (TextDocumentContentChangeEvent (InL partialChange)) =
  let TextDocumentContentChangePartial
        _range
        _rangeLength
        contents =
          partialChange
   in contents

programRelationNames :: AST.Program -> [Text.Text]
programRelationNames program =
  map
    (AST.simpleIdentifier . AST.relationLikeName)
    (AST.programRelationDeclarations program)

analyzeDocument ::
  ServerState ->
  Uri ->
  Text.Text ->
  LspM Config ()
analyzeDocument state uri contents =
  case uriToFilePath uri of
    Nothing ->
      liftIO $
        hPutStrLn
          stderr
          ("cannot analyze non-file URI: " <> show uri)
    Just filePath -> do
      liftIO $
        hPutStrLn stderr ("analyzing: " <> filePath)

      result <-
        liftIO $
          runFixenM $
            pipeline
              filePath
              (Text.unpack contents)
              (\_warning -> pure ())

      case result of
        Left compilerDiagnostics -> do
          let lspDiagnostics =
                map
                  toLspDiagnostic
                  ( Fixen.toFixenDiagnostics
                      compilerDiagnostics
                  )

          liftIO $
            hPutStrLn
              stderr
              ( "analysis failed with "
                  <> show (length lspDiagnostics)
                  <> " diagnostic(s)"
              )

          sendDocumentDiagnostics uri lspDiagnostics
        Right
          ( program
            , _ruleForests
            , _representation
            , _generatedCode
            ) -> do
            let analysis =
                  DocumentAnalysis
                    { analysisRelationNames =
                        programRelationNames program
                    }

            liftIO $ do
              storeDocument state uri analysis
              hPutStrLn stderr "analysis succeeded"
              hPutStrLn
                stderr
                ( "relations: "
                    <> show (analysisRelationNames analysis)
                )

            sendDocumentDiagnostics uri []
