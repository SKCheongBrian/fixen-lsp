{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixen.LSP.Server (
  runFixenLanguageServer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.IO (hPutStrLn, stderr)

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

import Fixen.Diagnostics qualified as Diagnostics
import Fixen.IR.AST qualified as AST
import Fixen.Monad (runFixenM)
import Fixen.Pipeline (pipeline)

type Config = ()
type ServerState = IORef.IORef (Map.Map Uri [Text.Text])

runFixenLanguageServer :: IO Int
runFixenLanguageServer = do
  state <- IORef.newIORef Map.empty
  runServer $ serverDefinition state

serverDefinition :: ServerState -> ServerDefinition Config
serverDefinition state =
  ServerDefinition
    { parseConfig = \_oldConfig _newConfig -> Right ()
    , onConfigChange = \_config -> pure ()
    , defaultConfig = ()
    , configSection = "fixen"
    , doInitialize = \environment _request ->
        pure $ Right environment
    , staticHandlers = const $ handlers state
    , interpretHandler = \environment ->
        Iso (runLspT environment) liftIO
    , options =
        defaultOptions
          { optTextDocumentSync =
              Just $
                TextDocumentSyncOptions
                  (Just True)
                  (Just TextDocumentSyncKind_Full)
                  (Just False)
                  (Just False)
                  Nothing
          }
    }

programRelationNames :: AST.Program -> [Text.Text]
programRelationNames program =
  map
    (AST.simpleIdentifier . AST.relationLikeName)
    (AST.programRelationDeclarations program)

handlers :: ServerState -> Handlers (LspM Config)
handlers state =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_notification ->
        liftIO $
          hPutStrLn stderr "fixen-lsp initialized"
    , -- textCompletion
      requestHandler SMethod_TextDocumentCompletion $
        \request
         responder -> do
            let TRequestMessage _jsonrpc _requestId _method params = request
                CompletionParams document _position _workDone _partialResult _context = params
                TextDocumentIdentifier uri = document
            cache <- liftIO $ IORef.readIORef state
            let cachedRelationNames = Map.findWithDefault [] uri cache
                relationCompletions = map relationCompletion cachedRelationNames
                keywordCompletions = map keywordCompletion fixenKeywords
                completions = relationCompletions <> keywordCompletions
            responder (Right (InL completions))
    , -- didOpen
      notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
        let TNotificationMessage _ _ params = notification
            DidOpenTextDocumentParams document = params
            TextDocumentItem uri _languageId version contents = document
        liftIO $ do
          hPutStrLn stderr ("opened: " <> show uri)
          hPutStrLn stderr ("version: " <> show version)
          hPutStrLn stderr ("characters: " <> show (Text.length contents))
        analyzeDocument state uri contents
    , -- didClose
      notificationHandler SMethod_TextDocumentDidClose $ \notification -> do
        let TNotificationMessage _ _ params = notification
            DidCloseTextDocumentParams document = params
            TextDocumentIdentifier uri = document
        liftIO $ do
          IORef.atomicModifyIORef' state $ \cache ->
            (Map.delete uri cache, ())
          hPutStrLn stderr ("closed: " <> show uri)
        sendDocumentDiagnostics uri []
    , -- didChange
      notificationHandler SMethod_TextDocumentDidChange $ \notification -> do
        let TNotificationMessage _ _ params = notification
            DidChangeTextDocumentParams document changes = params
            VersionedTextDocumentIdentifier uri version = document
        liftIO $ do
          hPutStrLn stderr ("changed: " <> show uri)
          hPutStrLn stderr ("version: " <> show version)
          hPutStrLn stderr ("changes: " <> show (length changes))
        mapM_ (analyzeDocument state uri . changeText) changes
    ]

fixenKeywords :: [Text.Text]
fixenKeywords =
  [ "module"
  , "where"
  , "import"
  , "qualified"
  , "as"
  , "include"
  , "rel"
  , "rule"
  , "if"
  , "partial"
  , "ord"
  , "lat"
  , "priority"
  , "query"
  , "phases"
  ]

completionItem ::
  CompletionItemKind ->
  Maybe Text.Text ->
  Text.Text ->
  CompletionItem
completionItem kind detail label =
  CompletionItem
    { _label = label
    , _labelDetails = Nothing
    , _kind = Just kind
    , _tags = Nothing
    , _detail = detail
    , _documentation = Nothing
    , _deprecated = Nothing
    , _preselect = Nothing
    , _sortText = Nothing
    , _filterText = Nothing
    , _insertText = Just label
    , _insertTextFormat = Nothing
    , _insertTextMode = Nothing
    , _textEdit = Nothing
    , _textEditText = Nothing
    , _additionalTextEdits = Nothing
    , _commitCharacters = Nothing
    , _command = Nothing
    , _data_ = Nothing
    }

keywordCompletion :: Text.Text -> CompletionItem
keywordCompletion =
  completionItem CompletionItemKind_Keyword Nothing

relationCompletion :: Text.Text -> CompletionItem
relationCompletion =
  completionItem
    CompletionItemKind_Function
    (Just "Fixen relation")

changeText :: TextDocumentContentChangeEvent -> Text.Text
changeText (TextDocumentContentChangeEvent (InR wholeDocument)) =
  let TextDocumentContentChangeWholeDocument contents = wholeDocument
   in contents
changeText (TextDocumentContentChangeEvent (InL partialChange)) =
  let TextDocumentContentChangePartial _range _rangeLength contents = partialChange
   in contents

toLspPosition :: Int -> Int -> Position
toLspPosition line column =
  Position
    (fromIntegral (max 0 (line - 1)))
    (fromIntegral (max 0 (column - 1)))

toLspRange :: Maybe Diagnostics.FixenSourceSpan -> Range
toLspRange maybeSpan =
  case maybeSpan of
    Nothing ->
      Range
        (Position 0 0)
        (Position 0 0)
    Just sourceSpan ->
      Range
        ( toLspPosition
            (Diagnostics.fixenStartLine sourceSpan)
            (Diagnostics.fixenStartColumn sourceSpan)
        )
        ( toLspPosition
            (Diagnostics.fixenEndLine sourceSpan)
            (Diagnostics.fixenEndColumn sourceSpan)
        )

toLspSeverity :: Diagnostics.FixenDiagnosticSeverity -> DiagnosticSeverity
toLspSeverity Diagnostics.FixenDiagnosticError = DiagnosticSeverity_Error
toLspSeverity Diagnostics.FixenDiagnosticWarning = DiagnosticSeverity_Warning

toLspDiagnostic :: Diagnostics.FixenDiagnostic -> Diagnostic
toLspDiagnostic diagnostic =
  Diagnostic
    { _range =
        toLspRange (Diagnostics.fixenDiagnosticSpan diagnostic)
    , _severity =
        Just
          ( toLspSeverity
              (Diagnostics.fixenDiagnosticSeverity diagnostic)
          )
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Just "fixen"
    , _message = Diagnostics.fixenDiagnosticMessage diagnostic
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }

sendDocumentDiagnostics :: Uri -> [Diagnostic] -> LspM Config ()
sendDocumentDiagnostics uri diagnostics =
  sendNotification
    SMethod_TextDocumentPublishDiagnostics
    (PublishDiagnosticsParams uri Nothing diagnostics)

analyzeDocument :: ServerState -> Uri -> Text.Text -> LspM Config ()
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
                  ( Diagnostics.toFixenDiagnostics
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
        Right (program, _ruleForests, _representation, _generatedCode) ->
          do
            let names = programRelationNames program
            liftIO $ do
              IORef.atomicModifyIORef' state $ \cache ->
                (Map.insert uri names cache, ())
              hPutStrLn stderr "analysis succeeded"
              hPutStrLn stderr ("relations: " <> show names)

            sendDocumentDiagnostics uri []
