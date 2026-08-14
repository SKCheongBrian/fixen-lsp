{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixen.LSP.Server (
  runFixenLanguageServer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import System.IO (hPutStrLn, stderr)

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

import Fixen.Diagnostics qualified as Diagnostics
import Fixen.Monad (runFixenM)
import Fixen.Pipeline (pipeline)

type Config = ()

runFixenLanguageServer :: IO Int
runFixenLanguageServer =
  runServer serverDefinition

serverDefinition :: ServerDefinition Config
serverDefinition =
  ServerDefinition
    { parseConfig = \_oldConfig _newConfig -> Right ()
    , onConfigChange = \_config -> pure ()
    , defaultConfig = ()
    , configSection = "fixen"
    , doInitialize = \environment _request ->
        pure $ Right environment
    , staticHandlers = const handlers
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

handlers :: Handlers (LspM Config)
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_notification ->
        liftIO $
          hPutStrLn stderr "fixen-lsp initialized"
    , notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
        let TNotificationMessage _ _ params = notification
            DidOpenTextDocumentParams document = params
            TextDocumentItem uri _languageId version contents = document
        liftIO $ do
          hPutStrLn stderr ("opened: " <> show uri)
          hPutStrLn stderr ("version: " <> show version)
          hPutStrLn stderr ("characters: " <> show (Text.length contents))
        analyzeDocument uri contents
    , notificationHandler SMethod_TextDocumentDidClose $ \notification -> do
        let TNotificationMessage _ _ params = notification
            DidCloseTextDocumentParams document = params
            TextDocumentIdentifier uri = document
        liftIO $ do
          hPutStrLn stderr ("closed: " <> show uri)
        sendDocumentDiagnostics uri []
    , notificationHandler SMethod_TextDocumentDidChange $ \notification -> do
        let TNotificationMessage _ _ params = notification
            DidChangeTextDocumentParams document changes = params
            VersionedTextDocumentIdentifier uri version = document
        liftIO $ do
          hPutStrLn stderr ("changed: " <> show uri)
          hPutStrLn stderr ("version: " <> show version)
          hPutStrLn stderr ("changes: " <> show (length changes))
        mapM_ (analyzeDocument uri . changeText) changes
    ]

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
    Just span ->
      Range
        ( toLspPosition
            (Diagnostics.fixenStartLine span)
            (Diagnostics.fixenStartColumn span)
        )
        ( toLspPosition
            (Diagnostics.fixenEndLine span)
            (Diagnostics.fixenEndColumn span)
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

analyzeDocument :: Uri -> Text.Text -> LspM Config ()
analyzeDocument uri contents =
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
        Right _compilerOutput -> do
          liftIO $
            hPutStrLn stderr "analysis succeeded"

          sendDocumentDiagnostics uri []
