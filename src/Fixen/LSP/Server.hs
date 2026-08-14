{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixen.LSP.Server (
  runFixenLanguageServer,
) where

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.IO (hPutStrLn, stderr)
import Data.Text qualified as Text

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
    , staticHandlers = \_clientCapabilities -> handlers
    , interpretHandler = \environment ->
        Iso (runLspT environment) liftIO
    , options = defaultOptions
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

analyzeDocument :: Uri -> Text.Text -> LspM Config ()
analyzeDocument uri contents =
  liftIO $ do
    hPutStrLn stderr ("analyzing:" <> show uri)
    hPutStrLn stderr ("analysis characters:" <> show (Text.length contents))













