module Fixen.LSP.Server (
  runFixenLanguageServer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.IO (hPutStrLn, stderr)

import Fixen.LSP.Analysis qualified as Analysis
import Fixen.LSP.Completion qualified as Completion
import Fixen.LSP.Diagnostics (
  sendDocumentDiagnostics,
 )
import Fixen.LSP.State (
  ServerState,
  deleteDocument,
  newServerState,
 )
import Fixen.LSP.Types (Config)

runFixenLanguageServer :: IO Int
runFixenLanguageServer = do
  state <- newServerState
  runServer (serverDefinition state)

serverDefinition ::
  ServerState ->
  ServerDefinition Config
serverDefinition state =
  ServerDefinition
    { parseConfig =
        \_oldConfig _newConfig -> Right ()
    , onConfigChange =
        \_config -> pure ()
    , defaultConfig = ()
    , configSection = "fixen"
    , doInitialize =
        \environment _request ->
          pure (Right environment)
    , staticHandlers =
        const (handlers state)
    , interpretHandler =
        \environment ->
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

handlers ::
  ServerState ->
  Handlers (LspM Config)
handlers state =
  mconcat
    [ Completion.completionHandlers state
    , notificationHandler
        SMethod_Initialized
        ( \_notification ->
            liftIO $
              hPutStrLn stderr "fixen-lsp initialized"
        )
    , notificationHandler
        SMethod_WorkspaceDidChangeConfiguration
        ( \_notification ->
            -- make Vscode shut up
            pure ()
        )
    , notificationHandler
        SMethod_SetTrace
        ( \_notification ->
            -- Make VSCode shut up
            pure ()
        )
    , requestHandler
        SMethod_TextDocumentSemanticTokensFull
        ( \_request responder ->
            -- Make neovim shut up about semantic tokens
            responder (Right (InL (SemanticTokens Nothing [])))
        )
    , notificationHandler
        SMethod_TextDocumentDidOpen
        ( \notification -> do
            let TNotificationMessage _ _ params =
                  notification
                DidOpenTextDocumentParams document =
                  params
                TextDocumentItem
                  uri
                  _languageId
                  version
                  contents =
                    document

            liftIO $ do
              hPutStrLn stderr ("opened: " <> show uri)
              hPutStrLn stderr ("version: " <> show version)
              hPutStrLn
                stderr
                ("characters: " <> show (Text.length contents))

            Analysis.analyzeDocument state uri contents
        )
    , notificationHandler
        SMethod_TextDocumentDidClose
        ( \notification -> do
            let TNotificationMessage _ _ params =
                  notification
                DidCloseTextDocumentParams document =
                  params
                TextDocumentIdentifier uri =
                  document

            liftIO $ do
              deleteDocument state uri
              hPutStrLn stderr ("closed: " <> show uri)

            sendDocumentDiagnostics uri []
        )
    , notificationHandler
        SMethod_TextDocumentDidChange
        ( \notification -> do
            let TNotificationMessage _ _ params =
                  notification
                DidChangeTextDocumentParams document changes =
                  params
                VersionedTextDocumentIdentifier uri version =
                  document

            liftIO $ do
              hPutStrLn stderr ("changed: " <> show uri)
              hPutStrLn stderr ("version: " <> show version)
              hPutStrLn stderr ("changes: " <> show (length changes))

            mapM_
              ( Analysis.analyzeDocument state uri
                  . Analysis.changeText
              )
              changes
        )
    ]
