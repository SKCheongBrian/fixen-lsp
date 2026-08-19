module Fixen.LSP.Definition
  ( definitionHandlers
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

import Fixen.LSP.State
  ( ServerState
  , lookupDocument
  )
import Fixen.LSP.TextDocument
  ( unqualifiedName
  , wordAtPosition
  )
import Fixen.LSP.Types
  ( Config
  , DefinitionInfo (..)
  , DocumentAnalysis (..)
  )

definitionHandlers
  :: ServerState
  -> Handlers (LspM Config)
definitionHandlers state =
  requestHandler
    SMethod_TextDocumentDefinition
    (definitionHandler state)

definitionHandler state request responder = do
  let TRequestMessage _jsonrpc _requestId _method params = request

      DefinitionParams
        document
        position
        _workDoneToken
        _partialResultToken =
          params

      TextDocumentIdentifier uri = document

  maybeAnalysis <- liftIO $ lookupDocument state uri

  let maybeDefinition = do
        analysis <- maybeAnalysis
        word <- wordAtPosition position (analysisContents analysis)

        let targetName = unqualifiedName word

        find (\definition -> definitionName definition == targetName) (analysisDefinitions analysis)

  case maybeDefinition of
    Nothing -> responder (Right (InR (InR Null)))

    Just definition ->
      responder $
        Right $
          InL $
            Definition $
              InL $
                Location
                  (definitionUri definition)
                  (definitionRange definition)
