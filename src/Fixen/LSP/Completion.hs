module Fixen.LSP.Completion (
  completionHandlers,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

import Fixen.LSP.State
  ( ServerState
  , lookupDocument
  )
import Fixen.LSP.Types
  ( Config
  , DocumentAnalysis 
    ( analysisRelationNames
    , analysisRuleNames
    )
  )

completionHandlers
  :: ServerState
  -> Handlers (LspM Config)
completionHandlers state =
  requestHandler
    SMethod_TextDocumentCompletion
    (completionHandler state)

completionHandler state request responder = do
  let TRequestMessage _jsonrpc _requestId _method params =
        request
      CompletionParams
        document
        _position
        _workDone
        _partialResult
        _context =
          params
      TextDocumentIdentifier uri = document

  maybeAnalysis <- liftIO $ lookupDocument state uri

  let cachedRelationNames = maybe [] analysisRelationNames maybeAnalysis
      cachedRuleNames = maybe [] analysisRuleNames maybeAnalysis
      relationCompletions = map relationCompletion cachedRelationNames
      ruleCompletions = map ruleCompletion cachedRuleNames
      keywordCompletions = map keywordCompletion fixenKeywords
      completions = relationCompletions <> ruleCompletions <> keywordCompletions

  responder (Right (InL completions))

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

completionItem
  :: CompletionItemKind
  -> Maybe Text.Text
  -> Text.Text
  -> CompletionItem
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
keywordCompletion = completionItem CompletionItemKind_Keyword Nothing

relationCompletion :: Text.Text -> CompletionItem
relationCompletion = completionItem CompletionItemKind_Function (Just "Fixen relation")

ruleCompletion :: Text.Text -> CompletionItem
ruleCompletion = completionItem CompletionItemKind_Function (Just "Fixen rule")
