module Fixen.LSP.Hover (
  hoverHandlers,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.List (find)
import Data.Text qualified as Text
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

import Fixen.LSP.State (
  ServerState,
  lookupDocument,
 )
import Fixen.LSP.Types (
  Config,
  DocumentAnalysis (..),
  HoverInfo (..),
 )

hoverHandlers :: ServerState -> Handlers (LspM Config)
hoverHandlers state = requestHandler SMethod_TextDocumentHover (hoverHandler state)

isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter character =
  isAlphaNum character || character == '_' || character == '\'' || character == '.'

atMay :: [a] -> Int -> Maybe a
atMay values index
  | index < 0 = Nothing
  | otherwise =
      case drop index values of
        value : _rest -> Just value
        [] -> Nothing

wordAtPosition :: Position -> Text.Text -> Maybe Text.Text
wordAtPosition (Position lineNumber columnNumber) contents = do
  lineText <- atMay (Text.lines contents) (fromIntegral lineNumber)

  let column = fromIntegral columnNumber
      (beforeCursor, afterCursor) = Text.splitAt column lineText
      leftPart = Text.takeWhileEnd isIdentifierCharacter beforeCursor
      rightPart = Text.takeWhile isIdentifierCharacter afterCursor
      word = leftPart <> rightPart

  if Text.null word then Nothing else Just word

unqualifiedName :: Text.Text -> Text.Text
unqualifiedName name =
  case reverse (Text.splitOn "." name) of
    finalPart : _rest -> finalPart
    [] -> name

findHoverContents :: Text.Text -> DocumentAnalysis -> Maybe Text.Text
findHoverContents name analysis = hoverContents <$> matchingEntry
  where
    matchingEntry = find matchesName (analysisHoverInfo analysis)
    matchesName entry = hoverName entry == unqualifiedName name

hoverHandler state request responder = do
  let TRequestMessage
        _jsonrpc
        _requestId
        _method
        params =
          request

      HoverParams
        document
        position
        _workDoneToken =
          params

      TextDocumentIdentifier uri = document

  maybeAnalysis <- liftIO $ lookupDocument state uri

  let maybeSignature = do
        analysis <- maybeAnalysis
        word <- wordAtPosition position (analysisContents analysis)
        findHoverContents word analysis

  case maybeSignature of
    Nothing -> responder (Right (InR Null))
    Just signature ->
      responder $
        Right $
          InL $
            Hover
              ( InL
                  ( MarkupContent
                      MarkupKind_Markdown
                      ( "```fixen\n"
                          <> signature
                          <> "\n```"
                      )
                  )
              )
              Nothing
