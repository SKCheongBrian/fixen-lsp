module Fixen.LSP.TextDocument
  ( unqualifiedName
  , wordAtPosition
  ) where

import Data.Char (isAlphaNum)
import Data.Text qualified as Text
import Language.LSP.Protocol.Types (Position (..))

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
