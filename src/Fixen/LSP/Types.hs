module Fixen.LSP.Types (
  Config,
  HoverInfo (..),
  DocumentAnalysis (..),
  DefinitionInfo (..),
) where

import Data.Text qualified as Text
import Language.LSP.Protocol.Types (Range, Uri)

type Config = ()

data DefinitionInfo = DefinitionInfo
  { definitionName :: Text.Text
  , definitionUri :: Uri
  , definitionRange :: Range
  }
  deriving (Eq, Show)

data HoverInfo = HoverInfo
  { hoverName :: Text.Text
  , hoverRange :: Maybe Range
  , hoverContents :: Text.Text
  }
  deriving (Eq, Show)

data DocumentAnalysis = DocumentAnalysis
  { analysisContents :: Text.Text
  , analysisRelationNames :: [Text.Text]
  , analysisRuleNames :: [Text.Text]
  , analysisHoverInfo :: [HoverInfo]
  , analysisDefinitions :: [DefinitionInfo]
  }
  deriving (Eq, Show)
