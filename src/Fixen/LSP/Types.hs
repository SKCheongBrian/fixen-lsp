module Fixen.LSP.Types (
  Config,
  HoverInfo (..),
  DocumentAnalysis (..),
) where

import Data.Text qualified as Text
import Language.LSP.Protocol.Types (Range)

type Config = ()

data HoverInfo = HoverInfo
  { hoverName :: Text.Text
  , hoverRange :: Maybe Range
  , hoverContents :: Text.Text
  }
  deriving (Eq, Show)

data DocumentAnalysis = DocumentAnalysis
  { analysisContents :: Text.Text
  , analysisRelationNames :: [Text.Text]
  , analysisHoverInfo :: [HoverInfo]
  }
  deriving (Eq, Show)

