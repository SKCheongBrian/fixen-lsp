module Fixen.LSP.Types (
  Config,
  DocumentAnalysis (..),
) where

import Data.Text qualified as Text

type Config = ()

data DocumentAnalysis = DocumentAnalysis
  { analysisRelationNames :: [Text.Text]
  }
  deriving (Eq, Show)

