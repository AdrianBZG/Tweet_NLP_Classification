{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text as TS
import qualified Data.Set as S
import           Data.Foldable
import           Data.Attoparsec.Text.Lazy as P
import qualified Data.Map.Strict as M
import           Data.Char
import           Control.Category ((>>>))


type CorpusWords = [T.Text]

data Corpus = Corpus
                  { linesCorpus :: [T.Text]
                  , len :: Int
                  } deriving (Show, Eq)

-- Palabra:
parseVocabulary :: T.Text -> Corpus
parseVocabulary file = let (header:rest) = T.lines file
                       in Corpus (T.drop 8 <$> rest) (read $ T.unpack $ T.drop 19 header)
