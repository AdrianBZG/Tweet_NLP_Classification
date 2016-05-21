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

parseCorpus :: T.Text -> Corpus -> (M.Map T.Text Int, Int, Int)
parseCorpus file voc = let Corpus w lines = parseFile file
                           words          = countWords w
                       in (mixVoc words, length $ M.toList words, lines)
    where mixVoc :: M.Map T.Text Int -> M.Map T.Text Int
          mixVoc mapWords = foldl' (\mapWords word ->
                                         case M.lookup word mapWords of
                                           Nothing -> M.insert word 0 mapWords
                                           Just _ -> mapWords
                                   ) mapWords (linesCorpus voc)


writeCorpus :: Int -> (M.Map T.Text Int, Int, Int) -> T.Text
writeCorpus vocCuantity (mapCorpus, numWords, lines)
    = TB.toLazyText $ M.foldlWithKey'
      (\acc word num ->
          let prob = getProbabilityLog num vocCuantity numWords
          in acc `mappend` TB.fromLazyText (format word num prob))
      (TB.fromLazyText header)
      mapCorpus
  where format word frec prob = T.concat
                                [ "Palabra:"
                                , word
                                , " Frec:"
                                , (T.pack $ show frec)
                                , " LogProb:"
                                , (T.pack $ show prob)
                                , "    Aux:: num  "
                                , (T.pack $ show numWords)
                                , "  voc "
                                , (T.pack $ show vocCuantity)
                                , "\n"
                                ]
        header = "Numero de documentos del corpus:" `mappend` (T.pack $ show lines) `mappend` "\n"
                 `mappend` "Numero de palabras del corpus:" `mappend` (T.pack $ show numWords) `mappend` "\n"
