{-# LANGUAGE OverloadedStrings #-}
module Unigrams where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import           Data.Foldable
import qualified Data.Map.Strict as M

import Common


parseVocabulary :: T.Text -> Corpus
parseVocabulary file = let (header:rest) = T.lines file
                       in Corpus (T.drop 8 <$> rest) (read $ T.unpack $ T.drop 19 header)

-- |
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


-- | Format result to output file
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
                                , "\n"
                                ]
        header = T.concat
                 [ "Numero de documentos del corpus:"
                 , (T.pack $ show lines)
                 , "\n"
                 , "Numero de palabras del corpus:"
                 , (T.pack $ show numWords)
                 , "\n"
                 ]

getNumDocuments :: T.Text -> Int
getNumDocuments = length . T.lines

-- | Count words
countWords :: [T.Text] -> M.Map T.Text Int
countWords words = foldl' (\mapCount word ->
                                     case M.lookup word mapCount of
                                       Nothing -> M.insert word 1 mapCount
                                       Just times -> M.insert word (times + 1) mapCount
                          ) M.empty words

-- | Calc probability
getProbabilityLog :: Int -> Int -> Int -> Double
getProbabilityLog nWord vocCuantity nValue =
    let nWord' = fromIntegral nWord
        vocCuantity' = fromIntegral vocCuantity
        nValue' = fromIntegral nValue
    in logBase 10 ((nWord' + 1) / (vocCuantity' + nValue' + 1))
