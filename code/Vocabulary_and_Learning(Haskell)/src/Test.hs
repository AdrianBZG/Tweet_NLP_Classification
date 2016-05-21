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

parseFile :: T.Text -> Corpus
parseFile file =
  let lines = T.lines file
      words = concatMap (T.drop 6
                         >>> T.words
                         >>> filter filterURL
                         >>> concatMap (T.toLower >>> weirdWords))
              (T.lines file)
  in Corpus words (length lines)

getNumDocuments :: T.Text -> Int
getNumDocuments = length . T.lines

-- | Filtramos todas las url, ya que son ruido
filterURL :: T.Text -> Bool
filterURL = not . T.isPrefixOf "http://"

-- | Se denominan palabras raras aquellas que al separarlas por espacios,
-- conforman una secuencia de letras, números y símbolos. Ej: Hola. ¡Que_Buen_Dia!! dinero$$
-- En el caso de los símbolos muchas repeticiones de ellos !!!! se reducen a !
weirdWords :: T.Text -> [T.Text]
weirdWords text =
    case eitherResult $ parse symAndWords text of
        Right a -> T.fromStrict <$> concat a
        Left _ -> []

    where symAndWords = P.many' $ choice
            [ ((: []) . ("#" `mappend`)) <$> (char '#' >> P.takeWhile1 (\x -> isAlphaNum x))
            , getImportantSymbols <$> P.takeWhile1 (\x -> isMark x || isPunctuation x || isSymbol x)
            , (: []) <$> P.takeWhile1 isLetter]


-- | Reduce los symbolos repetidos dentro de un texto
getImportantSymbols :: TS.Text -> [TS.Text]
getImportantSymbols = map (TS.pack . (: [])) . S.toList . S.fromList . TS.unpack

-- | Get all words without replicate
getWordsNoDuplicate :: CorpusWords -> CorpusWords
getWordsNoDuplicate = S.toList . S.fromList

-- | Count words
countWords :: CorpusWords -> M.Map T.Text Int
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
