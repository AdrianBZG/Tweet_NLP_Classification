{-# LANGUAGE OverloadedStrings #-}
module Common (parseFile, Corpus(..)) where

import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.Set as S
import           Data.Attoparsec.Text.Lazy as P
import           Data.Char
import           Control.Category ((>>>))

-- | Estructura para almacenar todas las palabras y el tamaño del corpus(no la cantidad de palabras)
data Corpus = Corpus
                  { linesCorpus :: [T.Text]
                  , len :: Int
                  } deriving (Show, Eq)

parseFile :: T.Text -> Corpus
parseFile file =
  let lines = T.lines file
      words = concatMap (T.drop 6
                         >>> T.words
                         >>> filter filterURL
                         >>> concatMap (T.toLower >>> weirdWords))
              (T.lines file)
  in Corpus words (length lines)


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
