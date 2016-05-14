module Main where

import System.Environment
import qualified Data.Text.Lazy.IO as T

import Unigrams
import Common
import Vocabulary

-- | Tiene como entrada varios ficheros de entrada
-- - El Volcabulario
-- - El corpus rel
-- - El corpus norel
main :: IO ()
main = do paramenters <- getArgs
          case paramenters of
            [vocabularyFile, corpus1File, corpus2File] ->
                do vocabulary <- parseVocabulary <$> T.readFile vocabularyFile
                   corpus1 <- flip parseCorpus vocabulary <$> T.readFile corpus1File
                   corpus2 <- flip parseCorpus vocabulary <$> T.readFile corpus2File
                   T.writeFile (corpus1File ++ "learn") (writeCorpus (len vocabulary) corpus1)
                   T.writeFile (corpus2File ++ "learn") (writeCorpus (len vocabulary) corpus2)
            [corpustodo] ->
                do Corpus text len <- parseFile <$> T.readFile corpustodo
                   let text' = getWordsNoDuplicate text
                   T.writeFile "vocabulary.txt" (textToFile text' (header text'))

            _ -> putStrLn ("Entrada incorrecta de archivos!! \n"
                          ++ "USAGE: ./executable vocabulary corpus1 corpus2")


