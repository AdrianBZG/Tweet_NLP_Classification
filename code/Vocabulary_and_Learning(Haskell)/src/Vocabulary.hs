{-# LANGUAGE OverloadedStrings #-}
module Vocabulary where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Set as S
import           Data.Foldable


-- | Get all words without replicate
getWordsNoDuplicate :: [T.Text] -> [T.Text]
getWordsNoDuplicate = S.toList . S.fromList

header text = "Numero de palabras:" `T.append` (T.pack $ show $ length text)

mkWord = ("\nPalabra:" `T.append`)

textToFile text header = TB.toLazyText $
  (foldl' (\acc str -> acc `mappend` TB.fromLazyText (mkWord str))
    (TB.fromLazyText header)
    text)
