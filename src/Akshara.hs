module Akshara where

import qualified Warna

data Akshara =
    Unknown
  | Space
  | Newline
  | Akshara {
    preConsonants :: [Warna.Warna],
    vowel :: Warna.Warna,
    postVowelMarker :: Maybe Warna.Warna,
    postConsonants :: [Warna.Warna]
  }