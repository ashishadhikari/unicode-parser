module Warna where

import qualified NepaliUnicode as NU
import qualified Text.Show.Unicode as TSU

data Warna =
    Unknown
  | Space
  | Hraswa Char
  | Deergha Char
  | PostVowelMarker Char
  | Consonant Char
  deriving (Eq)

isSpace :: Warna -> Bool
isSpace Space = True
isSpace _ = False

isVowel :: Warna -> Bool
isVowel w = any ($w) [isHraswa, isDeergha]

isHraswa :: Warna -> Bool
isHraswa (Hraswa _) = True
isHraswa _ = False

isDeergha :: Warna -> Bool
isDeergha (Deergha _) = True
isDeergha _ = False

isPostVowelMarker :: Warna -> Bool
isPostVowelMarker (PostVowelMarker _) = True
isPostVowelMarker _ = False

isConsonant :: Warna -> Bool
isConsonant (Consonant _) = True
isConsonant _ = False

instance Show Warna where
  show (Hraswa c) = TSU.ushow c
  show (Deergha c) = TSU.ushow c
  show (Consonant c) = TSU.ushow c
  show (PostVowelMarker c) = TSU.ushow c
  show Space = show ' '
  show Unknown = TSU.ushow '☐'

charToToken :: Char -> Warna
charToToken c
  | NU.isSpace c = Space
  | NU.isHraswa c = Hraswa c
  | NU.isDeergha c = Deergha c
  | NU.isConsonant c = Consonant c
  | NU.isPostVowelMarker c = PostVowelMarker c
  | otherwise = Unknown

multiLineLexer :: String -> [[Warna]]
multiLineLexer multiLineString = map lexer (lines multiLineString)

lexer :: String -> [Warna]
lexer [] = []
lexer (c:cs)
      | NU.isSpace c = Space : lexer cs
      | NU.isConsonant c = lexConsonant c cs
lexer (c:cs) = charToToken c : lexer cs

lexConsonant :: Char -> String -> [Warna]
lexConsonant c [] = [charToToken c, Hraswa 'अ']
lexConsonant c (c':c's)
  | NU.isVowelMarker c' = charToToken c : charToToken (NU.markerToVowel c') : lexer c's
  | NU.isHalantaMarker c' = charToToken c : lexer c's
  | otherwise = charToToken c : Hraswa 'अ' : lexer (c':c's)