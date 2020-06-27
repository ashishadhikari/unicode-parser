module Warna (
  Warna(..),
  lexer,
  multiLineLexer,
  isVowel,
  isPostVowelMarker,
  isConsonant,
  isSpace,
  isHraswa,
  isDeergha
) where

import qualified Text.Show.Unicode as TSU
import qualified GHC.Unicode as GhcUnicode

data Warna
  = Vowel Char
  | PostVowelMarker Char
  | Consonant Char
  | Space
  | Unknown Char
  deriving (Eq)

isSpace :: Warna -> Bool
isSpace Space = True
isSpace _ = False

isVowel :: Warna -> Bool
isVowel w = any ($w) [isHraswa, isDeergha]

isHraswa :: Warna -> Bool
isHraswa (Vowel c) = c `elem` "अइउऋ"
isHraswa _ = False

isDeergha :: Warna -> Bool
isDeergha (Vowel c) = c `elem` "आईऊएऐओऔ"
isDeergha _ = False

isPostVowelMarker :: Warna -> Bool
isPostVowelMarker (PostVowelMarker c) = case c of
  'ँ' -> True
  'ं' -> True
  'ः' -> True
  _ -> False
isPostVowelMarker _ = False

isConsonant :: Warna -> Bool
isConsonant (Consonant c)
  | c < 'क' = False
  | c <= 'ह' = True
  | otherwise = False
isConsonant _ = False

isHalantaMarker :: Char -> Bool
isHalantaMarker c = c == '्'

isVowelMarker :: Char -> Bool
isVowelMarker c = c /= markerToVowel c

markerToVowel :: Char -> Char
markerToVowel c = case c of
  'ा' -> 'आ'
  'ि' -> 'इ'
  'ी' -> 'ई'
  'ु' -> 'उ'
  'ू' -> 'ऊ'
  'ृ' -> 'ऋ'
  'े' -> 'ए'
  'ै' -> 'ऐ'
  'ो' -> 'ओ'
  'ौ' -> 'औ'
  c -> c

instance Show Warna where
  show (Vowel c) = TSU.ushow c
  show (Consonant c) = TSU.ushow c
  show (PostVowelMarker c) = TSU.ushow c
  show Space = show ' '
  show (Unknown c) = TSU.ushow c

charToToken :: Char -> Warna
charToToken c
  | GhcUnicode.isSpace c = Space
  | isVowel (Vowel c) = Vowel c
  | isConsonant (Consonant c) = Consonant c
  | isPostVowelMarker (PostVowelMarker c) = PostVowelMarker c
  | otherwise = Unknown c

multiLineLexer :: String -> [[Warna]]
multiLineLexer multiLineString = map lexer (lines multiLineString)

lexer :: String -> [Warna]
lexer [] = []
lexer (c:cs)
      | GhcUnicode.isSpace c = Space : lexer cs
      | isConsonant (Consonant c) = lexConsonant c cs
lexer (c:cs) = charToToken c : lexer cs

lexConsonant :: Char -> String -> [Warna]
lexConsonant c [] = [charToToken c, Vowel 'अ']
lexConsonant c (c':c's)
  | isVowelMarker c' = charToToken c : charToToken (markerToVowel c') : lexer c's
  | isHalantaMarker c' = charToToken c : lexer c's
  | otherwise = charToToken c : Vowel 'अ' : lexer (c':c's)