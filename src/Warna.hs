module Warna (
  Warna(..),
  lexer,
  multiLineLexer,
  isVowel,
  isPostVowelMarker,
  isConsonant,
  isSpace,
  isHraswa,
  isDeergha,
  toChars,
) where

import qualified Text.Show.Unicode as TSU
import qualified GHC.Unicode as GhcUnicode

type Warna = Char

isSpace :: Warna -> Bool
isSpace c = c == ' '

isVowel :: Warna -> Bool
isVowel w = any ($w) [isHraswa, isDeergha]

isHraswa :: Warna -> Bool
isHraswa c = c `elem` "अइउऋ"

isDeergha :: Warna -> Bool
isDeergha c = c `elem` "आईऊएऐओऔ"

isPostVowelMarker :: Warna -> Bool
isPostVowelMarker c = c == 'ँ' || c == 'ं' || c == 'ः'

isConsonant :: Warna -> Bool
isConsonant c = c >= 'क' && c <= 'ह'

isHalantaMarker :: Char -> Bool
isHalantaMarker c = c == '्'

isVowelMarker :: Char -> Bool
isVowelMarker c = c /= markerToVowel c

isIgnoreChar :: Char -> Bool
isIgnoreChar c = isPunctuation c || isNumber c || GhcUnicode.isSpace c

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` ",।॥ऽ-:!@#%&*()\"'/“”‘’\x200C\x200D"

isNumber :: Char -> Bool
isNumber c = c >= '०' && c <= '९'

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

toChar :: Warna -> Char
toChar c = c

toChars :: [Warna] -> String
toChars = map toChar

charToToken :: Char -> Warna
charToToken c = c

multiLineLexer :: String -> [[Warna]]
multiLineLexer multiLineString = map lexer (lines multiLineString)

lexer :: String -> [Warna]
lexer [] = []
lexer (c:cs)
      | isIgnoreChar c = lexIgnore (c:cs)
      | isConsonant c = lexConsonant c cs
lexer (c:cs) = charToToken c : lexer cs

lexIgnore :: String -> [Warna]
lexIgnore cs' =
  case span isIgnoreChar cs' of
    (cs, []) -> []
    (cs, rest) -> ' ' : lexer rest

lexConsonant :: Char -> String -> [Warna]
lexConsonant c [] = [charToToken c, 'अ']
lexConsonant c (c':c's)
  | isVowelMarker c' = charToToken c : charToToken (markerToVowel c') : lexer c's
  | isHalantaMarker c' = charToToken c : lexer c's
  | otherwise = charToToken c : 'अ' : lexer (c':c's)