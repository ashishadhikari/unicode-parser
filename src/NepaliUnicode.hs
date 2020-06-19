module NepaliUnicode where

import qualified GHC.Unicode as GhcUnicode

isConsonant :: Char -> Bool
isConsonant c = any ($c) [isStop, isSemiVowel, isSibilant]

isVowel :: Char -> Bool
isVowel c
  | c <= 'अ' = False
  | c <= 'औ' = True
  | otherwise = False

isVowelMarker :: Char -> Bool
isVowelMarker c = c /= markerToVowel c

markerToVowel :: Char -> Char
markerToVowel c
  | c == 'ा' = 'आ'
  | c == 'ि' = 'इ'
  | c == 'ी' = 'ई'
  | c == 'ु' = 'उ'
  | c == 'ू' = 'ऊ'
  | c == 'ृ' = 'ऋ'
  | c == 'े' = 'ए'
  | c == 'ै' = 'ऐ'
  | c == 'ो' = 'ओ'
  | c == 'ौ' = 'औ'
  | otherwise = c

isPostVowelMarker :: Char -> Bool
isPostVowelMarker c
  | c == 'ँ' = True
  | c == 'ं' = True
  | c == 'ः' = True
  | otherwise = False

isHalantaMarker :: Char -> Bool
isHalantaMarker c = c == '्'

isStop :: Char -> Bool
isStop c
  | c < 'क' = False
  | c <= 'म' = True
  | otherwise = False

isSemiVowel :: Char -> Bool
isSemiVowel c
  | c < 'य' = False
  | c <= 'व' = True
  | otherwise = False

isSibilant :: Char -> Bool
isSibilant c
  | c < 'श' = False
  | c <= 'ह' = True
  | otherwise = False

isSpace :: Char -> Bool
isSpace = GhcUnicode.isSpace
