module Akshara (
  Akshara(..),
  toAkshara,
  toAksharaMultiLine,
  isLaghu,
  isGuru,
) where

import qualified Warna

data Akshara =
    Unknown
  | Akshara {
    preConsonants :: [Warna.Warna],
    vowel :: Warna.Warna,
    postVowelMarker :: Maybe Warna.Warna,
    postConsonants :: [Warna.Warna]
  } deriving (Show, Eq)

isLaghu :: Akshara -> Maybe Akshara -> Bool
isLaghu (Akshara _ v Nothing []) Nothing = Warna.isHraswa v
isLaghu (Akshara _ v Nothing []) (Just (Akshara [] _ _ _)) = Warna.isHraswa v
isLaghu (Akshara _ v Nothing []) (Just (Akshara [_] _ _ _)) = Warna.isHraswa v
isLaghu _ _ = False

isGuru :: Akshara -> Maybe Akshara -> Bool
isGuru a mA = not (isLaghu a mA)

fromPreConsAndVowel :: [Warna.Warna] -> Warna.Warna -> Akshara
fromPreConsAndVowel preConsonants vowel = Akshara preConsonants vowel Nothing []

addPostVowelMarker :: Akshara -> Warna.Warna -> Akshara
addPostVowelMarker (Akshara preConsonants vowel postVowelMarker postConsonants) w
  | Warna.isPostVowelMarker w && null postConsonants = Akshara preConsonants vowel (Just w) []
  | otherwise = Unknown

addPostConsonant :: Akshara -> Warna.Warna -> Akshara
addPostConsonant (Akshara preConsonants vowel postVowelMarker postConsonants) w
  | Warna.isConsonant w = Akshara preConsonants vowel postVowelMarker (postConsonants ++ [w])
  | otherwise = Unknown

addPostConsonants :: Akshara -> [Warna.Warna] -> Akshara
addPostConsonants (Akshara preConsonants vowel postVowelMarker postConsonants) ws =
  Akshara preConsonants vowel postVowelMarker (postConsonants ++ ws)

hasPostConsonants :: Akshara -> Bool
hasPostConsonants (Akshara _ _ _ postConsonants) = not (null postConsonants)

stripLastConsonant :: Akshara -> (Akshara, Maybe Warna.Warna)
stripLastConsonant (Akshara preC v mayBePostVm postC)
  | null postC = (Akshara preC v mayBePostVm postC, Nothing)
  | otherwise = (Akshara preC v mayBePostVm (init postC), Just (last postC))
stripLastConsonant a = (a, Nothing)

toAksharaMultiLine :: [[Warna.Warna]] -> [[Akshara]]
toAksharaMultiLine = map toAkshara

toAkshara :: [Warna.Warna] -> [Akshara]
toAkshara = toAksharaWithContext (Left [])

toAksharaWithContext :: Either [Warna.Warna] Akshara -> [Warna.Warna] -> [Akshara]
toAksharaWithContext (Left ws) [] = [Unknown]
toAksharaWithContext (Right a) [] = [a]
toAksharaWithContext (Left ws') (w:ws)
  | Warna.isConsonant w = toAksharaConsonant (Left ws') (w:ws)
  | Warna.isVowel w = toAksharaVowel (Left ws') (w:ws)
  | otherwise = Unknown : toAksharaWithContext (Left []) ws
toAksharaWithContext (Right a) (w:ws)
  | Warna.isConsonant w = toAksharaConsonant (Right a) (w:ws)
  | Warna.isVowel w = toAksharaVowel (Right a) (w:ws)
  | Warna.isPostVowelMarker w = toAksharaPostVowelMarker a (w:ws)
  | Warna.isSpace w = a : toAksharaWithContext (Left []) ws
  | otherwise = a : Unknown : toAksharaWithContext (Left []) ws

toAksharaConsonant :: Either [Warna.Warna] Akshara -> [Warna.Warna] -> [Akshara]
toAksharaConsonant (Right a) ws =
  case span Warna.isConsonant ws of
    (cs, rest) -> toAksharaWithContext (Right (addPostConsonants a cs)) rest
toAksharaConsonant (Left ws) ws' =
  case span Warna.isConsonant  ws' of
    (cs, rest) -> toAksharaWithContext (Left (ws ++ cs)) rest

toAksharaVowel :: Either [Warna.Warna] Akshara -> [Warna.Warna] -> [Akshara]
toAksharaVowel (Right a) (w:ws) =
  case stripLastConsonant a of
    (a', Just w') -> a' : toAksharaWithContext (Right (fromPreConsAndVowel [w'] w)) ws
    (a', Nothing) -> a' : toAksharaWithContext (Right (fromPreConsAndVowel [] w)) ws
toAksharaVowel (Left ws') (w:ws) =
  toAksharaWithContext (Right (fromPreConsAndVowel ws' w)) ws

toAksharaPostVowelMarker :: Akshara -> [Warna.Warna] -> [Akshara]
toAksharaPostVowelMarker a (w:ws) = toAksharaWithContext (Right (addPostVowelMarker a w)) ws
