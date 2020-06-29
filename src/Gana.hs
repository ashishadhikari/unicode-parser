module Gana (
  Matra(..),
  Gana(..),
  toMatra,
  toGana
) where

import Akshara
import qualified Data.List.Split

data Matra = L | G
  deriving (Show, Eq)

data Gana = Y | M | T | R | J | B | N | S | Gana Matra
  deriving (Show, Eq)

toMatraSingle :: Akshara -> Matra
toMatraSingle a
  | isLaghu a = L
  | otherwise = G

toMatra :: [Akshara] -> [Matra]
toMatra = map toMatraSingle

toGana :: [Matra] -> [Gana]
toGana ms = concatMap toGanaFromThreeOrLessMatras (splitMatras ms)

splitMatras :: [Matra] -> [[Matra]]
splitMatras = Data.List.Split.chunksOf 3

toGanaFromThreeOrLessMatras :: [Matra] -> [Gana]
toGanaFromThreeOrLessMatras [] = []
toGanaFromThreeOrLessMatras [m] = [Gana m]
toGanaFromThreeOrLessMatras [m, m'] = [Gana m, Gana m']
toGanaFromThreeOrLessMatras [L, G, G] = [Y]
toGanaFromThreeOrLessMatras [G, G, G] = [M]
toGanaFromThreeOrLessMatras [G, G, L] = [T]
toGanaFromThreeOrLessMatras [G, L, G] = [R]
toGanaFromThreeOrLessMatras [L, G, L] = [J]
toGanaFromThreeOrLessMatras [G, L, L] = [B]
toGanaFromThreeOrLessMatras [L, L, L] = [N]
toGanaFromThreeOrLessMatras [L, L, G] = [S]
