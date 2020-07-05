module Chanda where

import Gana

chanda :: [Gana] -> ([Gana] -> Bool)
chanda gs = (gs ==)

bhujangaprayata :: [Gana] -> Bool
bhujangaprayata = chanda [Y, Y, Y, Y]


shardoolwikridita :: [Gana] -> Bool
shardoolwikridita = chanda [M, S, J, S, T, T, Gana G]
