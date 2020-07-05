{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lambda where

import Akshara
import Aws.Lambda
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Gana
import Warna

newtype Input
  = Input {codePoints :: [Int]}
  deriving (Generic, ToJSON, FromJSON)

data Result = Result
  { ws :: [Int],
    as :: [[Int]],
    ms :: String,
    gs :: String
  }
  deriving (Generic, ToJSON)

handler :: Input -> Context () -> IO (Either String Result)
handler (Input cs) context =
  if length cs > 2000
    then pure (Left "Input is too long")
    else pure (Right (createResultV2 (map toEnum cs)))

createResultV2 :: String -> Result
createResultV2 input =
  let ws = lexer input
   in let as = toAkshara ws
       in let ms = toMatra as
           in let gs = toGana ms
               in Result (toWs ws) (toAs as) (toMs ms) (toGs gs)

toWs :: [Warna] -> [Int]
toWs = map fromEnum . toChars

toAs :: [Akshara] -> [[Int]]
toAs = map toCodePoints

toMs :: [Matra] -> String
toMs = map matraToChar

toGs :: [Gana] -> String
toGs = map ganaToChar
