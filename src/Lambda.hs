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

newtype InputV2
  = InputV2 {codePointLines :: [[Int]]}
  deriving (Generic, ToJSON, FromJSON)

data Result = Result
  { ws :: [Int],
    as :: [[Int]],
    ms :: String,
    gs :: String
  }
  deriving (Generic, ToJSON)

isNotEmptyResult :: Result -> Bool
isNotEmptyResult (Result ws _ _ _) = not (null ws)

handler :: InputV2 -> Context () -> IO (Either String [Result])
handler (InputV2 ls) context =
  if isValidInput ls then
      pure (Right (handlerCoreMultiLine ls))
  else
      pure (Left "Input is too long")


handlerV1 :: Input -> Context () -> IO (Either String Result)
handlerV1 (Input cs) context =
  if length cs > 2000
    then pure (Left "Input is too long")
    else pure (Right (createResultV2 (map toEnum cs)))

handlerCoreMultiLine :: [[Int]] -> [Result]
handlerCoreMultiLine xs = filter isNotEmptyResult (map handlerCore xs)

handlerCore :: [Int] -> Result
handlerCore cs = createResultV2 (map toEnum cs)

isValidInput :: [[Int]] -> Bool
isValidInput xs = length xs <= 500

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
