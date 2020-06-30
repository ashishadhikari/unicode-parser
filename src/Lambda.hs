{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lambda where

import Akshara
import Aws.Lambda
import Data.Aeson (ToJSON)
import GHC.Generics
import Gana
import Warna

data Result = Result
  { warnas :: [Warna],
    aksharas :: [Akshara],
    matras :: [Matra],
    ganas :: [Gana]
  }
  deriving (Generic, ToJSON)

handler :: String -> Context () -> IO (Either String Result)
handler input context =
  if length input > 2000
    then pure (Left "Input is too long")
    else pure (Right (createResult input))

createResult :: String -> Result
createResult input =
  let ws = lexer input
   in let as = toAkshara ws
       in let ms = toMatra as
           in let gs = toGana ms
               in Result ws as ms gs
