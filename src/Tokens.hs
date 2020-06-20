module Tokens where

import qualified NepaliUnicode as NU
import qualified Text.Show.Unicode as TSU

-- TODO rename Token to Warna (वर्ण), then it can be fed into another lexer accepting Warna list
-- to produce Akshara
data Token =
    T_Unknown
  | T_Space
  | T_Newline
  | Hraswa Char
  | Deergha Char
  | PostVowelMarker Char
  | Consonant Char
  deriving (Eq)

instance Show Token where
  show (Hraswa c) = TSU.ushow c
  show (Deergha c) = TSU.ushow c
  show (Consonant c) = TSU.ushow c
  show (PostVowelMarker c) = TSU.ushow c
  show T_Space = show ' '
  show T_Newline = show '\n'
  show T_Unknown = TSU.ushow '☐'

charToToken :: Char -> Token
charToToken c
  | NU.isSpace c = T_Space
  | NU.isHraswa c = Hraswa c
  | NU.isDeergha c = Deergha c
  | NU.isConsonant c = Consonant c
  | NU.isPostVowelMarker c = PostVowelMarker c
  | otherwise = T_Unknown

multiLineLexer :: String -> [[Token]]
multiLineLexer multiLineString = map lexer (lines multiLineString)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | NU.isSpace c = T_Space : lexer cs
      | NU.isConsonant c = lexConsonant c cs
lexer (c:cs) = charToToken c : lexer cs

lexConsonant :: Char -> String -> [Token]
lexConsonant c [] = [charToToken c, Hraswa 'अ']
lexConsonant c (c':c's)
  | NU.isVowelMarker c' = charToToken c : charToToken (NU.markerToVowel c') : lexer c's
  | NU.isHalantaMarker c' = charToToken c : lexer c's
  | otherwise = charToToken c : Hraswa 'अ' : lexer (c':c's)