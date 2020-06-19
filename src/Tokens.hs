module Tokens where

import qualified NepaliUnicode as NU

data Token =
  T_Unknown
-- Space
  | T_Space
-- Vowels
  | T_a | T_aa | T_i | T_ii | T_u | T_uu
  | T_R | T_L
  | T_e | T_ai | T_o | T_au
-- Vowel Markers
  | T_m_aa | T_m_i | T_m_ii | T_m_u | T_m_uu
  | T_m_R | T_m_L
  | T_m_e | T_m_ai | T_m_o | T_m_au
-- Other markers
  | T_m_candrawindu
  | T_m_anuswar
  | T_m_wisarga
-- Consonants
  | T_ka | T_kha | T_ga | T_gha | T_nga
  | T_ca | T_cha | T_ja | T_jha | T_nya
  | T_Ta | T_Tha | T_Da | T_Dha | T_Na
  | T_ta | T_tha | T_da | T_dha | T_na
  | T_pa | T_pha | T_ba | T_bha | T_ma
  | T_ya | T_ra | T_la | T_wa
  | T_Sha | T_sha | T_sa
  | T_ha
  deriving (Show, Eq)
  

charToToken :: Char -> Token
-- space
charToToken c
  | NU.isSpace c = T_Space
--Vowels
charToToken 'अ' = T_a; charToToken 'आ' = T_aa
charToToken 'इ' = T_i; charToToken 'ई' = T_ii
charToToken 'उ' = T_u; charToToken 'ऊ' = T_uu
charToToken 'ऋ' = T_R; charToToken 'ऌ' = T_L
charToToken 'ए' = T_e; charToToken 'ऐ' = T_ai
charToToken 'ओ' = T_o; charToToken 'औ' = T_au
--Vowel markers
charToToken 'ा' = T_m_aa
charToToken 'ि' = T_m_i; charToToken 'ी' = T_m_ii
charToToken 'ु' = T_m_u; charToToken 'ू' = T_m_uu
charToToken 'ृ' = T_m_R; charToToken 'ॢ' = T_m_L
charToToken 'े' = T_m_e; charToToken 'ै' = T_m_ai
charToToken 'ो' = T_m_o; charToToken 'ौ' = T_m_au
--Other markers
charToToken 'ँ' = T_m_candrawindu
charToToken 'ं' = T_m_anuswar
charToToken 'ः' = T_m_wisarga
--Consonants
charToToken 'क' = T_ka; charToToken 'ख' = T_kha; charToToken 'ग' = T_ga; charToToken 'घ' = T_gha; charToToken 'ङ' = T_nga
charToToken 'च' = T_ca; charToToken 'छ' = T_cha; charToToken 'ज' = T_ja; charToToken 'झ' = T_jha; charToToken 'ञ' = T_nya
charToToken 'ट' = T_Ta; charToToken 'ठ' = T_Tha; charToToken 'ड' = T_Da; charToToken 'ढ' = T_Dha; charToToken 'ण' = T_Na
charToToken 'त' = T_ta; charToToken 'थ' = T_tha; charToToken 'द' = T_da; charToToken 'ध' = T_dha; charToToken 'न' = T_na
charToToken 'प' = T_pa; charToToken 'फ' = T_pha; charToToken 'ब' = T_ba; charToToken 'भ' = T_bha; charToToken 'म' = T_ma
charToToken 'य' = T_ya; charToToken 'र' = T_ra; charToToken 'ल' = T_la; charToToken 'व' = T_wa
charToToken 'श' = T_Sha; charToToken 'ष' = T_sha; charToToken 'स' = T_sa; charToToken 'ह' = T_ha
--Fallback
charToToken _ = T_Unknown

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | NU.isSpace c = T_Space : lexer cs
      | NU.isConsonant c = lexConsonant c cs
lexer (c:cs) = charToToken c : lexer cs

lexConsonant :: Char -> String -> [Token]
lexConsonant c [] = [charToToken c, T_a]
lexConsonant c (c':c's)
  | NU.isVowelMarker c' = charToToken c : charToToken (NU.markerToVowel c') : lexer c's
  | NU.isHalantaMarker c' = charToToken c : lexer c's
  | otherwise = charToToken c : T_a : lexer (c':c's)