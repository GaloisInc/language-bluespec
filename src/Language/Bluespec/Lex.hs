-- This corresponds to src/comp/Lex.hs in bsc.
module Language.Bluespec.Lex
  ( isIdChar
  , isSym
  ) where

import Data.Char (isAlphaNum)

import Language.Bluespec.Prelude

isSym :: Char -> Bool
isSym '!' = True; isSym '@' = True; isSym '#' = True; isSym '$' = True
isSym '%' = True; isSym '&' = True; isSym '*' = True; isSym '+' = True
isSym '.' = True; isSym '/' = True; isSym '<' = True; isSym '=' = True
isSym '>' = True; isSym '?' = True; isSym '\\' = True; isSym '^' = True
isSym '|' = True; isSym ':' = True; isSym '-' = True; isSym '~' = True
isSym ',' = True
isSym c | c >= '\x80' = c `elem` ['\162', '\163', '\164', '\165', '\166',
                                  '\167', '\168', '\169', '\170', '\171',
                                  '\172', '\173', '\174', '\175', '\176',
                                  '\177', '\178', '\179', '\180', '\181',
                                  '\183', '\184', '\185', '\186', '\187',
                                  '\188', '\189', '\190', '\191', '\215',
                                  '\247' ]

--isSym c | c >= '\x80' = isSymbol c
isSym _ = False

isIdChar :: Char -> Bool
isIdChar '\'' = True
isIdChar '_' = True

-- \176 (hexB0) (octal 0260) is the degree symbol
isIdChar '\176' = True
-- \180 (hexB4) (octal 0264) is the prime (acute accent) symbol
isIdChar '\180' = True
isIdChar c = isAlphaNum c
