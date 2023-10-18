-- This adapts definitions found in src/comp/PPrint.hs and src/comp/Pretty.hs
-- in bsc.
module Language.Bluespec.Pretty
  ( module Text.PrettyPrint

  , PDetail(..)

  , pparen
  , sepList
  , catList
  , maxPrec
  , lineWidth
  , linePref
  , pretty

  , s2par
  ) where

import Text.PrettyPrint

import Language.Bluespec.Prelude

data PDetail = PDReadable | PDAll | PDDebug | PDMark | PDInfo | PDNoqual deriving (Eq, Ord, Show)

pparen :: Bool -> Doc -> Doc
pparen False x = x
pparen True  x = text"(" <> x <> text")"

sepList :: [Doc] -> Doc -> Doc
sepList [] _ = empty
sepList xs s = sep (map (\x->x <> s) (init xs) ++ [last xs])

catList :: [Doc] -> Doc -> Doc
catList [] _ = empty
catList xs s = cat (map (\x->x <> s) (init xs) ++ [last xs])

maxPrec :: Int
maxPrec = 20

lineWidth, linePref :: Int
lineWidth = 120
linePref = 100

-- Produces a string from a text "x" in Normal mode with "w" line
-- length, "w/m" ribbons per line.
pretty :: Int -> Int -> Doc -> String
pretty w m x = fullRender PageMode w (toEnum w / toEnum m) string_txt "\n" x

-- The function which tells fullRender how to compose Doc elements
-- into a String.
string_txt :: TextDetails -> String -> String
string_txt (Chr c)   s  = c:s
string_txt (Str s1)  s2 = s1 ++ s2
string_txt (PStr s1) s2 = s1 ++ s2

-- Pretty printing utilities

-- Creates a paragraph
s2par :: String -> Doc
s2par str = fsep $ map text (words str)
