-- This adapts definitions found in src/comp/PPrint.hs and src/comp/Pretty.hs
-- in bsc.
module Language.Bluespec.Pretty
  ( PDetail
  , pdReadable
  , pdAll
  , pdDebug
  , pdMark
  , pdInfo
  , pdNoqual

  , ppReadable
  , ppReadableIndent
  , ppAll
  , ppDebug

  , pparen
  , sepList
  , ppr
  , maxPrec

  , s2par
  ) where

import Text.PrettyPrint.HughesPJClass

import Language.Bluespec.Prelude

type PDetail = PrettyLevel

pdReadable :: PDetail
pdReadable = PrettyLevel 1

pdAll :: PDetail
pdAll = PrettyLevel 2

pdDebug :: PDetail
pdDebug = PrettyLevel 3

pdMark :: PDetail
pdMark = PrettyLevel 4

pdInfo :: PDetail
pdInfo = PrettyLevel 5

pdNoqual :: PDetail
pdNoqual = PrettyLevel 6

ppReadable :: Pretty a => a -> String
ppReadable = ppr pdReadable

ppReadableIndent :: Pretty a => Int -> a -> String
ppReadableIndent i = pprIndent i pdReadable

ppAll :: Pretty a => a -> String
ppAll = ppr pdAll

ppDebug :: Pretty a => a -> String
ppDebug = ppr pdDebug

pparen :: Bool -> Doc -> Doc
pparen False x = x
pparen True  x = text"(" <> x <> text")"

sepList :: [Doc] -> Doc -> Doc
sepList [] _ = empty
sepList xs s = sep (map (\x->x <> s) (init xs) ++ [last xs])

maxPrec :: Int
maxPrec = 20

ppr :: Pretty a => PDetail -> a -> String
ppr d = pretty lineWidth linePref . pPrintPrec d 0

pprIndent :: Pretty a => Int -> PDetail -> a -> String
pprIndent i d = pretty lineWidth linePref . nest i . pPrintPrec d 0

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
