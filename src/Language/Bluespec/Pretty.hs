-- This adapts definitions found in src/comp/PPrint.hs and src/comp/Pretty.hs
-- in bsc.
module Language.Bluespec.Pretty
  ( module Text.PrettyPrint

  , PDetail(..)

  , pparen
  , sepList
  , maxPrec

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

maxPrec :: Int
maxPrec = 20

-- Pretty printing utilities

-- Creates a paragraph
s2par :: String -> Doc
s2par str = fsep $ map text (words str)
