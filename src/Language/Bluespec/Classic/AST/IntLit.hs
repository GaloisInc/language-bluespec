-- This corresponds to src/comp/IntLit.hs in bsc.
module Language.Bluespec.Classic.AST.IntLit
  ( IntLit(..)
  , ilDec
  , ilSizedDec
  , ilHex
  , ilSizedHex
  , ilBin
  , ilSizedBin
  ) where

import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.IntegerUtil
import Language.Bluespec.Prelude

data IntLit = IntLit { ilWidth :: Maybe Integer,
                       ilBase  :: Integer,
                       ilValue :: Integer }

instance Eq IntLit where
     IntLit { ilValue = i1 } == IntLit { ilValue = i2 }  =  i1 == i2
     IntLit { ilValue = i1 } /= IntLit { ilValue = i2 }  =  i1 /= i2

instance Ord IntLit where
     IntLit { ilValue = i1 } <= IntLit { ilValue = i2 }  =  i1 <= i2
     IntLit { ilValue = i1 } <  IntLit { ilValue = i2 }  =  i1 <  i2
     IntLit { ilValue = i1 } >= IntLit { ilValue = i2 }  =  i1 >= i2
     IntLit { ilValue = i1 } >  IntLit { ilValue = i2 }  =  i1 >  i2
     IntLit { ilValue = i1 } `compare` IntLit { ilValue = i2 }  =  i1 `compare` i2

instance Show IntLit where
     showsPrec _ (IntLit { ilValue = i, ilWidth = _mw, ilBase = b }) s =
         -- width of 0 means don't pad with leading zeros
         integerFormatPref 0 b i ++ s

instance PPrint IntLit where
     pPrint _d _p i = text (show i)

ilDec :: Integer -> IntLit
ilDec i = IntLit { ilWidth = Nothing, ilBase = 10, ilValue = i }

ilSizedDec :: Integer -> Integer -> IntLit
ilSizedDec w i = IntLit { ilWidth = Just w, ilBase = 10, ilValue = i }

ilHex :: Integer -> IntLit
ilHex i = IntLit { ilWidth = Nothing, ilBase = 16, ilValue = i }

ilSizedHex :: Integer -> Integer -> IntLit
ilSizedHex w i = IntLit { ilWidth = Just w, ilBase = 16, ilValue = i }

ilBin :: Integer -> IntLit
ilBin i = IntLit { ilWidth = Nothing, ilBase = 2,  ilValue = i }

ilSizedBin :: Integer -> Integer -> IntLit
ilSizedBin w i = IntLit { ilWidth = Just w, ilBase = 2, ilValue = i }
