-- This corresponds to src/comp/IntLit.hs in bsc.
module Language.Bluespec.Classic.AST.IntLit
  ( IntLit(..)
  ) where

import Text.PrettyPrint.HughesPJClass

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

instance Pretty IntLit where
     pPrintPrec _d _p i = text (show i)
