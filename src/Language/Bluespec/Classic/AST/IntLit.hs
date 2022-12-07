-- This corresponds to src/comp/IntLit.hs in bsc.
module Language.Bluespec.Classic.AST.IntLit where

data IntLit = IntLit { ilWidth :: Maybe Integer,
                       ilBase  :: Integer,
                       ilValue :: Integer }
