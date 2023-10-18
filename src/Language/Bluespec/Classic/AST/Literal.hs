-- This corresponds to src/comp/Literal.hs in bsc.
module Language.Bluespec.Classic.AST.Literal
  ( Literal(..)
  ) where

import Language.Bluespec.Classic.AST.IntLit
import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.Prelude
import Language.Bluespec.SystemVerilog.AST.Pretty

data Literal
        = LString String
        | LChar Char
        | LInt IntLit
        | LReal Double
        | LPosition -- a position literal is a placeholder for the position in CLiteral
        deriving (Eq, Ord, Show)

instance PPrint Literal where
    pPrint _ _ (LString s) = text (show s)
    pPrint _ _ (LChar c) = text (show c)
    pPrint d p (LInt i) = pPrint d p i
    pPrint d p (LReal r) = pPrint d p r
    pPrint _ _ LPosition = text ("<Position>")

instance PVPrint Literal where
    pvPrint _ _ (LString s) = text (show s)
    pvPrint _ _ (LChar c) = text (show [c])
    pvPrint d p (LInt i) = pvPrint d p i
    pvPrint d p (LReal r) = pvPrint d p r
    pvPrint _ _ LPosition = text ("<Position>")
