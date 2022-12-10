-- This corresponds to src/comp/Literal.hs in bsc.
module Language.Bluespec.Classic.AST.Literal
  ( Literal(..)
  ) where

import Text.PrettyPrint.HughesPJClass

import Language.Bluespec.Classic.AST.IntLit
import Language.Bluespec.Prelude

data Literal
        = LString String
        | LChar Char
        | LInt IntLit
        | LReal Double
        | LPosition -- a position literal is a placeholder for the position in CLiteral
        deriving (Eq, Ord, Show)

instance Pretty Literal where
    pPrintPrec _ _ (LString s) = text (show s)
    pPrintPrec _ _ (LChar c) = text (show c)
    pPrintPrec d p (LInt i) = pPrintPrec d p i
    pPrintPrec d p (LReal r) = pPrintPrec d p r
    pPrintPrec _ _ LPosition = text ("<Position>")
