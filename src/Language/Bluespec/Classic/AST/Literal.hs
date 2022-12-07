-- This corresponds to src/comp/Literal.hs in bsc.
module Language.Bluespec.Classic.AST.Literal where

import Language.Bluespec.Classic.AST.IntLit

data Literal
        = LString String
        | LChar Char
        | LInt IntLit
        | LReal Double
        | LPosition -- a position literal is a placeholder for the position in CLiteral
