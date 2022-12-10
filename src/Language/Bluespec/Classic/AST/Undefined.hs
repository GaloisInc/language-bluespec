-- This corresponds to src/comp/Undefined.hs in bsc.
module Language.Bluespec.Classic.AST.Undefined
  ( UndefKind(..)
  ) where

import Language.Bluespec.Prelude

-- Undefined values in BSC carry information about their origin.
-- (The evaluator uses this for choosing error messages and optimizations.)

-- * UNotUsed is for values that we expect will never be used, such as
--   in unreachable code or the return value for an expression whose value
--   is unused.

-- * UNoMatch is the value returned from a case expression when no arm
--   matches but some value still needs to be returned.

-- * UDontCare is an explicit dont-care value written by the user, or
--   any other dont-care value that doesn't fit the above kinds.

data UndefKind = UNotUsed | UDontCare | UNoMatch
  deriving (Eq, Ord, Show)
