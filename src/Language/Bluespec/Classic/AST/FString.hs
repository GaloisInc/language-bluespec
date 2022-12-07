-- This corresponds to src/comp/FStringCompat.hs in bsc.
module Language.Bluespec.Classic.AST.FString where

import Language.Bluespec.Classic.AST.SString

-- wrapper to make SStrings look like FStrings

newtype FString = FString SString
