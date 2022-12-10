-- This corresponds to src/comp/SpeedyString.hs in bsc.
module Language.Bluespec.Classic.AST.SString
  ( SString
  ) where

import Data.Text (Text)

-- bsc represents SStrings as interned strings, but we simplify things as bit
-- and represent them as Text.
type SString = Text
