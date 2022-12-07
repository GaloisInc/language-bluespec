-- This corresponds to src/comp/Id.hs in bsc.
module Language.Bluespec.Classic.AST.Id where

import Language.Bluespec.Classic.AST.FString

-- Ids are a bit more complicated than this in bsc, but we simplify things and
-- represent them as a simple FString.
newtype Id = Id FString

-- Long names

type Longname = [Id]
