{-# LANGUAGE FlexibleInstances #-}
-- This corresponds to src/comp/Position.hs in bsc.
module Language.Bluespec.Classic.AST.Position
  ( Position(..)
  , bestPosition
  , noPosition
  , updatePosStdlib
  , HasPosition(..)
  ) where

import Text.PrettyPrint.HughesPJClass

import Language.Bluespec.Prelude

-- For now, we don't track positions, although we may do so in the future.
data Position = NoPos
  deriving (Eq, Ord, Show)

instance Pretty Position where
    pPrintPrec _ _ NoPos = text "<NoPos>"

bestPosition :: Position -> Position -> Position
bestPosition p1 p2 = if p1 == noPosition then p2 else p1

noPosition :: Position
noPosition = NoPos

updatePosStdlib :: Position -> Bool -> Position
updatePosStdlib pos _is_stdlib = pos

class HasPosition a where
    getPosition :: a -> Position

instance HasPosition Position where
    getPosition p = p

instance (HasPosition a) => HasPosition (Maybe a) where
    getPosition (Just x) = getPosition x
    getPosition Nothing = noPosition

instance (HasPosition a, HasPosition b) => HasPosition (Either a b) where
    getPosition (Right x) = getPosition x
    getPosition (Left x) = getPosition x

instance (HasPosition a) => HasPosition [a] where
    getPosition [] = noPosition
    getPosition (x:xs) = getPosition x `bestPosition` getPosition xs

instance (HasPosition a, HasPosition b) => HasPosition (a, b) where
    getPosition (x, y) = getPosition x `bestPosition` getPosition y

instance (HasPosition a, HasPosition b, HasPosition c) => HasPosition (a, b, c) where
    getPosition (x, y, z) = getPosition x `bestPosition` getPosition y `bestPosition` getPosition z

instance (HasPosition a, HasPosition b, HasPosition c, HasPosition d) => HasPosition (a, b, c, d) where
    getPosition (x, y, z, w) = getPosition x `bestPosition` getPosition y `bestPosition` getPosition z `bestPosition` getPosition w

instance HasPosition String where
    getPosition _x = noPosition
