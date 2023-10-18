module Language.Bluespec.SystemVerilog.AST.Pretty
  ( module Language.Bluespec.Pretty

  , PVPrint(..)
  , pvpReadable
  , pvpReadableIndent
  , pvpAll
  , pvpDebug
  , pvpr
  ) where

import Debug.Trace (trace)

import Language.Bluespec.Prelude
import Language.Bluespec.Pretty
import Language.Bluespec.Util (itos)

class PVPrint a where
    pvPrint :: PDetail -> Int -> a -> Doc

pvpReadable :: (PVPrint a) => a -> String
pvpReadable = pvpr PDReadable

pvpReadableIndent :: (PVPrint a) => Int -> a -> String
pvpReadableIndent i = pvprIndent i PDReadable

pvpAll :: (PVPrint a) => a -> String
pvpAll = pvpr PDAll

pvpDebug :: (PVPrint a) => a -> String
pvpDebug = pvpr PDDebug

pvpr :: PVPrint a => PDetail -> a -> String
pvpr d = pretty lineWidth linePref . pvPrint d 0

pvprIndent :: PVPrint a => Int -> PDetail -> a -> String
pvprIndent i d = pretty lineWidth linePref . nest i . pvPrint d 0

instance PVPrint Int where
    pvPrint _ _ x = text (itos x)

instance PVPrint Integer where
    pvPrint _ _ x = text (itos x)

instance PVPrint Bool where
    pvPrint _ _ x = text (show x)

instance PVPrint Char where
    pvPrint _ _ x = text (show x)

instance PVPrint Double where
    pvPrint _ _ x = text (show x)

instance PVPrint Float where
    pvPrint _ _ x = text (show x)

instance (PVPrint a, PVPrint b) => PVPrint (a, b) where
    pvPrint d _ (x, y) = text "(" <> sep [pvPrint d 0 x <> text ",", pvPrint d 0 y] <> text ")"

instance (PVPrint a, PVPrint b, PVPrint c) => PVPrint (a, b, c) where
    pvPrint d _ (x, y, z) = text "(" <> sep [pvPrint d 0 x <> text ",", pvPrint d 0 y <> text ",", pvPrint d 0 z] <> text ")"

instance (PVPrint a, PVPrint b, PVPrint c, PVPrint d) => PVPrint (a, b, c, d) where
    pvPrint d _ (x, y, z, w) = text "(" <> sep [pvPrint d 0 x <> text ",", pvPrint d 0 y <> text ",", pvPrint d 0 z <> text ",", pvPrint d 0 w] <> text ")"

instance (PVPrint a, PVPrint b, PVPrint c, PVPrint d, PVPrint e) => PVPrint (a, b, c, d, e) where
    pvPrint d _ (x, y, z, w, v) = text "(" <> sep [pvPrint d 0 x <> text ",", pvPrint d 0 y <> text ",", pvPrint d 0 z <> text ",", pvPrint d 0 w <> text ",", pvPrint d 0 v] <> text ")"

instance (PVPrint a) => PVPrint [a] where
    pvPrint _d _ [] = text "[]"
    pvPrint  d _ xs =
        case reverse (map (pvPrint d 0) xs) of
        (y:ys) ->
                    let ys' = map (<> text ",") ys
                        xs' = reverse (y:ys')
--                    in  text "[" <> csep xs' <> text "]"
                    in  text "[" <> sep xs' <> text "]"
        [] -> trace "This cannot happen" (text "[]")

instance (PVPrint a, PVPrint b) => PVPrint (Either a b) where
    pvPrint d p (Left x) = pvparen (p>9) (text"(Left" <+> pvPrint d 10 x <> text")")
    pvPrint d p (Right x) = pvparen (p>9) (text"(Right" <+> pvPrint d 10 x <> text")")

instance (PVPrint a) => PVPrint (Maybe a) where
    pvPrint _ _ Nothing = text"Nothing"
    pvPrint d p (Just x) = pvparen (p>9) (text"Just (" <> pvPrint d 10 x <> text")")
pvparen :: Bool -> Doc -> Doc
pvparen False x = x
pvparen True  x = text"(" <> x <> text")"
