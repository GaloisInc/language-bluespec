-- This corresponds to src/comp/PPrint.hs in bsc.
module Language.Bluespec.Classic.AST.Pretty
  ( module Language.Bluespec.Pretty

  , PPrint(..)
  , ppReadable
  , ppReadableIndent
  , ppAll
  , ppDebug
  , ppr
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.Bluespec.Prelude
import Language.Bluespec.Pretty
import Language.Bluespec.Util (itos)

class PPrint a where
    pPrint :: PDetail -> Int -> a -> Doc

ppReadable :: (PPrint a) => a -> String
ppReadable = ppr PDReadable

ppReadableIndent :: (PPrint a) => Int -> a -> String
ppReadableIndent i = pprIndent i PDReadable

ppAll :: (PPrint a) => a -> String
ppAll = ppr PDAll

ppDebug :: (PPrint a) => a -> String
ppDebug = ppr PDDebug

ppr :: PPrint a => PDetail -> a -> String
ppr d = pretty lineWidth linePref . pPrint d 0

pprIndent :: PPrint a => Int -> PDetail -> a -> String
pprIndent i d = pretty lineWidth linePref . nest i . pPrint d 0

instance PPrint Int where
    pPrint _ _ x = text (itos x)

instance PPrint Integer where
    pPrint _ _ x = text (itos x)

instance PPrint Bool where
    pPrint _ _ x = text (show x)

instance PPrint Char where
    pPrint _ _ x = text (show x)

-- instance PPrint String where
--     pPrint _ _ x = doubleQuotes $ text x

instance PPrint Double where
    pPrint _ _ x = text (show x)

instance PPrint Float where
    pPrint _ _ x = text (show x)

instance (PPrint a, PPrint b) => PPrint (a, b) where
    pPrint d _ (x, y) = text "(" <> sep [pPrint d 0 x <> text ",", pPrint d 0 y] <> text ")"

instance (PPrint a, PPrint b, PPrint c) => PPrint (a, b, c) where
    pPrint d _ (x, y, z) = text "(" <> sep [pPrint d 0 x <> text ",", pPrint d 0 y <> text ",", pPrint d 0 z] <> text ")"

instance (PPrint a, PPrint b, PPrint c, PPrint d) => PPrint (a, b, c, d) where
    pPrint d _ (x, y, z, w) = text "(" <> sep [pPrint d 0 x <> text ",", pPrint d 0 y <> text ",", pPrint d 0 z <> text ",", pPrint d 0 w] <> text ")"

instance (PPrint a, PPrint b, PPrint c, PPrint d, PPrint e) => PPrint (a, b, c, d, e) where
    pPrint d _ (x, y, z, w, v) = text "(" <> sep [pPrint d 0 x <> text ",", pPrint d 0 y <> text ",", pPrint d 0 z <> text ",", pPrint d 0 w <> text ",", pPrint d 0 v] <> text ")"

instance (PPrint a, PPrint b, PPrint c, PPrint d, PPrint e, PPrint f) => PPrint (a, b, c, d, e, f) where
    pPrint d _ (x, y, z, w, v, u) = text "(" <> sep [pPrint d 0 x <> text ",", pPrint d 0 y <> text ",", pPrint d 0 z <> text ",", pPrint d 0 w <> text ",", pPrint d 0 v <> text ",", pPrint d 0 u] <> text ")"

instance (PPrint a, PPrint b, PPrint c, PPrint d, PPrint e, PPrint f, PPrint g) => PPrint (a, b, c, d, e, f, g) where
    pPrint d _ (x, y, z, w, v, u, t) = text "(" <> sep [pPrint d 0 x <> text ",", pPrint d 0 y <> text ",", pPrint d 0 z <> text ",", pPrint d 0 w <> text ",", pPrint d 0 v <> text ",", pPrint d 0 u <> text ",", pPrint d 0 t] <> text ")"

instance (PPrint a) => PPrint [a] where
    pPrint d _ xs = let xs' = map (pPrint d 0) xs
                    in  text "[" <> commaSep xs' <> text "]"

instance (PPrint a, PPrint b) => PPrint (Either a b) where
    pPrint d p (Left x) = pparen (p>9) (text"(Left" <+> pPrint d 10 x <> text")")
    pPrint d p (Right x) = pparen (p>9) (text"(Right" <+> pPrint d 10 x <> text")")

instance (PPrint a) => PPrint (Maybe a) where
    pPrint _ _ Nothing = text"Nothing"
    pPrint d p (Just x) = pparen (p>9) (text"Just (" <> pPrint d 10 x <> text")")

instance PPrint () where
    pPrint _ _ () = text "()"

instance (PPrint a, PPrint b) => PPrint (M.Map a b) where
    pPrint d _i m = vsep [pPrint d 0 k <+> text "->" <+> pPrint d 0 v
                         | (k, v) <- M.toList m]

instance (PPrint a, Ord a) => PPrint (S.Set a) where
    pPrint d i s = pPrint d i (S.toList s)

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

-- print a list with entries separated by commas, either
-- horizontally or vertically, depending on what fits
commaSep :: [Doc] -> Doc
commaSep [] = empty
commaSep ds = sep ([x <> comma | x <- init ds] ++ [last ds])
