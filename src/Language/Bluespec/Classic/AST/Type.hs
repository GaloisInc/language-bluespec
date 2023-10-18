{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This corresponds to src/comp/CType.hs in bsc.
module Language.Bluespec.Classic.AST.Type
  ( Type(..)
  , TyVar(..)
  , TyCon(..)
  , TISort(..)
  , StructSubType(..)
  , CType
  , Kind(..)
  , PartialKind(..)
  , CTypeclass(..)
  , CPred(..)
  , CQType(..)

  , baseKVar
  , cTNum
  , getArrows
  , isFn
  , isModule
  , isTConArrow
  , isTConPair
  , leftCon
  , newIds
  , unravel
  ) where

import Data.Char (chr)

import Language.Bluespec.Classic.AST.Builtin.Ids
import Language.Bluespec.Classic.AST.FString
import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.Pragma
import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.Prelude
import Language.Bluespec.SystemVerilog.AST.Pretty
import Language.Bluespec.Util

-- | Representation of types
data Type = TVar TyVar         -- ^ type variable
          | TCon TyCon         -- ^ type constructor
          | TAp Type Type      -- ^ type-level application
          | TGen Position Int  -- ^ quantified type variable used in type schemes
          | TDefMonad Position -- ^ not used after CVParserImperative
    deriving Show

instance Eq Type where
    x == y  =  cmp x y == EQ

instance Ord Type where
    compare x y = cmp x y

instance PPrint Type where
    pPrint _d _p (TCon (TyCon unit _ _)) | unit == idPrimUnit = text "()"
    pPrint  d _p (TCon c) = pPrint d 0 c
    pPrint  d _p (TVar i) = pPrint d 0 i
    pPrint  d  p (TAp (TAp (TCon pair) a) b) | isTConPair pair =
        pparen (p >= 0) (sep [pPrint d 0 a <> text ",", pPrint d (-1) b])
    pPrint d p (TAp (TAp (TCon arr) a) r) | isTConArrow arr =
        pparen (p > 8) (sep [pPrint d 9 a <+> text "->", pPrint d 8 r])
    pPrint d p (TAp e e') = pparen (p>9) $
        sep [pPrint d 9 e, pPrint d 10 e']
    pPrint _d _p (TDefMonad _) = text ("TDefMonad")
    pPrint  d  p (TGen _ n) = pparen True (text "TGen" <+> pPrint d p n)

instance PVPrint Type where
    pvPrint _d _p (TCon (TyCon special _ _))
        | special == idPrimUnit = text "void"
        -- These are needed when printing a function/method,
        -- because Action/ActionValue are keywords which introduce implicit
        -- action..endaction or actionvalue..endactionvalue blocks.
        -- It used to be that displaying "Prelude::Action" would print
        -- broken code, because then it's no longer using the keyword.
        -- So this code is to ensure that those places print the keyword,
        -- but it affects all other printing of the type, too.
        | special == idAction = text "Action"
        | special == idActionValue = text "ActionValue"
    pvPrint  d _p (TCon c) = pvPrint d 0 c
    pvPrint  d _p (TVar i) = pvPrint d 0 i
    pvPrint _d _p _ty@(TAp (TCon (TyCon av _ _)) (TCon (TyCon special _ _)))
        -- ActionValue#(void) ==> Action
        | (qualEq idActionValue av) && (qualEq special idPrimUnit)
            = text "Action"
    pvPrint d p ty@(TAp (TAp (TCon special) _a) _b)
        | isTConPair special =
            let ts = tUnmkTuple ty
                n  = length ts
            in  t"Tuple" <> t(show n) <> pvParameterTypes d ts
        | isTConArrow special =
            pparen (p > 8) (ppTypedId d Nothing (t"f") ty newIds)
--        pparen (p > 8) (sep [pvPrint d 9 a <+> text "->", pvPrint d 8 r])
    pvPrint d p (TAp e e') = pparen (p>9) $
        let (x, ys) = unravel (TAp e e')
        in pvPrint d 9 x <> t"#(" <> sepList (map (pvPrint d 0) ys) (text ",") <> t")"
    pvPrint _d _p (TGen _ n) = text ('_':show n)
    pvPrint _d _p (TDefMonad _) = text "default_module_type"

instance HasPosition Type where
    getPosition (TVar var) = getPosition var
    getPosition (TCon con) = getPosition con
    getPosition (TAp f a) = getPosition f `bestPosition` getPosition a
    getPosition (TGen pos _) = pos
    getPosition (TDefMonad pos) = pos

isFn :: Type -> Bool
isFn (TAp (TAp (TCon arr) _) _) = isTConArrow arr
isFn _         = False

newIds :: [Doc]
newIds = map t (newIdsn 1)

newIdsn :: Integer -> [String]
newIdsn n = ("x" ++ itos n):(newIdsn (n+1))

pp :: (PVPrint a) => PDetail -> a -> Doc
pp d x = pvPrint d 0 x

ppLabelledTypedId :: PDetail -> Doc -> Maybe Id -> Bool -> Doc -> Type -> [Doc] -> Doc
ppLabelledTypedId d intro modId isFnlike i ty ids =
  let (ys, x) = getArrows ty
      ity = case x of (TAp (TCon _) y) -> y;
                      z -> z
      g [] = empty
      g xs = t"#(" <>  sepList xs (t",") <> t")"
      f [] = intro <+> pp d x <+> i <> (if isFnlike then t"()" else empty)
      f xs = if isModule modId x
              then t"module" <+> i <> g xs <> t"(" <> pvPrint d 0 ity <> t")"
              else intro <+> pvPrint d 9 x <+> i <> t"(" <> sepList xs (text ",") <> t")"
      zs = zipWith (\ y i' -> ppTypedId d Nothing i' y newIds) ys ids
  in f zs

ppTypedId :: PDetail -> Maybe Id -> Doc -> Type -> [Doc] -> Doc
ppTypedId d mi i y = ppLabelledTypedId d (if isFn y then t"function" else empty) mi (isFn y) i y

pvParameterTypes :: PDetail -> [Type] -> Doc
pvParameterTypes _d [] = empty
pvParameterTypes  d ts =
    t"#(" <> sepList (map (\ y -> pvPrint d 0 y) ts) (t ",") <> t ")"

unravel :: Type -> (Type,[Type])
unravel (TAp (TAp a b) c) =
  let (x, ys) = unravel (TAp a b)
  in (x, ys ++[c])
unravel (TAp a b) = (a, [b])
unravel x = error ("unravel bad: " ++ show x)

t :: String -> Doc
t s = text s

tUnmkTuple :: Type -> [Type]
tUnmkTuple (TAp (TAp (TCon pair) a) b) | isTConPair pair =
  let bs = tUnmkTuple b
  in a:bs
tUnmkTuple x = [x]

cTNum :: Integer -> Position -> CType
cTNum n pos = TCon (TyNum n pos)

getArrows :: Type -> ([Type], Type)
getArrows t' = getArrowsAccum [] t'
    where getArrowsAccum ts (TAp (TAp (TCon arr) a) r) | isTConArrow arr
                              = getArrowsAccum (a:ts) r
          getArrowsAccum ts r = (reverse ts, r)

isModule :: Maybe Id -> Type -> Bool
isModule _ (TAp (TCon (TyCon i _ _)) _) =  likeModule i
isModule (Just i') (TAp (TVar (TyVar i _ _)) _) =  i == i'
isModule _ _          =  False

isTConArrow :: TyCon -> Bool
isTConArrow (TyCon i _ _) =  i == idArrow noPosition
isTConArrow t' = error("isTConArrow: not TCon " ++ show t')

isTConPair :: TyCon -> Bool
isTConPair (TyCon i _ _) =  i == idPrimPair
isTConPair t' = error("isTConPair: not TCon " ++ show t')

-- | used to do the sorting of instances
-- so that overlapping matches go to the most specific
-- TAp first because it brings forward instances with larger structure
-- see the Has_tpl_n instances in the Prelude
cmp :: Type -> Type -> Ordering
cmp (TAp f1 a1) (TAp f2 a2) = compare (f1, a1) (f2, a2)
cmp (TAp _  _)  _           = LT
cmp (TCon c1) (TCon c2) = compare c1 c2
cmp (TCon _)  (TAp _ _) = GT
cmp (TCon _)  _         = LT
cmp (TVar _) (TCon _)   = GT
cmp (TVar _) (TAp _ _)  = GT
cmp (TVar v1) (TVar v2) = compare v1 v2
cmp (TVar _)  _         = LT
cmp (TGen _ i1) (TGen _ i2) = compare i1 i2
cmp (TGen _ _) (TDefMonad _) = LT
cmp (TGen _ _) _        = GT
cmp (TDefMonad _) (TDefMonad _) = EQ
cmp (TDefMonad _) _  = GT

-- | Representation of a type variable
data TyVar = TyVar { tv_name :: Id    -- ^ name of the type variable
                   , tv_num  :: Int   -- ^ number for a generated type variable
                   , tv_kind :: Kind  -- ^ kind of the type variable
                   }
    deriving Show

instance Eq TyVar where
    TyVar i n _ == TyVar i' n' _  =  (n, i) == (n', i')

instance Ord TyVar where
    TyVar i n _ <= TyVar i' n' _  =  (n, i) <= (n', i')
    TyVar i n _ <  TyVar i' n' _  =  (n, i) <  (n', i')
    TyVar i n _ >= TyVar i' n' _  =  (n, i) >= (n', i')
    TyVar i n _ >  TyVar i' n' _  =  (n, i) >  (n', i')
    TyVar i n _ `compare` TyVar i' n' _  =  (n, i) `compare` (n', i')

instance PPrint TyVar where
    pPrint d _ (TyVar i _ _) = ppVarId d i

instance PVPrint TyVar where
    pvPrint d _ (TyVar i _ _) = pvpId d i

instance HasPosition TyVar where
    getPosition (TyVar name _ _) = getPosition name

-- | Representation of a type constructor
data TyCon = -- | A constructor for a type of value kind
             TyCon { tcon_name :: Id           -- ^ name of the type constructor
                   , tcon_kind :: (Maybe Kind) -- ^ kind of the type constructor
                   , tcon_sort :: TISort       -- ^ purpose of the type constructor
                   }
             -- | A constructor for a type of numeric kind
           | TyNum { tynum_value :: Integer  -- ^ type-level numeric value
                   , tynum_pos   :: Position -- ^ position of introduction
                   }
             -- | A constructor for a type of string kind
           | TyStr { tystr_value :: FString  -- ^ type-level string value
                   , tystr_pos   :: Position -- ^ position of introduction
                   }
    deriving Show

instance Eq TyCon where
    TyCon i k _ == TyCon i' k' _  =  qualEq i i' && k == k'
    TyNum i _   == TyNum i' _     =  i == i'
    TyStr s _   == TyStr s' _     =  s == s'
    _           == _              =  False

instance Ord TyCon where
    TyCon i k _ `compare` TyCon i' k' _   =  (getIdBase i, getIdQual i, k) `compare` (getIdBase i', getIdQual i', k')
    TyCon _ _ _ `compare` TyNum _  _      =  LT
    TyCon _ _ _ `compare` TyStr _  _      =  LT
    TyNum _ _   `compare` TyCon _  _  _   =  GT
    TyNum i _   `compare` TyNum i' _      =  i `compare` i'
    TyNum _ _   `compare` TyStr _  _      =  LT
    TyStr _ _   `compare` TyCon _  _  _   =  GT
    TyStr _ _   `compare` TyNum _  _      =  GT
    TyStr s _   `compare` TyStr s' _      =  s `compare` s'

instance PPrint TyCon where
    pPrint  d _ (TyCon i _ _) = ppConId d i
    pPrint _d _ (TyNum i _) = text (itos i)
    pPrint _d _ (TyStr s _) = text (show s)

instance PVPrint TyCon where
    pvPrint  d _ (TyCon i _ _) = pvpId d i
    pvPrint _d _ (TyNum i _) = text (itos i)
    pvPrint _d _ (TyStr s _) = text (show s)

instance HasPosition TyCon where
    getPosition (TyCon name _k _) = getPosition name
    getPosition (TyNum _ pos) = pos
    getPosition (TyStr _ pos) = pos

data TISort
        = -- type synonym
          TItype Integer Type
        | TIdata { tidata_cons :: [Id]
                 , tidata_enum :: Bool
                 }
        | TIstruct StructSubType [Id]
          -- primitive abstract type
          -- e.g. Integer, Bit, Module, etc.
        | TIabstract
        deriving (Eq, Ord, Show)

instance PPrint TISort where
    pPrint  d  p (TItype n t') = pparen (p>0) $ text "TItype" <+> pPrint d 0 n <+> pPrint d 1 t'
    pPrint  d  p (TIdata is enum) = pparen (p>0) $ text (if enum then "TIdata (enum)" else "TIdata") <+> pPrint d 1 is
    pPrint  d  p (TIstruct ss is) = pparen (p>0) $ text "TIstruct" <+> pPrint d 1 ss <+> pPrint d 1 is
    pPrint _d _p (TIabstract) = text "TIabstract"

data StructSubType
        = SStruct
        | SClass
        | SDataCon { sdatacon_id :: Id
                   , sdatacon_named_fields :: Bool
                   }
        | SInterface [IfcPragma]
        | SPolyWrap { spolywrap_id :: Id         -- ^ name of the type with the wrapped field
                    , spolywrap_ctor :: Maybe Id -- ^ name of the data constructor
                    , spolywrap_field :: Id      -- ^ name of the wrapped field
                    }
        deriving (Eq, Ord, Show)

instance PPrint StructSubType where
    pPrint _ _ ss = text (show ss)

type CType = Type

leftCon :: CType -> Maybe Id
leftCon (TAp f _) = leftCon f
leftCon (TCon (TyCon i _ _)) = Just i
leftCon _ = Nothing

-- | Representation of kinds
data Kind = KStar           -- ^ kind of a simple value type
          | KNum            -- ^ kind of a simple numeric type
          | KStr            -- ^ kind of a simple string type
          | Kfun Kind Kind  -- ^ kind of type constructors (type-level function)
          | KVar Int        -- ^ generated kind variable (used only during kind inference)
    deriving (Eq, Ord, Show)

instance PPrint Kind where
    pPrint _ _ KStar = text "*"
    pPrint _ _ KNum = text "#"
    pPrint _ _ KStr = text "$"
    pPrint d p (Kfun l r) = pparen (p>9) $ pPrint d 10 l <+> text "->" <+> pPrint d 9 r
    pPrint _ _ (KVar i) = text (showKVar i)

instance PVPrint Kind where
    pvPrint _ _ KStar = text "*"
    pvPrint _ _ KNum = text "#"
    pvPrint _ _ KStr = text "$"
    pvPrint d p (Kfun l r) = pparen (p>9) $ pvPrint d 10 l <+> text "->" <+> pvPrint d 9 r
    pvPrint _ _ (KVar i) = text (show i)

-- KIMisc.newKVar starts at this number
baseKVar :: Int
baseKVar = 1000

-- Display the kind variable with letters
showKVar :: Int -> String
showKVar v =
    let
        makeDigit x = chr (x + 97)  -- 97 = ASCII a

        showDigits :: Int -> String
        showDigits x | (x < 26) = [makeDigit x]
        showDigits x = (showDigits (x `div` 26)) ++ [makeDigit (x `mod` 26)]
    in
        if (v < baseKVar)
        then (itos v)
        else (showDigits (v - baseKVar))

-- Used for providing partial Kind information
data PartialKind
        = PKNoInfo -- this is what makes it partial
        | PKStar
        | PKNum
        | PKStr
        | PKfun PartialKind PartialKind
        deriving (Eq, Ord, Show)

instance PPrint PartialKind where
    pPrint _ _ PKNoInfo = text "?"
    pPrint _ _ PKStar = text "*"
    pPrint _ _ PKNum = text "#"
    pPrint _ _ PKStr = text "$"
    pPrint d p (PKfun l r) =
        pparen (p>9) $ pPrint d 10 l <+> text "->" <+> pPrint d 9 r

instance PVPrint PartialKind where
    pvPrint _ _ PKNoInfo = text "?"
    pvPrint _ _ PKStar = text "*"
    pvPrint _ _ PKNum = text "#"
    pvPrint _ _ PKStr = text "$"
    pvPrint d p (PKfun l r) = pparen (p>9) $ pvPrint d 10 l <+> text "->" <+> pvPrint d 9 r

-- | A named typeclass
newtype CTypeclass = CTypeclass Id
    deriving (Eq, Ord, Show, PPrint, HasPosition)

instance PVPrint CTypeclass where
   pvPrint d p (CTypeclass i) = pvPrint d p i

-- | Representation of the provisos and other class constraints
data CPred = CPred { cpred_tc   :: CTypeclass  -- ^ constraint class, e.g., "Eq"
                   , cpred_args :: [CType]     -- ^ argument types
                   }
        deriving (Eq, Ord, Show)

instance PPrint CPred where
    pPrint d _p (CPred (CTypeclass c) []) = ppConId d c
    pPrint d _p (CPred (CTypeclass c) ts) = ppConId d c <+> sep (map (pPrint d (maxPrec+1)) ts)

instance PVPrint CPred where
    pvPrint d _p (CPred (CTypeclass c) []) = pvpId d c
    pvPrint d _p (CPred (CTypeclass c) ts) = pvpId d c <> text "#(" <> sepList (map (pvPrint d 0) ts) (text ",") <> text ")"

instance HasPosition CPred where
    getPosition (CPred c ts) = getPosition (c, ts)

data CQType = CQType [CPred] CType
    deriving (Eq, Ord, Show)

instance PPrint CQType where
    pPrint d  p (CQType [] ct) = pPrint d p ct
    pPrint d _p (CQType preds ct) = sep [text "(" <> sepList (map (pPrint d 0) preds) (text ",") <> text ")" <+> text "=>", pPrint d 0 ct]

instance PVPrint CQType where
    pvPrint d  p (CQType [] ct) = pvPrint d p ct
    pvPrint d _p (CQType preds ct) = sep [text "(" <> sepList (map (pvPrint d 0) preds) (text ",") <> text ") =>", pvPrint d 0 ct]

instance HasPosition CQType where
    -- prefer t' to ps, since that is a better position for BSV
    getPosition (CQType ps t') = getPosition t' `bestPosition` getPosition ps
