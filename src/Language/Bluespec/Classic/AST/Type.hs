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
  , isTConArrow
  , isTConPair
  , leftCon
  ) where

import Data.Char (chr)
import Text.PrettyPrint.HughesPJClass

import Language.Bluespec.Classic.AST.Builtin.Ids
import Language.Bluespec.Classic.AST.FString
import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.Pragma
import Language.Bluespec.Prelude
import Language.Bluespec.Pretty
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

instance Pretty Type where
    pPrintPrec _d _p (TCon (TyCon unit _ _)) | unit == idPrimUnit = text "()"
    pPrintPrec  d _p (TCon c) = pPrintPrec d 0 c
    pPrintPrec  d _p (TVar i) = pPrintPrec d 0 i
    pPrintPrec  d  p (TAp (TAp (TCon pair) a) b) | isTConPair pair =
        pparen (p >= 0) (sep [pPrintPrec d 0 a <> text ",", pPrintPrec d (-1) b])
    pPrintPrec d p (TAp (TAp (TCon arr) a) r) | isTConArrow arr =
        pparen (p > 8) (sep [pPrintPrec d 9 a <+> text "->", pPrintPrec d 8 r])
    pPrintPrec d p (TAp e e') = pparen (p>9) $
        sep [pPrintPrec d 9 e, pPrintPrec d 10 e']
    pPrintPrec _d _p (TDefMonad _) = text ("TDefMonad")
    pPrintPrec  d  p (TGen _ n) = pparen True (text "TGen" <+> pPrintPrec d p n)

instance HasPosition Type where
    getPosition (TVar var) = getPosition var
    getPosition (TCon con) = getPosition con
    getPosition (TAp f a) = getPosition f `bestPosition` getPosition a
    getPosition (TGen pos _) = pos
    getPosition (TDefMonad pos) = pos

cTNum :: Integer -> Position -> CType
cTNum n pos = TCon (TyNum n pos)

isTConArrow :: TyCon -> Bool
isTConArrow (TyCon i _ _) =  i == idArrow noPosition
isTConArrow t = error("isTConArrow: not TCon " ++ show t)

isTConPair :: TyCon -> Bool
isTConPair (TyCon i _ _) =  i == idPrimPair
isTConPair t = error("isTConPair: not TCon " ++ show t)

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

instance Pretty TyVar where
    pPrintPrec d _ (TyVar i _ _) = ppVarId d i

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

instance Pretty TyCon where
    pPrintPrec  d _ (TyCon i _ _) = ppConId d i
    pPrintPrec _d _ (TyNum i _) = text (itos i)
    pPrintPrec _d _ (TyStr s _) = text (show s)

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

instance Pretty TISort where
    pPrintPrec  d  p (TItype n t) = pparen (p>0) $ text "TItype" <+> pPrintPrec d 0 n <+> pPrintPrec d 1 t
    pPrintPrec  d  p (TIdata is enum) = pparen (p>0) $ text (if enum then "TIdata (enum)" else "TIdata") <+> pPrintPrec d 1 is
    pPrintPrec  d  p (TIstruct ss is) = pparen (p>0) $ text "TIstruct" <+> pPrintPrec d 1 ss <+> pPrintPrec d 1 is
    pPrintPrec _d _p (TIabstract) = text "TIabstract"

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

instance Pretty StructSubType where
    pPrintPrec _ _ ss = text (show ss)

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

instance Pretty Kind where
    pPrintPrec _ _ KStar = text "*"
    pPrintPrec _ _ KNum = text "#"
    pPrintPrec _ _ KStr = text "$"
    pPrintPrec d p (Kfun l r) = pparen (p>9) $ pPrintPrec d 10 l <+> text "->" <+> pPrintPrec d 9 r
    pPrintPrec _ _ (KVar i) = text (showKVar i)

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

instance Pretty PartialKind where
    pPrintPrec _ _ PKNoInfo = text "?"
    pPrintPrec _ _ PKStar = text "*"
    pPrintPrec _ _ PKNum = text "#"
    pPrintPrec _ _ PKStr = text "$"
    pPrintPrec d p (PKfun l r) =
        pparen (p>9) $ pPrintPrec d 10 l <+> text "->" <+> pPrintPrec d 9 r

-- | A named typeclass
newtype CTypeclass = CTypeclass Id
    deriving (Eq, Ord, Show, Pretty, HasPosition)

-- | Representation of the provisos and other class constraints
data CPred = CPred { cpred_tc   :: CTypeclass  -- ^ constraint class, e.g., "Eq"
                   , cpred_args :: [CType]     -- ^ argument types
                   }
        deriving (Eq, Ord, Show)

instance Pretty CPred where
    pPrintPrec d _p (CPred (CTypeclass c) []) = ppConId d c
    pPrintPrec d _p (CPred (CTypeclass c) ts) = ppConId d c <+> sep (map (pPrintPrec d (fromIntegral (maxPrec+1))) ts)

instance HasPosition CPred where
    getPosition (CPred c ts) = getPosition (c, ts)

data CQType = CQType [CPred] CType
    deriving (Eq, Ord, Show)

instance Pretty CQType where
    pPrintPrec d  p (CQType [] ct) = pPrintPrec d p ct
    pPrintPrec d _p (CQType preds ct) = sep [text "(" <> sepList (map (pPrintPrec d 0) preds) (text ",") <> text ")" <+> text "=>", pPrintPrec d 0 ct]

instance HasPosition CQType where
    -- prefer t to ps, since that is a better position for BSV
    getPosition (CQType ps t) = getPosition t `bestPosition` getPosition ps
