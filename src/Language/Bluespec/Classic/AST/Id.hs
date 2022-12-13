-- This corresponds to src/comp/Id.hs and src/comp/IdPrint.hs in bsc.
module Language.Bluespec.Classic.AST.Id
  ( Id
  , addIdProp
  , addIdProps
  , createPositionString
  , enumId
  , getIdBase
  , getIdBaseString
  , getIdPosition
  , getIdProps
  , getIdQual
  , getIdQualString
  , getIdString
  , mkId
  , mkQId
  , ppConId
  , ppId
  , ppVarId
  , qualEq
  , setBadId
  , setIdProps

  , IdProp(..)

  , Longname
  ) where

import Data.Char (isDigit)
import qualified Data.List as L
import Text.PrettyPrint.HughesPJClass

import Language.Bluespec.Classic.AST.Builtin.FStrings
import Language.Bluespec.Classic.AST.FString
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Lex
import Language.Bluespec.Prelude
import Language.Bluespec.Pretty
import Language.Bluespec.Util

data Id = Id { id_pos :: !Position,
               id_mfs :: !FString,
               id_fs :: !FString,
               id_props :: [IdProp] {- , id_stab :: Int -}
             }

idEq :: Id -> Id -> Bool
idEq a b = (id_fs a == id_fs b) && (id_mfs a == id_mfs b)

idCompare :: Id -> Id -> Ordering
idCompare a b = case (compare (id_fs a) (id_fs b)) of
                EQ -> compare (id_mfs a) (id_mfs b)
                LT -> LT
                GT -> GT

instance Eq Id where
        a == b = idEq a b

instance Ord Id where
    compare  = idCompare

instance Show Id where
    show = show_brief

instance Pretty Id where
    pPrintPrec d _p i
      | d == pdDebug
      = text (local_show i)
      | otherwise
      = if (dbgLevel >= 1)
             then text ((getIdString i) ++
                        "_"  ++
                        (createPositionString (getIdPosition i)))
             else text (getIdString i)

instance HasPosition Id where
    getPosition i = getIdPosition i

local_show :: Id -> String
local_show id' =
    let
        pos = getIdPosition id'
        mfs = getIdQualString id'
        fs = getIdBaseString id'
        str = show pos ++ " " ++
                show mfs ++ " " ++
                show fs
    in str

show_brief :: Id -> String
show_brief i =
    case (getFString (id_mfs i), getFString (id_fs i)) of
    ("", str) -> add_props str
    (pkg, str) -> add_props (pkg ++ "::" ++ str)
  where add_props str | null (id_props i) = str
                      | otherwise = str ++ show (id_props i)

createPositionString :: Position -> String
createPositionString _ = "<NoPos>"

-- Create an id of the form "<str>_<index>".
-- This is used to ENUMerate a list of Ids with the same name,
-- but with uniquifying numbers.
--
-- Note: The Ids created with this are marked as "bad".  If these Ids
-- need to be created from a user-given name, consider creating a new
-- interface for this which takes Id and not String, and derives its
-- properties from that Id.
enumId :: String -> Position -> Int -> Id
enumId str pos index =
    let id_str = tmpFString index ("_" ++ str ++ itos index)
    in  setBadId
            (Id pos fsEmpty id_str [])

getIdBase :: Id -> FString
getIdBase a = id_fs a

getIdBaseString :: Id -> String
getIdBaseString a = getFString $ getIdBase a

getIdPosition :: Id -> Position
getIdPosition a = id_pos a

getIdProps :: Id -> [IdProp]
getIdProps a = id_props a

getIdQual :: Id -> FString
getIdQual a = id_mfs a

getIdQualString :: Id -> String
getIdQualString a = getFString $ getIdQual a

getIdString :: Id -> String
getIdString a | mfs == fsEmpty = getFString fs
              | otherwise = getFString mfs ++ "." ++ getFString fs
    where mfs = getIdQual a
          fs = getIdBase a

mkId :: Position -> FString -> Id
mkId pos fs =
    let value = Id pos fsEmpty fs []
    in -- trace("ID: " ++ (ppReadable value)) $
       value

-- Qualified with a path.
mkQId :: Position -> FString -> FString -> Id
mkQId pos mfs fs
    | fs == fsEmpty = Id pos fsEmpty fsEmpty []
    | isDigit (head (getFString fs)) = Id pos fsEmpty fs [] -- XXX
    | otherwise = Id pos mfs fs []

ppConId :: PDetail -> Id -> Doc
ppConId d i
  | d == pdDebug
  = pPrintPrec pdDebug 0 i
  | otherwise
  = -- text ( "props:" ++ show (getIdProps i)) <>
    case (getIdBaseString i) of
    "->" -> text "(->)"                -- arrow
    s@(_:_) | all isDigit s -> text s  -- numbers
    _ -> text (getIdStringCon i)       -- constructor-identifiers

ppId :: PDetail -> Id -> Doc
ppId d i
  | d == pdDebug
  = pPrintPrec pdDebug 0 i
  | otherwise
  = if (dbgLevel >= 1)
    then case (getIdBaseString i) of
          "->" -> text "(->)"                          -- arrow
          s@(c:_) | isDigit c -> text( s ++ "_" ++ (createPositionString (getIdPosition i)))
          c:_ | isIdChar c -> text ((getIdString i) ++ "_" ++ (createPositionString (getIdPosition i)))
          '$':c:_ | isIdChar c -> text (getIdString i) -- task names
          _ -> text ("(" ++ (getIdString i) ++ "_" ++ (createPositionString (getIdPosition i)))
    else case (getIdBaseString i) of
          "->" -> text "(->)"                          -- arrow
          s@(c:_) | isDigit c -> text s                -- numbers
          c:_ | isIdChar c -> text (getIdString i)     -- identifiers
          '$':c:_ | isIdChar c -> text (getIdString i) -- task names
          _ -> text ("("++getIdString i++")")          -- infix operators

ppVarId :: PDetail -> Id -> Doc
ppVarId d i
  | d == pdDebug
  = pPrintPrec pdDebug 0 i
  | otherwise
  = if (dbgLevel >= 1)
    then case (getIdBaseString i) of
    s | all isSym s -> text ("("++ (getIdStringOp i) ++ (createPositionString (getIdPosition i)) ++
                             ")") -- infix operators
    '$':c:_ | isIdChar c -> text ((getIdStringVar i) ++ (createPositionString (getIdPosition i)))
    _ -> text ((getIdStringVar i) ++ (createPositionString (getIdPosition i)))
    else case (getIdBaseString i) of
    s | all isSym s -> text ("("++getIdStringOp i ++ ")") -- infix operators
    '$':c:_ | isIdChar c -> text (getIdStringVar i) -- task names
    _ -> text (getIdStringVar i)

qualEq :: Id -> Id -> Bool
qualEq a b | getIdQual a == fsEmpty || getIdQual b == fsEmpty = getIdBase a == getIdBase b
qualEq a b = a == b

setBadId :: Id -> Id
setBadId idx = addIdProp idx IdP_bad_name

setIdProps :: Id -> [IdProp] -> Id
setIdProps a l = a { id_props = l }

-- These used to encode properties in .bi files
getIdStringCon :: Id -> String
getIdStringCon = getIdString
getIdStringVar :: Id -> String
getIdStringVar = getIdString
getIdStringOp :: Id -> String
getIdStringOp  = getIdString

data IdProp = IdPCanFire
              | IdPWillFire
              | IdPProbe
              | IdPInternal
              | IdPReady                -- interface ready signal
              | IdPGeneratedIfc         -- generated interface name
              | IdPMeth
              | IdPCommutativeTCon      -- commutative type constructor
              | IdP_enable
              | IdP_keep
              | IdP_keepEvenUnused
              | IdPRule
              | IdPSplitRule
              | IdPDict                 -- is a dictionary
              | IdPRenaming             -- id for temporary renaming
              | IdP_suffixed            -- a _nn suffix has been added
              | IdP_SuffixCount Integer -- the number of suffixes added ... not to be used with IdP_suffixed
              | IdP_bad_name            -- a name generated without good information (e.g., __d5)
              | IdP_from_rhs            -- a name generated from the right-hand-side of an assignment (e.g., x_PLUS_5__d32)
              | IdP_signed              -- in C backend, an id created from $signed()
              | IdP_NakedInst           -- id associated with a "naked" instantiation (i.e. without a bind)
              | IdPDisplayName FString  -- provide an alternate display string
              | IdP_hide
              | IdP_hide_all
              | IdP_TypeJoin Id Id      -- Internally generated type name (anonymous structs)
                                        -- Arguments are the original type and constructor name
              | IdPMethodPredicate      -- is a predicate of a method call in a rule
              -- the Id of meth calls on imported/synthesized modules
              -- can be tagged with the position of inlined method calls
              -- that it was contained in (the top methods are last)
              | IdPInlinedPositions [Position]
              -- used by the BSV parser to keep track of which array types
              -- were introduced from bracket syntax
              | IdPParserGenerated
        deriving (Eq, Ord, Show)

instance Pretty IdProp where
    pPrintPrec d _ (IdPInlinedPositions poss) =
        pparen True (text "IdPInlinedPositions" <+> pPrintPrec d 0 poss)
    pPrintPrec _ _ prop = text (show prop)

-- #############################################################################
-- # Methods for adding properties to Id's, checking for them etc.
-- #############################################################################

addIdProp :: Id -> IdProp -> Id
addIdProp a prop = setIdProps a (L.union (getIdProps a) [prop])

addIdProps :: Id -> [IdProp] -> Id
addIdProps a propl = setIdProps a (L.union (getIdProps a) propl)

-- Long names

type Longname = [Id]
