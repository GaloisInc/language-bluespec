-- This corresponds to src/comp/VModInfo.hs in bsc.
module Language.Bluespec.Classic.AST.VModInfo
  ( VName(..)
  , VPathInfo(..)
  , VeriPortProp(..)
  , VArgInfo(..)
  , VPort
  , VSchedInfo
  , VFieldInfo(..)
  , InputClockInf
  , OutputClockInf
  , VOscPort
  , VInputGatePort
  , VOutputGatePort
  , VClockInfo(..)
  , ResetInf
  , VResetInfo(..)
  ) where

import qualified Data.List as L

import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.Classic.AST.SchedInfo
import Language.Bluespec.Prelude
import Language.Bluespec.SystemVerilog.AST.Pretty

newtype VName = VName String
        deriving (Eq, Ord, Show)

instance PPrint VName where
    pPrint _ _ (VName s) = text s

instance PVPrint VName where
    pvPrint _ _ (VName s) = text s

newtype VPathInfo = VPathInfo [(VName, VName)]
                  deriving (Eq, Ord, Show)

instance PPrint VPathInfo where
    pPrint _d _p (VPathInfo []) =
        s2par "No combinational paths from inputs to outputs"
    pPrint d p (VPathInfo nns) =
        let
            -- function to join paths going to the same output
            joinPaths [] = []
            joinPaths zs@((_,o):_) =
                case (L.partition ((== o) . snd) zs) of
                    (xs, ys) -> (map fst xs, o) : joinPaths ys

            -- function to display one pair
            ppOne ([i],out) = pPrint d p i <+> text "->" <+> pPrint d p out
            ppOne (ins,out) = text "(" <>
                              sepList (map (pPrint d p) ins) (text ",") <>
                              text ")" <+> text "->" <+> pPrint d p out
        in
            s2par "Combinational paths from inputs to outputs:" $+$
            nest 2 (vcat (map ppOne (joinPaths nns)))

instance PVPrint VPathInfo where
    pvPrint d p v = ppPathInfo d p v

instance HasPosition VFieldInfo where
  getPosition (Method { vf_name = n }) = getPosition n
  getPosition (Clock i)                 = getPosition i -- or noPosition?
  getPosition (Reset i)                 = getPosition i -- or noPosition?
  getPosition (Inout { vf_name = n })  = getPosition n

ppPathInfo :: PDetail -> Int -> VPathInfo -> Doc
ppPathInfo d p ps = pPrint d p ps

-- the order is important for VIOProps ("unused" should come last)
data VeriPortProp = VPreg
                  | VPconst
                  | VPinhigh
                  | VPouthigh
                  | VPclock
                  | VPclockgate
                  | VPreset
                  | VPinout
                  | VPunused
        deriving (Eq, Ord, Show)

instance PPrint VeriPortProp where
    pPrint _ _ VPreg = text "reg"
    pPrint _ _ VPclock = text "clock"
    pPrint _ _ VPconst = text "const"
    pPrint _ _ VPinhigh = text "inhigh"
    pPrint _ _ VPouthigh = text "outhigh"
    pPrint _ _ VPunused = text "unused"
    pPrint _ _ VPreset  = text "reset"
    pPrint _ _ VPinout  = text "inout"
    pPrint _ _ VPclockgate = text "clock gate"

instance PVPrint VeriPortProp where
    pvPrint _d _p v = text (drop 2 (show v))

data VArgInfo = Param VName    -- named module parameter
              -- named module port, with associated clock and reset
              | Port VPort (Maybe Id) (Maybe Id)
              | ClockArg Id    -- named clock
              | ResetArg Id    -- named reset
              -- named module inout, with associated clock and reset
              | InoutArg VName (Maybe Id) (Maybe Id)
              deriving (Eq, Ord, Show)

instance PPrint VArgInfo where
    pPrint d _p (Param x) = text "param " <> pPrint d 0 x <> text ";"
    pPrint d _p (Port x mclk mrst) =
        text "port " <> pPrint d 0 x <+>
        ppMClk d mclk <+> ppMRst d mrst <>
        text ";"
    pPrint d p (ClockArg x) = text "clockarg " <> pPrint d p x <> text ";"
    pPrint d p (ResetArg x) = text "resetarg " <> pPrint d p x <> text ";"
    pPrint d _p (InoutArg x mclk mrst) =
        text "inoutarg " <> pPrint d 0 x <+>
        ppMClk d mclk <+> ppMRst d mrst <>
        text ";"

ppMClk :: PDetail -> Maybe Id -> Doc
ppMClk d mclk =
    let clk = case mclk of
                  Nothing -> text "no_clock"
                  Just c  -> pPrint d 0 c
    in  text "clocked_by (" <> clk <> text ")"

ppMRst :: PDetail -> Maybe Id -> Doc
ppMRst d mrst =
    let rst = case mrst of
                  Nothing -> text "no_reset"
                  Just r  -> pPrint d 0 r
    in  text "reset_by (" <> rst <> text ")"

type VPort = (VName, [VeriPortProp])

type VSchedInfo = SchedInfo Id

-- XXX why does the VFieldInfo not contain a ready signal?
data VFieldInfo = Method { vf_name   :: Id, -- method name
                           vf_clock  :: (Maybe Id), -- optional clock
                           -- optional because the method may be combinational
                           vf_reset  :: (Maybe Id), -- optional reset
                           -- optional because the method may be independent of a reset signal
                           vf_mult   :: Integer, -- multiplicity
                           vf_inputs :: [VPort],
                           vf_output :: Maybe VPort,
                           vf_enable :: Maybe VPort }
                | Clock { vf_name :: Id } -- output clock name
                                           -- connection information is in the ClockInfo
                | Reset { vf_name :: Id } -- output reset name
                                           -- connection information is in the ResetInfo
                | Inout { vf_name :: Id, -- output inout name
                          vf_inout :: VName,
                          vf_clock :: (Maybe Id), -- optional clock
                          vf_reset :: (Maybe Id) } -- optional reset
                deriving (Eq, Ord, Show)

instance PPrint VFieldInfo where
    pPrint d p (Method n c r m i o e) =
      text "method " <> pout o <> pPrint d p n <> pmult m <>
      pins i <> pena e <+> ppMClk d c <+> ppMRst d r <>
      text ";"
        where pout Nothing = empty
              pout (Just po) = pPrint d p po
              pmult 1  = empty
              pmult n' = text "[" <> pPrint d p n' <> text "]"
              pins []  = empty
              pins i'  = text "(" <> sepList (map (pPrint d p) i') (text ",") <> text ")"
              pena Nothing = empty
              pena (Just en) = text " enable (" <> pPrint d p en <> text ")"
    pPrint d p (Clock i) = text "clock " <> pPrint d p i <> text ";"
    pPrint d p (Reset i) = text "reset " <> pPrint d p i <> text ";"
    pPrint d p (Inout n port c r) =
        text "inout " <> pPrint d p n <+>
        text "(" <> pPrint d p port <> text ")" <+>
        ppMClk d c <+> ppMRst d r <> text ";"

-- describes the clocks imported by a module
type InputClockInf = (Id, Maybe (VOscPort, VInputGatePort))

-- describes the clocks exported by a module
type OutputClockInf = (Id, Maybe (VOscPort, VOutputGatePort))

-- no VeriPortProp needed, so we use VName and not VPort
type VOscPort = VName

-- gate port for input clocks
-- Either there is no port, in which case the boolean indicates
-- whether the gate is assumed True or is unneeded, or there is gate port.
type VInputGatePort = Either Bool VName

-- gate port for output clocks
-- Either there is no port, in which case users should assume a value True,
-- or there is a port, and we annotate whether it is "outhigh" with a
-- port property (VPouthigh)
type VOutputGatePort = Maybe VPort

data VClockInfo = ClockInfo {
                 -- named list of input clocks
                 input_clocks :: [InputClockInf],
                 -- named list of output clocks
                 output_clocks :: [OutputClockInf],
                 -- edges in the ancestor relationships (transitive)
                 -- first clock is parent, second is child
                 ancestorClocks :: [(Id, Id)],
                 -- other sibling relationships
                 -- transitive (but often trumped by ancestry)
                 -- method calls are permitted across sibling relationships
                 -- but *both* gate conditions must be enforced
                 siblingClocks :: [(Id, Id)] }
                deriving (Eq, Ord, Show)

instance PPrint VClockInfo where
    pPrint d _p (ClockInfo in_cs out_cs as ss) =
        vcat (map pOutCInf out_cs ++
              map pInCInf in_cs ++
              map pAnc as ++
              map pSib ss)
        where
              pOutCInf (i,mc) = text "clock " <> pPrint d 0 i <>
                                text "(" <> pOutMClk mc <> text ");"
              pOutMClk Nothing = empty
              pOutMClk (Just (vn, mg)) = pPrint d 0 vn <> pOutMGate mg
              pOutMGate Nothing = empty
              pOutMGate (Just (vn, vpps)) =
                  text ", " <> pPortProps vpps <> pPrint d 0 vn
              pPortProps [] = empty
              pPortProps (vp:vpps) =
                  text "{-" <> pPrint d 0 vp <> text "-} " <> pPortProps vpps

              pInCInf (i,mc) = text "clock " <> pPrint d 0 i <>
                               text "(" <> pInMClk mc <> text ");"
              pInMClk Nothing = empty
              pInMClk (Just (vn, mg)) = pPrint d 0 vn <> pInMGate mg
              pInMGate (Left True)  = text ", {-inhigh-}"
              pInMGate (Left False) = text ", {-unused-}"
              pInMGate (Right vn)   = text ", " <> pPrint d 0 vn

              pAnc (i,j) = text "ancestor (" <> pPrint d 0 i <> text ", " <>
                           pPrint d 0 j <> text ");"
              pSib (i,j) = text "sibling (" <> pPrint d 0 i <> text ", " <>
                           pPrint d 0 j <> text ");"

instance HasPosition VClockInfo where
  getPosition (ClockInfo { output_clocks = ((id',_):_)}) = getPosition id'
  getPosition (ClockInfo { input_clocks = ((id',_):_)}) = getPosition id'
  getPosition _ = noPosition

-- reset name, Verilog port (optional), clock (optional)
type ResetInf = (Id, (Maybe VName, Maybe Id))

-- basic information on reset signals
-- more information to be added (sync/async, clock relationships, etc.)
data VResetInfo = ResetInfo {
                   input_resets  :: [ResetInf],
                   output_resets :: [ResetInf]
                  }
  deriving (Eq, Ord, Show)

instance PPrint VResetInfo where
    pPrint d p (ResetInfo in_rs out_rs) = vcat (map pRInf (out_rs ++ in_rs))
      where t = text
            pRInf (i,(mn,mc)) =
                t"reset " <> pPrint d p i <>
                t"(" <> pMRst mn <> t")" <+>
                t"clocked_by(" <> pMClk mc <> t");"
            pMRst Nothing  = empty
            pMRst (Just n) = pPrint d p n
            pMClk Nothing  = t"no_clock"
            pMClk (Just c) = pPrint d p c
