-- This corresponds to src/comp/VModInfo.hs in bsc.
module Language.Bluespec.Classic.AST.VModInfo where

import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.SchedInfo

newtype VName = VName String

newtype VPathInfo = VPathInfo [(VName, VName)]

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

data VArgInfo = Param VName    -- named module parameter
              -- named module port, with associated clock and reset
              | Port VPort (Maybe Id) (Maybe Id)
              | ClockArg Id    -- named clock
              | ResetArg Id    -- named reset
              -- named module inout, with associated clock and reset
              | InoutArg VName (Maybe Id) (Maybe Id)

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

-- reset name, Verilog port (optional), clock (optional)
type ResetInf = (Id, (Maybe VName, Maybe Id))

-- basic information on reset signals
-- more information to be added (sync/async, clock relationships, etc.)
data VResetInfo = ResetInfo {
                   input_resets  :: [ResetInf],
                   output_resets :: [ResetInf]
                  }
