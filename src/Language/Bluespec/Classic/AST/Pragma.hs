-- This corresponds to src/comp/Pragma.hs in bsc.
module Language.Bluespec.Classic.AST.Pragma where

import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.SchedInfo

data Pragma
        = Pproperties Id [PProp]-- module Id and properties associate with
        | Pnoinline [Id]        -- [Id] is a list of functions which should not be inlined

-- Module definition properties:
data PProp
        = PPverilog                        -- generate verilog
        | PPforeignImport Id               -- wrapper for a foreign import
            -- (Id is link name, needed for dependency check, if we're
            --  generating the .ba file for the link name, not the src name)
        | PPalwaysReady        [Longname]         -- no ready signals for these methods ([] means all)
        | PPalwaysEnabled [Longname]       -- execute on every cycle
        | PPenabledWhenReady [Longname]    -- enable is equivalent to ready
        | PPscanInsert Integer             -- insert scan chain ports
        | PPbitBlast                       -- do "bit blasting",
                                           --     e.g., split multibit ports
        | PPCLK String                     -- clock port prefix
        | PPGATE String                    -- gate port prefix
        | PPRSTN String                    -- reset port prefix
        | PPclock_osc  [(Id,String)]       -- port name for clock
        | PPclock_gate [(Id,String)]       -- port name for gate
        | PPgate_inhigh [Id]               -- clock args with inhigh gates
        | PPgate_unused [Id]               -- clock args with unused gates
        | PPreset_port [(Id,String)]       -- port name for reset
        | PParg_param [(Id,String)]        -- name for parameter argument
        | PParg_port [(Id,String)]         -- port name for other arguments
        | PParg_clocked_by [(Id,String)]   -- clocks of module arguments
        | PParg_reset_by [(Id,String)]     -- resets of module arguments
        | PPoptions [String]               -- compiler options
        | PPgate_input_clocks [Id]         -- list of clock args with gates
        | PPmethod_scheduling (MethodConflictInfo Longname)
                        -- scheduling constraints for interface arg methods
        | PPdoc String          -- comment to carry through to Verilog
        | PPperf_spec [[Id]]    -- method composition order for performance specs
        | PPclock_family    [Id]   -- ids of input clocks in the same family
        | PPclock_ancestors [[Id]] -- input clock ancestry sequences
        -- module arguments which should generate to params instead of ports
        | PPparam [Id]
        | PPinst_name Id
        | PPinst_hide
        | PPinst_hide_all
        | PPdeprecate String

data RulePragma
    = RPfireWhenEnabled
    | RPnoImplicitConditions
    | RPaggressiveImplicitConditions
    | RPconservativeImplicitConditions
    | RPnoWarn -- suppress (on a per-rule basis) warnings G0023, G0036, and G0117
    | RPwarnAllConflicts
    | RPcanScheduleFirst
    | RPclockCrossingRule
    | RPdoc String  -- comment to carry through to Verilog
    | RPhide

data SchedulePragma id_t
    = SPUrgency [id_t]
    | SPExecutionOrder [id_t]
    | SPMutuallyExclusive [[id_t]]
    | SPConflictFree [[id_t]]
    | SPPreempt [id_t] [id_t]
    | SPSchedule (MethodConflictInfo id_t)

type CSchedulePragma = SchedulePragma Longname

-- Interface definition pragmas
-- These pragma are associated with interfaces and/or the fields within the interface
-- The first arg is the field name which the attribute is associated with
data IfcPragma
    =  PIArgNames     [Id]      -- arg names used as dummy names (XX this can be removed?)
    | PIPrefixStr     String    -- Method or interface
    | PIResultName    String    -- name for the result of the method AV or value methods
    | PIRdySignalName String    -- name for the ready signal on this method
    | PIEnSignalName  String    -- name for the enable signal
    | PIAlwaysRdy               -- ifc or methods tagged as always ready
    | PIAlwaysEnabled           -- ifc or methods tagged as always enabled
