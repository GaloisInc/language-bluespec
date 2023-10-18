-- This corresponds to src/comp/Pragma.hs in bsc.
module Language.Bluespec.Classic.AST.Pragma
  ( Pragma(..)
  , PProp(..)
  , RulePragma(..)
  , SchedulePragma(..)
  , CSchedulePragma
  , IfcPragma(..)

  , filterIArgNames
  , filterPrintIfcArgs
  , ppPProp
  , pvpPProp
  ) where

import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.Classic.AST.SchedInfo
import Language.Bluespec.Prelude
import Language.Bluespec.SystemVerilog.AST.Pretty
import Language.Bluespec.Util

data Pragma
        = Pproperties Id [PProp]-- module Id and properties associate with
        | Pnoinline [Id]        -- [Id] is a list of functions which should not be inlined
        deriving (Eq, Ord, Show)

instance PPrint Pragma where
    pPrint d _p (Pproperties i pps) =
        (text "{-# properties" <+> ppId d i <+> text "= { ") <>
          sepList (map (pPrint d 0) pps) (text ",") <> text " } #-}"
    pPrint d _p (Pnoinline is) =
        text "{-# noinline" <+> sep (map (ppId d) is) <+> text " #-}"

instance PVPrint Pragma where
    pvPrint d _p (Pproperties _i pps) =
        foldr ($+$) empty (map (pvpPProp d) pps)
    pvPrint d _p (Pnoinline is) =
        text "(* noinline" <+> sep (map (pvpId d) is) <+> text " *)"

instance HasPosition Pragma where
    getPosition (Pproperties i _) = getPosition i
    getPosition (Pnoinline (i:_)) = getPosition i
    getPosition (Pnoinline [])    = error "HasPosition(Pragma).getPosition: Pnoinline []"

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
      deriving (Eq, Ord, Show)

instance PPrint PProp where
    pPrint  d _ (PPscanInsert i) = text "scanInsert = " <+> pPrint d 0 i
    pPrint _d _ (PPCLK s) = text ("clock_prefix = " ++ s)
    pPrint _d _ (PPGATE s) = text ("gate_prefix = " ++ s)
    pPrint _d _ (PPRSTN s) = text ("reset_prefix = " ++ s)
    pPrint  d _ (PPclock_osc xs) =
        text "clock_osc = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrint d _ (PPclock_gate xs) =
        text "clock_gate = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrint d _ (PPgate_inhigh is) =
        text "gate_inhigh = {" <> sepList (map (ppId d) is) (text ",") <> text "}"
    pPrint d _ (PPgate_unused is) =
        text "gate_unused = {" <> sepList (map (ppId d) is) (text ",") <> text "}"
    pPrint d _ (PPreset_port xs) =
        text "reset_port = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrint d _ (PParg_param xs) =
        text "arg_param = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrint d _ (PParg_port xs) =
        text "arg_port = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrint d _ (PParg_clocked_by xs) =
        text "clocked_by = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrint d _ (PParg_reset_by xs) =
        text "reset_by = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrint _d _ (PPoptions os) =
        text "options = {"
        <> sepList (map (text . show) os) (text ",")
        <> text "}"
    pPrint _d _ (PPdoc comment) = text ("doc = " ++ doubleQuote comment)
    pPrint _d _ (PPdeprecate comment) = text ("deprecate = " ++ doubleQuote comment)
    pPrint _d _ (PPinst_hide) = text "hide"
    pPrint _d _p v = text (drop 2 (show v))

instance PVPrint PProp where
    pvPrint  d _ (PPscanInsert i) = text "scan_insert =" <+> pvPrint d 0 i
    pvPrint _d _ (PPCLK s) = text ("clock_prefix = " ++ s)
    pvPrint _d _ (PPGATE s) = text ("gate_prefix = " ++ s)
    pvPrint _d _ (PPRSTN s) = text ("reset_prefix = " ++ s)
    pvPrint  d _ (PPclock_osc xs) =
        text "clock_osc = {"
        <> sepList [ text "(" <> pvpId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pvPrint d _ (PPclock_gate xs) =
        text "clock_gate = {"
        <> sepList [ text "(" <> pvpId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pvPrint d _ (PPgate_inhigh is) =
        text "gate_inhigh = {" <> sepList (map (pvpId d) is) (text ",") <> text "}"
    pvPrint d _ (PPgate_unused is) =
        text "gate_unused = {" <> sepList (map (pvpId d) is) (text ",") <> text "}"

    pvPrint d _ (PPreset_port xs) =
        text "reset_port = {"
        <> sepList [ text "(" <> pvpId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pvPrint d _ (PParg_param xs) =
        text "param_port = {"
        <> sepList [ text "(" <> pvpId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pvPrint d _ (PParg_port xs) =
        text "arg_port = {"
        <> sepList [ text "(" <> pvpId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pvPrint d _ (PParg_clocked_by xs) =
        text "clocked_by = {"
        <> sepList [ text "(" <> pvpId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pvPrint d _ (PParg_reset_by xs) =
        text "reset_by = {"
        <> sepList [ text "(" <> pvpId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pvPrint _d _ (PPoptions os) = text "options = {" <> sepList (map (text . show) os) (text ",") <> text "}"
    pvPrint _d _ (PPverilog) = text "synthesize"
    pvPrint _d _ (PPalwaysReady _ms) = text "always_ready"
    pvPrint _d _ (PPalwaysEnabled _ms) = text "always_enabled"
    pvPrint _d _ (PPenabledWhenReady _ms) = text "enabled_when_ready"
    pvPrint _d _ (PPbitBlast) = text "bit_blast"
    pvPrint _d _ (PPdoc comment) = text ("doc = " ++ doubleQuote comment)
    pvPrint _d _ (PPdeprecate comment) = text ("deprecate = " ++ doubleQuote comment)
    pvPrint  d _ (PPparam ids) = text "param = \"" <> sepList (map (pvpId d) ids) (text ",") <> text "\""
    pvPrint  d _ (PPinst_name i) = text "inst_name = \"" <> pvpId d i <> text "\""
    pvPrint _d _ (PPinst_hide) = text "inst_hide"
    pvPrint _d _p v = text (drop 2 (show v))

ppPProp :: PDetail -> PProp -> Doc
ppPProp d pprop = text "{-#" <+> pPrint d 0 pprop <+> text "#-};"

pvpPProp :: PDetail -> PProp -> Doc
pvpPProp d pprop = text "(*" <+> pvPrint d 0 pprop <+> text "*)"

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
      deriving (Eq, Ord, Show)

-- used for classic printing of CSyntax
-- and by various internal dumps of ISyntax/ASyntax
instance PPrint RulePragma where
    pPrint _d _p RPfireWhenEnabled = text "{-# ASSERT fire when enabled #-}"
    pPrint _d _p RPnoImplicitConditions =
        text "{-# ASSERT no implicit conditions #-}"
    pPrint _d _p RPcanScheduleFirst =
        text "{-# ASSERT can schedule first #-}"
    pPrint _d _p RPaggressiveImplicitConditions =
        text "{-# aggressive_implicit_conditions #-}"
    pPrint _d _p RPconservativeImplicitConditions =
        text "{-# conservative_implicit_conditions #-}"
    pPrint _d _p RPnoWarn =
        text "{-# no_warn #-}"
    pPrint _d _p RPwarnAllConflicts =
        text "{-# warn_all_conflicts #-}"
    pPrint _d _p RPclockCrossingRule =
        text "{-# clock-crossing rule #-}"
    pPrint _d _p (RPdoc comment) =
        text ("{-# doc = " ++ doubleQuote comment ++ " #-}")
    pPrint _d _p RPhide =
        text ("{-# hide #-}")

data SchedulePragma id_t
    = SPUrgency [id_t]
    | SPExecutionOrder [id_t]
    | SPMutuallyExclusive [[id_t]]
    | SPConflictFree [[id_t]]
    | SPPreempt [id_t] [id_t]
    | SPSchedule (MethodConflictInfo id_t)
      deriving (Eq, Ord, Show)

instance (PPrint t, Ord t) => PPrint (SchedulePragma t) where
    pPrint d p (SPUrgency ids) =
        text "{-# ASSERT descending urgency: " <+>
            pPrint d p ids <+> text "#-}"
    pPrint d p (SPExecutionOrder ids) =
        text "{-# ASSERT execution order: " <+>
            pPrint d p ids <+> text "#-}"
    pPrint d p (SPMutuallyExclusive idss) =
        text "{-# ASSERT mutually exclusive: " <+>
            pPrint d p idss <+> text "#-}"
    pPrint d p (SPConflictFree idss) =
        text "{-# ASSERT conflict-free: " <+>
            pPrint d p idss <+> text "#-}"
    pPrint d p (SPPreempt ids1 ids2) =
        text "{-# ASSERT preempt: " <+>
            pPrint d p ids1 <+> pPrint d p ids2 <+> text "#-}"
    pPrint d p (SPSchedule s) =
        text "{-# ASSERT schedule: " <+>
            pPrint d p s <+>  text "#-}"

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
      deriving (Eq, Ord, Show)

instance PPrint IfcPragma where
    pPrint  d _ (PIArgNames ids)       = text "arg_names ="   <+>
                                             brackets ( (sepList (map (ppVarId d) ids) comma) )
    pPrint _d _ (PIPrefixStr flds)     = text "prefixs ="       <+> doubleQuotes (text flds)
    pPrint _d _ (PIRdySignalName flds) = text "ready ="        <+> doubleQuotes (text flds)
    pPrint _d _ (PIEnSignalName flds)  = text "enable ="       <+> doubleQuotes (text flds)
    pPrint _d _ (PIResultName flds)    = text "result ="       <+> doubleQuotes (text flds)
    pPrint _d _ (PIAlwaysRdy )         = text "always_ready "
    pPrint _d _ (PIAlwaysEnabled )     = text "always_enabled "

instance PVPrint  IfcPragma where
    pvPrint  d _ (PIArgNames ids)       = text "ports ="   <+>
                                          brackets ( (sepList (map (doubleQuotes . (ppVarId d)) ids) comma) )
    pvPrint _d _ (PIPrefixStr flds)     = text "prefix ="       <+> doubleQuotes (text flds)
    pvPrint _d _ (PIRdySignalName flds) = text "ready ="        <+> doubleQuotes (text flds)
    pvPrint _d _ (PIEnSignalName flds)  = text "enable ="       <+> doubleQuotes (text flds)
    pvPrint _d _ (PIResultName flds)    = text "result ="       <+> doubleQuotes (text flds)
    pvPrint _d _ (PIAlwaysRdy )         = text "always_ready "
    pvPrint _d _ (PIAlwaysEnabled )     = text "always_enabled "

-- convenience function -- extract out PIArgNames ids.
filterIArgNames :: [IfcPragma] -> [Id]
filterIArgNames prags = concatMap getArgNames prags
    where getArgNames :: IfcPragma -> [Id]
          getArgNames (PIArgNames names) = names
          getArgNames _                  = []

filterPrintIfcArgs :: [IfcPragma] -> [IfcPragma]
filterPrintIfcArgs prags = filter isPrintArg prags
    where isPrintArg (PIArgNames _names) = False
          isPrintArg _x                  = True
