-- This corresponds to src/comp/Pragma.hs in bsc.
module Language.Bluespec.Classic.AST.Pragma
  ( Pragma(..)
  , PProp(..)
  , RulePragma(..)
  , SchedulePragma(..)
  , CSchedulePragma
  , IfcPragma(..)

  , ppPProp
  ) where

import Text.PrettyPrint.HughesPJClass

import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.SchedInfo
import Language.Bluespec.Prelude
import Language.Bluespec.Pretty
import Language.Bluespec.Util

data Pragma
        = Pproperties Id [PProp]-- module Id and properties associate with
        | Pnoinline [Id]        -- [Id] is a list of functions which should not be inlined
        deriving (Eq, Ord, Show)

instance Pretty Pragma where
    pPrintPrec d _p (Pproperties i pps) =
        (text "{-# properties" <+> ppId d i <+> text "= { ") <>
          sepList (map (pPrintPrec d 0) pps) (text ",") <> text " } #-}"
    pPrintPrec d _p (Pnoinline is) =
        text "{-# noinline" <+> sep (map (ppId d) is) <+> text " #-}"

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

instance Pretty PProp where
    pPrintPrec  d _ (PPscanInsert i) = text "scanInsert = " <+> pPrintPrec d 0 i
    pPrintPrec _d _ (PPCLK s) = text ("clock_prefix = " ++ s)
    pPrintPrec _d _ (PPGATE s) = text ("gate_prefix = " ++ s)
    pPrintPrec _d _ (PPRSTN s) = text ("reset_prefix = " ++ s)
    pPrintPrec  d _ (PPclock_osc xs) =
        text "clock_osc = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrintPrec d _ (PPclock_gate xs) =
        text "clock_gate = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrintPrec d _ (PPgate_inhigh is) =
        text "gate_inhigh = {" <> sepList (map (ppId d) is) (text ",") <> text "}"
    pPrintPrec d _ (PPgate_unused is) =
        text "gate_unused = {" <> sepList (map (ppId d) is) (text ",") <> text "}"
    pPrintPrec d _ (PPreset_port xs) =
        text "reset_port = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrintPrec d _ (PParg_param xs) =
        text "arg_param = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrintPrec d _ (PParg_port xs) =
        text "arg_port = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrintPrec d _ (PParg_clocked_by xs) =
        text "clocked_by = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrintPrec d _ (PParg_reset_by xs) =
        text "reset_by = {"
        <> sepList [ text "(" <> ppId d i <> text "," <> (text s) <> text ")"
                   | (i,s) <- xs ]
                   (text ",")
        <> text "}"
    pPrintPrec _d _ (PPoptions os) =
        text "options = {"
        <> sepList (map (text . show) os) (text ",")
        <> text "}"
    pPrintPrec _d _ (PPdoc comment) = text ("doc = " ++ doubleQuote comment)
    pPrintPrec _d _ (PPdeprecate comment) = text ("deprecate = " ++ doubleQuote comment)
    pPrintPrec _d _ (PPinst_hide) = text "hide"
    pPrintPrec _d _p v = text (drop 2 (show v))

ppPProp :: PDetail -> PProp -> Doc
ppPProp d pprop = text "{-#" <+> pPrintPrec d 0 pprop <+> text "#-};"

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
instance Pretty RulePragma where
    pPrintPrec _d _p RPfireWhenEnabled = text "{-# ASSERT fire when enabled #-}"
    pPrintPrec _d _p RPnoImplicitConditions =
        text "{-# ASSERT no implicit conditions #-}"
    pPrintPrec _d _p RPcanScheduleFirst =
        text "{-# ASSERT can schedule first #-}"
    pPrintPrec _d _p RPaggressiveImplicitConditions =
        text "{-# aggressive_implicit_conditions #-}"
    pPrintPrec _d _p RPconservativeImplicitConditions =
        text "{-# conservative_implicit_conditions #-}"
    pPrintPrec _d _p RPnoWarn =
        text "{-# no_warn #-}"
    pPrintPrec _d _p RPwarnAllConflicts =
        text "{-# warn_all_conflicts #-}"
    pPrintPrec _d _p RPclockCrossingRule =
        text "{-# clock-crossing rule #-}"
    pPrintPrec _d _p (RPdoc comment) =
        text ("{-# doc = " ++ doubleQuote comment ++ " #-}")
    pPrintPrec _d _p RPhide =
        text ("{-# hide #-}")

data SchedulePragma id_t
    = SPUrgency [id_t]
    | SPExecutionOrder [id_t]
    | SPMutuallyExclusive [[id_t]]
    | SPConflictFree [[id_t]]
    | SPPreempt [id_t] [id_t]
    | SPSchedule (MethodConflictInfo id_t)
      deriving (Eq, Ord, Show)

instance (Pretty t, Ord t) => Pretty (SchedulePragma t) where
    pPrintPrec d p (SPUrgency ids) =
        text "{-# ASSERT descending urgency: " <+>
            pPrintPrec d p ids <+> text "#-}"
    pPrintPrec d p (SPExecutionOrder ids) =
        text "{-# ASSERT execution order: " <+>
            pPrintPrec d p ids <+> text "#-}"
    pPrintPrec d p (SPMutuallyExclusive idss) =
        text "{-# ASSERT mutually exclusive: " <+>
            pPrintPrec d p idss <+> text "#-}"
    pPrintPrec d p (SPConflictFree idss) =
        text "{-# ASSERT conflict-free: " <+>
            pPrintPrec d p idss <+> text "#-}"
    pPrintPrec d p (SPPreempt ids1 ids2) =
        text "{-# ASSERT preempt: " <+>
            pPrintPrec d p ids1 <+> pPrintPrec d p ids2 <+> text "#-}"
    pPrintPrec d p (SPSchedule s) =
        text "{-# ASSERT schedule: " <+>
            pPrintPrec d p s <+>  text "#-}"

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

instance Pretty IfcPragma where
    pPrintPrec  d _ (PIArgNames ids)       = text "arg_names ="   <+>
                                             brackets ( (sepList (map (ppVarId d) ids) comma) )
    pPrintPrec _d _ (PIPrefixStr flds)     = text "prefixs ="       <+> doubleQuotes (text flds)
    pPrintPrec _d _ (PIRdySignalName flds) = text "ready ="        <+> doubleQuotes (text flds)
    pPrintPrec _d _ (PIEnSignalName flds)  = text "enable ="       <+> doubleQuotes (text flds)
    pPrintPrec _d _ (PIResultName flds)    = text "result ="       <+> doubleQuotes (text flds)
    pPrintPrec _d _ (PIAlwaysRdy )         = text "always_ready "
    pPrintPrec _d _ (PIAlwaysEnabled )     = text "always_enabled "
