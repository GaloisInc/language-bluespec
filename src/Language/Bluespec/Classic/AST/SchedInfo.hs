-- This corresponds to src/comp/SchedInfo.hs in bsc.
module Language.Bluespec.Classic.AST.SchedInfo where

data SchedInfo idtype = SchedInfo {
        methodConflictInfo :: MethodConflictInfo idtype,
        -- methods which have a rule that must execute between them
        -- (in the given order) and the list of rules
        -- XXX should we include the rule names?  they don't exist
        -- XXX on the boundary, but could be useful for debugging
        rulesBetweenMethods :: [((idtype, idtype), [idtype])],
        -- methods which have a rule that must execute before it
        -- along with the list of rules involved.
        -- Left  = rule directly before this method
        -- Right = method called by this method which requires a rule before it
        rulesBeforeMethods :: [(idtype,[Either idtype idtype])],
        -- methods which allow an unsynchronized clock domain crossing
        clockCrossingMethods :: [idtype]
        }


{- a CF b     => a & b have the same effect when executed in parallel
                 in the same rule, or when executed in either order
                 in different rules in the same cycle.
   a SB b     => a & b have the same effect when executed in parallel
                 in the same rule, or when a is executed before b in
                 different rules in the same cycle.  Executing b before
                 a within one cycle is illegal.
   ME [a,b,c] => only one of a,b or c can execute in any cycle.
   a P b      => a & b may be executed in parallel within a single
                 rule.  It is illegal to execute a & b in the same
                 cycle in two different rules, in any order.
   a SBR b    => a may be executed before b in different rules in
                 the same cycle.  It is illegal to execute a & b in
                 parallel in a single rule, or to execute b before a
                 in two different rules in the same cycle.
   a C b      => a & b may not execute in the same cycle, whether in
                 one rule or two, in any order.
   EXT a      => two executions of a cannot occur in one rule.
                 a & b can execute in two different rules in the same
                 cycle but the effect will be different.  The
                 difference must be resolved external to the module.
-}
data MethodConflictInfo idtype =
    MethodConflictInfo {
        sCF  :: [(idtype, idtype)],
        sSB  :: [(idtype, idtype)],
        sME  :: [[idtype]],
        sP   :: [(idtype, idtype)],
        sSBR :: [(idtype, idtype)],
        sC   :: [(idtype, idtype)],
        sEXT :: [idtype]
    }
