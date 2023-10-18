-- This corresponds to src/comp/SchedInfo.hs in bsc.
module Language.Bluespec.Classic.AST.SchedInfo
  ( SchedInfo(..)
  , MethodConflictInfo(..)
  , makeMethodConflictDocs
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.Prelude

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
        deriving (Eq, Ord, Show)

instance (PPrint idtype, Ord idtype) => PPrint (SchedInfo idtype) where
    pPrint d _p si =
        sep [text "SchedInfo",
             pPrint d 0 (methodConflictInfo si),
             pPrint d 0 (rulesBetweenMethods si),
             pPrint d 0 (rulesBeforeMethods si),
             pPrint d 0 (clockCrossingMethods si)]

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
    deriving (Eq, Ord, Show)

instance (PPrint idtype, Ord idtype) => PPrint (MethodConflictInfo idtype) where
    pPrint d p mci =
        let ds = makeMethodConflictDocs (pPrint d p) ppReadable "[" "]" mci
        in  text "[" <> sepList ds (text ",") <> text "]"

-- Given:
--   * a printing function for ids (pPrint or pvPrint)
--   * a function for turning the ids into strings for sorting order
--   * start and stop enclosure for a list (we assume comma-separated)
--   * a MethodConflictInfo structure
-- Produce:
--   a list of Doc for the MethodConflictInfo info
--   (un-factored from pairs to groups)
--
makeMethodConflictDocs :: (Ord idtype) =>
                          (idtype -> Doc) ->
                          (idtype -> String) ->
                          String -> String ->
                          MethodConflictInfo idtype -> [Doc]
makeMethodConflictDocs pId sId listStart listEnd
        (MethodConflictInfo { sCF=sCF0, sSB=sSB0, sME=sME0, sP=sP0,
                              sSBR=sSBR0, sC=sC0, sEXT=sEXT0 }) =
    [pp m <+> text "CF"  <+> pp m' | (m,m') <- toPairsOfLists sCF0  ] ++
    [pp m <+> text "SB"  <+> pp m' | (m,m') <- toPairsOfLists sSB0  ] ++
    [         text "ME"  <+> pp m  |  m     <- sME_ordered          ] ++
    [pp m <+> text "P"   <+> pp m' | (m,m') <- toPairsOfLists sP0   ] ++
    [pp m <+> text "SBR" <+> pp m' | (m,m') <- toPairsOfLists sSBR0 ] ++
    [pp m <+> text "C"   <+> pp m' | (m,m') <- toPairsOfLists sC0   ] ++
    (if null sEXT0 then [] else [text "EXT" <+> pp sEXT_ordered])
  where
      pp [m] = pId m
      pp ms  =
          text listStart <> (sepList (map pId ms) (text ",")) <> text listEnd

      collect ps = M.fromListWith (S.union) [(a,S.singleton b) | (a,b) <- ps]
      toPairsOfLists ps = let m1 = collect ps
                              m2 = collect [(s,a) | (a,s) <- M.toList m1]
                          in sortLP [ (sortI (S.toList s2), sortI (S.toList s1))
                                    | (s1,s2) <- M.toList m2
                                    ]

      sortI  = L.sortBy (\ i1 i2 -> (sId i1) `compare` (sId i2))
      sortL  = L.sortBy (\ is1 is2 -> (map sId is1) `compare` (map sId is2))
      sortLP = L.sortBy (\(is1,_) (is2,_) -> (map sId is1) `compare` (map sId is2))
      sME_ordered  = sortL (map sortI sME0)
      sEXT_ordered = sortI sEXT0
