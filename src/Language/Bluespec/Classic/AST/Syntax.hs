module Language.Bluespec.Classic.AST.Syntax
  ( CPackage(..)
  , CExport(..)
  , CImport(..)
  , CSignature(..)
  , CFixity(..)
  , CDefn(..)
  , IdK(..)
  , CFunDeps
  , CExpr(..)
  , CLiteral(..)
  , COp(..)
  , CSummands
  , CInternalSummand(..)
  , COSummands
  , COriginalSummand(..)
  , CField(..)
  , CFields
  , CCaseArm(..)
  , CCaseArms
  , CStmt(..)
  , CStmts
  , CMStmt(..)
  , CRule(..)
  , CDefl(..)
  , CDef(..)
  , CClause(..)
  , CQual(..)
  , CPat(..)
  , CPOp(..)
  , CInclude(..)

  , cApply
  , cVApply
  , getName
  , iKName
  ) where

import Data.Char (isAlpha, toLower)
import qualified Data.List as L

import Language.Bluespec.Classic.AST.Builtin.Ids
import Language.Bluespec.Classic.AST.Builtin.FStrings
import Language.Bluespec.Classic.AST.FString
import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.IntLit
import Language.Bluespec.Classic.AST.Literal
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.Pragma
import Language.Bluespec.Classic.AST.Pretty
import Language.Bluespec.Classic.AST.SchedInfo
import Language.Bluespec.Classic.AST.Type
import Language.Bluespec.Classic.AST.Undefined
import Language.Bluespec.Classic.AST.VModInfo
import Language.Bluespec.IntegerUtil
import Language.Bluespec.Lex
import Language.Bluespec.Log2
import Language.Bluespec.Prelude
import Language.Bluespec.SystemVerilog.AST.Pretty
import Language.Bluespec.Util

-- Complete package
data CPackage = CPackage
                Id                -- package name
                (Either [CExport]
                        [CExport]) -- export identifiers
                                  -- Left exps = export only exps
                                  -- Right exps = export everything but exps
                [CImport]         -- imported identifiers
                [CFixity]         -- fixity declarations for infix operators
                [CDefn]                  -- top level definitions
                [CInclude]        -- any `include files
        deriving (Eq, Ord, Show)

instance PPrint CPackage where
    pPrint d _ (CPackage i exps imps fixs def includes) =
        (t"package" <+> ppConId d i <> ppExports d exps <+> t "where {") $+$
        pBlock d 0 True (map (pp d) imps ++ map (pp d) fixs ++ map (pp d) def ++ map (pp d) includes)

instance PVPrint CPackage where
    pvPrint d _ (CPackage i exps imps fixs def includes) =
        (t"package" <+> pvp d i <> t";") $+$ empty $+$
        pBlockNT d 0 True (pvpExports d exps ++ map (pvp d) imps ++ map (pvp d) fixs ++ pdefs d def ++ map (pvp d) includes) (t"\n") $+$
        (t"endpackage:" <+> pvp d i)

pdefs :: PDetail -> [CDefn] -> [Doc]
pdefs _ [] = []
pdefs d (df1@(CPragma (Pproperties i1 _props)):df2@(CValueSign (CDef i2 _ _)):rest)
    | i1==i2 =
  (p2defs d df1 df2):(pdefs d rest)
pdefs d dfs@((CPragma (Pnoinline [i1])):(CValueSign (CDef i2 _ _)):_)
    | i1==i2 =
  (t"(* noinline *)") : (pdefs d $ tail dfs)
pdefs d (df:dfs) = (pvp d df):(pdefs d dfs)

-- XXX excluded identifiers are commented out because BSV does not support them (yet)
pvpExports :: PDetail -> Either [CExport] [CExport] -> [Doc]
pvpExports _d (Right []) = []
pvpExports  d (Right excludes) = [ text "/* Do not export (unsupported)" ] ++ map (pvp d) excludes ++ [ text "*/" ]
pvpExports  d (Left exports) = map (pvp d) exports

p2defs :: PDetail -> CDefn -> CDefn -> Doc
p2defs d (CPragma (Pproperties _ props))
         (CValueSign _df2@(CDef i qt@(CQType ps ty) cs@[CClause cps [] cexp])) | all isVar cps =
      let (ys, x) = getArrows ty
          ity = case x of (TAp (TCon _) y) -> y;
                          (TAp (TVar _) y) -> y;
                          z -> z
          f [] = empty
          f xs = t"#(" <>
                 sepList (zipWith (\ x' c -> -- t"parameter" <+>
                                            pvPrint d 0 x' <> t"" <+> pvPrint d 10 c)
                                  xs cps)
                 (t",") <> t")"
          (mId,ps') = findModId ps
          line1 = t"module" <+> pvpId d i <> f ys <> t"(" <> pvPrint d 0 ity <> t")"
       in
        if isModule mId x then
         (pProps d props $+$
         (case cexp of
           (Cmodule _ sts) ->
             (pBlockNT d 0 False
              [line1,
               if ps'==[]
               then empty
               else t "  provisos (" <> sepList (map (pvPrint d 0) ps') (t ",") <> t")"] empty)
              <> (t";") $+$
             (pvBlock d 2 False (map (pvp d) (reorderStmts sts))  empty (t"endmodule:" <+> pvp d i))
           e -> (ppValueSignRest d (pvpId d i) ps' True True line1 e "module")))
        else (pProps d props $+$ pvpValueSign d i [] qt cs)

p2defs d (CPragma (Pproperties i1 props)) (CValueSign df2@(CDef i2 _ _)) | i1==i2 =
  pProps d props $+$ pvPrint d 0 df2

p2defs _d d1 d2 = error ("p2defs (" ++ show d1 ++ ")(" ++ show d2 ++ ")")

pProps :: PDetail -> [PProp] -> Doc
pProps _ [] = empty
pProps d ps = t"(*" <+> sepList (map (pvPrint d 0) ps) (text ",") <+> text "*)"

data CExport
        = CExpVar Id    -- export a variable identifier
        | CExpCon Id    -- export a constructor
        | CExpConAll Id -- export an identifier and constructors
                        -- (datatypes, interfaces, etc.)
        | CExpPkg Id    -- export an entire package
        deriving (Eq, Ord, Show)

instance PPrint CExport where
    pPrint d _p (CExpVar i) = ppVarId d i
    pPrint d _p (CExpCon i) = ppConId d i
    pPrint d _p (CExpConAll i) = ppConId d i <> t"(..)"
    pPrint d _p (CExpPkg i) = t"package" <+> ppId d i

instance PVPrint CExport where
    pvPrint d _p (CExpVar i) = t"export" <+> pvpId d i <> t ";"
    pvPrint d _p (CExpCon i) = t"export" <+> pvpId d i <> t ";"
    pvPrint d _p (CExpConAll i) = t"export" <+> pvpId d i <> t"(..)" <> t";"
    pvPrint d _p (CExpPkg i) = t"export" <+> pvpId d i <> t"::*" <> t";"

ppExports :: PDetail -> Either [CExport] [CExport] -> Doc
ppExports _d (Right []) = empty
ppExports  d (Right noexps) = t " hiding (" <> sepList (map (pp d) noexps) (t",") <> t")"
ppExports  d (Left exports) = t "(" <> sepList (map (pp d) exports) (t",") <> t")"

data CImport
        = CImpId Bool Id                                -- Bool indicates qualified
        | CImpSign String Bool CSignature
        deriving (Eq, Ord, Show)

instance PPrint CImport where
    pPrint d _p (CImpId q i) = t"import" <+> ppQualified q <+> ppConId d i
    pPrint d _p (CImpSign _ q (CSignature i _ _ _)) = t"import" <+> ppQualified q <+> ppConId d i <+> t "..."

instance PVPrint CImport where
    pvPrint d _p (CImpId q i) = t"import" <+> pvpId d i <> t "::*" <> t";" <+> ppQualified q
    pvPrint d _p (CImpSign _ q (CSignature i _ _ _)) = t"import" <+> pvpId d i <> t "::*" <+> t "...;" <+> ppQualified q

ppQualified :: Bool -> Doc
ppQualified True = text "qualified"
ppQualified False = empty

-- Package signature from import
data CSignature
        = CSignature Id [Id] [CFixity] [CDefn]        -- package name, imported packages, definitions
        deriving (Eq, Ord, Show)

instance PPrint CSignature where
    pPrint d _ (CSignature i imps fixs def) =
        (t"signature" <+> ppConId d i <+> t "where" <+> t "{") $+$
        pBlock d 0 True (map pi' imps ++ map (pp d) fixs ++ map (pp d) def)
      where pi' i' = t"import" <+> ppConId d i'

instance PVPrint CSignature where
    pvPrint d _ (CSignature i imps fixs def) =
        (t"signature" <+> pvp d i <+> t "where {") $+$
        pvBlock d 0 True (map pi' imps ++ map (pvp d) fixs ++ map (pvp d) def) (t";") (t"}")
      where pi' i' = t"include" <+> pvpId d i'

data CFixity
        = CInfix  Integer Id
        | CInfixl Integer Id
        | CInfixr Integer Id
        deriving (Eq, Ord, Show)

instance PPrint CFixity where
    pPrint d _ (CInfix  p i) = text "infix"  <+> text (show p) <+> ppInfix d i
    pPrint d _ (CInfixl p i) = text "infixl" <+> text (show p) <+> ppInfix d i
    pPrint d _ (CInfixr p i) = text "infixr" <+> text (show p) <+> ppInfix d i

instance PVPrint CFixity where
    pvPrint d _ (CInfix  p i) = text "`infix"  <+> text (show p) <+> pvpInfix d i
    pvPrint d _ (CInfixl p i) = text "`infixl" <+> text (show p) <+> pvpInfix d i
    pvPrint d _ (CInfixr p i) = text "`infixr" <+> text (show p) <+> pvpInfix d i

-- Top level definition
data CDefn
        = Ctype IdK [Id] CType
        | Cdata { cd_visible :: Bool,
                  cd_name :: IdK,
                  cd_type_vars :: [Id],
                  cd_original_summands :: COSummands,
                  cd_internal_summands :: CSummands,
                  cd_derivings :: [CTypeclass] }
        | Cstruct Bool StructSubType IdK [Id] CFields
                  [CTypeclass]
                  -- Bool indicates the constrs are visible
                  -- first [Id] are the names of this definition's argument type variables
                  -- last [CTypeclass] are derived classes
        -- incoherent_matches superclasses name_with_kind variables fundeps default_methods
        | Cclass (Maybe Bool) [CPred] IdK [Id] CFunDeps CFields
        | Cinstance CQType [CDefl]
        | CValue Id [CClause]
        | CValueSign CDef
        | Cforeign { cforg_name :: Id,
                     cforg_type :: CQType,
                     cforg_foreign_name :: Maybe String,
                     cforg_ports :: Maybe ([String], [String]) }
        | Cprimitive Id CQType
        | CprimType IdK
        | CPragma Pragma
        -- only in package signatures
        | CIinstance Id CQType
        -- CItype is imported abstractly
        | CItype IdK [Id] [Position] -- positions of use that caused export
        | CIclass (Maybe Bool) [CPred] IdK [Id] CFunDeps [Position] -- positions of use that caused export
        | CIValueSign Id CQType
        deriving (Eq, Ord, Show)

instance PPrint CDefn where
    pPrint d _p (Ctype i as ty) =
        sep [sep ((t"type" <+> ppConIdK d i) : map (nest 2 . ppVarId d) as) <+> t "=",
                  nest 2 (pp d ty)]
    pPrint d _p (Cdata { cd_visible = vis,
                        cd_name = i,
                        cd_type_vars = as,
                        cd_original_summands = cs@(_:_),
                        cd_internal_summands = [],
                        cd_derivings = _ds }) =                -- a hack to print original constructors
        sep [sep ((t"data" <+> ppConIdK d i) : map (nest 2 . ppVarId d) as) <> t(if vis then " =" else " =="),
                  nest 2 (ppOSummands d cs)]
    pPrint d _p (Cdata { cd_visible = vis,
                        cd_name = i,
                        cd_type_vars = as,
                        cd_internal_summands = cs,
                        cd_derivings = ds }) =
        sep [sep ((t"data" <+> ppConIdK d i) : map (nest 2 . ppVarId d) as) <> t(if vis then " =" else " =="),
                  nest 2 (ppSummands d cs)]
        <> ppDer d ds
    pPrint d _p (Cstruct vis (SInterface prags) i as fs ds) =
        (t("interface ") <> sep (ppConIdK d i : map (nest 2 . ppVarId d) as) <+> ppIfcPragma d prags <+> t(if vis then "= {" else "== {")) $+$
        pBlock d 4 False (map (ppField d) fs) <> ppDer d ds
    pPrint d _p (Cstruct vis _ss i as fs ds) =
        (t("struct ") <> sep (ppConIdK d i : map (nest 2 . ppVarId d) as) <+> t(if vis then "= {" else "== {")) $+$
        pBlock d 4 False (map (ppField d) fs) <> ppDer d ds
    pPrint d _p (Cclass incoh ps ik is fd ss) =
        (t_cls <+> ppPreds d ps (sep (ppConIdK d ik : map (ppVarId d) is)) <> ppFDs d fd <+> t "where {") $+$
        pBlock d 4 False (map (ppField d) ss)
      where t_cls = case incoh of
                     Just False -> t"class coherent"
                     Just True  -> t"class incoherent"
                     Nothing    -> t"class"
    pPrint d _p (Cinstance qt ds) =
        (t"instance" <+> pPrint d 0 qt <+> t "where {") $+$
        pBlock d 4 False (map (pPrint d 0) ds)
    pPrint d p (CValueSign def) = pPrint d p def
    pPrint d p (CValue i cs) =
        vcat (map (\ cl -> ppClause d p [ppVarId d i] cl <> t";") cs)
    pPrint d _p (Cprimitive i ty) =
        text "primitive" <+> ppVarId d i <+> t "::" <+> pp d ty
    pPrint d  p (CPragma pr) = pPrint d p pr
    pPrint d _p (CprimType ik) =
        t"primitive type" <+>
        -- don't use ppConIdK because this syntax has no parentheses
        case (ik) of
            (IdK i)        -> ppConId d i
            (IdKind i k)   -> ppConId d i <+> t "::" <+> pp d k
            (IdPKind i pk) -> ppConId d i <+> t "::" <+> pp d pk
    pPrint d _p (Cforeign i ty oname opnames) =
        text "foreign" <+> ppVarId d i <+> t "::" <+> pp d ty <> (case oname of Nothing -> text ""; Just s -> text (" = " ++ show s)) <> (case opnames of Nothing -> text ""; Just (is, os) -> t"," <> pparen True (sep (map (text . show) is ++ po os)))
      where po [o] = [text ",", text (show o)]
            po os = [t"(" <> sepList (map (text . show) os) (t",") <> t ")"]
    pPrint d _p (CIinstance i qt) =
        t"instance" <+> ppConId d i <+> pPrint d 0 qt
    pPrint d _p (CItype i as _positions) =
        sep (t"type" <+> ppConIdK d i : map (nest 2 . ppVarId d) as)
    pPrint d _p (CIclass incoh ps ik is fd _positions) =
        t_cls <+> ppPreds d ps (sep (ppConIdK d ik : map (nest 2 . ppVarId d) is)) <> ppFDs d fd
      where t_cls = case incoh of
                     Just False -> t"class coherent"
                     Just True  -> t"class incoherent"
                     Nothing    -> t"class"
    pPrint d _p (CIValueSign i ty) = ppVarId d i <+> t "::" <+> pp d ty

instance PVPrint CDefn where
    pvPrint d _p (Ctype i [] ty) =
        sep [t"typedef",
                  nest 2 (pvp d ty),
                  pvp d i <> t";"]
    pvPrint d _p (Ctype i as ty) =
        sep [t"typedef",
                  nest 2 (pvp d ty) <+> pvp d i <+>
                  pvParameterTypeVars d as <> t ";"]

    pvPrint d _p (Cdata { cd_visible = vis,
                         cd_name = i,
                         cd_type_vars = as,
                         cd_original_summands = cs@(_:_),
                         cd_internal_summands = [],
                         cd_derivings = _ds }) =  -- a hack to print original constructors
        sep [sep ((t"data1" <+> pvp d i) : map (nest 2 . pvPrint d maxPrec) as) <+>
                     t(if vis then "=" else "=="),
                  nest 2 (ppOSummands d cs)]

    pvPrint d _p (Cdata { cd_visible = _vis,
                         cd_name = i,
                         cd_type_vars = as,
                         cd_internal_summands = cs,
                         cd_derivings = ds }) =
      let typarams = pvParameterTypeVars d as
          ppCon summand = pvPrint d 0 (cis_arg_type summand) <+>
                          pvpId d (getCISName summand) -- XXX print all the names?
          ppIde summand  = pvpId d (getCISName summand) -- XXX print all the names?
          isVoid (CInternalSummand { cis_arg_type = TCon (TyCon unit _ _) }) =
              (unit == idPrimUnit)
          isVoid _ = False
          isEnum = all isVoid
      in
       (if isEnum cs
         then
          t"typedef enum {" $+$
          sepList (map (nest 2 . ppIde) cs) (t",") <> (t"}")
         else
          t"typedef union tagged {" $+$
          pvBlock d 4 False (map ppCon cs) (t";") (t"}"))
       <+> pvp d i <+> typarams <+> pvpDer d ds <> t";"

    pvPrint d _p (Cstruct vis (SInterface ps) i [] fs ds) =
        ppIfcPrags d (Just ps) $$
        (t"interface" <+>
         pvp d i <> t";" <+> if vis then empty else t"/*") $+$
        pvBlock d 4 False (map (pvpField d (t"method") True) fs) (t";") (t"endinterface:" <+> pvp d i) <+> pvpDer d ds
    pvPrint d _p (Cstruct vis (SInterface ps) i as fs ds) =
        ppIfcPrags d (Just ps) $$
        (t"interface" <+>
         pvPrint d 9 i <+> pvParameterTypeVars d as
         <> t";" <+> (if vis then empty else t"/*")) $+$
        pvBlock d 4 False (map (pvpField d (t"method") True) fs)(t";") (t"endinterface:" <+> pvp d i) <+> (if vis then empty else t"*/") <+> pvpDer d ds


    pvPrint d _p (Cstruct _vis _ i as fs ds) =
      let typarams = pvParameterTypeVars d as
--          ppCon (i, ty) = pvPrint d 0 ty <+> pvpId d i
      in
        t"typedef struct" <+> t "{" $+$
        pvBlock d 4 False (map (pvpField d empty False) fs)(t";") (t"} ") <>
        pvp d i <+> typarams <+> pvpDer d ds <> t";"

{-        (t"typedef struct" <+>
         pvp d i <> t(if vis then ";" else "; /*")) $+$
        pBlock d 4 False (map (ppField d "" False) fs)  (t";") (t"}") <+> pvpDer d ds
-}

    pvPrint d p (CValueSign def) = pvPrint d p def

    pvPrint d _p (Cclass Nothing ps ik is fd ss) =
       ((pBlockNT d 0 False
        [t"typeclass" <+> pvp d ik <+> pvParameterTypeVars d is,
         pvpFDs d fd,
         if ps==[]
           then empty
           else t "  provisos (" <> sepList (map (pvPrint d 0) ps) (t",") <> t")"] empty)<> (t";")) $+$
       pBlockNT d 4 False (map (\s -> pvpField d (t"function") True s <> t";") ss) empty $+$
       t"endtypeclass"

    pvPrint d _p (Cinstance (CQType ps ty) ds) =
      let (x, ys) = unravel ty
      in
       ((pBlockNT d 0 False
        [t"instance" <+> pvPrint d 9 x <> pvParameterTypes d ys,
         if ps==[]
           then empty
           else t "  provisos (" <> sepList (map (pvPrint d 0) ps) (t",") <> t")"] empty)<> (t";")) $+$
       pBlockNT d 4 False (map (pvPrint d 0) ds) empty $+$
       t"endinstance"

    pvPrint d _p (Cprimitive i ty) =
        text "primitive" <+> pvpId d i <+> t "::" <+> pvp d ty

    pvPrint d p (CPragma pr) = pvPrint d p pr

    pvPrint d _p (CprimType (IdKind i k)) =
        t"primitive type" <+> pvp d i <+> t "::" <+> pvp d k

    pvPrint d _p (Cforeign i ty oname opnames) =
        text "foreign" <+> pvpId d i <+> t "::"
                <+> pvp d ty
                <> (case oname of Nothing -> empty; Just s -> text (" = " ++ show s))
                <> (case opnames of
                    Nothing -> empty;
                    Just (is, os) ->
                        t"," <> pparen True (sep (map (text . show) is ++ po os)))
      where po [o] = [text ",", text (show o)]
            po os = [t"(" <> sepList (map (text . show) os) (t",") <> t ")"]

{-
    -- XXX These are not in BSV
    pvPrint d p (CIinstance i qt) =
        t"instance" <+> pvpId d i <> t"" <+> pvPrint d 0 qt
    pvPrint d p (CItype i as usePositions) =
        sep (t"type" <+> pvp d i : map (nest 2 . pvPrint d maxPrec) as)

    pvPrint d p (Cclass (Just _) ps ik is fd ss) =

    pvPrint d p (CIclass incoh ps ik is fds usePositions) =
        let pdoc = if ps==[]
                   then empty
                   else t "  provisos (" <>
                        sepList (map (pvPrint d 0) ps) (t",") <> t")"
        in  (pBlockNT d 0 False
              [t"class" <+> pvp d ik <+> pvParameterTypeVars d is,
               pvpFDs d fd, pdoc] empty)
            <> (t";")
    pvPrint d p (CIValueSign i ty) = pvpId d i <+> t "::" <+> pvp d ty
-}
    pvPrint _d _p x = error ("pvPrint CDefn bad: " ++ show x)

instance HasPosition CDefn where
    getPosition d = getPosition (getName d)

pvpFD :: PDetail -> ([Id], [Id]) -> Doc
pvpFD d (as,rs) = sep (map (pvpId d) as) <+> t "->" <+> sep (map (pvpId d) rs)

pvpFDs :: PDetail -> CFunDeps -> Doc
pvpFDs _d [] = empty
pvpFDs  d fd = text "  dependencies" <+> sepList (map (pvpFD d) fd) (t",")

pvParameterTypes :: PDetail -> [Type] -> Doc
pvParameterTypes _d [] = empty
pvParameterTypes  d ts =
    t"#(" <> sepList (map (\ y -> pvPrint d 0 y) ts) (t ",") <> t ")"

pvParameterTypeVars :: PDetail -> [Id] -> Doc
pvParameterTypeVars _d [] = empty
pvParameterTypeVars  d as =
    t"#(" <> sepList (map (\ y -> t"type" <+> pvPrint d 0 y) as) (t ",") <> t ")"

-- Since IdPKind is only expected in some disjuncts of CDefn, we could
-- create a separate IdPK for those cases, but that seems like overkill.
-- IdPKind in other locations will just be treated like IdK (no kind info).
data IdK
        = IdK Id
        | IdKind Id Kind
        -- this should not exist after typecheck
        | IdPKind Id PartialKind
        deriving (Eq, Ord, Show)

instance PPrint IdK where
    pPrint d  p (IdK i) = pPrint d p i
    pPrint d _p (IdKind i k) = pparen True $ pp d i <+> t "::" <+> pp d k
    pPrint d _p (IdPKind i pk) = pparen True $ pp d i <+> t "::" <+> pp d pk

instance PVPrint IdK where
    pvPrint d  p (IdK i) = pvPrint d p i
    pvPrint d _p (IdKind i k) = pparen True $ pvp d i <+> t "::" <+> pvp d k
    pvPrint d _p (IdPKind i pk) = pparen True $ pvp d i <+> t "::" <+> pvp d pk

instance HasPosition IdK where
    getPosition (IdK i) = getPosition i
    getPosition (IdKind i _) = getPosition i
    getPosition (IdPKind i _) = getPosition i

pBlock :: PDetail -> Int -> Bool -> [Doc] -> Doc
pBlock _ _n _ [] = t"}"
pBlock _  n nl xs =
        (t (replicate n ' ') <>
        foldr1 ($+$) (map (\ x -> x <> if nl then t";" $+$ t"" else t";") (init xs) ++ [last xs])) $+$
        t"}"

-- Renamed from pBlock in src/comp/CVPrint.hs to avoid naming conflicts.
pvBlock :: p -> Int -> Bool -> [Doc] -> Doc -> Doc -> Doc
pvBlock _d _n _ [] _ ket = ket
pvBlock _d  n nl xs sep' ket =
        (t (replicate n ' ') <>
        foldr1 ($+$) (map (\ x -> x <> if nl then sep' $+$ empty else sep') xs))  $+$
        ket

pBlockNT :: PDetail -> Int -> Bool -> [Doc] -> Doc -> Doc
pBlockNT _ _n _ [] _ = empty
pBlockNT _  n nl xs sep' =
        (t (replicate n ' ') <>
                foldr1 ($+$) (map (\ x -> x <> if nl then sep' $+$ empty else sep') xs))

ppDer :: PDetail -> [CTypeclass] -> Doc
ppDer _d [] = text ""
ppDer  d is = text " deriving (" <> sepList (map (pPrint d 0) is) (text ",") <> text ")"

-- Renamed from ppDer in src/comp/CVPrint.hs to avoid naming conflicts.
pvpDer :: PDetail -> [CTypeclass] -> Doc
pvpDer _d [] = empty
pvpDer  d is = text "deriving (" <> sepList (map (pvPrint d 0) is) (text ",") <> text ")"

type CFunDeps = [([Id],[Id])]

-- Expressions
data CExpr
        = CLam (Either Position Id) CExpr
        | CLamT (Either Position Id) CQType CExpr
        | Cletseq [CDefl] CExpr -- rhs of "let x = x" refers to previous def
                                --   before current let or in earlier arm
        | Cletrec [CDefl] CExpr -- rhs of "let x = x" refers to self
        | CSelect CExpr Id                        -- expr, field id
        | CCon Id [CExpr]                        -- constructor id, arguments
        | Ccase Position CExpr CCaseArms
        -- Either a struct type or a constructor with named fields.
        -- The 'Maybe Bool' argument can indicate if it is specifically
        -- one or the other (True for struct), otherwise the typechecker
        -- will attempt to determine which is intended.
        | CStruct (Maybe Bool) Id [(Id, CExpr)]
        | CStructUpd CExpr [(Id, CExpr)]

        -- for hardware writes
        -- lhs <= rhs
        | Cwrite Position CExpr CExpr

        | CAny Position UndefKind
        | CVar Id
        | CApply CExpr [CExpr]
        | CTaskApply CExpr [CExpr] -- system task calls
        | CTaskApplyT CExpr CType [CExpr] -- type-checked $task (only $display) calls (the type is the inferred function type for the varargs task)
        | CLit CLiteral
        | CBinOp CExpr Id CExpr
        | CHasType CExpr CQType
        | Cif Position CExpr CExpr CExpr
        -- x[a]
        | CSub Position CExpr CExpr
        -- x[a:b]
        | CSub2 CExpr CExpr CExpr
        -- x[a:b] = y
        | CSubUpdate Position CExpr (CExpr, CExpr) CExpr
        | Cmodule Position [CMStmt]
        | Cinterface Position (Maybe Id) [CDefl]
        | CmoduleVerilog
              CExpr               -- expr for the module name (type String)
              Bool                -- whether it is a user-imported module
              VClockInfo          -- clocks
              VResetInfo          -- resets
              [(VArgInfo,CExpr)]  -- input arguments
              [VFieldInfo]        -- output interface fields
              VSchedInfo          -- scheduling annotations
              VPathInfo           -- path annotations
        | CForeignFuncC Id CQType -- link name, wrapped type
        | Cdo Bool CStmts        -- Bool indicates recursive binding
        | Caction Position CStmts
        | Crules [CSchedulePragma] [CRule]
        -- used before operator parsing
        | COper [COp]
        -- from deriving
        | CCon1 Id Id CExpr                        -- type id, con id, expr
        | CSelectTT Id CExpr Id                        -- type id, expr, field id
        -- INTERNAL in type checker
        | CCon0 (Maybe Id) Id                        -- type id, constructor id
        -- Not part of the surface syntax, used after type checking
        | CConT Id Id [CExpr]                        -- type id, constructor id, arguments
        | CStructT CType [(Id, CExpr)]
        | CSelectT Id Id                        -- type id, field id
        | CLitT CType CLiteral
        | CAnyT Position UndefKind CType
        | CmoduleVerilogT CType
              CExpr               -- expr for the module name (type String)
              Bool                -- whether it is a user-imported module
              VClockInfo          -- clocks
              VResetInfo          -- resets
              [(VArgInfo,CExpr)]  -- input arguments
              [VFieldInfo]        -- output interface fields
              VSchedInfo          -- scheduling annotations
              VPathInfo           -- path annotations
        | CForeignFuncCT Id CType -- link name, primitive type
        | CTApply CExpr [CType]
        -- for passing pprops as values
        | Cattributes [(Position,PProp)]
        deriving (Eq, Ord, Show)

instance PPrint CExpr where
    pPrint d p (CLam ei e) = ppQuant "\\ "  d p ei e
    pPrint d p (CLamT ei _ty e) = ppQuant "\\ "  d p ei e
    pPrint d p (Cletseq [] e) = pparen (p > 0) $
        (t"letseq in" <+> pp d e)
    pPrint d p (Cletseq ds e) = pparen (p > 0) $
        (t"letseq" <+> text "{" <+> foldr1 ($+$) (map (pp d) ds)) <+> text "}" $+$
        (t"in  " <> pp d e)
    pPrint d p (Cletrec [] e) = pparen (p > 0) $
        (t"let in" <+> pp d e)
    pPrint d p (Cletrec ds e) = pparen (p > 0) $
        (t"let" <+> t "{" <+> foldr1 ($+$) (map (pp d) ds)) <+> t "}" $+$
        (t"in  " <> pp d e)
    pPrint  d  p (CSelect e i) = pparen (p > (maxPrec+2)) $ pPrint d (maxPrec+2) e <> t"." <> ppVarId d i
    pPrint  d _p (CCon i []) = ppConId d i
    pPrint  d  p (CCon i es) = pPrint d p (CApply (CCon i []) es)
    pPrint  d  p (Ccase _pos e arms) = pparen (p > 0) $ ppCase d e arms
    pPrint _d _p (CAny {}) = text "_"
    pPrint  d _p (CVar i) = ppVarId d i
    pPrint _d _p (CStruct _ tyc []) | tyc == idPrimUnit = text "()"
    pPrint  d  p (CStruct _ tyc ies) = pparen (p > 0) $ pPrint d (maxPrec+1) tyc <+> t "{" <+> sepList (map f ies ++ [t"}"]) (t";")
        where f (i, e) = ppVarId d i <+> t "=" <+> pp d e
    pPrint d p (CStructUpd e ies) = pparen (p > 0) $ pPrint d (maxPrec+1) e <+> t "{" <+> sepList (map f ies ++ [t"}"]) (t";")
        where f (i, e') = ppVarId d i <+> t "=" <+> pp d e'
    pPrint d p (Cwrite _ e v)  = pparen (p > 0) $ pPrint d (maxPrec+1) e <+> t ":=" <+> pPrint d p v
    pPrint d p (CApply e [])
      | d == PDReadable
      = pPrint PDReadable p e
    pPrint d p (CApply e es) = pparen (p>(maxPrec-1)) $
        sep (pPrint d (maxPrec-1) e : map (nest 2 . ppApArg) es)
        where ppApArg e' = pPrint d maxPrec e'
    pPrint d p (CTaskApply e es) = pparen (p>(maxPrec-1)) $
        sep (pPrint d (maxPrec-1) e : map (nest 2 . ppApArg) es)
        where ppApArg e' = pPrint d maxPrec e'
    -- XXX: should include t?
    pPrint d p (CTaskApplyT e _t es) = pparen (p>(maxPrec-1)) $
        sep (pPrint d (maxPrec-1) e : map (nest 2 . ppApArg) es)
        where ppApArg e' = pPrint d maxPrec e'
    pPrint d  p (CLit l) = pPrint d p l
    pPrint d  p (CBinOp e1 i e2) = ppOp d p i e1 e2
    pPrint d  p (CHasType e t') = pparen (p>0) $ pPrint d maxPrec e <> text "::" <> pPrint d maxPrec t'
    pPrint d  p (Cif _pos c tr e) = pparen (p>0) (sep [t"if" <+> pp d c <+> t "then", nest 4 (pp d tr), t"else", nest 4 (pp d e)])
    pPrint d _p (CSub _pos e s) = pPrint d maxPrec e <> t"[" <> pp d s <> t"]"
    pPrint d _p (CSub2 e h l) = pPrint d maxPrec e <> t"[" <> pp d h <> t":" <> pp d l <> t"]"
    pPrint d  p (CSubUpdate _pos e (h, l) rhs) = pPrint d p (CSub2 e h l) <> t"=" <> pPrint d maxPrec rhs
    pPrint d _p (Cmodule _ is) = t"module {" $+$ pBlock d 2 False (map (pp d) is)
    pPrint d  p (Cinterface _pos Nothing ds) =
        pparen (p>0) (t"interface {" $+$ pBlock d 2 False (map (pp d) ds))
    pPrint d p (Cinterface _pos (Just i) ds) =
        pparen (p>0) (t"interface" <+> pp d i <+> t "{" $+$ pBlock d 2 False (map (pp d) ds))
    pPrint d _p (CmoduleVerilog m _ui c r ses fs sch _ps) =
        sep [
          t"module verilog" <+> pp d m <+>
          pp d c <> t"" <+> pp d r <+> t"",
          nest 4 (if null ses then t"" else pparen True (sepList (map ppA ses) (t","))),
          nest 4 (t"{" $+$ pBlock d 2 False (map f fs)),
          nest 4 (pp d sch) ]
          where mfi _s Nothing = empty
                mfi  s (Just i) = t s <+> ppVarId d i
                mfp _s Nothing = empty
                mfp  s (Just (VName s', _)) = t s <+> t s'
                f (Clock i) = t "clock_field " <> ppVarId d i
                f (Reset i) = t "reset_field " <> ppVarId d i
                f (Inout i (VName p') mc mr) =
                    t "inout_field " <> ppVarId d i <+> t p' <+>
                    mfi "clocked_by" mc <+> mfi "reset_by" mr
                f (Method i mc mr n ps mo me) =
                    ppVarId d i <> g n <+> t "=" <+> t (unwords (map h ps)) <+>
                    mfi "clocked_by" mc <+> mfi "reset_by" mr <+> mfp "output" mo <+> mfp "enable" me
                g 1 = t""
                g n = t("[" ++ itos n ++ "]")
                h (s,[]) = show s
                h (s,ps) = show s ++ "{" ++ L.intercalate "," (map (drop 2 . show) ps) ++ "}"
                ppA (ai, e) = text "(" <> text (ppReadable ai) <> text "," <+> pp d e <> text ")"
    pPrint d _p (CForeignFuncC i _wrap_ty) =
        -- There's no real Classic syntax for this:
        t"ForeignFuncC" <+> pp d i
    pPrint d p (Cdo _ ss) = pparen (p>0) $ t "do" <+> t "{" <+> sepList (map (pPrint d 0) ss ++ [t"}"]) (t";")
    pPrint d p (Caction _ ss) = pparen (p>0) $ t "action" <+> t "{" <+> sepList (map (pPrint d 0) ss ++ [t"}"]) (t";")
    pPrint d p (Crules [] rs) = pparen (p>0) $ t"rules {" $+$ pBlock d 2 False (map (pp d) rs)
    pPrint d p (Crules ps rs) = pPrint d p ps $+$
                                (pparen (p>0) $ t"rules {" $+$ pBlock d 2 False (map (pp d) rs))
    pPrint d p (COper ops) = pparen (p > maxPrec-1) (sep (map (pPrint d (maxPrec-1)) ops))
    ----
    pPrint d p (CCon1 _ i e) = pPrint d p (CCon i [e])
    pPrint d p (CSelectTT _ e i) = pparen (p > (maxPrec+2)) $ pPrint d (maxPrec+2) e <> t"." <> ppVarId d i
    ----
    pPrint d _p (CCon0 _ i) = ppConId d i
    ----
    pPrint d p (CConT _ i es) = pPrint d p (CCon i es)
    pPrint d p (CStructT ty ies) = pPrint d p (CStruct (Just True) tyc ies)
        -- where (Just tyc) = leftCon ty
        where tyc = case leftCon ty of
                      Just tyc' -> tyc'
                      Nothing   -> error "Syntax.Pretty(CExpr): CStructT (leftCon ty failed)"
    pPrint  d _p (CSelectT _ i) = text "." <> ppVarId d i
    pPrint  d  p (CLitT _ l) = pPrint d p l
    pPrint _d _p (CAnyT _pos _uk _t) = text "_"
    pPrint  d  p (CmoduleVerilogT _ m ui c mr ses fs sch ps) = pPrint d p (CmoduleVerilog m ui c mr ses fs sch ps)
    pPrint  d _p (CForeignFuncCT i _prim_ty) = t"ForeignFuncC" <+> pp d i
    pPrint  d  p (CTApply e ts) = pparen (p>(maxPrec-1)) $
        sep (pPrint d (maxPrec-1) e : map (nest 2 . ppApArg) ts)
        where ppApArg ty = t"\183" <> pPrint d maxPrec ty
    pPrint d _p (Cattributes pps) = pparen True $ text "Attributes" <+> pPrint d 0 (map snd pps)

instance PVPrint CExpr where
    pvPrint d p (CLam i e) = ppQuant "\\ "  d p i e
    pvPrint d p (CLamT i _ty e) = ppQuant "\\ "  d p i e
    pvPrint d p (Cletrec [] e) = pparen (p > 0) $
        (t"/* empty letseq */" $+$ pvp d e)
    --pvPrint d p (Cletrec ds e) = pparen (p > 0) $
    --        (t"let" <+> foldr1 ($+$) (map (pvp d) ds)) $+$
    --  (t"in  " <> pvp d e)
    pvPrint d p (Cletrec ds e) =
        t "/* letrec */" $+$
        if (p>1) then t"(begin" <+> ppLet <>t";"$+$ t"end)"
                 else if (p==1) then t"begin" <+> ppLet <>t";"$+$ t"end"
                 else ppLet
          where ppLet = ((foldr1 ($+$) (map (pvp d) ds)) $+$ pparen True (pvp d e))
    pvPrint d p (Cletseq [] e) = pparen (p > 0) $
        (t"let in" <+> pvp d e)
    --pvPrint d p (Cletrec ds e) = pparen (p > 0) $
    --        (t"let" <+> foldr1 ($+$) (map (pvp d) ds)) $+$
    --  (t"in  " <> pvp d e)
    pvPrint d p (Cletseq ds e) =
        if (p>1) then t"(begin" <+> ppLet <>t";"$+$ t"end)"
                 else if (p==1) then t"begin" <+> ppLet <>t";"$+$ t"end"
                 else ppLet
          where ppLet = ((foldr1 ($+$) (map (pvp d) ds)) $+$ pparen True (pvp d e))
    -- undo ._read desugaring
    pvPrint d p (CSelect e i) | i `qualEq` id_read noPosition = pvPrint d p e
    pvPrint d p (CSelect e i) = pparen (p > (maxPrec+2)) $ pvPrint d (maxPrec+2) e <> t"." <> pvpId d i

--    pvPrint d p (CCon i es) = pparen (p>(maxPrec-1)) $
--        pvpId d i <> t"{" (sepList (map (pvp d) es) (t",") ) <> t"}"
--    pvPrint d p (CCon i es) =  pvPrint d p (cVApply i es)
    pvPrint d p (CCon i [p2@(CCon i' _)])|
          getIdString i /= "," && getIdString i' == "," =
       pparen (p>(maxPrec-1)) $ (pvpId d i)<+> pparen True (pvp d p2)
    pvPrint d p (CCon i [p2@(CBinOp _ i' _)])|
          getIdString i /= "," && getIdString i' == "," =
       pparen (p>(maxPrec-1)) $ (pvpId d i)<+> pparen True (pvp d p2)
    pvPrint d _p (CCon i []) = pvpId d i
    pvPrint d _p (CCon i as) | getIdString i == "," = ppTuple d as
    pvPrint d  p (CCon i as) =
     pparen (p>(maxPrec-1)) $
       (pvpId d i) <+> pparen True (sepList(map (pvPrint d 1) as) (t","))

    pvPrint d p (Ccase _pos e arms) =
        if (p>1) then t"(begin" <+> ppCase d e arms $+$ t"end)"
                 else if (p==1) then t"begin" <+> ppCase d e arms $+$ t"end"
                 else ppCase d e arms
    pvPrint _d _p (CAny {}) = text "?"
    pvPrint  d _p (CVar i) = pvpId d i
    pvPrint _d _p (CStruct _ tyc []) | tyc == idPrimUnit = text "()"
    pvPrint  d  p (CStruct mb tyc ies) =
      pparen (p > 0) $
          mtagged <+> pvPrint d (maxPrec+1) tyc <+> t "{" <+> sepList (map f ies ) (t",") <> t"}"
        where f (i, e) = pvpId d i <+> t ":" <+> pvp d e
              mtagged = case mb of
                          Just False -> text "tagged"
                          _ -> empty
    pvPrint d _p (CStructUpd e ies) = ppStrUpd d e ies
--        sep (pvPrint d (maxPrec-1) e : map (nest 2 . ppApArg) es)
--      where ppApArg e = pvPrint d maxPrec e
    pvPrint d p (Cwrite _pos e v)  = pparen (p > 0) $ pvPrint d (maxPrec+1) e <+> t "<=" <+> pvPrint d p v
    pvPrint d p (CApply (CVar i) [pos, v, idx]) | i == idPrimSelectFn noPosition =
      pvPrint d p (CSub (getPosition pos) v idx)
    pvPrint d p (CApply (CVar i) [CHasType (CVar _) (CQType [] (TAp (TCon _) ty))])
        | getIdBaseString i == "primValueOf"
      = pparen (p>(maxPrec-1)) $ t"valueOf" <> pparen True (pvp d ty)

    pvPrint d _p (CApply (CVar i)
                 [CHasType the_lit@(CLit (CLiteral _
                                          ( LInt (IntLit _w _b _v))))
                  (CQType [] (TAp (TCon (TyCon i2 _ _)) (TCon (TyNum nTy _))))])
          | getIdBaseString i == "unpack" && getIdBaseString i2 == "Bit"
      = (t $ show nTy) <> (pvp d the_lit)

    pvPrint d p (CApply e@(CVar _) es) = pparen (p>(maxPrec-1)) $
        pvp d e <> pparen True (sepList (map (pvPrint d 1) es) (t",") )
    pvPrint d p (CApply e es) = pparen (p>(maxPrec-1)) $
        pparen True (pvp d e) <> pparen True (sepList (map (pvPrint d 1) es) (t",") )
    pvPrint d p (CTaskApply e es) = pparen (p>(maxPrec-1)) $
        pvp d e <> pparen True (sepList (map (pvPrint d 1) es) (t",") )
    pvPrint d p (CTaskApplyT e _tt es) = pparen (p>(maxPrec-1)) $
        pvp d e <> pparen True (sepList (map (pvPrint d 1) es) (t",") )
    pvPrint d p (CLit l) = pvPrint d p l
    pvPrint d p (CBinOp e1 i e2) = pvpOp d p i e1 e2
    pvPrint d p (CHasType e t') = pparen (p>0) $
        pvPrint d maxPrec t' <> text "'" <> (pparen True $ pvp d e)
    pvPrint d p (Cif _pos c tr e) = pparen (p>0) (sep [pvPrint d 1 c <+> t "?", nest 4 (pvPrint d 1 tr), t":", nest 4 (pvPrint d 1 e)])
    pvPrint d _p (CSub _pos e s) = pvPrint d maxPrec e <> t"[" <> pvp d s <> t"]"
    pvPrint d _p (CSub2 e h l) = pvPrint d maxPrec e <> t"[" <> pvp d h <> t":" <> pvp d l <> t"]"
    -- XXX not valid BSV
    pvPrint d p (CSubUpdate _pos e_vec (e_h, e_l) e_rhs) = pvPrint d p (CSub2 e_vec e_h e_l) <> t"=" <> pvPrint d p e_rhs
    pvPrint d _p (Cmodule _ is) =
     t"module " $+$ pvBlock d 2 False (map (pvp d) (reorderStmts is)) empty (t"endmodule")
--  pvPrint d p (Cinterface Nothing ds) =
--        (t"interface {" $+$ pvBlock d 2 False (map (pvp d) ds) (t";") (t"}"))
    pvPrint d _p (Cinterface _pos Nothing ds) =
        (pBlockNT d 0 False (map (ppM d) ds) empty)
--    pvPrint d p (CLValueSign def me) = optWhen d me $ pvPrint d p def
    pvPrint d _p (Cinterface _pos (Just i) ds) =
        (t"interface" <+> pvp d i) $+$
        (pvBlock d 2 False (map (ppM d) ds)  empty (t"endinterface:" <+> pvp d i))
    pvPrint d p (CmoduleVerilog m _ui c r ses fs sch ps) =
        sep [
          t"(unexpected) module verilog" <+> pvp d m <> t";",
          (if c==(ClockInfo [][][][]) then empty else pPrint d p c),
          (if r==(ResetInfo [][]) then empty else pPrint d p r),
          nest 4 (if null ses then empty else pparen True (sepList (map ppA ses) (t","))),
          nest 4 (t"{" $+$ pvBlock d 2 False (map (ppVeriMethod d Nothing) fs) (t";") (t"}")),
          nest 4 (pvp d sch),
          nest 4 (pvp d ps) ]
          where ppA (s, e) = text "(" <> text (show s) <> text "," <+> pvp d e <> text ")"
    pvPrint d _p (CForeignFuncC i _wrap_ty) =
        t"(unexpected) ForeignFuncC" <+> pvp d i
    pvPrint d p (Cdo _ ss) = pparen (p>0) $ t "actionvalue" $+$ nest 2 (ppActions d ss True) $+$ t "endactionvalue"
    pvPrint d p (Caction _ ss) = pparen (p>0) $ ppActions d ss False
    pvPrint d p (Crules ps rs) = ppRules d p ps rs False
    ----
    pvPrint _d _p (COper []) = empty
    pvPrint  d  p (COper [(CRand p1)]) = pvPrint d p p1
    pvPrint  d  p (COper [(CRand p1), (CRator _ i), (CRand p2)])
      = pvpOp d p i p1 p2
    pvPrint d p (COper ops) =
      let (ys,zs,i) = findSpecialOps ops
      in if (null zs) then pparen (p > maxPrec-1) (sep (map (pvPrint d (maxPrec-1)) ys))
         else pvpOp d p i (COper ys) (COper zs)
    ----
    pvPrint d p (CCon1 _ i e) = pvPrint d p (CCon i [e])
    pvPrint d p (CSelectTT _ e i) = pvPrint d p (CSelect e i)
    ----
    pvPrint d _p (CCon0 _ i) = pvpId d i
    ----
    pvPrint d p (CConT _ i es) = pvPrint d p (CCon i es)
    pvPrint d p (CStructT ty ies) = pvPrint d p (CStruct (Just True) tyc ies)
        where tyc = fromJustOrErr "pvPrint CStructT" (leftCon ty)
    pvPrint  d _p (CSelectT _ i) = text "." <> pvpId d i
    pvPrint  d  p (CLitT _ l) = pvPrint d p l
    pvPrint _d _p (CAnyT _pos _uk _t) = text "?"
    pvPrint  d  p (CmoduleVerilogT _ m ui c r ses fs sch ps) =
        pvPrint d p (CmoduleVerilog m ui c r ses fs sch ps)
    pvPrint d _p (CForeignFuncCT i _prim_ty) =
        t"(unexpected) ForeignFuncC" <+> pvp d i
    pvPrint d p (CTApply e ts) = pparen (p>(maxPrec-1)) $
        sep (pvPrint d (maxPrec-1) e : map (nest 2 . ppApArg) ts)
        where ppApArg ty = t"\183" <> pvPrint d maxPrec ty
    pvPrint d _p (Cattributes pps) =
        text "Attributes" <> pparen True (pvPrint d 0 (map snd pps))
--    pvPrint d p x = internalError ("pvPrint CExpr bad: " ++ show x)

instance HasPosition CExpr where
    getPosition (CLam ei _) = getPosition ei
    getPosition (CLamT ei _ _) = getPosition ei
    getPosition (Cletseq ds e) = getPosition (ds, e)
    getPosition (Cletrec ds e) = getPosition (ds, e)
    getPosition (CSelect e _) = getPosition e
    getPosition (CSelectTT _ e _) = getPosition e
    getPosition (CCon c _) = getPosition c
    getPosition (Ccase pos _ _) = pos
    getPosition (CStruct _ i _) = getPosition i
    getPosition (CStructUpd e _) = getPosition e
    getPosition (Cwrite pos _ _) = pos
    getPosition (CAny pos _) = pos
    getPosition (CVar i) = getPosition i
    getPosition (CApply e _) = getPosition e
    getPosition (CTaskApply e _) = getPosition e
    getPosition (CTaskApplyT e _ _) = getPosition e
    getPosition (CLit l) = getPosition l
    getPosition (CBinOp e _ _) = getPosition e
    getPosition (CHasType e _) = getPosition e
    getPosition (Cif pos _ _ _) = pos
    getPosition (CSub pos _ _) = pos
    getPosition (CSub2 e _ _) = getPosition e
    getPosition (CSubUpdate pos _ _ _) = pos
    getPosition (CCon1 _ c _) = getPosition c
    getPosition (Cmodule pos _) = pos
    getPosition (Cinterface pos _i _ds) = pos
    getPosition (CmoduleVerilog e _ _ _ ses fs _ _) =
        getPosition (e, map snd ses, fs)
    getPosition (CmoduleVerilogT _ e _ _ _ ses fs _ _) =
        getPosition (e, map snd ses, fs)
    getPosition (CForeignFuncC i _) = getPosition i
    getPosition (CForeignFuncCT i _) = getPosition i
    getPosition (Cdo _ ss) = getPosition ss
    getPosition (Caction pos _ss) = pos
    getPosition (Crules _ rs) = getPosition rs
    getPosition (COper es) = getPosition es
    getPosition (Cattributes pps) =
        -- take the position of the first pprop with a good position
        getPosition (map fst pps)
    --
    getPosition (CTApply e ts) = getPosition (e, ts)
    getPosition (CConT _ c _) = getPosition c
    getPosition (CCon0 _ c) = getPosition c
    getPosition (CSelectT _ i) = getPosition i
    getPosition (CLitT _ l) = getPosition l
    getPosition (CAnyT pos _ _) = pos
    getPosition e = error ("no match in getPosition: " ++ ppReadable e)

isInstantiating :: CExpr -> Bool
isInstantiating (CApply (CVar i) _es)  | take 2 (getIdBaseString i) == "mk" = True
isInstantiating (CVar i)  | take 2 (getIdBaseString i) == "mk" = True
isInstantiating _ = False

ppPathInfo :: PDetail -> Int -> VPathInfo -> Doc
ppPathInfo d p ps = pPrint d p ps

ppPortProps :: PDetail -> [VeriPortProp] -> Doc
ppPortProps _d [] = empty
ppPortProps  d (vp:vpps) =
    text "(* " <> pvPrint d 0 vp <> text " *) " <> ppPortProps d vpps

ppSchedInfo :: PDetail -> Int -> VSchedInfo -> [Doc]
ppSchedInfo d p (SchedInfo mci rms rbm ccm) =
    let ds = makeMethodConflictDocs (pvPrint d p) pvpReadable "(" ")" mci
        mci_docs = map (\x  -> text "schedule" <+> x) ds
        rms_docs = map (\p' -> text "rule_between" <+> pvPrint d 0 p') rms
        rbm_docs = map (\p' -> text "rule_before" <+> pvPrint d 0 p') rbm
        ccm_docs = map (\p' -> text "cross-domain" <+> pvPrint d 0 p') ccm
    in  mci_docs ++ rms_docs ++ rbm_docs ++ ccm_docs

ppStrUpd :: PDetail -> CExpr -> [(Id, CExpr)] -> Doc
ppStrUpd d e ies =
  t"(begin let new_struct_ = " <> pvPrint d 0 e <> t";" $+$
  sepList(map (\ (i,e')-> t"  new_struct_."<>pvpId d i<+>t"="<+>pvPrint d 0 e'<>t";")ies)empty $+$
  t"new_struct_; end)"

ppTuple :: PDetail -> [CExpr] -> Doc
ppTuple d es =
  let n = length es
  in t("tuple" ++ show n) <> pparen True (sepList (map (pvPrint d 1) es) (t","))

ppVeriArg :: PDetail -> (VArgInfo, CExpr) -> Doc
ppVeriArg d (Param (VName s), e) = t("parameter " ++ s ++ " = ")<> pvPrint d 0 e
ppVeriArg d (Port ((VName s), pps) mclk mrst, e) =
    let clk_name = case mclk of
                       Nothing  -> t "no_clock"
                       Just clk -> pvPrint d 0 clk
        rst_name = case mrst of
                       Nothing  -> t "no_reset"
                       Just clk -> pvPrint d 0 clk
    in  t "port" <+> ppPortProps d pps <> t s <+>
        t "clocked_by (" <> clk_name <> t ")" <+>
        t "reset_by (" <> rst_name <> t ")>" <+>
        t "=" <+> pvPrint d 0 e
ppVeriArg d (ClockArg i, e)  = t"clock " <> pvpId d i <> t" = " <> pvPrint d 0 e
ppVeriArg d (ResetArg i, e)  = t"reset " <> pvpId d i <> t" = " <> pvPrint d 0 e
ppVeriArg d (InoutArg (VName s) mclk mrst, e) =
    let clk_name = case mclk of
                       Nothing  -> t "no_clock"
                       Just clk -> pvPrint d 0 clk
        rst_name = case mrst of
                       Nothing  -> t "no_reset"
                       Just clk -> pvPrint d 0 clk
    in  t "inout" <+> t s <+>
        t "clocked_by (" <> clk_name <> t ")" <+>
        t "reset_by (" <> rst_name <> t ")>" <+>
        t "=" <+> pvPrint d 0 e

ppVeriMethod :: PDetail -> Maybe VPort -> VFieldInfo -> Doc
ppVeriMethod d _  (Clock i) = t"clock " <> pvpId d i
ppVeriMethod d _  (Reset i) = t"reset " <> pvpId d i
ppVeriMethod d _  (Inout i (VName s) mclk mrst) =
  t"ifc_inout" <+> (pvpId d i) <+> t ("(" ++ s ++ ")") <+>
  (case mclk of
     Nothing -> empty
     Just i' -> t"clocked_by (" <> pvpId d i' <> t")") <+>
  (case mrst of
     Nothing -> empty
     Just i' -> t"reset_by (" <> pvpId d i' <> t")")
ppVeriMethod d mr (Method i mc mreset n pts mo me) =
  let f _ _ Nothing = empty
      f before after (Just (VName vn, prs)) =
         (case prs of
          [] -> empty
          xs -> t"(*" <+> sepList (map (pvPrint d 0) xs) (t ",") <> t" *) ") <>
         (t (before ++ vn ++ after))
  in
   t"method " <>
   (f "" " " mo) <>
   (pvpId d i <>
   (if n == 1 then empty else (t"[" <> (pvp d n) <> t"]")) <>
   (t"(" <> sepList (map (f "" "" . Just) pts) (t",") <> t")") <>
   (f " enable (" ")" me) <>
   (f " ready ("  ")" mr) <>
   (case mc of
     Nothing -> empty
     Just i' -> t" clocked_by (" <> pvpId d i' <> t")") <>
   (case mreset of
     Nothing -> empty
     Just i' -> t" reset_by (" <> pvpId d i' <> t")"))

separgs :: PDetail -> CExpr -> (Doc, Doc)
separgs d (CApply e es) = (pvp d e,
        t"#(" <> sepList (map (pvp d) es) (t",") <>t")" )
separgs d e = (pvp d e, empty)

data CLiteral = CLiteral Position Literal deriving (Show)

instance Eq CLiteral where
        CLiteral _ l == CLiteral _ l'  =  l == l'

instance Ord CLiteral where
        CLiteral _ l `compare` CLiteral _ l'  =  l `compare` l'

instance PPrint CLiteral where
    pPrint d p (CLiteral _ l) = pPrint d p l

instance PVPrint CLiteral where
    pvPrint d p (CLiteral _ l) = pvPrint d p l

instance HasPosition CLiteral where
    getPosition (CLiteral p _) = p

ppQuant :: String -> PDetail -> Int -> Either Position Id -> CExpr -> Doc
ppQuant s d p ei e =
    let ppI (Left _) = text "_"
        ppI (Right i) = pPrint d 0 i
    in  pparen (p>0) (sep [t s <> ppI ei <+> t "->", pvp d e])

data COp
        = CRand CExpr    -- operand
        | CRator Int Id  -- infix operator Id, Int is the number of arguments?
        deriving (Eq, Ord, Show)

instance PPrint COp where
    pPrint d _ (CRand p) = pp d p
    pPrint d _ (CRator _ i) = ppInfix d i

instance PVPrint COp where
    pvPrint d pn (CRand p) = pvPrint d pn p
    pvPrint d _ (CRator _ i) = pvpInfix d i

instance HasPosition COp where
    getPosition (CRand e) = getPosition e
    getPosition (CRator _ i) = getPosition i

findPs :: CExpr -> [CExpr]
findPs (CBinOp e1 i e2) | getBSVIdString i == "," = e1:(findPs e2)
findPs e = [e]

findSpecialOps :: [COp] -> ([COp],[COp],Id)
findSpecialOps [] = ([],[],undefined)
findSpecialOps [x] = ([x],[],undefined)
findSpecialOps [_x,_y] = error "bad list of operators and operands"
findSpecialOps ((CRand e1):(CRator _ i):(CRand e2):xs) |
                            (isIdChar (head (getBSVIdString i)) || (getBSVIdString i =="++")) =
  let w = CBinOp e1 i e2
  in findSpecialOps ((CRand w):xs)
findSpecialOps (x:(_y@(CRator _ i)):xs) | (getBSVIdString i) == "$" =
  ([x], xs, i)
findSpecialOps (x:y:xs) =
  let (zs,ys,i) = findSpecialOps xs
  in (x:y:zs, ys, i)

ppCase :: PDetail -> CExpr -> [CCaseArm] -> Doc
ppCase detail scrutinee arms =
    (t"case" <+> pvp detail scrutinee <+> t "of {") $+$
    pBlock detail 0 False (map ppArm arms)
  where ppArm arm =
            sep [pPrint detail 0 (cca_pattern arm) <>
                 pvpQuals detail (cca_filters arm) <+> t "-> ",
                 nest 2 (pvp detail (cca_consequent arm))]

ppOp :: PDetail -> Int -> Id -> CExpr -> CExpr -> Doc
ppOp d pd i p1 p2 =
        pparen (pd > 0) (sep [pPrint d 1 p1 <> t"" <+> ppInfix d i, pPrint d 1 p2])
{-
        let (p, lp, rp) =
                case getFixity i of
                FInfixl p -> (p, p, p+1)
                FInfixr p -> (p, p+1, p)
                FInfix  p -> (p, p+1, p+1)
        in pparen (d > PDReadable || pd>p)
                  (sep [pPrint d lp p1 <> t"" <+> ppInfix d i, pPrint d rp p2])
-}

-- Renamed from ppOp in src/comp/CVPrint.hs to avoid naming conflicts.
pvpOp :: PDetail -> Int -> Id -> CExpr -> CExpr -> Doc
pvpOp d pd i p1 p2 =
  let rand1 = pvPrint d 1 p1
      rand2 = pvPrint d 1 p2
      ppr' = pparen (pd > 2) -- to distinguish from (_?_:_)
  in
      (case getBSVIdString i of
        "," -> let ps = p1:(findPs p2)
               in ppTuple d ps
        "$" -> ppr' (case p1 of
                    (CApply e es) -> pvPrint d 0 (CApply e (es++[p2]))
                    (CCon i' es) -> pvPrint d 0 (CCon i' (es++[p2]))
                    _ -> rand1 <> pparen True rand2
                  )
        "++" -> t"{" <> sepList (map (pvPrint d 0) [p1,p2]) (t",") <> t"}"
        "<+" -> ppr'(t "preempts" <> pparen True (sep [ rand1 <> t",", rand2]))
        "+>" -> ppr'(t "preempted" <> pparen True (sep [ rand1 <> t",", rand2]))
        "<->" -> ppr'(t "mkConnection" <> pparen True (sep [ rand1 <> t",", rand2]))




        s@(c:_) | isIdChar c -> ppr'(t s <> pparen True (sep [ rand1 <> t",", rand2]))
        _                    -> ppr'(sep [rand1 <> t"" <+> pvpInfix d i, rand2])
      )

type CSummands = [CInternalSummand]

-- summand in internal form (each summand only takes a single argument
-- whose type is CType)
-- Data constructors can have multiple names for a constructor (for backwards
-- compatibility with old names), but the first name is the primary name
-- (used in compiler output etc).
data CInternalSummand =
    CInternalSummand { cis_names :: [Id],
                       cis_arg_type :: CType,
                       cis_tag_encoding :: Integer }
    deriving (Eq, Ord, Show)

instance HasPosition CInternalSummand where
    getPosition summand = getPosition (cis_names summand)

-- return only the primary name
getCISName :: CInternalSummand -> Id
getCISName cis = case (cis_names cis) of
                     [] -> error "getCISName: empty cis_names"
                     (cn:_) -> cn

-- original summands (taking a list of arguments, each of whose types
-- is given by CQType); the Int is a hack to support Enums with
-- noncontiguous Bits encodings
-- Data constructors can have multiple names for a constructor (for backwards
-- compatibility with old names), but the first name is the primary name
-- (used in compiler output etc).
type COSummands = [COriginalSummand]

data COriginalSummand =
    COriginalSummand { cos_names :: [Id],
                       cos_arg_types :: [CQType],
                       cos_field_names :: Maybe [Id],
                       cos_tag_encoding :: Maybe Integer }
    deriving (Eq, Ord, Show)

-- if CQType is a function, [IfcPragmas] (if present) lists argument names
-- (used by the backend to generate pretty names for module ports)
data CField = CField { cf_name :: Id,
                       cf_pragmas :: Maybe [IfcPragma],
                       cf_type :: CQType,
                       cf_default :: [CClause],
                       cf_orig_type :: Maybe CType
                     }
              deriving (Eq, Ord, Show)

instance PPrint CField where
    pPrint d _p f = ppField d f

ppField :: PDetail -> CField -> Doc
ppField detail field =
  let fid = cf_name field
      dfl = cf_default field
  in
    (ppVarId detail fid <+> t "::" <+> pp detail (cf_type field)
        <+> maybe empty (ppIfcPragma detail) (cf_pragmas field) <>
        if (null dfl) then empty else text ";") $$
    -- display the default, if it exists
    if (null dfl)
     then empty
     else let ppC cl = ppClause detail 0 [ppVarId detail fid] cl
          in  foldr1 (\ x y -> x <> text ";" $$ y) (map ppC dfl)
    -- XXX not including orig_type

-- Renamed from ppField in src/comp/CVPrint.hs to avoid naming conflicts.
pvpField :: PDetail -> Doc -> Bool -> CField -> Doc
pvpField detail intro isFn' field =
  let CQType f_provisos f_type = cf_type field
      field_arg_ids = case (cf_pragmas field) of
                      Just f_prags -> map (pvpId detail) argids
                          where argids = filterIArgNames f_prags
                      Nothing -> newIds
      pragmas = ppIfcPrags detail (cf_pragmas field)
      types = ppLabelledTypedId detail intro Nothing isFn'
              (pvpId detail (cf_name field)) f_type field_arg_ids
      provisos | null f_provisos = empty
               | otherwise =
                   t "provisos (" <>
                   sepList (map (pvPrint detail 0) f_provisos) (t ",") <>
                   t ")"
      -- XXX there is not BSV syntax for field defaults
  in  pragmas $+$ types $+$ provisos

ppIfcPragma :: PDetail -> [IfcPragma] -> Doc
ppIfcPragma _detail [] = empty
ppIfcPragma  detail ps =
        text "{-#" <+>
        sep (punctuate comma (map (pPrint detail 0) ps ) )
        <+> text "#-}"

ppIfcPrags :: PDetail -> Maybe [IfcPragma] -> Doc
ppIfcPrags _ Nothing = empty
ppIfcPrags _ (Just []) = empty
ppIfcPrags d (Just xs) = if (null filtered) then empty else prt
    where filtered = (filterPrintIfcArgs xs)
          prt = t"(*" <+> sep (punctuate comma (map (pvPrint d 0) filtered )) <+> t"*)"

ppFDs :: PDetail -> CFunDeps -> Doc
ppFDs _d [] = empty
ppFDs  d fd = text " |" <+> sepList (map (ppFD d) fd) (t",")

ppFD :: PDetail -> ([Id], [Id]) -> Doc
ppFD d (as,rs) = sep (ppVarId d <$> as) <+> t "->" <+> sep (ppVarId d <$> rs)

ppPreds :: PDetail -> [CPred] -> Doc -> Doc
ppPreds _d [] x = x
ppPreds  d preds x = t "(" <> sepList (map (pPrint d 0) preds) (t ",") <> t ") =>" <+> x

ppConIdK :: PDetail -> IdK -> Doc
ppConIdK d (IdK i) = ppConId d i
ppConIdK d (IdKind i k) = pparen True $ ppConId d i <+> t "::" <+> pp d k
ppConIdK d (IdPKind i pk) = pparen True $ ppConId d i <+> t "::" <+> pp d pk

type CFields = [CField] -- just a list of CField

-- redundant
--type Ids = [Id]
data CCaseArm = CCaseArm { cca_pattern :: CPat,
                           cca_filters :: [CQual],
                           cca_consequent :: CExpr }
              deriving (Eq, Ord, Show)

instance HasPosition CCaseArm where
    getPosition arm =
        getPosition (cca_pattern arm) `bestPosition`
        getPosition (cca_filters arm) `bestPosition`
        getPosition (cca_consequent arm)

type CCaseArms = [CCaseArm] -- [(CPat, [CQual], CExpr)]

data CStmt
          -- bind cexpr of type cqtype to cpat; id, if present, is instance name
        = CSBindT CPat (Maybe CExpr) [(Position,PProp)] CQType CExpr
          -- bind cexpr to cpat; id, if present, is instance name
        | CSBind CPat (Maybe CExpr) [(Position,PProp)] CExpr
        | CSletseq [CDefl] -- rhs of "let x = x" refers to previous def
                           --   before current let or in earlier arm
        | CSletrec [CDefl] -- rhs of "let x = x" refers to self
        | CSExpr (Maybe CExpr) CExpr
        deriving (Eq, Ord, Show)

instance PPrint CStmt where
    pPrint d _p (CSBindT pat _inst pprops ty e) =
        foldr ($+$) empty $
            (map (ppPProp d . snd) pprops) ++
            [pp d pat <+> t "::" <+> pp d ty <+> t "<-" <+> pp d e]
    pPrint d _p (CSBind pat _inst pprops e) =
        foldr ($+$) empty $
            (map (ppPProp d . snd) pprops) ++
            [pp d pat <+> t "<-" <+> pp d e]
    pPrint _d _p (CSletseq []) = error "Syntax.Pretty(CStmt): CSletseq []"
    pPrint  d _p (CSletseq ds) = text "letseq" <+> text "{" <+> foldr1 ($+$) (map (pp d) ds) <+> text "}"
    pPrint _d _p (CSletrec []) = error "Syntax.Pretty(CStmt): CSletrec []"
    pPrint  d _p (CSletrec ds) = text "let" <+> text "{" <+> foldr1 ($+$) (map (pp d) ds) <+> text "}"
    pPrint  d p (CSExpr _ e) = pPrint d p e

instance PVPrint CStmt where
    pvPrint d _p (CSBindT (CPVar i) maybeInstName pprops (CQType _ ty) e) =
      let -- (tx, tys) = unravel ty
          (ep, argsp) = separgs d e
          instName = case maybeInstName of
                     Just name -> pvp d name
                     Nothing -> text "the_" <> pvp d i
          isInst = isInstantiating e
      in
        foldr ($+$) empty (map (pvpPProp d . snd) pprops) $+$
        (pvp d ty <> t"" <+> pvp d i <> t(if isInst then "();" else ";")) $+$
        (if isInst
          then ep <> {- f tys <> -} argsp <+> instName <> t"(" <> pvp d i <> t");"
          else pvp d i <+> t "<-" <+> pvp d e <> t";")

    pvPrint d _p (CSBindT pat _ pprops ty e) =
        foldr ($+$) empty $
            (map (pvpPProp d . snd) pprops) ++
            [pvp d ty <+> pvp d pat <+> t "<-" <+> pvp d e <> t";"]
    pvPrint d _p (CSBind pat _ pprops e) =
        foldr ($+$) empty $
            (map (pvpPProp d . snd) pprops) ++
            [t"let" <+> pvp d pat <+> t "<-" <+> pvp d e <> t";"]
    pvPrint d _p (CSletrec ds) = foldr1 ($+$) (map (pvp d) ds)
    pvPrint d _p (CSletseq ds) = foldr1 ($+$) (map (pvp d) ds)
    pvPrint d  p (CSExpr _ e@(Ccase _ _ _)) = pvPrint d p e
    pvPrint d  p (CSExpr _ e) = pvPrint d (p+2) e <> t";"

instance HasPosition CStmt where
    getPosition (CSBindT p _i _pps _t _e) = getPosition p
    getPosition (CSBind p _i _pps _e) = getPosition p
    getPosition (CSletseq ds) = getPosition ds
    getPosition (CSletrec ds) = getPosition ds
    getPosition (CSExpr _ e) = getPosition e

type CStmts = [CStmt]

data CMStmt
        = CMStmt CStmt
        | CMrules CExpr
        | CMinterface CExpr
        | CMTupleInterface Position [CExpr]
        deriving (Eq, Ord, Show)

instance PPrint CMStmt where
    pPrint d p (CMStmt s) = pPrint d p s
    pPrint d p (CMrules e) = pPrint d p e
    pPrint d p (CMinterface e) = pPrint d p (cVApply (idReturn (getPosition e)) [e])
    pPrint d p (CMTupleInterface _ es) = text"(" <> sepList (map (pPrint d p) es) (text ",") <> text ")"

instance PVPrint CMStmt where
    pvPrint d p (CMStmt (CSExpr _ (CApply (CVar i) [Crules ps rs])))
        | getIdBaseString i == "addRules"
      = ppRules d p ps rs True
    pvPrint d p (CMStmt (CSExpr _ (COper [CRand (CVar i), CRator _ i', CRand (Crules ps rs)])))
        | getIdBaseString i == "addRules" && getIdBaseString i' == "$"
      = ppRules d p ps rs True
    pvPrint d p (CMStmt s) = pvPrint d p s
    pvPrint d p (CMrules (Crules ps rs)) = ppRules d p ps rs True
    pvPrint d p (CMrules e) = pvPrint d p e
    pvPrint d p (CMinterface (Cinterface pos (Just _) e )) = pvPrint d p (Cinterface pos Nothing e)
    pvPrint d p (CMinterface e) = pvPrint d p e
    pvPrint d p (CMTupleInterface _ es) =
        let n = length es
        in t ("return(tuple"++show n++"(") <>
            sepList (map (pvPrint d p) es) (text ",") <> text "));"

instance HasPosition CMStmt where
    getPosition (CMStmt s) = getPosition s
    getPosition (CMrules e) = getPosition e
    getPosition (CMinterface e) = getPosition e
    getPosition (CMTupleInterface pos _e) = pos

isIfce :: CMStmt -> Bool
isIfce (CMinterface _) = True
isIfce (CMTupleInterface _ _) = True
isIfce _ = False

reorderStmts :: [CMStmt] -> [CMStmt]
reorderStmts stmts = [x | x <- stmts, not(isIfce x)] ++ [x | x <- stmts, isIfce x]

data CRule
        = CRule [RulePragma] (Maybe CExpr) [CQual] CExpr
        | CRuleNest [RulePragma] (Maybe CExpr) [CQual] [CRule]
        deriving (Eq, Ord, Show)

instance HasPosition CRule where
    getPosition (CRule _ i qs e) = getPosition (i, qs, e)
    getPosition (CRuleNest _ i qs rs) = getPosition (i, qs, rs)

instance PPrint CRule where
        pPrint d _p (CRule rps mlbl mqs e) =
                ppRPS d rps $+$
                (case mlbl of Nothing -> t""; Just i -> pp d i <> t": ") <> sep [ppQuals d mqs, t "  ==>",
                nest 4 (pp d e)]
        pPrint d _p (CRuleNest rps mlbl mqs rs) =
                ppRPS d rps $+$
                (case mlbl of Nothing -> t""; Just i -> pp d i <> t": ") <>
                        (ppQuals d mqs $+$ pBlock d 2 False (map (pp d) rs))

instance PVPrint CRule where
    pvPrint d _p (CRule [] mlbl mqs e) =
            (t"rule" <+> ppRuleName d mlbl) <+> ppRQuals d mqs $+$
             nest 3 (ppRuleBody d e) $+$
            (case mlbl of Just _ -> t "endrule:" <+> ppRuleName d mlbl;
                          _ -> t"endrule")

    pvPrint d _p (CRule rps mlbl mqs e) =
            pvpRPS d rps $+$
            (t"rule" <+> ppRuleName d mlbl) <+> ppRQuals d mqs $+$
             nest 3 (ppRuleBody d e) $+$
            (case mlbl of Just _ -> t "endrule:" <+> ppRuleName d mlbl;
                          _ -> t"endrule")

    pvPrint d _p (CRuleNest rps mlbl mqs rs) =
            pvpRPS d rps $+$ t"rule" <+> ppRuleName d mlbl <+>
              (pvpQuals d mqs $+$ pvBlock d 2 False (map (pvp d) rs) (t";") (t"endrule"))

ppActions :: PDetail -> CStmts -> Bool -> Doc
ppActions d ss naked =
  let tas = pBlockNT d 0 False (map (pvPrint d 0) ss)  empty
  in if naked then tas else (t"action" $+$ nest 2 tas $+$ t"endaction")

ppRPS :: PDetail -> [RulePragma] -> Doc
ppRPS _d [] = text ""
ppRPS  d rps = vcat (map (pPrint d 0) rps)

-- Renamed from ppRPS in src/comp/CVPrint.hs to avoid naming conflicts.
pvpRPS :: PDetail -> [RulePragma] -> Doc
pvpRPS _ [] = empty
pvpRPS _ rps = t"(*" <+> sepList (map ppRP rps) (t",") <+> t "*)"

ppRP :: RulePragma -> Doc
ppRP RPfireWhenEnabled                = t"fire_when_enabled"
ppRP RPnoImplicitConditions           = t"no_implicit_conditions"
ppRP RPaggressiveImplicitConditions   = t"aggressive_implicit_conditions"
ppRP RPconservativeImplicitConditions = t"conservative_implicit_conditions"
ppRP RPnoWarn                         = t"no_warn"
ppRP RPwarnAllConflicts                  = t"warn_all_conflicts"
ppRP RPcanScheduleFirst               = t"can_schedule_first"
ppRP RPclockCrossingRule              = t"clock_crossing_rule"
ppRP (RPdoc comment)                  = t ("doc = " ++ quote comment)
ppRP RPhide                           = t"hide"

ppRQuals :: PDetail -> [CQual] -> Doc
ppRQuals _d [] = empty
ppRQuals  d qs = t"(" <> sepList (map (pvp d) qs) (t" && ") <> t");"

ppRuleBody :: PDetail -> CExpr -> Doc
ppRuleBody d (Cletrec ds (Caction _ ss)) =
           (foldr1 ($+$) (map (pvp d) ds)) $+$ ppActions d ss True
ppRuleBody d (Caction _ ss) = ppActions d ss True
ppRuleBody d e              = pvp d e <> t";"

ppRuleName :: PDetail -> Maybe CExpr -> Doc
ppRuleName _d Nothing = t"dummy_name"
ppRuleName _d (Just(CLit(CLiteral _ (LString s)))) = t(f s) where
   f s' = map (\ c -> if c==' ' then '_' else c) ((toLower (head s')):(tail s'))
ppRuleName d (Just i) = pvp d i

ppRules :: PDetail -> Int -> [CSchedulePragma] -> [CRule] -> Bool -> Doc
ppRules d _p [] rs naked =
  let trs = pBlockNT d 0 False (map (pvp d) rs)  empty
  in if naked then trs else (t"rules" $+$ nest 2 trs $+$ t"endrules")

ppRules d p ps rs _ =
  let trs = pBlockNT d 0 False (map (pvp d) rs)  empty
  in pPrint d p ps $+$ (t"rules" $+$ nest 2 trs $+$ t"endrules")

-- | A definition with a binding. Can occur as a let expression, let statement
-- in a do block, a typeclass instance defn, or bindings in an interface.
data CDefl                -- [CQual] part is the when clause used in an interface
                          -- binding, ie the explicit condition attached to each method
        = CLValueSign CDef [CQual]     -- let x :: T = e2 -- explicit type sig
        | CLValue Id [CClause] [CQual] -- let y = e2      -- no explicit type sig
        | CLMatch CPat CExpr           -- let [z] = e3
        deriving (Eq, Ord, Show)

instance PPrint CDefl where
    pPrint d p (CLValueSign def me) = optWhen d me $ pPrint d p def
    pPrint d p (CLValue i cs me) = optWhen d me $
        foldr1 ($+$) (map (\ cl -> ppClause d p [ppVarId d i] cl <> t";") cs)
    pPrint d p (CLMatch pat e) = ppClause d p [] (CClause [pat] [] e)

instance PVPrint CDefl where
    pvPrint d p (CLValueSign def me) = optWhen d me $ pvPrint d p def
    pvPrint d _p (CLValue i cs me) = optWhen d me $
        foldr1 ($+$) (map (\ cl -> pvpClause d [pvpId d i] cl) cs)
--    pvPrint d p (CLMatch ps@(CPCon i _) e) | getIdBaseString i == "," =
--        t "match {" <> (catList(map (pvPrint d maxPrec) (pUnmkTuple ps)) (t","))<>t"} =" $+$
--                  nest 4 (pvp d e) <> t";"
    pvPrint d _p (CLMatch pat e) =
        t"match"<+> pvp d pat <+> t"=" $+$
                  nest 4 (pvp d e) <> t";"

instance HasPosition CDefl where
    getPosition (CLValueSign d _) = getPosition d
    getPosition (CLValue i _ _) = getPosition i
    getPosition (CLMatch p e) = getPosition (p, e)

optIf :: PDetail -> [CQual] -> Doc
optIf _ [] = empty
optIf d qs = t"if (" <> sepList (map (pvp d) qs) (t" && ") <> t")"

optWhen :: PDetail -> [CQual] -> Doc -> Doc
optWhen _d [] s = s
optWhen  d qs s = s $+$ (t"    " <> pvpQuals d qs)

ppM :: PDetail -> CDefl -> Doc
ppM d (CLValue i [CClause _ _ (Cinterface _ (Just i1) subIfc)] []) =
    sep ((t"interface"<+>pvpId d i1 <+> pvpId d i <>t";"):
         (map (\ si -> nest 2 (ppM d si)) subIfc) ++
         [t"endinterface:"<+> pvpId d i])
ppM d (CLValue i [cl] me) =
        (ppMClause d [pvpId d i] cl (optIf d me))
        $+$ t"endmethod:" <+> pvpId d i
ppM d (CLValueSign (CDef i _ [cl]) me) =
        (ppMClause d [pvpId d i] cl (optIf d me))
        $+$ t"endmethod:" <+> pvpId d i
ppM d def = pvPrint d 0 def

ppMBody :: PDetail -> CExpr -> Doc
ppMBody d e@(Cdo _ _) = pvp d e
ppMBody d e@(Caction _ _) = pvp d e
ppMBody d e = ppBody d False e

ppMClause :: PDetail -> [Doc] -> CClause -> Doc -> Doc
ppMClause d [x] (CClause ps [] e) mIf | all isVar ps =
       sep [t"method" <+> x <> t"(" <>
                   sepList (map (pvPrint d maxPrec) ps) (t",") <> t")" <+> mIf <+> t";",
            nest 2 (ppMBody d e)]
ppMClause d xs (CClause ps mqs e) mIf =
        sep [t"// APPROXIMATELY:",
                  t"method" <+> sep (xs ++ map (pvPrint d maxPrec) ps) <+> pvpQuals d mqs <+> mIf <+> t "=",
                  nest 4 (pvp d e)]

ppValueSign :: PDetail -> Id -> [TyVar] -> CQType -> [CClause] -> Doc
ppValueSign d i [] ty cs =
        (ppVarId d i <+> t "::" <+> pp d ty <> t";") $+$
        foldr1 ($+$) (map (\ cl -> ppClause d 0 [ppVarId d i] cl <> t";") cs)
ppValueSign d i vs ty cs =
        (ppVarId d i <+> t ":: /\\" <> sep (map (pPrint d maxPrec) vs) <> t"." <> pp d ty <> t";") $+$
        foldr1 ($+$) (map (\ cl -> ppClause d 0 [ppVarId d i] cl <> t";") cs)

-- Renamed from ppValueSign in src/comp/CVPrint.hs to avoid naming conflicts.
pvpValueSign :: PDetail -> Id -> [TyVar] -> CQType -> [CClause] -> Doc

pvpValueSign d i [] (CQType ps ty) [CClause cs [] cexp] | all isVar cs =
  let id' = pvpId d i
      (modId,ps') = findModId ps
      line1 = ppTypedId d modId id' ty (map (t . getCPVString) cs)
  in ppValueSignRest d id' ps' (isFn ty) False line1 cexp "function"

pvpValueSign d i [] ty cs =
        (pvpId d i <+> t "::" <+> pvp d ty <> t";") $+$
        foldr1 ($+$) (map (\ cl -> pvpClause d [pvpId d i] cl) cs)
pvpValueSign d i vs ty cs =
        (pvpId d i <+> t ":: /\\" <> sep (map (pvPrint d maxPrec) vs) <> t"." <> pvp d ty <> t";") $+$
        foldr1 ($+$) (map (\ cl -> pvpClause d [pvpId d i] cl) cs)

ppClause :: PDetail -> Int -> [Doc] -> CClause -> Doc
ppClause d _p xs (CClause ps mqs e) =
        sep [sep (xs ++ map (pPrint d maxPrec) ps) <> ppQuals d mqs <+> t "= ",
                  nest 4 (pp d e)]

-- Renamed from ppClause in src/comp/CVPrint.hs to avoid naming conflicts.
pvpClause :: PDetail -> [Doc] -> CClause -> Doc
pvpClause d xs (CClause [] mqs e) =
        sep [t"let" <+> sep xs <> pvpQuals d mqs <+> t "= ",
                  nest 4 (pvp d e)]
        <> t";"
pvpClause d xs (CClause ps [] e) =
    let ids' = xs ++ map (ppCP d) ps
        (i, ids) = unconsOrErr "CVPrint.ppClause" ids'
        line1 = ppUntypedId d i ids
    in ppValueSignRest d i [] True False line1 e "function"

pvpClause _d _xs (CClause ps mqs e) =
    error ("CClause: "++ show (ps,mqs,e))

ppCP :: PDetail -> CPat -> Doc
ppCP d p =
    case pUnmkTuple p of
     [x] -> pvp d x
     xs  -> t"{"<>(catList(map (pvp d) xs) (t","))<>t"}"

ppLabelledTypedId :: PDetail -> Doc -> Maybe Id -> Bool -> Doc -> Type -> [Doc] -> Doc
ppLabelledTypedId d intro modId isFnlike i ty ids =
  let (ys, x) = getArrows ty
      ity = case x of (TAp (TCon _) y) -> y;
                      z -> z
      g [] = empty
      g xs = t"#(" <>  sepList xs (t",") <> t")"
      f [] = intro <+> pvp d x <+> i <> (if isFnlike then t"()" else empty)
      f xs = if isModule modId x
              then t"module" <+> i <> g xs <> t"(" <> pvPrint d 0 ity <> t")"
              else intro <+> pvPrint d 9 x <+> i <> t"(" <> sepList xs (text ",") <> t")"
      zs = zipWith (\ y i' -> ppTypedId d Nothing i' y newIds) ys ids
  in f zs

ppTypedId :: PDetail -> Maybe Id -> Doc -> Type -> [Doc] -> Doc
ppTypedId d mi i y = ppLabelledTypedId d (if isFn y then t"function" else empty) mi (isFn y) i y

ppUntypedId :: PDetail -> Doc -> [Doc] -> Doc
ppUntypedId _d i ids =
  (t"function") <+> i <> t"(" <> sepList ids (text ",") <> t")"

-- Definition, local or global
data CDef
        = CDef Id CQType [CClause]                        -- before type checking
        | CDefT Id [TyVar] CQType [CClause]                -- after type checking, with type variables from the CQType
        deriving (Eq, Ord, Show)

instance PPrint CDef where
    pPrint d _p (CDef  i    ty cs) = ppValueSign d i [] ty cs
    pPrint d _p (CDefT i vs ty cs) = ppValueSign d i vs ty cs

instance PVPrint CDef where
    pvPrint d _p (CDefT i vs ty cs) = pvpValueSign d i vs ty cs

    -- XXX this seems out of date with the BSV parser
    pvPrint d p (CDef i (CQType ps ty) [CClause cps []
        (CmoduleVerilog m _ui c r args meths sch pts)]) | all isVar cps =
      let (ys, x) = getArrows ty
          ity = case x of (TAp (TCon _) y) -> y;
                          z -> z
          s (CLit (CLiteral _ (LString x'))) = x'
          s x' = error ("pvPrint CDef not lit: " ++ show x')
          f [] = empty
          f xs = t"#(" <>
                 sepList (zipWith (\ x' c' -> -- t"parameter" <+>
                                            pvPrint d 0 x' <> t"" <+> pvPrint d 10 c')
                                  xs cps)
                 (t",") <> t")"
          pOutMClk Nothing = empty
          pOutMClk (Just ((VName s'), mg)) = t s' <> pOutMGate mg
          pOutMGate Nothing = empty
          pOutMGate (Just (VName s', vpps)) = t", " <> ppPortProps d vpps <> t s'
          pInMClk Nothing = empty
          pInMClk (Just ((VName s'), mg)) = t s' <> pInMGate mg
          -- these technically need a placeholder gate name (CLK_GATE?)
          pInMGate (Left True) = empty -- text ", (* inhigh *)"
          pInMGate (Left False) = text ", (* unused *)"
          pInMGate (Right (VName s')) = t", " <> t s'
          noInputResets = null (input_resets r)
          (mId,ps') = findModId ps
       in (if isModule mId x -- xxx readies xxx
           then
           (((pBlockNT d 0 False
              [t"import \"BVI\"" <+> t(s m) <>
               t" = module" <+> pvpId d i <> f ys <> t"(" <> pvPrint d 0 ity <> t")",
               if ps'==[]
               then empty
               else t "  provisos (" <> sepList (map (pvPrint d 0) ps') (t ",") <> t")"] empty)
             <> (t";")) $+$
            (pvBlock d 2 False
             ((let ClockInfo in_cs out_cs as ss = c
               in ((map (\ (i', mc) ->
                         -- we could print this as "input_clock" if we want
                         t"clock" <+> pvp d i' <+> t"(" <> pOutMClk mc <> t")") out_cs) ++
                   (map (\ (i', mc) ->
                         -- we could print this as "output_clock" if we want
                         t"clock" <+> pvp d i' <+> t"(" <> pInMClk mc <> t")") in_cs) ++
                   (map (\ (i1, i2) -> t"ancestor" <+> pvp d i1 <>t","<+> pvp d i2) as) ++
                   (map (\ (i1, i2) -> t"sibling" <+> pvp d i1 <>t","<+> pvp d i2) ss))) ++
              (if noInputResets then [t"no_reset"] else []) ++
              (map (pPrint d p) (input_resets r)) ++ -- XXX is this right?
              (map (pPrint d p) (output_resets r)) ++ -- XXX is this right?
              (map (ppVeriArg d) args) ++
              (map (ppVeriMethod d Nothing) meths) ++
              (ppSchedInfo d p sch) ++
              [ppPathInfo d p pts])
             (t";")
             (t"endmodule:" <+> pvp d i)))
           else t "ERROR (for verilog module): module not of module type"
          )

    -- for bsc2bsv, if CForeignFuncC is ever supported in Classic
    pvPrint d _p (CDef bsv_id cqt [CClause [] [] (CForeignFuncC c_id _)]) =
      t"import \"BDPI\"" <+>
      (if (bsv_id == c_id)
       then empty
       else t (getIdString c_id) <+> t "="
      ) <+>
      pvpField d (t"function") True (CField bsv_id Nothing cqt [] Nothing) <> t";" $+$
      t"endfunction"

    pvPrint d _p df@(CDef i (CQType _ _) [CClause cps [] _]) | all isVar cps =
      p2defs d (CPragma (Pproperties i [])) (CValueSign df)

    pvPrint d _p (CDef  i    ty cs) = pvpValueSign d i [] ty cs

instance HasPosition CDef where
    getPosition (CDef i _ _) = getPosition i
    getPosition (CDefT i _ _ _) = getPosition i

ppBodyLets :: PDetail -> [CDefl] -> Doc
ppBodyLets _d [] = empty
ppBodyLets  d (d1:ds) =
   t"  " <> pvp d d1 $+$ ppBodyLets d ds

ppBody :: PDetail -> Bool -> CExpr -> Doc
ppBody d isMod (Cletrec [CLValueSign (CDef i1 t1 c1) q1]
                (Cletrec [CLValueSign (CDef i2 _ _) _] e))
                         | i1 == mkIdPost i2 fsAcute =
        ppBodyLets d [CLValueSign (CDef i2 t1 c1) q1] $+$
        (ppBody d isMod e)
ppBody d isMod (Cletrec ds e) =
        ppBodyLets d ds $+$
        (ppBody d isMod e)
ppBody d True e = (pparen True (pvp d e) <> t";")
ppBody d _ e = (t"  return" <+> pparen True (pvPrint d 1 e) <> t";")

findModId :: [CPred] -> (Maybe Id, [CPred])
findModId [] = (Nothing,[])
findModId (p:ps) =
   case p of
     (CPred (CTypeclass isM) [TVar (TyVar iM _ _), _]) | getIdBaseString isM == getIdBaseString idIsModule
       -> (Just iM,ps)
     _ -> let (i,ps') = findModId ps in (i,p:ps')

ppValueSignRest :: PDetail -> Doc -> [CPred] -> Bool -> Bool -> Doc -> CExpr -> String -> Doc
ppValueSignRest d i ps isFnT isMod line1 cexp ender =
   ((pBlockNT d 0 False
        [line1,
         if ps==[]
           then empty
           else t "  provisos (" <> sepList (map (pvPrint d 0) ps) (t",") <> t")"] empty)<> (t";")) $+$
   (if isFnT then
                (ppBody d isMod cexp $+$ t ("end"++ender++":") <+> i)
               else (i <+> t "=" <+> pvPrint d 2 cexp <> t ";"))

-- Definition clause
-- each interface's definitions (within the module) correspond to one of these
data CClause
        = CClause [CPat]                -- arguments (including patterns)
                  [CQual]               -- qualifier on the args
                  CExpr                 -- the body
        deriving (Eq, Ord, Show)

instance PPrint CClause where
    pPrint d p cl = ppClause d p [] cl

instance PVPrint CClause where
    pvPrint d _p cl = pvpClause d [] cl

instance HasPosition CClause where
    getPosition (CClause ps qs e) = getPosition (ps, qs, e)

-- Pattern matching
data CQual
        = CQGen CType CPat CExpr
        | CQFilter CExpr
        deriving (Eq, Ord, Show)

instance PPrint CQual where
        pPrint d _p (CQGen _ pa e) = pp d pa <+> t "<-" <+> pp d e
        pPrint d _p (CQFilter e) = pp d e

instance PVPrint CQual where
        pvPrint d _p (CQGen _ _pattern expr) =
            pvp d expr <+> t "matches" <+> pvp d expr
        pvPrint d _p (CQFilter e) = pvp d e

instance HasPosition CQual where
    getPosition (CQGen _ p _) = getPosition p
    getPosition (CQFilter e) = getPosition e

ppQuals :: PDetail -> [CQual] -> Doc
ppQuals _d [] = t""
ppQuals  d qs = t" when" <+> sepList (map (pp d) qs) (t",")

-- Renamed from ppQuals in src/comp/CVPrint.hs to avoid naming conflicts.
pvpQuals :: PDetail -> [CQual] -> Doc
pvpQuals _d [] = empty
pvpQuals  d qs = t"&&&" <+> sepList (map (pvp d) qs) (t" && ")

ppOSummands :: PDetail -> [COriginalSummand] -> Doc
ppOSummands d cs = sepList (map (nest 2 . ppOCon) cs) (t" |")
  where ppOCon summand =
            let pp_name = case (cos_names summand) of
                            [cn] -> ppConId d cn
                            cns -> text "(" <>
                                   sepList (map (ppConId d) cns) (text ",") <>
                                   text ")"
                pp_args = map (pPrint d maxPrec) (cos_arg_types summand)
                pp_encoding =
                    case cos_tag_encoding summand of
                    Nothing -> empty
                    Just num ->
                        text "{-# tag " <+> pPrint d 0 num <+> text "#-}"
            in  sep (pp_name : pp_encoding : pp_args)

ppSummands :: PDetail -> [CInternalSummand] -> Doc
ppSummands d cs = sepList (map (nest 2 . ppCon) cs) (t" |")
  where ppCon summand =
            let pp_name = case (cis_names summand) of
                            [cn] -> ppConId d cn
                            cns -> text "(" <>
                                   sepList (map (ppConId d) cns) (text ",") <>
                                   text ")"
                pp_arg = pPrint d maxPrec (cis_arg_type summand)
            in  sep [pp_name, pp_arg]

data CPat
        = CPCon Id [CPat]
        -- Either a struct type or a constructor with named fields.
        -- The 'Maybe Bool' argument can indicate if it is specifically
        -- one or the other (True for struct), otherwise the typechecker
        -- will attempt to determine which is intended.
        | CPstruct (Maybe Bool) Id [(Id, CPat)]
        | CPVar Id
        | CPAs Id CPat
        | CPAny Position
        | CPLit CLiteral
        -- position, base, [(length, value or don't-care)] starting from MSB
        -- note that length is length in digits, not bits!
        | CPMixedLit Position Integer [(Integer, Maybe Integer)]
        -- used before operator parsing
        | CPOper [CPOp]
        -- generated by deriving code
        | CPCon1 Id Id CPat                        -- first Id is type of constructor
        -- After type checking
        | CPConTs Id Id [CType] [CPat]
        deriving (Eq, Ord, Show)

instance PPrint CPat where
    pPrint d p (CPVar a) = pPrint d p a
    pPrint d p (CPCon i as) = pparen (p>(maxPrec-1)) $ sep (ppConId d i : map (pPrint d maxPrec) as)
    pPrint _d _p (CPstruct _ tyc []) | tyc == idPrimUnit = text "()"
    pPrint  d _p (CPstruct _ tyc [(_, fst'), (_, snd')]) | tyc == idPrimPair =
        pparen True (pPrint d 0 fst' <> t"," <+> pPrint d 0 snd')
    pPrint d p (CPstruct _ i fs) = pparen (p>(maxPrec-1)) $ ppConId d i <+> t "{" <+> sep (map ppField' fs ++ [t"}"])
        where ppField' (i', CPVar i'') | i' == i'' = ppVarId d i' <> t";"
              ppField' (i', p') = ppVarId d i' <+> t "=" <+> pp d p' <> t";"
    pPrint  d _p (CPAs a pp') = pPrint d maxPrec a <> t"@" <> pPrint d maxPrec pp'
    pPrint _d _p (CPAny _) = text "_"
    pPrint  d  p (CPLit l) = pPrint d p l
    pPrint _d _p (CPMixedLit _ base ps) =
        let digitBits = log2 base
            f (len, Just val) = integerFormat (len `div` digitBits) base val
            f (len, Nothing)  = L.genericReplicate (len `div` digitBits) '?'
            pref  2 = "0b"
            pref  8 = "0o"
            pref 10 = ""
            pref 16 = "0x"
            pref x = error ("bad radix to CPMixedLit: " ++ show x)
        in  text (pref base ++ concatMap f ps)
    pPrint d p (CPOper ops) = pparen (p > maxPrec-1) (sep (map (pPrint d (maxPrec-1)) ops))
    pPrint d p (CPCon1 _ i a) = pPrint d p (CPCon i [a])
    ----
    pPrint d p (CPConTs _ i ts as) = pparen (p>(maxPrec-1)) $ sep (ppConId d i : map ppApArg ts ++ map (pPrint d maxPrec) as)
        where ppApArg ty = t"\183" <> pPrint d maxPrec ty

instance PVPrint CPat where
    pvPrint d _p (CPVar i) = pvpPId d i

    pvPrint d p (CPCon i [p2@(CPCon i' _)])|
          getIdString i /= "," && getIdString i' == "," =
       pparen (p>(maxPrec-1)) $ (t"tagged" <+> pvpId d i)<+> pvp d p2


    pvPrint d p (CPCon i []) =
       pparen (p>(maxPrec-1)) (t"tagged" <+> pvpId d i)

    pvPrint d p pat@(CPCon i as) =
     let notTpl = getIdString i /= ","
         bs = if notTpl then as else pUnmkTuple pat
     in pparen (notTpl && p>(maxPrec-1)) $
       (if notTpl then t"tagged" <+> pvpId d i else empty )<+> t "{" <>
                (catList(map (pvPrint d maxPrec) bs) (t","))<>t"}"

    pvPrint _d _p (CPstruct _ tyc []) | tyc == idPrimUnit = text "()"
    pvPrint  d _p (CPstruct _ tyc [(_, fst'), (_, snd')]) | tyc == idPrimPair =
        pparen True (pvPrint d 0 fst' <> t"," <+> pvPrint d 0 snd')
    pvPrint d p (CPstruct _ i fs) = pparen (p>(maxPrec-1)) $ pvpId d i <+> t "{" <+> sep (map ppFld fs ++ [t"}"])
        where ppFld (i', CPVar i'') | i' == i'' = pvpId d i' <> t";"
              ppFld (i', p') = pvpId d i' <+> t "=" <+> pvp d p' <> t";"
    pvPrint  d _p (CPAs a pp') = pvPrint d maxPrec a <> t"@" <> pvPrint d maxPrec pp'
    pvPrint _d _p (CPAny _) = text ".*"
    pvPrint  d  p (CPLit l) = pvPrint d p l
    pvPrint _d _p (CPMixedLit _ base ps) =
        let digitBits = log2 base
            f (len, Just val) = integerFormat (len `div` digitBits) base val
            f (len, Nothing)  = L.genericReplicate (len `div` digitBits) '?'
            pref  2 = "'b"
            pref  8 = "'o"
            pref 10 = "'d"
            pref 16 = "'h"
            pref x = error ("bad radix to CPMixedLit: " ++ show x)
        in  text (pref base ++ concatMap f ps)
    pvPrint d p (CPOper ops) = pparen (p > maxPrec-1) (sep (map (pvPrint d (maxPrec-1)) ops))
    pvPrint d p (CPCon1 _ i a) = pvPrint d p (CPCon i [a])
    ----
    pvPrint d p (CPConTs _ i ts as) = pparen (p>(maxPrec-1)) $ sep (pvpId d i : map ppApArg ts ++ map (pvPrint d maxPrec) as)
        where ppApArg ty = t"\183" <> pvPrint d maxPrec ty

instance HasPosition CPat where
    getPosition (CPCon c _) = getPosition c
    getPosition (CPstruct _ c _) = getPosition c
    getPosition (CPVar i) = getPosition i
    getPosition (CPAs i _) = getPosition i
    getPosition (CPAny p) = p
    getPosition (CPLit l) = getPosition l
    getPosition (CPMixedLit p _ _) = p
    getPosition (CPOper ps) = getPosition ps
    getPosition (CPCon1 _ c _) = getPosition c
    getPosition (CPConTs _ c _ _) = getPosition c

isVar :: CPat -> Bool
isVar (CPVar _) = True
isVar _         = False

getCPVString :: CPat -> String
getCPVString (CPVar i) = getBSVIdString i
getCPVString x = error ("getCPVString bad: " ++ show x)

pUnmkTuple :: CPat -> [CPat]
pUnmkTuple (CPCon i [e,es]) | getIdBaseString i == "," =
  let bs = pUnmkTuple es
  in e:bs
pUnmkTuple x = [x]

data CPOp
        = CPRand CPat
        | CPRator Int Id
        deriving (Eq, Ord, Show)

instance PPrint CPOp where
    pPrint d _ (CPRand p) = pp d p
    pPrint d _ (CPRator _ i) = ppInfix d i

instance PVPrint CPOp where
    pvPrint d _ (CPRand p) = pvp d p
    pvPrint d _ (CPRator _ i) = pvpInfix d i

instance HasPosition CPOp where
    getPosition (CPRand p) = getPosition p
    getPosition (CPRator _ i) = getPosition i

ppInfix :: PDetail -> Id -> Doc
ppInfix _d i =
    --case getIdString i of
    --s@(c:_) | isIdChar c -> t"`" <> t s <> t"`"
    --s -> t s
    let p = getIdQual i
        b = getIdBase i
    in if (p==fsEmpty) then
              (case getFString b of
               s@(c:_) | isIdChar c -> t"`" <> t s <> t"`"
               s -> t s)
        else (t"`" <> t (getFString p) <> t "." <>
              (case getFString b of
               s@(c:_) | isIdChar c -> t s
               s -> t "(" <> t s <> t")") <> t"`")

-- Renamed from ppInfix in src/comp/CVPrint.hs to avoid naming conflicts.
pvpInfix :: PDetail -> Id -> Doc
pvpInfix _d i =
    case getBSVIdString i of
    ":=" -> t "<="
    "/=" -> t "!="
    s@(c:_) | isIdChar c -> t"`" <> t s <> t"`"
    s -> t s

pp :: (PPrint a) => PDetail -> a -> Doc
pp d x = pPrint d 0 x

-- Renamed from pp in src/comp/CVPrint.hs to avoid naming conflicts.
pvp :: (PVPrint a) => PDetail -> a -> Doc
pvp d x = pvPrint d 0 x

t :: String -> Doc
t s = text s

newtype CInclude
       = CInclude String
    deriving (Eq, Ord, Show)

instance PPrint CInclude where
    pPrint d p (CInclude s) = pPrint d p s

instance PVPrint CInclude where
    pvPrint _d _p (CInclude i) = t"`include" <+> (doubleQuotes $ t i)

--------
-- Utilities

cApply :: Int -> CExpr -> [CExpr] -> CExpr
cApply _n e [] = e
cApply _n (CCon i es) es' = CCon i (es ++ es')
cApply _n (CConT t' i es) es' = CConT t' i (es ++ es')
cApply _n (CApply e es) es' = CApply e (es ++ es')
cApply _n (CTaskApply e es) es' = CTaskApply e (es ++ es')
cApply _n e as = CApply e as

cVApply :: Id -> [CExpr] -> CExpr
cVApply i _es | isTaskName (getIdBaseString i) =
    error $ "cVApply to " ++ (show i) ++ "\n"
cVApply i es = cApply 2 (CVar i) es

-- tasks start with $ followed by a letter
isTaskName :: String -> Bool
isTaskName ('$':c:_) = isAlpha c
isTaskName _ = False

getName :: CDefn -> Either Position Id
getName (CValue i _) = Right i
getName (CValueSign (CDef i _ _)) = Right i
getName (CValueSign (CDefT i _ _ _)) = Right i
getName (Cprimitive i _) = Right i
getName (CprimType i) = Right $ iKName i
getName (CPragma pr) = Left $ getPosition pr
getName (Cforeign { cforg_name = i }) = Right i
getName (Ctype i _ _) = Right $ iKName i
getName (Cdata { cd_name = name }) = Right $ iKName name
getName (Cstruct _ _ i _ _ _) = Right $ iKName i
getName (Cclass _ _ i _ _ _) = Right $ iKName i
getName (Cinstance qt _) = Left $ getPosition qt
getName (CItype i _ _) = Right $ iKName i
getName (CIclass _ _ i _ _ _) = Right $ iKName i
getName (CIinstance _ qt) = Left $ getPosition qt
getName (CIValueSign i _) = Right i

iKName :: IdK -> Id
iKName (IdK i) = i
iKName (IdKind i _) = i
iKName (IdPKind i _) = i
