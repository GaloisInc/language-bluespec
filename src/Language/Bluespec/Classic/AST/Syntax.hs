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

import Data.Char (isAlpha)
import qualified Data.List as L
import Text.PrettyPrint.HughesPJClass

import Language.Bluespec.Classic.AST.FString
import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.Lex
import Language.Bluespec.Classic.AST.Literal
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.Pragma
import Language.Bluespec.Classic.AST.PreIds
import Language.Bluespec.Classic.AST.PreStrings
import Language.Bluespec.Classic.AST.Type
import Language.Bluespec.Classic.AST.Undefined
import Language.Bluespec.Classic.AST.VModInfo
import Language.Bluespec.IntegerUtil
import Language.Bluespec.Log2
import Language.Bluespec.Prelude
import Language.Bluespec.Pretty
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

instance Pretty CPackage where
    pPrintPrec d _ (CPackage i exps imps fixs def includes) =
        (t"package" <+> ppConId d i <> ppExports d exps <+> t "where {") $+$
        pBlock d 0 True (map (pp d) imps ++ map (pp d) fixs ++ map (pp d) def ++ map (pp d) includes)

data CExport
        = CExpVar Id    -- export a variable identifier
        | CExpCon Id    -- export a constructor
        | CExpConAll Id -- export an identifier and constructors
                        -- (datatypes, interfaces, etc.)
        | CExpPkg Id    -- export an entire package
        deriving (Eq, Ord, Show)

instance Pretty CExport where
    pPrintPrec d _p (CExpVar i) = ppVarId d i
    pPrintPrec d _p (CExpCon i) = ppConId d i
    pPrintPrec d _p (CExpConAll i) = ppConId d i <> t"(..)"
    pPrintPrec d _p (CExpPkg i) = t"package" <+> ppId d i

ppExports :: PDetail -> Either [CExport] [CExport] -> Doc
ppExports _d (Right []) = empty
ppExports  d (Right noexps) = t " hiding (" <> sepList (map (pp d) noexps) (t",") <> t")"
ppExports  d (Left exports) = t "(" <> sepList (map (pp d) exports) (t",") <> t")"

data CImport
        = CImpId Bool Id                                -- Bool indicates qualified
        | CImpSign String Bool CSignature
        deriving (Eq, Ord, Show)

instance Pretty CImport where
    pPrintPrec d _p (CImpId q i) = t"import" <+> ppQualified q <+> ppConId d i
    pPrintPrec d _p (CImpSign _ q (CSignature i _ _ _)) = t"import" <+> ppQualified q <+> ppConId d i <+> t "..."

ppQualified :: Bool -> Doc
ppQualified True = text "qualified"
ppQualified False = empty

-- Package signature from import
data CSignature
        = CSignature Id [Id] [CFixity] [CDefn]        -- package name, imported packages, definitions
        deriving (Eq, Ord, Show)

instance Pretty CSignature where
    pPrintPrec d _ (CSignature i imps fixs def) =
        (t"signature" <+> ppConId d i <+> t "where" <+> t "{") $+$
        pBlock d 0 True (map pi' imps ++ map (pp d) fixs ++ map (pp d) def)
      where pi' i' = t"import" <+> ppConId d i'

data CFixity
        = CInfix  Integer Id
        | CInfixl Integer Id
        | CInfixr Integer Id
        deriving (Eq, Ord, Show)

instance Pretty CFixity where
    pPrintPrec d _ (CInfix  p i) = text "infix"  <+> text (show p) <+> ppInfix d i
    pPrintPrec d _ (CInfixl p i) = text "infixl" <+> text (show p) <+> ppInfix d i
    pPrintPrec d _ (CInfixr p i) = text "infixr" <+> text (show p) <+> ppInfix d i

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

instance Pretty CDefn where
    pPrintPrec d _p (Ctype i as ty) =
        sep [sep ((t"type" <+> ppConIdK d i) : map (nest 2 . ppVarId d) as) <+> t "=",
                  nest 2 (pp d ty)]
    pPrintPrec d _p (Cdata { cd_visible = vis,
                        cd_name = i,
                        cd_type_vars = as,
                        cd_original_summands = cs@(_:_),
                        cd_internal_summands = [],
                        cd_derivings = _ds }) =                -- a hack to print original constructors
        sep [sep ((t"data" <+> ppConIdK d i) : map (nest 2 . ppVarId d) as) <> t(if vis then " =" else " =="),
                  nest 2 (ppOSummands d cs)]
    pPrintPrec d _p (Cdata { cd_visible = vis,
                        cd_name = i,
                        cd_type_vars = as,
                        cd_internal_summands = cs,
                        cd_derivings = ds }) =
        sep [sep ((t"data" <+> ppConIdK d i) : map (nest 2 . ppVarId d) as) <> t(if vis then " =" else " =="),
                  nest 2 (ppSummands d cs)]
        <> ppDer d ds
    pPrintPrec d _p (Cstruct vis (SInterface prags) i as fs ds) =
        (t("interface ") <> sep (ppConIdK d i : map (nest 2 . ppVarId d) as) <+> ppIfcPragma d prags <+> t(if vis then "= {" else "== {")) $+$
        pBlock d 4 False (map (ppField d) fs) <> ppDer d ds
    pPrintPrec d _p (Cstruct vis _ss i as fs ds) =
        (t("struct ") <> sep (ppConIdK d i : map (nest 2 . ppVarId d) as) <+> t(if vis then "= {" else "== {")) $+$
        pBlock d 4 False (map (ppField d) fs) <> ppDer d ds
    pPrintPrec d _p (Cclass incoh ps ik is fd ss) =
        (t_cls <+> ppPreds d ps (sep (ppConIdK d ik : map (ppVarId d) is)) <> ppFDs d fd <+> t "where {") $+$
        pBlock d 4 False (map (ppField d) ss)
      where t_cls = case incoh of
                     Just False -> t"class coherent"
                     Just True  -> t"class incoherent"
                     Nothing    -> t"class"
    pPrintPrec d _p (Cinstance qt ds) =
        (t"instance" <+> pPrintPrec d 0 qt <+> t "where {") $+$
        pBlock d 4 False (map (pPrintPrec d 0) ds)
    pPrintPrec d p (CValueSign def) = pPrintPrec d p def
    pPrintPrec d p (CValue i cs) =
        vcat (map (\ cl -> ppClause d p [ppVarId d i] cl <> t";") cs)
    pPrintPrec d _p (Cprimitive i ty) =
        text "primitive" <+> ppVarId d i <+> t "::" <+> pp d ty
    pPrintPrec d  p (CPragma pr) = pPrintPrec d p pr
    pPrintPrec d _p (CprimType ik) =
        t"primitive type" <+>
        -- don't use ppConIdK because this syntax has no parentheses
        case (ik) of
            (IdK i)        -> ppConId d i
            (IdKind i k)   -> ppConId d i <+> t "::" <+> pp d k
            (IdPKind i pk) -> ppConId d i <+> t "::" <+> pp d pk
    pPrintPrec d _p (Cforeign i ty oname opnames) =
        text "foreign" <+> ppVarId d i <+> t "::" <+> pp d ty <> (case oname of Nothing -> text ""; Just s -> text (" = " ++ show s)) <> (case opnames of Nothing -> text ""; Just (is, os) -> t"," <> pparen True (sep (map (text . show) is ++ po os)))
      where po [o] = [text ",", text (show o)]
            po os = [t"(" <> sepList (map (text . show) os) (t",") <> t ")"]
    pPrintPrec d _p (CIinstance i qt) =
        t"instance" <+> ppConId d i <+> pPrintPrec d 0 qt
    pPrintPrec d _p (CItype i as _positions) =
        sep (t"type" <+> ppConIdK d i : map (nest 2 . ppVarId d) as)
    pPrintPrec d _p (CIclass incoh ps ik is fd _positions) =
        t_cls <+> ppPreds d ps (sep (ppConIdK d ik : map (nest 2 . ppVarId d) is)) <> ppFDs d fd
      where t_cls = case incoh of
                     Just False -> t"class coherent"
                     Just True  -> t"class incoherent"
                     Nothing    -> t"class"
    pPrintPrec d _p (CIValueSign i ty) = ppVarId d i <+> t "::" <+> pp d ty

instance HasPosition CDefn where
    getPosition d = getPosition (getName d)

-- Since IdPKind is only expected in some disjuncts of CDefn, we could
-- create a separate IdPK for those cases, but that seems like overkill.
-- IdPKind in other locations will just be treated like IdK (no kind info).
data IdK
        = IdK Id
        | IdKind Id Kind
        -- this should not exist after typecheck
        | IdPKind Id PartialKind
        deriving (Eq, Ord, Show)

instance Pretty IdK where
    pPrintPrec d  p (IdK i) = pPrintPrec d p i
    pPrintPrec d _p (IdKind i k) = pparen True $ pp d i <+> t "::" <+> pp d k
    pPrintPrec d _p (IdPKind i pk) = pparen True $ pp d i <+> t "::" <+> pp d pk

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

ppDer :: PDetail -> [CTypeclass] -> Doc
ppDer _d [] = text ""
ppDer  d is = text " deriving (" <> sepList (map (pPrintPrec d 0) is) (text ",") <> text ")"

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

instance Pretty CExpr where
    pPrintPrec d p (CLam ei e) = ppQuant "\\ "  d p ei e
    pPrintPrec d p (CLamT ei _ty e) = ppQuant "\\ "  d p ei e
    pPrintPrec d p (Cletseq [] e) = pparen (p > 0) $
        (t"letseq in" <+> pp d e)
    pPrintPrec d p (Cletseq ds e) = pparen (p > 0) $
        (t"letseq" <+> foldr1 ($+$) (map (pp d) ds)) $+$
        (t"in  " <> pp d e)
    pPrintPrec d p (Cletrec [] e) = pparen (p > 0) $
        (t"let in" <+> pp d e)
    pPrintPrec d p (Cletrec ds e) = pparen (p > 0) $
        (t"let" <+> foldr1 ($+$) (map (pp d) ds)) $+$
        (t"in  " <> pp d e)
    pPrintPrec  d  p (CSelect e i) = pparen (p > (fromIntegral maxPrec+2)) $ pPrintPrec d (fromIntegral maxPrec+2) e <> t"." <> ppVarId d i
    pPrintPrec  d _p (CCon i []) = ppConId d i
    pPrintPrec  d  p (CCon i es) = pPrintPrec d p (CApply (CCon i []) es)
    pPrintPrec  d  p (Ccase _pos e arms) = pparen (p > 0) $ ppCase d e arms
    pPrintPrec _d _p (CAny {}) = text "_"
    pPrintPrec  d _p (CVar i) = ppVarId d i
    pPrintPrec _d _p (CStruct _ tyc []) | tyc == idPrimUnit = text "()"
    pPrintPrec  d  p (CStruct _ tyc ies) = pparen (p > 0) $ pPrintPrec d (fromIntegral maxPrec+1) tyc <+> t "{" <+> sepList (map f ies ++ [t"}"]) (t";")
        where f (i, e) = ppVarId d i <+> t "=" <+> pp d e
    pPrintPrec d p (CStructUpd e ies) = pparen (p > 0) $ pPrintPrec d (fromIntegral maxPrec+1) e <+> t "{" <+> sepList (map f ies ++ [t"}"]) (t";")
        where f (i, e') = ppVarId d i <+> t "=" <+> pp d e'
    pPrintPrec d p (Cwrite _ e v)  = pparen (p > 0) $ pPrintPrec d (fromIntegral maxPrec+1) e <+> t ":=" <+> pPrintPrec d p v
    pPrintPrec d p (CApply e [])
      | d == pdReadable
      = pPrintPrec pdReadable p e
    pPrintPrec d p (CApply e es) = pparen (p>(fromIntegral maxPrec-1)) $
        sep (pPrintPrec d (fromIntegral maxPrec-1) e : map (nest 2 . ppApArg) es)
        where ppApArg e' = pPrintPrec d (fromIntegral maxPrec) e'
    pPrintPrec d p (CTaskApply e es) = pparen (p>(fromIntegral maxPrec-1)) $
        sep (pPrintPrec d (fromIntegral maxPrec-1) e : map (nest 2 . ppApArg) es)
        where ppApArg e' = pPrintPrec d (fromIntegral maxPrec) e'
    -- XXX: should include t?
    pPrintPrec d p (CTaskApplyT e _t es) = pparen (p>(fromIntegral maxPrec-1)) $
        sep (pPrintPrec d (fromIntegral maxPrec-1) e : map (nest 2 . ppApArg) es)
        where ppApArg e' = pPrintPrec d (fromIntegral maxPrec) e'
    pPrintPrec d  p (CLit l) = pPrintPrec d p l
    pPrintPrec d  p (CBinOp e1 i e2) = ppOp d p i e1 e2
    pPrintPrec d  p (CHasType e t') = pparen (p>0) $ pPrintPrec d (fromIntegral maxPrec) e <> text "::" <> pPrintPrec d (fromIntegral maxPrec) t'
    pPrintPrec d  p (Cif _pos c tr e) = pparen (p>0) (sep [t"if" <+> pp d c <+> t "then", nest 4 (pp d tr), t"else", nest 4 (pp d e)])
    pPrintPrec d _p (CSub _pos e s) = pPrintPrec d (fromIntegral maxPrec) e <> t"[" <> pp d s <> t"]"
    pPrintPrec d _p (CSub2 e h l) = pPrintPrec d (fromIntegral maxPrec) e <> t"[" <> pp d h <> t":" <> pp d l <> t"]"
    pPrintPrec d  p (CSubUpdate _pos e (h, l) rhs) = pPrintPrec d p (CSub2 e h l) <> t"=" <> pPrintPrec d (fromIntegral maxPrec) rhs
    pPrintPrec d _p (Cmodule _ is) = t"module {" $+$ pBlock d 2 False (map (pp d) is)
    pPrintPrec d  p (Cinterface _pos Nothing ds) =
        pparen (p>0) (t"interface {" $+$ pBlock d 2 False (map (pp d) ds))
    pPrintPrec d p (Cinterface _pos (Just i) ds) =
        pparen (p>0) (t"interface" <+> pp d i <+> t "{" $+$ pBlock d 2 False (map (pp d) ds))
    pPrintPrec d _p (CmoduleVerilog m _ui c r ses fs sch _ps) =
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
    pPrintPrec d _p (CForeignFuncC i _wrap_ty) =
        -- There's no real Classic syntax for this:
        t"ForeignFuncC" <+> pp d i
    pPrintPrec d p (Cdo _ ss) = pparen (p>0) $ t "do" <+> t "{" <+> sepList (map (pPrintPrec d 0) ss ++ [t"}"]) (t";")
    pPrintPrec d p (Caction _ ss) = pparen (p>0) $ t "action" <+> t "{" <+> sepList (map (pPrintPrec d 0) ss ++ [t"}"]) (t";")
    pPrintPrec d p (Crules [] rs) = pparen (p>0) $ t"rules {" $+$ pBlock d 2 False (map (pp d) rs)
    pPrintPrec d p (Crules ps rs) = pPrintPrec d p ps $+$
                                (pparen (p>0) $ t"rules {" $+$ pBlock d 2 False (map (pp d) rs))
    pPrintPrec d p (COper ops) = pparen (p > fromIntegral maxPrec-1) (sep (map (pPrintPrec d (fromIntegral maxPrec-1)) ops))
    ----
    pPrintPrec d p (CCon1 _ i e) = pPrintPrec d p (CCon i [e])
    pPrintPrec d p (CSelectTT _ e i) = pparen (p > (fromIntegral maxPrec+2)) $ pPrintPrec d (fromIntegral maxPrec+2) e <> t"." <> ppVarId d i
    ----
    pPrintPrec d _p (CCon0 _ i) = ppConId d i
    ----
    pPrintPrec d p (CConT _ i es) = pPrintPrec d p (CCon i es)
    pPrintPrec d p (CStructT ty ies) = pPrintPrec d p (CStruct (Just True) tyc ies)
        -- where (Just tyc) = leftCon ty
        where tyc = case leftCon ty of
                      Just tyc' -> tyc'
                      Nothing   -> error "Syntax.Pretty(CExpr): CStructT (leftCon ty failed)"
    pPrintPrec  d _p (CSelectT _ i) = text "." <> ppVarId d i
    pPrintPrec  d  p (CLitT _ l) = pPrintPrec d p l
    pPrintPrec _d _p (CAnyT _pos _uk _t) = text "_"
    pPrintPrec  d  p (CmoduleVerilogT _ m ui c mr ses fs sch ps) = pPrintPrec d p (CmoduleVerilog m ui c mr ses fs sch ps)
    pPrintPrec  d _p (CForeignFuncCT i _prim_ty) = t"ForeignFuncC" <+> pp d i
    pPrintPrec  d  p (CTApply e ts) = pparen (p>(fromIntegral maxPrec-1)) $
        sep (pPrintPrec d (fromIntegral maxPrec-1) e : map (nest 2 . ppApArg) ts)
        where ppApArg ty = t"\183" <> pPrintPrec d (fromIntegral maxPrec) ty
    pPrintPrec d _p (Cattributes pps) = pparen True $ text "Attributes" <+> pPrintPrec d 0 (map snd pps)

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

data CLiteral = CLiteral Position Literal deriving (Show)

instance Eq CLiteral where
        CLiteral _ l == CLiteral _ l'  =  l == l'

instance Ord CLiteral where
        CLiteral _ l `compare` CLiteral _ l'  =  l `compare` l'

instance Pretty CLiteral where
    pPrintPrec d p (CLiteral _ l) = pPrintPrec d p l

instance HasPosition CLiteral where
    getPosition (CLiteral p _) = p

ppQuant :: String -> PDetail -> Rational -> Either Position Id -> CExpr -> Doc
ppQuant s d p ei e =
    let ppI (Left _) = text "_"
        ppI (Right i) = pPrintPrec d 0 i
    in  pparen (p>0) (sep [t s <> ppI ei <+> t "->", pp d e])

data COp
        = CRand CExpr    -- operand
        | CRator Int Id  -- infix operator Id, Int is the number of arguments?
        deriving (Eq, Ord, Show)

instance Pretty COp where
    pPrintPrec d _ (CRand p) = pp d p
    pPrintPrec d _ (CRator _ i) = ppInfix d i

instance HasPosition COp where
    getPosition (CRand e) = getPosition e
    getPosition (CRator _ i) = getPosition i

ppCase :: PDetail -> CExpr -> [CCaseArm] -> Doc
ppCase detail scrutinee arms =
    (t"case" <+> pp detail scrutinee <+> t "of {") $+$
    pBlock detail 0 False (map ppArm arms)
  where ppArm arm =
            sep [pPrintPrec detail 0 (cca_pattern arm) <>
                 ppQuals detail (cca_filters arm) <+> t "-> ",
                 nest 2 (pp detail (cca_consequent arm))]

ppOp :: PDetail -> Rational -> Id -> CExpr -> CExpr -> Doc
ppOp d pd i p1 p2 =
        pparen (pd > 0) (sep [pPrintPrec d 1 p1 <> t"" <+> ppInfix d i, pPrintPrec d 1 p2])
{-
        let (p, lp, rp) =
                case getFixity i of
                FInfixl p -> (p, p, p+1)
                FInfixr p -> (p, p+1, p)
                FInfix  p -> (p, p+1, p+1)
        in pparen (d > PDReadable || pd>p)
                  (sep [pPrint d lp p1 <> t"" <+> ppInfix d i, pPrint d rp p2])
-}

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

instance Pretty CField where
    pPrintPrec d _p f = ppField d f

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

ppIfcPragma :: PDetail -> [IfcPragma] -> Doc
ppIfcPragma _detail [] = empty
ppIfcPragma  detail ps =
        text "{-#" <+>
        sep (punctuate comma (map (pPrintPrec detail 0) ps ) )
        <+> text "#-}"

ppFDs :: PDetail -> CFunDeps -> Doc
ppFDs _d [] = empty
ppFDs  d fd = text " |" <+> sepList (map (ppFD d) fd) (t",")

ppFD :: PDetail -> ([Id], [Id]) -> Doc
ppFD d (as,rs) = sep (ppVarId d <$> as) <+> t "->" <+> sep (ppVarId d <$> rs)

ppPreds :: PDetail -> [CPred] -> Doc -> Doc
ppPreds _d [] x = x
ppPreds  d preds x = t "(" <> sepList (map (pPrintPrec d 0) preds) (t ",") <> t ") =>" <+> x

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

instance Pretty CStmt where
    pPrintPrec d _p (CSBindT pat _inst pprops ty e) =
        foldr ($+$) empty $
            (map (ppPProp d . snd) pprops) ++
            [pp d pat <+> t "::" <+> pp d ty <+> t "<-" <+> pp d e]
    pPrintPrec d _p (CSBind pat _inst pprops e) =
        foldr ($+$) empty $
            (map (ppPProp d . snd) pprops) ++
            [pp d pat <+> t "<-" <+> pp d e]
    pPrintPrec _d _p (CSletseq []) = error "Syntax.Pretty(CStmt): CSletseq []"
    pPrintPrec  d _p (CSletseq ds) = text "letseq" <+> foldr1 ($+$) (map (pp d) ds)
    pPrintPrec _d _p (CSletrec []) = error "Syntax.Pretty(CStmt): CSletrec []"
    pPrintPrec  d _p (CSletrec ds) = text "let" <+> foldr1 ($+$) (map (pp d) ds)
    pPrintPrec  d p (CSExpr _ e) = pPrintPrec d p e

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

instance Pretty CMStmt where
    pPrintPrec d p (CMStmt s) = pPrintPrec d p s
    pPrintPrec d p (CMrules e) = pPrintPrec d p e
    pPrintPrec d p (CMinterface e) = pPrintPrec d p (cVApply (idReturn (getPosition e)) [e])
    pPrintPrec d p (CMTupleInterface _ es) = text"(" <> sepList (map (pPrintPrec d p) es) (text ",") <> text ")"

instance HasPosition CMStmt where
    getPosition (CMStmt s) = getPosition s
    getPosition (CMrules e) = getPosition e
    getPosition (CMinterface e) = getPosition e
    getPosition (CMTupleInterface pos _e) = pos

data CRule
        = CRule [RulePragma] (Maybe CExpr) [CQual] CExpr
        | CRuleNest [RulePragma] (Maybe CExpr) [CQual] [CRule]
        deriving (Eq, Ord, Show)

instance HasPosition CRule where
    getPosition (CRule _ i qs e) = getPosition (i, qs, e)
    getPosition (CRuleNest _ i qs rs) = getPosition (i, qs, rs)

instance Pretty CRule where
        pPrintPrec d _p (CRule rps mlbl mqs e) =
                ppRPS d rps $+$
                (case mlbl of Nothing -> t""; Just i -> pp d i <> t": ") <> sep [ppQuals d mqs, t "  ==>",
                nest 4 (pp d e)]
        pPrintPrec d _p (CRuleNest rps mlbl mqs rs) =
                ppRPS d rps $+$
                (case mlbl of Nothing -> t""; Just i -> pp d i <> t": ") <>
                        (ppQuals d mqs $+$ pBlock d 2 False (map (pp d) rs))

ppRPS :: PDetail -> [RulePragma] -> Doc
ppRPS _d [] = text ""
ppRPS  d rps = vcat (map (pPrintPrec d 0) rps)

-- | A definition with a binding. Can occur as a let expression, let statement
-- in a do block, a typeclass instance defn, or bindings in an interface.
data CDefl                -- [CQual] part is the when clause used in an interface
                          -- binding, ie the explicit condition attached to each method
        = CLValueSign CDef [CQual]     -- let x :: T = e2 -- explicit type sig
        | CLValue Id [CClause] [CQual] -- let y = e2      -- no explicit type sig
        | CLMatch CPat CExpr           -- let [z] = e3
        deriving (Eq, Ord, Show)

instance Pretty CDefl where
    pPrintPrec d p (CLValueSign def me) = optWhen d me $ pPrintPrec d p def
    pPrintPrec d p (CLValue i cs me) = optWhen d me $
        foldr1 ($+$) (map (\ cl -> ppClause d p [ppVarId d i] cl <> t";") cs)
    pPrintPrec d p (CLMatch pat e) = ppClause d p [] (CClause [pat] [] e)

instance HasPosition CDefl where
    getPosition (CLValueSign d _) = getPosition d
    getPosition (CLValue i _ _) = getPosition i
    getPosition (CLMatch p e) = getPosition (p, e)

optWhen :: PDetail -> [CQual] -> Doc -> Doc
optWhen _d [] s = s
optWhen  d qs s = s $+$ (t"    " <> ppQuals d qs)

ppValueSign :: PDetail -> Id -> [TyVar] -> CQType -> [CClause] -> Doc
ppValueSign d i [] ty cs =
        (ppVarId d i <+> t "::" <+> pp d ty <> t";") $+$
        foldr1 ($+$) (map (\ cl -> ppClause d 0 [ppVarId d i] cl <> t";") cs)
ppValueSign d i vs ty cs =
        (ppVarId d i <+> t ":: /\\" <> sep (map (pPrintPrec d (fromIntegral maxPrec)) vs) <> t"." <> pp d ty <> t";") $+$
        foldr1 ($+$) (map (\ cl -> ppClause d 0 [ppVarId d i] cl <> t";") cs)

ppClause :: PDetail -> Rational -> [Doc] -> CClause -> Doc
ppClause d _p xs (CClause ps mqs e) =
        sep [sep (xs ++ map (pPrintPrec d (fromIntegral maxPrec)) ps) <> ppQuals d mqs <+> t "= ",
                  nest 4 (pp d e)]

-- Definition, local or global
data CDef
        = CDef Id CQType [CClause]                        -- before type checking
        | CDefT Id [TyVar] CQType [CClause]                -- after type checking, with type variables from the CQType
        deriving (Eq, Ord, Show)

instance Pretty CDef where
    pPrintPrec d _p (CDef  i    ty cs) = ppValueSign d i [] ty cs
    pPrintPrec d _p (CDefT i vs ty cs) = ppValueSign d i vs ty cs

instance HasPosition CDef where
    getPosition (CDef i _ _) = getPosition i
    getPosition (CDefT i _ _ _) = getPosition i

-- Definition clause
-- each interface's definitions (within the module) correspond to one of these
data CClause
        = CClause [CPat]                -- arguments (including patterns)
                  [CQual]               -- qualifier on the args
                  CExpr                 -- the body
        deriving (Eq, Ord, Show)

instance Pretty CClause where
    pPrintPrec d p cl = ppClause d p [] cl

instance HasPosition CClause where
    getPosition (CClause ps qs e) = getPosition (ps, qs, e)

-- Pattern matching
data CQual
        = CQGen CType CPat CExpr
        | CQFilter CExpr
        deriving (Eq, Ord, Show)

instance Pretty CQual where
        pPrintPrec d _p (CQGen _ pa e) = pp d pa <+> t "<-" <+> pp d e
        pPrintPrec d _p (CQFilter e) = pp d e

instance HasPosition CQual where
    getPosition (CQGen _ p _) = getPosition p
    getPosition (CQFilter e) = getPosition e

ppQuals :: PDetail -> [CQual] -> Doc
ppQuals _d [] = t""
ppQuals  d qs = t" when" <+> sepList (map (pp d) qs) (t",")

ppOSummands :: PDetail -> [COriginalSummand] -> Doc
ppOSummands d cs = sepList (map (nest 2 . ppOCon) cs) (t" |")
  where ppOCon summand =
            let pp_name = case (cos_names summand) of
                            [cn] -> ppConId d cn
                            cns -> text "(" <>
                                   sepList (map (ppConId d) cns) (text ",") <>
                                   text ")"
                pp_args = map (pPrintPrec d (fromIntegral maxPrec)) (cos_arg_types summand)
                pp_encoding =
                    case cos_tag_encoding summand of
                    Nothing -> empty
                    Just num ->
                        text "{-# tag " <+> pPrintPrec d 0 num <+> text "#-}"
            in  sep (pp_name : pp_encoding : pp_args)

ppSummands :: PDetail -> [CInternalSummand] -> Doc
ppSummands d cs = sepList (map (nest 2 . ppCon) cs) (t" |")
  where ppCon summand =
            let pp_name = case (cis_names summand) of
                            [cn] -> ppConId d cn
                            cns -> text "(" <>
                                   sepList (map (ppConId d) cns) (text ",") <>
                                   text ")"
                pp_arg = pPrintPrec d (fromIntegral maxPrec) (cis_arg_type summand)
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

instance Pretty CPat where
    pPrintPrec d p (CPVar a) = pPrintPrec d p a
    pPrintPrec d p (CPCon i as) = pparen (p>(fromIntegral maxPrec-1)) $ sep (ppConId d i : map (pPrintPrec d (fromIntegral maxPrec)) as)
    pPrintPrec _d _p (CPstruct _ tyc []) | tyc == idPrimUnit = text "()"
    pPrintPrec  d _p (CPstruct _ tyc [(_, fst'), (_, snd')]) | tyc == idPrimPair =
        pparen True (pPrintPrec d 0 fst' <> t"," <+> pPrintPrec d 0 snd')
    pPrintPrec d p (CPstruct _ i fs) = pparen (p>(fromIntegral maxPrec-1)) $ ppConId d i <+> t "{" <+> sep (map ppField' fs ++ [t"}"])
        where ppField' (i', CPVar i'') | i' == i'' = ppVarId d i' <> t";"
              ppField' (i', p') = ppVarId d i' <+> t "=" <+> pp d p' <> t";"
    pPrintPrec  d _p (CPAs a pp') = pPrintPrec d (fromIntegral maxPrec) a <> t"@" <> pPrintPrec d (fromIntegral maxPrec) pp'
    pPrintPrec _d _p (CPAny _) = text "_"
    pPrintPrec  d  p (CPLit l) = pPrintPrec d p l
    pPrintPrec _d _p (CPMixedLit _ base ps) =
        let digitBits = log2 base
            f (len, Just val) = integerFormat (len `div` digitBits) base val
            f (len, Nothing)  = L.genericReplicate (len `div` digitBits) '?'
            pref  2 = "0b"
            pref  8 = "0o"
            pref 10 = ""
            pref 16 = "0x"
            pref x = error ("bad radix to CPMixedLit: " ++ show x)
        in  text (pref base ++ concatMap f ps)
    pPrintPrec d p (CPOper ops) = pparen (p > fromIntegral maxPrec-1) (sep (map (pPrintPrec d (fromIntegral maxPrec-1)) ops))
    pPrintPrec d p (CPCon1 _ i a) = pPrintPrec d p (CPCon i [a])
    ----
    pPrintPrec d p (CPConTs _ i ts as) = pparen (p>(fromIntegral maxPrec-1)) $ sep (ppConId d i : map ppApArg ts ++ map (pPrintPrec d (fromIntegral maxPrec)) as)
        where ppApArg ty = t"\183" <> pPrintPrec d (fromIntegral maxPrec) ty

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

data CPOp
        = CPRand CPat
        | CPRator Int Id
        deriving (Eq, Ord, Show)

instance Pretty CPOp where
    pPrintPrec d _ (CPRand p) = pp d p
    pPrintPrec d _ (CPRator _ i) = ppInfix d i

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

pp :: (Pretty a) => PDetail -> a -> Doc
pp d x = pPrintPrec d 0 x

t :: String -> Doc
t s = text s

newtype CInclude
       = CInclude String
    deriving (Eq, Ord, Show)

instance Pretty CInclude where
    pPrintPrec d p (CInclude s) = pPrintPrec d p s

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
