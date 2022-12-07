module Language.Bluespec.Classic.AST.Syntax where

import Language.Bluespec.Classic.AST.Id
import Language.Bluespec.Classic.AST.Literal
import Language.Bluespec.Classic.AST.Position
import Language.Bluespec.Classic.AST.Pragma
import Language.Bluespec.Classic.AST.Type
import Language.Bluespec.Classic.AST.Undefined
import Language.Bluespec.Classic.AST.VModInfo

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

data CExport
        = CExpVar Id    -- export a variable identifier
        | CExpCon Id    -- export a constructor
        | CExpConAll Id -- export an identifier and constructors
                        -- (datatypes, interfaces, etc.)
        | CExpPkg Id    -- export an entire package

data CImport
        = CImpId Bool Id                                -- Bool indicates qualified
        | CImpSign String Bool CSignature

-- Package signature from import
data CSignature
        = CSignature Id [Id] [CFixity] [CDefn]        -- package name, imported packages, definitions

data CFixity
        = CInfix  Integer Id
        | CInfixl Integer Id
        | CInfixr Integer Id

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

-- Since IdPKind is only expected in some disjuncts of CDefn, we could
-- create a separate IdPK for those cases, but that seems like overkill.
-- IdPKind in other locations will just be treated like IdK (no kind info).
data IdK
        = IdK Id
        | IdKind Id Kind
        -- this should not exist after typecheck
        | IdPKind Id PartialKind

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

data CLiteral = CLiteral Position Literal

data COp
        = CRand CExpr    -- operand
        | CRator Int Id  -- infix operator Id, Int is the number of arguments?

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

-- if CQType is a function, [IfcPragmas] (if present) lists argument names
-- (used by the backend to generate pretty names for module ports)
data CField = CField { cf_name :: Id,
                       cf_pragmas :: Maybe [IfcPragma],
                       cf_type :: CQType,
                       cf_default :: [CClause],
                       cf_orig_type :: Maybe CType
                     }

type CFields = [CField] -- just a list of CField

-- redundant
--type Ids = [Id]
data CCaseArm = CCaseArm { cca_pattern :: CPat,
                           cca_filters :: [CQual],
                           cca_consequent :: CExpr }

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

type CStmts = [CStmt]

data CMStmt
        = CMStmt CStmt
        | CMrules CExpr
        | CMinterface CExpr
        | CMTupleInterface Position [CExpr]

data CRule
        = CRule [RulePragma] (Maybe CExpr) [CQual] CExpr
        | CRuleNest [RulePragma] (Maybe CExpr) [CQual] [CRule]

-- | A definition with a binding. Can occur as a let expression, let statement
-- in a do block, a typeclass instance defn, or bindings in an interface.
data CDefl                -- [CQual] part is the when clause used in an interface
                          -- binding, ie the explicit condition attached to each method
        = CLValueSign CDef [CQual]     -- let x :: T = e2 -- explicit type sig
        | CLValue Id [CClause] [CQual] -- let y = e2      -- no explicit type sig
        | CLMatch CPat CExpr           -- let [z] = e3

-- Definition, local or global
data CDef
        = CDef Id CQType [CClause]                        -- before type checking
        | CDefT Id [TyVar] CQType [CClause]                -- after type checking, with type variables from the CQType

-- Definition clause
-- each interface's definitions (within the module) correspond to one of these
data CClause
        = CClause [CPat]                -- arguments (including patterns)
                  [CQual]               -- qualifier on the args
                  CExpr                 -- the body

-- Pattern matching
data CQual
        = CQGen CType CPat CExpr
        | CQFilter CExpr

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

data CPOp
        = CPRand CPat
        | CPRator Int Id

newtype CInclude
       = CInclude String
