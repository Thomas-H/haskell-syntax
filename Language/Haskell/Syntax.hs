{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A suite of datatypes describing the abstract syntax of
-- <http://www.haskell.org/onlinereport/ Haskell 98> plus a few extensions:
--
--   * multi-parameter type classes
--
--   * parameters of type class assertions are unrestricted
--
-- This module has been changed so that show is a real show.
-- For GHC, we also derive Typeable and Data for all types.

-----------------------------------------------------------------------------

module Language.Haskell.Syntax (
    -- * Modules
    Module(..), ExportSpec(..),
    ImportDecl(..), ImportSpec(..), Assoc(..),
    -- * Declarations
    Decl(..), ConDecl(..), BangType(..),
    Match(..), Rhs(..), GuardedRhs(..),
    Safety(..),
    -- * Class Assertions and Contexts
    QualType(..), Context, Asst,
    -- * Types
    Type(..),
    -- * Expressions
    Exp(..), Stmt(..), FieldUpdate(..),
    Alt(..), GuardedAlts(..), GuardedAlt(..),
    -- * Patterns
    Pat(..), PatField(..),
    -- * Literals
    Literal(..),
    -- * Variables, Constructors and Operators
    ModuleName(..), QName(..), Name(..), QOp(..), Op(..),
    SpecialCon(..), CName(..),

    -- * Builtin names

    -- ** Modules
    prelude_mod, main_mod,
    -- ** Main function of a program
    main_name,
    -- ** Constructors
    unit_con_name, tuple_con_name, list_cons_name,
    unit_con, tuple_con,
    -- ** Type constructors
    unit_tycon_name, fun_tycon_name, list_tycon_name, tuple_tycon_name,
    unit_tycon, fun_tycon, list_tycon, tuple_tycon,

    -- * Source coordinates
    SrcLoc(..),
  ) where


#ifdef __GLASGOW_HASKELL__
import           Data.Generics.Basics
import           Data.Generics.Instances ()
#endif

-- | A position in the source.
data SrcLoc = SrcLoc {
                srcFilename :: String,
                srcLine     :: Int,
                srcColumn   :: Int
                }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | The name of a Haskell module.
newtype ModuleName = ModuleName String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.

data SpecialCon
        = UnitCon             -- ^ unit type and data constructor @()@
        | ListCon             -- ^ list type constructor @[]@
        | FunCon              -- ^ function type constructor @->@
        | TupleCon Int        -- ^ /n/-ary tuple type and data
                              --   constructors @(,)@ etc
        | Cons                -- ^ list data constructor @(:)@
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data QName
        = Qual ModuleName Name    -- ^ name qualified with a module name
        | UnQual Name             -- ^ unqualified name
        | Special SpecialCon      -- ^ built-in constructor with special syntax
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent variables, and also constructors.
data Name
        = Ident String        -- ^ /varid/ or /conid/
        | Symbol String       -- ^ /varsym/ or /consym/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data QOp
        = QVarOp QName      -- ^ variable operator (/qvarop/)
        | QConOp QName      -- ^ constructor operator (/qconop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Operators, appearing in @infix@ declarations.
data Op
        = VarOp Name        -- ^ variable operator (/varop/)
        | ConOp Name        -- ^ constructor operator (/conop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data CName
        = VarName Name      -- ^ name of a method or field
        | ConName Name      -- ^ name of a data constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A Haskell source module.
data Module = Module SrcLoc ModuleName (Maybe [ExportSpec])
                         [ImportDecl] [Decl]
#ifdef __GLASGOW_HASKELL__
  deriving (Show,Typeable,Data)
#else
  deriving (Show)
#endif

-- | Export specification.
data ExportSpec
         = EVar QName                       -- ^ variable
         | EAbs QName                       -- ^ @T@:
                        -- a class or datatype exported abstractly,
                        -- or a type synonym.
         | EThingAll QName                  -- ^ @T(..)@:
                        -- a class exported with all of its methods, or
                        -- a datatype exported with all of its constructors.
         | EThingWith QName [CName]         -- ^ @T(C_1,...,C_n)@:
                        -- a class exported with some of its methods, or
                        -- a datatype exported with some of its constructors.
         | EModuleContents ModuleName       -- ^ @module M@:
                        -- re-export a module.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Import declaration.
data ImportDecl = ImportDecl
        { importLoc       :: SrcLoc     -- ^ position of the @import@ keyword.
        , importModule    :: ModuleName -- ^ name of the module imported.
        , importQualified :: Bool       -- ^ imported @qualified@?
        , importAs        :: Maybe ModuleName  -- ^ optional alias name in an
                                        -- @as@ clause.
        , importSpecs     :: Maybe (Bool,[ImportSpec])
                        -- ^ optional list of import specifications.
                        -- The 'Bool' is 'True' if the names are excluded
                        -- by @hiding@.
        }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Import specification.
data ImportSpec
         = IVar Name                        -- ^ variable
         | IAbs Name                        -- ^ @T@:
                        -- the name of a class, datatype or type synonym.
         | IThingAll Name                   -- ^ @T(..)@:
                        -- a class imported with all of its methods, or
                        -- a datatype imported with all of its constructors.
         | IThingWith Name [CName]        -- ^ @T(C_1,...,C_n)@:
                        -- a class imported with some of its methods, or
                        -- a datatype imported with some of its constructors.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Associativity of an operator.
data Assoc
         = AssocNone  -- ^ non-associative operator (declared with @infix@)
         | AssocLeft  -- ^ left-associative operator (declared with @infixl@).
         | AssocRight -- ^ right-associative operator (declared with @infixr@)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Decl
         = TypeDecl    SrcLoc Name [Name] Type
         | DataDecl    SrcLoc Context Name [Name] [ConDecl] [QName]
         | InfixDecl   SrcLoc Assoc Int [Op]
         | NewTypeDecl SrcLoc Context Name [Name] ConDecl [QName]
         | ClassDecl   SrcLoc Context Name [Name] [Decl]
         | InstDecl    SrcLoc Context QName [Type] [Decl]
         | DefaultDecl SrcLoc [Type]
         | TypeSig     SrcLoc [Name] QualType
         | FunBind     [Match]
         | PatBind     SrcLoc Pat Rhs {-where-} [Decl]
         | ForeignImport SrcLoc String Safety String Name Type
         | ForeignExport SrcLoc String String Name Type
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Clauses of a function binding.
data Match
         = Match SrcLoc Name [Pat] Rhs {-where-} [Decl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declaration of a data constructor.
data ConDecl
         = ConDecl SrcLoc Name [BangType]
                            -- ^ ordinary data constructor
         | RecDecl SrcLoc Name [([Name],BangType)]
                            -- ^ record constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The type of a constructor argument or field, optionally including
-- a strictness annotation.
data BangType
         = BangedTy   Type  -- ^ strict component, marked with \"@!@\"
         | UnBangedTy Type  -- ^ non-strict component
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The right hand side of a function or pattern binding.
data Rhs
         = UnGuardedRhs Exp     -- ^ unguarded right hand side (/exp/)
         | GuardedRhss  [GuardedRhs]
                                -- ^ guarded right hand side (/gdrhs/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A guarded right hand side @|@ /exp/ @=@ /exp/.
-- The first expression will be Boolean-valued.
data GuardedRhs
         = GuardedRhs SrcLoc Exp Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Safety level for invoking a foreign entity
data Safety
        = Safe        -- ^ call may generate callbacks
        | Unsafe      -- ^ call will not generate callbacks
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data QualType
         = QualType Context Type
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Haskell types and type constructors.
data Type
         = TyFun   Type Type        -- ^ function type
         | TyTuple [Type]           -- ^ tuple type
         | TyApp   Type Type        -- ^ application of a type constructor
         | TyVar   Name             -- ^ type variable
         | TyCon   QName            -- ^ named type or type constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

type Context = [Asst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
type Asst    = (QName,[Type])

-- | /literal/.
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data Literal
        = Char        Char            -- ^ character literal
        | String      String          -- ^ string literal
        | Int         Integer         -- ^ integer literal
        | Frac        Rational        -- ^ floating point literal
        | CharPrim    Char            -- ^ GHC unboxed character literal
        | StringPrim  String          -- ^ GHC unboxed string literal
        | IntPrim     Integer         -- ^ GHC unboxed integer literal
        | FloatPrim   Rational        -- ^ GHC unboxed float literal
        | DoublePrim  Rational        -- ^ GHC unboxed double literal
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Haskell expressions.
--
-- /Notes:/
--
-- * Because it is difficult for parsers to distinguish patterns from
--   expressions, they typically parse them in the same way and then check
--   that they have the appropriate form.  Hence the expression type
--   includes some forms that are found only in patterns.  After these
--   checks, these constructors should not be used.
--
-- * The parser does not take precedence and associativity into account,
--   so it will leave 'InfixApp's associated to the left.
--
-- * The 'Language.Haskell.Pretty.Pretty' instance for 'Exp' does not
--   add parentheses in printing.

data Exp
        = Var QName                   -- ^ variable
        | Con QName                   -- ^ data constructor
        | Lit Literal                 -- ^ literal constant
        | InfixApp Exp QOp Exp        -- ^ infix application
        | App Exp Exp                 -- ^ ordinary application
        | NegApp Exp                  -- ^ negation expression @-@ /exp/
        | Lambda SrcLoc [Pat] Exp     -- ^ lambda expression
        | Let [Decl] Exp              -- ^ local declarations with @let@
        | If Exp Exp Exp              -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
        | Case Exp [Alt]              -- ^ @case@ /exp/ @of@ /alts/
        | Do [Stmt]                   -- ^ @do@-expression:
                                      -- the last statement in the list
                                      -- should be an expression.
        | Tuple [Exp]                 -- ^ tuple expression
        | List [Exp]                  -- ^ list expression
        | Paren Exp                   -- ^ parenthesized expression
        | LeftSection Exp QOp         -- ^ left section @(@/exp/ /qop/@)@
        | RightSection QOp Exp        -- ^ right section @(@/qop/ /exp/@)@
        | RecConstr QName [FieldUpdate]
                                      -- ^ record construction expression
        | RecUpdate Exp [FieldUpdate]
                                      -- ^ record update expression
        | EnumFrom Exp                -- ^ unbounded arithmetic sequence,
                                      -- incrementing by 1
        | EnumFromTo Exp Exp          -- ^ bounded arithmetic sequence,
                                      -- incrementing by 1
        | EnumFromThen Exp Exp        -- ^ unbounded arithmetic sequence,
                                      -- with first two elements given
        | EnumFromThenTo Exp Exp Exp  -- ^ bounded arithmetic sequence,
                                      -- with first two elements given
        | ListComp Exp [Stmt]         -- ^ list comprehension
        | ExpTypeSig SrcLoc Exp QualType
                                      -- ^ expression type signature
        | AsPat Name Exp              -- ^ patterns only
        | WildCard                    -- ^ patterns only
        | IrrPat Exp                  -- ^ patterns only
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A pattern, to be matched against a value.
data Pat
        = PVar Name                   -- ^ variable
        | PLit Literal                -- ^ literal constant
        | PNeg Pat                    -- ^ negated pattern
        | PInfixApp Pat QName Pat
                                      -- ^ pattern with infix data constructor
        | PApp QName [Pat]            -- ^ data constructor and argument
                                      -- patterns
        | PTuple [Pat]                -- ^ tuple pattern
        | PList [Pat]                 -- ^ list pattern
        | PParen Pat                  -- ^ parenthesized pattern
        | PRec QName [PatField]       -- ^ labelled pattern
        | PAsPat Name Pat             -- ^ @\@@-pattern
        | PWildCard                   -- ^ wildcard pattern (@_@)
        | PIrrPat Pat                 -- ^ irrefutable pattern (@~@)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fpat/ in a labeled record pattern.
data PatField
        = PFieldPat QName Pat
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | This type represents both /stmt/ in a @do@-expression,
--   and /qual/ in a list comprehension.
data Stmt
        = Generator SrcLoc Pat Exp
                            -- ^ a generator /pat/ @<-@ /exp/
        | Qualifier Exp     -- ^ an /exp/ by itself: in a @do@-expression,
                            -- an action whose result is discarded;
                            -- in a list comprehension, a guard expression
        | LetStmt [Decl]    -- ^ local bindings
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fbind/ in a labeled record construction or update expression.
data FieldUpdate
        = FieldUpdate QName Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /alt/ in a @case@ expression.
data Alt
        = Alt SrcLoc Pat GuardedAlts [Decl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data GuardedAlts
        = UnGuardedAlt Exp          -- ^ @->@ /exp/
        | GuardedAlts  [GuardedAlt] -- ^ /gdpat/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A guarded alternative @|@ /exp/ @->@ /exp/.
-- The first expression will be Boolean-valued.
data GuardedAlt
        = GuardedAlt SrcLoc Exp Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: ModuleName
prelude_mod           = ModuleName "Prelude"
main_mod              = ModuleName "Main"

main_name :: Name
main_name             = Ident "main"

unit_con_name :: QName
unit_con_name         = Special UnitCon

tuple_con_name :: Int -> QName
tuple_con_name i      = Special (TupleCon (i+1))

list_cons_name :: QName
list_cons_name        = Special Cons

unit_con :: Exp
unit_con              = Con unit_con_name

tuple_con :: Int -> Exp
tuple_con i           = Con (tuple_con_name i)

unit_tycon_name, fun_tycon_name, list_tycon_name :: QName
unit_tycon_name       = unit_con_name
fun_tycon_name        = Special FunCon
list_tycon_name       = Special ListCon

tuple_tycon_name :: Int -> QName
tuple_tycon_name i    = tuple_con_name i

unit_tycon, fun_tycon, list_tycon :: Type
unit_tycon            = TyCon unit_tycon_name
fun_tycon             = TyCon fun_tycon_name
list_tycon            = TyCon list_tycon_name

tuple_tycon :: Int -> Type
tuple_tycon i         = TyCon (tuple_tycon_name i)
