-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Pretty
-- Copyright   :  (c) The GHC Team, Noel Winstanley 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty printer for Haskell.
--
-----------------------------------------------------------------------------

module Language.Haskell.Pretty
  ( -- * Pretty printing
    Pretty,
    prettyPrintStyleMode,
    prettyPrintWithMode,
    prettyPrint,

    -- * Pretty-printing styles (from "Text.PrettyPrint.HughesPJ")
    P.Style(..),
    P.style,
    P.Mode(..),

    -- * Haskell formatting modes
    PPMode(..),
    Indent,
    PPLayout(..),
    defaultMode
  ) where

import           Language.Haskell.Syntax

import           Control.Applicative as App (Applicative (..))
import           Control.Monad           (ap)

import qualified Text.PrettyPrint        as P

infixl 5 $$$

-----------------------------------------------------------------------------

-- | Varieties of layout we can use.
data PPLayout = PPOffsideRule   -- ^ classical layout
              | PPSemiColon     -- ^ classical layout made explicit
              | PPInLine        -- ^ inline decls, with newlines between them
              | PPNoLayout      -- ^ everything on a single line
              deriving Eq

type Indent = Int

-- | Pretty-printing parameters.
--
-- /Note:/ the 'onsideIndent' must be positive and less than all other indents.
data PPMode = PPMode {
                                -- | indentation of a class or instance
                classIndent  :: Indent,
                                -- | indentation of a @do@-expression
                doIndent     :: Indent,
                                -- | indentation of the body of a
                                -- @case@ expression
                caseIndent   :: Indent,
                                -- | indentation of the declarations in a
                                -- @let@ expression
                letIndent    :: Indent,
                                -- | indentation of the declarations in a
                                -- @where@ clause
                whereIndent  :: Indent,
                                -- | indentation added for continuation
                                -- lines that would otherwise be offside
                onsideIndent :: Indent,
                                -- | blank lines between statements?
                spacing      :: Bool,
                                -- | Pretty-printing style to use
                layout       :: PPLayout,
                                -- | add GHC-style @LINE@ pragmas to output?
                linePragmas  :: Bool,
                                -- | not implemented yet
                comments     :: Bool
                }

-- | The default mode: pretty-print using the offside rule and sensible
-- defaults.
defaultMode :: PPMode
defaultMode = PPMode{
                      classIndent = 8,
                      doIndent = 3,
                      caseIndent = 4,
                      letIndent = 4,
                      whereIndent = 6,
                      onsideIndent = 2,
                      spacing = True,
                      layout = PPOffsideRule,
                      linePragmas = False,
                      comments = True
                      }

-- | Pretty printing monad
newtype DocM s a = DocM (s -> a)

instance Functor (DocM s) where
         fmap f xs = do x <- xs; return (f x)

-- | @since 1.0.2.0
instance App.Applicative (DocM s) where
        pure = retDocM
        (<*>) = ap
        (*>) = then_DocM

instance Monad (DocM s) where
        (>>=) = thenDocM
        (>>) = (*>)
        return = pure

{-# INLINE thenDocM #-}
{-# INLINE then_DocM #-}
{-# INLINE retDocM #-}
{-# INLINE unDocM #-}
{-# INLINE getPPEnv #-}

thenDocM :: DocM s a -> (a -> DocM s b) -> DocM s b
thenDocM m k = DocM $ (\s -> case unDocM m $ s of a -> unDocM (k a) $ s)

then_DocM :: DocM s a -> DocM s b -> DocM s b
then_DocM m k = DocM $ (\s -> case unDocM m $ s of _ -> unDocM k $ s)

retDocM :: a -> DocM s a
retDocM a = DocM (\_s -> a)

unDocM :: DocM s a -> (s -> a)
unDocM (DocM f) = f

-- all this extra stuff, just for this one function.
getPPEnv :: DocM s s
getPPEnv = DocM id

-- So that pp code still looks the same
-- this means we lose some generality though

-- | The document type produced by these pretty printers uses a 'PPMode'
-- environment.
type Doc = DocM PPMode P.Doc

-- | Things that can be pretty-printed, including all the syntactic objects
-- in "Language.Haskell.Syntax".
class Pretty a where
        -- | Pretty-print something in isolation.
        pretty :: a -> Doc
        -- | Pretty-print something in a precedence context.
        prettyPrec :: Int -> a -> Doc
        pretty = prettyPrec 0
        prettyPrec _ = pretty

-- The pretty printing combinators

empty :: Doc
empty = return P.empty

nest :: Int -> Doc -> Doc
nest i m = m >>= return . P.nest i


-- Literals

text :: String -> Doc
text = return . P.text
-- ptext = return . P.text

char :: Char -> Doc
char = return . P.char

int :: Int -> Doc
int = return . P.int

integer :: Integer -> Doc
integer = return . P.integer

float :: Float -> Doc
float = return . P.float

double :: Double -> Doc
double = return . P.double

-- rational :: Rational -> Doc
-- rational = return . P.rational

-- Simple Combining Forms

parens, brackets, braces :: Doc -> Doc
parens d = d >>= return . P.parens
brackets d = d >>= return . P.brackets
braces d = d >>= return . P.braces
-- quotes d = d >>= return . P.quotes
-- doubleQuotes d = d >>= return . P.doubleQuotes

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- Constants

semi,comma,space,equals :: Doc
semi = return P.semi
comma = return P.comma
-- colon = return P.colon
space = return P.space
equals = return P.equals

-- lparen,rparen,lbrack,rbrack,lbrace,rbrace :: Doc
-- lparen = return  P.lparen
-- rparen = return  P.rparen
-- lbrack = return  P.lbrack
-- rbrack = return  P.rbrack
-- lbrace = return  P.lbrace
-- rbrace = return  P.rbrace

-- Combinators

(<<>>),(<+>),($$) :: Doc -> Doc -> Doc
aM <<>> bM = do{a<-aM;b<-bM;return (a P.<> b)}
aM <+> bM = do{a<-aM;b<-bM;return (a P.<+> b)}
aM $$ bM = do{a<-aM;b<-bM;return (a P.$$ b)}
-- aM $+$ bM = do{a<-aM;b<-bM;return (a P.$+$ b)}

hcat,hsep,vcat,fsep :: [Doc] -> Doc
hcat dl = sequence dl >>= return . P.hcat
hsep dl = sequence dl >>= return . P.hsep
vcat dl = sequence dl >>= return . P.vcat
-- sep dl = sequence dl >>= return . P.sep
-- cat dl = sequence dl >>= return . P.cat
fsep dl = sequence dl >>= return . P.fsep
-- fcat dl = sequence dl >>= return . P.fcat

-- Some More

-- hang :: Doc -> Int -> Doc -> Doc
-- hang dM i rM = do{d<-dM;r<-rM;return $ P.hang d i r}

-- Yuk, had to cut-n-paste this one from Pretty.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (d1:ds) = go d1 ds
                   where
                     go d []     = [d]
                     go d (e:es) = (d <<>> p) : go e es

-- | render the document with a given style and mode.
renderStyleMode :: P.Style -> PPMode -> Doc -> String
renderStyleMode ppStyle ppMode d = P.renderStyle ppStyle . unDocM d $ ppMode

-- --- | render the document with a given mode.
-- renderWithMode :: PPMode -> Doc -> String
-- renderWithMode = renderStyleMode P.style

-- -- | render the document with 'defaultMode'.
-- render :: Doc -> String
-- render = renderWithMode defaultMode

-- | pretty-print with a given style and mode.
prettyPrintStyleMode :: Pretty a => P.Style -> PPMode -> a -> String
prettyPrintStyleMode ppStyle ppMode = renderStyleMode ppStyle ppMode . pretty

-- | pretty-print with the default style and a given mode.
prettyPrintWithMode :: Pretty a => PPMode -> a -> String
prettyPrintWithMode = prettyPrintStyleMode P.style

-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> String
prettyPrint = prettyPrintWithMode defaultMode

-- fullRenderWithMode :: PPMode -> P.Mode -> Int -> Float ->
--                       (P.TextDetails -> a -> a) -> a -> Doc -> a
-- fullRenderWithMode ppMode m i f fn e mD =
--                    P.fullRender m i f fn e $ (unDocM mD) ppMode
--
--
-- fullRender :: P.Mode -> Int -> Float -> (P.TextDetails -> a -> a)
--               -> a -> Doc -> a
-- fullRender = fullRenderWithMode defaultMode

-------------------------  Pretty-Print a Module --------------------
instance Pretty Module where
        pretty (Module pos m mbExports imp decls) =
                markLine pos $
                topLevel (ppModuleHeader m mbExports)
                         (map pretty imp ++ map pretty decls)

--------------------------  Module Header ------------------------------
ppModuleHeader :: ModuleName -> Maybe [ExportSpec] ->  Doc
ppModuleHeader m mbExportList = mySep [
        text "module",
        pretty m,
        maybePP (parenList . map pretty) mbExportList,
        text "where"]

instance Pretty ModuleName where
        pretty (ModuleName modName) = text modName

instance Pretty ExportSpec where
        pretty (EVar name)                = pretty name
        pretty (EAbs name)                = pretty name
        pretty (EThingAll name)           = pretty name <<>> text "(..)"
        pretty (EThingWith name nameList) =
                pretty name <<>> (parenList . map pretty $ nameList)
        pretty (EModuleContents m)       = text "module" <+> pretty m

instance Pretty ImportDecl where
        pretty (ImportDecl pos m qual mbName mbSpecs) =
                markLine pos $
                mySep [text "import",
                       if qual then text "qualified" else empty,
                       pretty m,
                       maybePP (\m' -> text "as" <+> pretty m') mbName,
                       maybePP exports mbSpecs]
            where
                exports (b,specList) =
                        if b then text "hiding" <+> specs else specs
                    where specs = parenList . map pretty $ specList

instance Pretty ImportSpec where
        pretty (IVar name)                = pretty name
        pretty (IAbs name)                = pretty name
        pretty (IThingAll name)           = pretty name <<>> text "(..)"
        pretty (IThingWith name nameList) =
                pretty name <<>> (parenList . map pretty $ nameList)

-------------------------  Declarations ------------------------------
instance Pretty Decl where
        pretty (TypeDecl loc name nameList htype) =
                blankline $
                markLine loc $
                mySep ( [text "type", pretty name]
                        ++ map pretty nameList
                        ++ [equals, pretty htype])

        pretty (DataDecl loc context name nameList constrList derives) =
                blankline $
                markLine loc $
                mySep ( [text "data", ppContext context, pretty name]
                        ++ map pretty nameList)
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                        $$$ ppDeriving derives)

        pretty (NewTypeDecl pos context name nameList constr derives) =
                blankline $
                markLine pos $
                mySep ( [text "newtype", ppContext context, pretty name]
                        ++ map pretty nameList)
                        <+> equals <+> (pretty constr $$$ ppDeriving derives)

        --m{spacing=False}
        -- special case for empty class declaration
        pretty (ClassDecl pos context name nameList []) =
                blankline $
                markLine pos $
                mySep ( [text "class", ppContext context, pretty name]
                        ++ map pretty nameList)
        pretty (ClassDecl pos context name nameList declList) =
                blankline $
                markLine pos $
                mySep ( [text "class", ppContext context, pretty name]
                        ++ map pretty nameList ++ [text "where"])
                $$$ ppBody classIndent (map pretty declList)

        -- m{spacing=False}
        -- special case for empty instance declaration
        pretty (InstDecl pos context name args []) =
                blankline $
                markLine pos $
                mySep ( [text "instance", ppContext context, pretty name]
                        ++ map ppAType args)
        pretty (InstDecl pos context name args declList) =
                blankline $
                markLine pos $
                mySep ( [text "instance", ppContext context, pretty name]
                        ++ map ppAType args ++ [text "where"])
                $$$ ppBody classIndent (map pretty declList)

        pretty (DefaultDecl pos htypes) =
                blankline $
                markLine pos $
                text "default" <+> parenList (map pretty htypes)

        pretty (TypeSig pos nameList qualType) =
                blankline $
                markLine pos $
                mySep ((punctuate comma . map pretty $ nameList)
                      ++ [text "::", pretty qualType])

        pretty (ForeignImport pos conv safety entity name ty) =
                blankline $
                markLine pos $
                mySep $ [text "foreign", text "import", text conv, pretty safety] ++
                        (if null entity then [] else [text (show entity)]) ++
                        [pretty name, text "::", pretty ty]

        pretty (ForeignExport pos conv entity name ty) =
                blankline $
                markLine pos $
                mySep $ [text "foreign", text "export", text conv] ++
                        (if null entity then [] else [text (show entity)]) ++
                        [pretty name, text "::", pretty ty]

        pretty (FunBind matches) =
                ppBindings (map pretty matches)

        pretty (PatBind pos pat rhs whereDecls) =
                markLine pos $
                myFsep [pretty pat, pretty rhs] $$$ ppWhere whereDecls

        pretty (InfixDecl pos assoc prec opList) =
                blankline $
                markLine pos $
                mySep ([pretty assoc, int prec]
                       ++ (punctuate comma . map pretty $ opList))

instance Pretty Assoc where
        pretty AssocNone  = text "infix"
        pretty AssocLeft  = text "infixl"
        pretty AssocRight = text "infixr"

instance Pretty Safety where
        pretty Safe   = text "safe"
        pretty Unsafe = text "unsafe"

instance Pretty Match where
        pretty (Match pos f ps rhs whereDecls) =
                markLine pos $
                myFsep (lhs ++ [pretty rhs])
                $$$ ppWhere whereDecls
            where
                lhs = case ps of
                        l:r:ps' | isSymbolName f ->
                                let hd = [pretty l, ppName f, pretty r] in
                                if null ps' then hd
                                else parens (myFsep hd) : map (prettyPrec 2) ps'
                        _ -> pretty f : map (prettyPrec 2) ps

ppWhere :: [Decl] -> Doc
ppWhere [] = empty
ppWhere l  = nest 2 (text "where" $$$ ppBody whereIndent (map pretty l))

------------------------- Data & Newtype Bodies -------------------------
instance Pretty ConDecl where
        pretty (RecDecl _pos name fieldList) =
                pretty name <<>> (braceList . map ppField $ fieldList)

        pretty (ConDecl _pos name@(Symbol _) [l, r]) =
                myFsep [prettyPrec prec_btype l, ppName name,
                        prettyPrec prec_btype r]
        pretty (ConDecl _pos name typeList) =
                mySep $ ppName name : map (prettyPrec prec_atype) typeList

ppField :: ([Name],BangType) -> Doc
ppField (names, ty) =
        myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]

instance Pretty BangType where
        prettyPrec _ (BangedTy ty)   = char '!' <<>> ppAType ty
        prettyPrec p (UnBangedTy ty) = prettyPrec p ty

ppDeriving :: [QName] -> Doc
ppDeriving []  = empty
ppDeriving [d] = text "deriving" <+> ppQName d
ppDeriving ds  = text "deriving" <+> parenList (map ppQName ds)

------------------------- Types -------------------------
instance Pretty QualType where
        pretty (QualType context htype) =
                myFsep [ppContext context, pretty htype]

ppBType :: Type -> Doc
ppBType = prettyPrec prec_btype

ppAType :: Type -> Doc
ppAType = prettyPrec prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1  -- left argument of ->,
                -- or either argument of an infix data constructor
prec_atype = 2  -- argument of type or data constructor, or of a class

instance Pretty Type where
        prettyPrec p (TyFun a b) = parensIf (p > 0) $
                myFsep [ppBType a, text "->", pretty b]
        prettyPrec _ (TyTuple l) = parenList . map pretty $ l
        prettyPrec p (TyApp a b)
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = parensIf (p > prec_btype) $
                        myFsep [pretty a, ppAType b]
        prettyPrec _ (TyVar name) = pretty name
        prettyPrec _ (TyCon name) = pretty name

------------------------- Expressions -------------------------
instance Pretty Rhs where
        pretty (UnGuardedRhs e)        = equals <+> pretty e
        pretty (GuardedRhss guardList) = myVcat . map pretty $ guardList

instance Pretty GuardedRhs where
        pretty (GuardedRhs _pos guard body) =
                myFsep [char '|', pretty guard, equals, pretty body]

instance Pretty Literal where
        pretty (Int i)        = integer i
        pretty (Char c)       = text (show c)
        pretty (String s)     = text (show s)
        pretty (Frac r)       = double (fromRational r)
        -- GHC unboxed literals:
        pretty (CharPrim c)   = text (show c)           <<>> char '#'
        pretty (StringPrim s) = text (show s)           <<>> char '#'
        pretty (IntPrim i)    = integer i               <<>> char '#'
        pretty (FloatPrim r)  = float  (fromRational r) <<>> char '#'
        pretty (DoublePrim r) = double (fromRational r) <<>> text "##"

instance Pretty Exp where
        pretty (Lit l) = pretty l
        -- lambda stuff
        pretty (InfixApp a op b) = myFsep [pretty a, pretty op, pretty b]
        pretty (NegApp e) = myFsep [char '-', pretty e]
        pretty (App a b) = myFsep [pretty a, pretty b]
        pretty (Lambda _loc expList body) = myFsep $
                char '\\' : map pretty expList ++ [text "->", pretty body]
        -- keywords
        pretty (Let expList letBody) =
                myFsep [text "let" <+> ppBody letIndent (map pretty expList),
                        text "in", pretty letBody]
        pretty (If cond thenexp elsexp) =
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        pretty (Case cond altList) =
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        pretty (Do stmtList) =
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        -- Constructors & Vars
        pretty (Var name) = pretty name
        pretty (Con name) = pretty name
        pretty (Tuple expList) = parenList . map pretty $ expList
        -- weird stuff
        pretty (Paren e) = parens . pretty $ e
        pretty (LeftSection e op) = parens (pretty e <+> pretty op)
        pretty (RightSection op e) = parens (pretty op <+> pretty e)
        pretty (RecConstr c fieldList) =
                pretty c <<>> (braceList . map pretty $ fieldList)
        pretty (RecUpdate e fieldList) =
                pretty e <<>> (braceList . map pretty $ fieldList)
        -- patterns
        -- special case that would otherwise be buggy
        pretty (AsPat name (IrrPat e)) =
                myFsep [pretty name <<>> char '@', char '~' <<>> pretty e]
        pretty (AsPat name e) = hcat [pretty name, char '@', pretty e]
        pretty WildCard = char '_'
        pretty (IrrPat e) = char '~' <<>> pretty e
        -- Lists
        pretty (List list) =
                bracketList . punctuate comma . map pretty $ list
        pretty (EnumFrom e) =
                bracketList [pretty e, text ".."]
        pretty (EnumFromTo from to) =
                bracketList [pretty from, text "..", pretty to]
        pretty (EnumFromThen from thenE) =
                bracketList [pretty from <<>> comma, pretty thenE, text ".."]
        pretty (EnumFromThenTo from thenE to) =
                bracketList [pretty from <<>> comma, pretty thenE,
                             text "..", pretty to]
        pretty (ListComp e stmtList) =
                bracketList ([pretty e, char '|']
                             ++ (punctuate comma . map pretty $ stmtList))
        pretty (ExpTypeSig _pos e ty) =
                myFsep [pretty e, text "::", pretty ty]

------------------------- Patterns -----------------------------

instance Pretty Pat where
        prettyPrec _ (PVar name) = pretty name
        prettyPrec _ (PLit lit) = pretty lit
        prettyPrec _ (PNeg p) = myFsep [char '-', pretty p]
        prettyPrec p (PInfixApp a op b) = parensIf (p > 0) $
                myFsep [pretty a, pretty (QConOp op), pretty b]
        prettyPrec p (PApp n ps) = parensIf (p > 1) $
                myFsep (pretty n : map pretty ps)
        prettyPrec _ (PTuple ps) = parenList . map pretty $ ps
        prettyPrec _ (PList ps) =
                bracketList . punctuate comma . map pretty $ ps
        prettyPrec _ (PParen p) = parens . pretty $ p
        prettyPrec _ (PRec c fields) =
                pretty c <<>> (braceList . map pretty $ fields)
        -- special case that would otherwise be buggy
        prettyPrec _ (PAsPat name (PIrrPat pat)) =
                myFsep [pretty name <<>> char '@', char '~' <<>> pretty pat]
        prettyPrec _ (PAsPat name pat) =
                hcat [pretty name, char '@', pretty pat]
        prettyPrec _ PWildCard = char '_'
        prettyPrec _ (PIrrPat pat) = char '~' <<>> pretty pat

instance Pretty PatField where
        pretty (PFieldPat name pat) =
                myFsep [pretty name, equals, pretty pat]

------------------------- Case bodies  -------------------------
instance Pretty Alt where
        pretty (Alt _pos e gAlts decls) =
                myFsep [pretty e, pretty gAlts] $$$ ppWhere decls

instance Pretty GuardedAlts where
        pretty (UnGuardedAlt e)      = text "->" <+> pretty e
        pretty (GuardedAlts altList) = myVcat . map pretty $ altList

instance Pretty GuardedAlt where
        pretty (GuardedAlt _pos e body) =
                myFsep [char '|', pretty e, text "->", pretty body]

------------------------- Statements in monads & list comprehensions -----
instance Pretty Stmt where
        pretty (Generator _loc e from) =
                pretty e <+> text "<-" <+> pretty from
        pretty (Qualifier e) = pretty e
        pretty (LetStmt declList) =
                text "let" $$$ ppBody letIndent (map pretty declList)

------------------------- Record updates
instance Pretty FieldUpdate where
        pretty (FieldUpdate name e) =
                myFsep [pretty name, equals, pretty e]

------------------------- Names -------------------------
instance Pretty QOp where
        pretty (QVarOp n) = ppQNameInfix n
        pretty (QConOp n) = ppQNameInfix n

ppQNameInfix :: QName -> Doc
ppQNameInfix name
        | isSymbolName (getName name) = ppQName name
        | otherwise = char '`' <<>> ppQName name <<>> char '`'

instance Pretty QName where
        pretty name = parensIf (isSymbolName (getName name)) (ppQName name)

ppQName :: QName -> Doc
ppQName (UnQual name) = ppName name
ppQName (Qual m name) = pretty m <<>> char '.' <<>> ppName name
ppQName (Special sym) = text (specialName sym)

instance Pretty Op where
        pretty (VarOp n) = ppNameInfix n
        pretty (ConOp n) = ppNameInfix n

ppNameInfix :: Name -> Doc
ppNameInfix name
        | isSymbolName name = ppName name
        | otherwise = char '`' <<>> ppName name <<>> char '`'

instance Pretty Name where
        pretty name = parensIf (isSymbolName name) (ppName name)

ppName :: Name -> Doc
ppName (Ident s)  = text s
ppName (Symbol s) = text s

instance Pretty CName where
        pretty (VarName n) = pretty n
        pretty (ConName n) = pretty n

isSymbolName :: Name -> Bool
isSymbolName (Symbol _) = True
isSymbolName _            = False

getName :: QName -> Name
getName (UnQual s)         = s
getName (Qual _ s)         = s
getName (Special Cons)   = Symbol ":"
getName (Special FunCon) = Symbol "->"
getName (Special s)        = Ident (specialName s)

specialName :: SpecialCon -> String
specialName UnitCon      = "()"
specialName ListCon      = "[]"
specialName FunCon       = "->"
specialName (TupleCon n) = "(" ++ replicate (n-1) ',' ++ ")"
specialName Cons         = ":"

ppContext :: Context -> Doc
ppContext []      = empty
ppContext context = mySep [parenList (map ppAsst context), text "=>"]

-- hacked for multi-parameter type classes

ppAsst :: Asst -> Doc
ppAsst (a,ts) = myFsep (ppQName a : map ppAType ts)

------------------------- pp utils -------------------------
maybePP :: (a -> Doc) -> Maybe a -> Doc
maybePP _ Nothing   = empty
maybePP pp (Just a) = pp a

parenList :: [Doc] -> Doc
parenList = parens . myFsepSimple . punctuate comma

braceList :: [Doc] -> Doc
braceList = braces . myFsepSimple . punctuate comma

bracketList :: [Doc] -> Doc
bracketList = brackets . myFsepSimple

-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-
flatBlock :: [Doc] -> Doc
flatBlock = braces . (space <<>>) . hsep . punctuate semi

-- Same, but put each thing on a separate line
prettyBlock :: [Doc] -> Doc
prettyBlock = braces . (space <<>>) . vcat . punctuate semi

-- Monadic PP Combinators -- these examine the env

blankline :: Doc -> Doc
blankline dl = do{e<-getPPEnv;if spacing e && layout e /= PPNoLayout
                              then space $$ dl else dl}
topLevel :: Doc -> [Doc] -> Doc
topLevel header dl = do
         e <- fmap layout getPPEnv
         case e of
             PPOffsideRule -> header $$ vcat dl
             PPSemiColon   -> header $$ prettyBlock dl
             PPInLine      -> header $$ prettyBlock dl
             PPNoLayout    -> header <+> flatBlock dl

ppBody :: (PPMode -> Int) -> [Doc] -> Doc
ppBody f dl = do
        e <- fmap layout getPPEnv
        i <- fmap f getPPEnv
        case e of
            PPOffsideRule -> nest i . vcat $ dl
            PPSemiColon   -> nest i . prettyBlock $ dl
            _             -> flatBlock dl

ppBindings :: [Doc] -> Doc
ppBindings dl = do
        e <- fmap layout getPPEnv
        case e of
            PPOffsideRule -> vcat dl
            PPSemiColon   -> vcat . punctuate semi $ dl
            _             -> hsep . punctuate semi $ dl

($$$) :: Doc -> Doc -> Doc
a $$$ b = layoutChoice (a $$) (a <+>) b

mySep :: [Doc] -> Doc
mySep = layoutChoice mySep' hsep
        where
        -- ensure paragraph fills with indentation.
        mySep' [x]    = x
        mySep' (x:xs) = x <+> fsep xs
        mySep' []     = error "Internal error: mySep"

myVcat :: [Doc] -> Doc
myVcat = layoutChoice vcat hsep

myFsepSimple :: [Doc] -> Doc
myFsepSimple = layoutChoice fsep hsep

-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
myFsep :: [Doc] -> Doc
myFsep = layoutChoice fsep' hsep
        where   fsep' [] = empty
                fsep' (d:ds) = do
                        e <- getPPEnv
                        let n = onsideIndent e
                        nest n (fsep (nest (-n) d:ds))

layoutChoice :: (a -> Doc) -> (a -> Doc) -> a -> Doc
layoutChoice a b dl = do e <- getPPEnv
                         if layout e == PPOffsideRule ||
                            layout e == PPSemiColon
                          then a dl else b dl

-- Prefix something with a LINE pragma, if requested.
-- GHC's LINE pragma actually sets the current line number to n-1, so
-- that the following line is line n.  But if there's no newline before
-- the line we're talking about, we need to compensate by adding 1.

markLine :: SrcLoc -> Doc -> Doc
markLine loc doc = do
        e <- getPPEnv
        let y = srcLine loc
        let line l =
              text ("{-# LINE " ++ show l ++ " \"" ++ srcFilename loc ++ "\" #-}")
        if linePragmas e then layoutChoice (line y $$) (line (y+1) <+>) doc
              else doc
