> {
> -----------------------------------------------------------------------------
> -- |
> -- Module      :  Language.Haskell.Parser
> -- Copyright   :  (c) Simon Marlow, Sven Panne 1997-2000
> -- License     :  BSD-style (see the file libraries/base/LICENSE)
> --
> -- Maintainer  :  libraries@haskell.org
> -- Stability   :  experimental
> -- Portability :  portable
> --
> -- Haskell parser.
> --
> -----------------------------------------------------------------------------
>
> module Language.Haskell.Parser (
>               parseModule, parseModuleWithMode,
>               ParseMode(..), defaultParseMode, ParseResult(..)) where
>
> import Language.Haskell.Syntax
> import Language.Haskell.ParseMonad
> import Language.Haskell.Lexer
> import Language.Haskell.ParseUtils
> }

ToDo: Check exactly which names must be qualified with Prelude (commas and friends)
ToDo: Inst (MPCs?)
ToDo: Polish constr a bit
ToDo: Ugly: exp0b is used for lhs, pat, exp0, ...
ToDo: Differentiate between record updates and labeled construction.

-----------------------------------------------------------------------------
Conflicts: 2 shift/reduce

2 for ambiguity in 'case x of y | let z = y in z :: Bool -> b'
        (don't know whether to reduce 'Bool' as a btype or shift the '->'.
         Similarly lambda and if.  The default resolution in favour of the
         shift means that a guard can never end with a type signature.
         In mitigation: it's a rare case and no Haskell implementation
         allows these, because it would require unbounded lookahead.)
        There are 2 conflicts rather than one because contexts are parsed
        as btypes (cf ctype).

-----------------------------------------------------------------------------

> %token
>       VARID    { VarId $$ }
>       QVARID   { QVarId $$ }
>       CONID    { ConId $$ }
>       QCONID   { QConId $$ }
>       VARSYM   { VarSym $$ }
>       CONSYM   { ConSym $$ }
>       QVARSYM  { QVarSym $$ }
>       QCONSYM  { QConSym $$ }
>       INT      { IntTok $$ }
>       RATIONAL { FloatTok $$ }
>       CHAR     { Character $$ }
>       STRING   { StringTok $$ }

Symbols

>       '('     { LeftParen }
>       ')'     { RightParen }
>       ';'     { SemiColon }
>       '{'     { LeftCurly }
>       '}'     { RightCurly }
>       vccurly { VRightCurly }                 -- a virtual close brace
>       '['     { LeftSquare }
>       ']'     { RightSquare }
>       ','     { Comma }
>       '_'     { Underscore }
>       '`'     { BackQuote }

Reserved operators

>       '..'    { DotDot }
>       ':'     { Colon }
>       '::'    { DoubleColon }
>       '='     { Equals }
>       '\\'    { Backslash }
>       '|'     { Bar }
>       '<-'    { LeftArrow }
>       '->'    { RightArrow }
>       '@'     { At }
>       '~'     { Tilde }
>       '=>'    { DoubleArrow }
>       '-'     { Minus }
>       '!'     { Exclamation }

Reserved Ids

>       'case'          { KW_Case }
>       'class'         { KW_Class }
>       'data'          { KW_Data }
>       'default'       { KW_Default }
>       'deriving'      { KW_Deriving }
>       'do'            { KW_Do }
>       'else'          { KW_Else }
>       'foreign'       { KW_Foreign }
>       'if'            { KW_If }
>       'import'        { KW_Import }
>       'in'            { KW_In }
>       'infix'         { KW_Infix }
>       'infixl'        { KW_InfixL }
>       'infixr'        { KW_InfixR }
>       'instance'      { KW_Instance }
>       'let'           { KW_Let }
>       'module'        { KW_Module }
>       'newtype'       { KW_NewType }
>       'of'            { KW_Of }
>       'then'          { KW_Then }
>       'type'          { KW_Type }
>       'where'         { KW_Where }

Special Ids

>       'as'            { KW_As }
>       'export'        { KW_Export }
>       'hiding'        { KW_Hiding }
>       'qualified'     { KW_Qualified }
>       'safe'          { KW_Safe }
>       'unsafe'        { KW_Unsafe }

> %monad { P }
> %lexer { lexer } { EOF }
> %name parse
> %tokentype { Token }
> %%

-----------------------------------------------------------------------------
Module Header

> module :: { Module }
>       : srcloc 'module' modid maybeexports 'where' body
>               { Module $1 $3 $4 (fst $6) (snd $6) }
>       | srcloc body
>               { Module $1 main_mod (Just [EVar (UnQual main_name)])
>                                                       (fst $2) (snd $2) }

> body :: { ([ImportDecl],[Decl]) }
>       : '{'  bodyaux '}'                      { $2 }
>       | open bodyaux close                    { $2 }

> bodyaux :: { ([ImportDecl],[Decl]) }
>       : optsemis impdecls semis topdecls      { (reverse $2, $4) }
>       | optsemis                topdecls      { ([], $2) }
>       | optsemis impdecls optsemis            { (reverse $2, []) }
>       | optsemis                              { ([], []) }

> semis :: { () }
>       : optsemis ';'                          { () }

> optsemis :: { () }
>       : semis                                 { () }
>       | {- empty -}                           { () }

-----------------------------------------------------------------------------
The Export List

> maybeexports :: { Maybe [ExportSpec] }
>       :  exports                              { Just $1 }
>       |  {- empty -}                          { Nothing }

> exports :: { [ExportSpec] }
>       : '(' exportlist optcomma ')'           { reverse $2 }
>       | '(' optcomma ')'                      { [] }

> optcomma :: { () }
>       : ','                                   { () }
>       | {- empty -}                           { () }

> exportlist :: { [ExportSpec] }
>       :  exportlist ',' export                { $3 : $1 }
>       |  export                               { [$1]  }

> export :: { ExportSpec }
>       :  qvar                                 { EVar $1 }
>       |  qtyconorcls                          { EAbs $1 }
>       |  qtyconorcls '(' '..' ')'             { EThingAll $1 }
>       |  qtyconorcls '(' ')'                  { EThingWith $1 [] }
>       |  qtyconorcls '(' cnames ')'           { EThingWith $1 (reverse $3) }
>       |  'module' modid                       { EModuleContents $2 }

-----------------------------------------------------------------------------
Import Declarations

> impdecls :: { [ImportDecl] }
>       : impdecls semis impdecl                { $3 : $1 }
>       | impdecl                               { [$1] }

> impdecl :: { ImportDecl }
>       : srcloc 'import' optqualified modid maybeas maybeimpspec
>                               { ImportDecl $1 $4 $3 $5 $6 }

> optqualified :: { Bool }
>       : 'qualified'                           { True  }
>       | {- empty -}                           { False }

> maybeas :: { Maybe ModuleName }
>       : 'as' modid                            { Just $2 }
>       | {- empty -}                           { Nothing }


> maybeimpspec :: { Maybe (Bool, [ImportSpec]) }
>       : impspec                               { Just $1 }
>       | {- empty -}                           { Nothing }

> impspec :: { (Bool, [ImportSpec]) }
>       : opthiding '(' importlist optcomma ')' { ($1, reverse $3) }
>       | opthiding '(' optcomma ')'            { ($1, []) }

> opthiding :: { Bool }
>       : 'hiding'                              { True }
>       | {- empty -}                           { False }

> importlist :: { [ImportSpec] }
>       :  importlist ',' importspec            { $3 : $1 }
>       |  importspec                           { [$1]  }

> importspec :: { ImportSpec }
>       :  var                                  { IVar $1 }
>       |  tyconorcls                           { IAbs $1 }
>       |  tyconorcls '(' '..' ')'              { IThingAll $1 }
>       |  tyconorcls '(' ')'                   { IThingWith $1 [] }
>       |  tyconorcls '(' cnames ')'            { IThingWith $1 (reverse $3) }

> cnames :: { [CName] }
>       :  cnames ',' cname                     { $3 : $1 }
>       |  cname                                { [$1]  }

> cname :: { CName }
>       :  var                                  { VarName $1 }
>       |  con                                  { ConName $1 }

-----------------------------------------------------------------------------
Fixity Declarations

> fixdecl :: { Decl }
>       : srcloc infix prec ops                 { InfixDecl $1 $2 $3 (reverse $4) }

> prec :: { Int }
>       : {- empty -}                           { 9 }
>       | INT                                   {% checkPrec $1 }

> infix :: { Assoc }
>       : 'infix'                               { AssocNone  }
>       | 'infixl'                              { AssocLeft  }
>       | 'infixr'                              { AssocRight }

> ops   :: { [Op] }
>       : ops ',' op                            { $3 : $1 }
>       | op                                    { [$1] }

-----------------------------------------------------------------------------
Top-Level Declarations

Note: The report allows topdecls to be empty. This would result in another
shift/reduce-conflict, so we don't handle this case here, but in bodyaux.

> topdecls :: { [Decl] }
>       : topdecls1 optsemis            {% checkRevDecls $1 }

> topdecls1 :: { [Decl] }
>       : topdecls1 semis topdecl       { $3 : $1 }
>       | topdecl                       { [$1] }

> topdecl :: { Decl }
>       : srcloc 'type' simpletype '=' type
>                       { TypeDecl $1 (fst $3) (snd $3) $5 }
>       | srcloc 'data' ctype '=' constrs deriving
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               return (DataDecl $1 cs c t (reverse $5) $6) } }
>       | srcloc 'newtype' ctype '=' constr deriving
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               return (NewTypeDecl $1 cs c t $5 $6) } }
>       | srcloc 'class' ctype optcbody
>                       {% do { (cs,c,vs) <- checkClassHeader $3;
>                               return (ClassDecl $1 cs c vs $4) } }
>       | srcloc 'instance' ctype optvaldefs
>                       {% do { (cs,c,ts) <- checkInstHeader $3;
>                               return (InstDecl $1 cs c ts $4) } }
>       | srcloc 'default' '(' typelist ')'
>                       { DefaultDecl $1 $4 }
>       | foreigndecl   { $1 }
>       | decl          { $1 }

> typelist :: { [Type] }
>       : types                         { reverse $1 }
>       | type                          { [$1] }
>       | {- empty -}                   { [] }

> decls :: { [Decl] }
>       : optsemis decls1 optsemis      {% checkRevDecls $2 }
>       | optsemis                      { [] }

> decls1 :: { [Decl] }
>       : decls1 semis decl             { $3 : $1 }
>       | decl                          { [$1] }

> decl :: { Decl }
>       : signdecl                      { $1 }
>       | fixdecl                       { $1 }
>       | valdef                        { $1 }

> decllist :: { [Decl] }
>       : '{'  decls '}'                { $2 }
>       | open decls close              { $2 }

> signdecl :: { Decl }
>       : srcloc vars '::' ctype        { TypeSig $1 (reverse $2) $4 }

ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
instead of qvar, we get another shift/reduce-conflict. Consider the
following programs:

   { (+) :: ... }          only var
   { (+) x y  = ... }      could (incorrectly) be qvar

We re-use expressions for patterns, so a qvar would be allowed in patterns
instead of a var only (which would be correct). But deciding what the + is,
would require more lookahead. So let's check for ourselves...

> vars  :: { [Name] }
>       : vars ',' var                  { $3 : $1 }
>       | qvar                          {% do { n <- checkUnQual $1;
>                                               return [n] } }

Foreign declarations
- calling conventions are uninterpreted
- external entities are not parsed
- special ids are not allowed as internal names

> foreigndecl :: { Decl }
>       : srcloc 'foreign' 'import' VARID optsafety optentity fvar '::' type
>                       { ForeignImport $1 $4 $5 $6 $7 $9 }
>       | srcloc 'foreign' 'export' VARID optentity fvar '::' type
>                       { ForeignExport $1 $4 $5 $6 $8 }

> optsafety :: { Safety }
>       : 'safe'                        { Safe }
>       | 'unsafe'                      { Unsafe }
>       | {- empty -}                   { Safe }

> optentity :: { String }
>       : STRING                        { $1 }
>       | {- empty -}                   { "" }

> fvar :: { Name }
>       : VARID                         { Ident $1 }
>       | '(' varsym ')'                { $2 }

-----------------------------------------------------------------------------
Types

> type :: { Type }
>       : btype '->' type               { TyFun $1 $3 }
>       | btype                         { $1 }

> btype :: { Type }
>       : btype atype                   { TyApp $1 $2 }
>       | atype                         { $1 }

> atype :: { Type }
>       : gtycon                        { TyCon $1 }
>       | tyvar                         { TyVar $1 }
>       | '(' types ')'                 { TyTuple (reverse $2) }
>       | '[' type ']'                  { TyApp list_tycon $2 }
>       | '(' type ')'                  { $2 }

> gtycon :: { QName }
>       : qconid                        { $1 }
>       | '(' ')'                       { unit_tycon_name }
>       | '(' '->' ')'                  { fun_tycon_name }
>       | '[' ']'                       { list_tycon_name }
>       | '(' commas ')'                { tuple_tycon_name $2 }


(Slightly edited) Comment from GHC's hsparser.y:
"context => type" vs  "type" is a problem, because you can't distinguish between

        foo :: (Baz a, Baz a)
        bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

with one token of lookahead.  The HACK is to parse the context as a btype
(more specifically as a tuple type), then check that it has the right form
C a, or (C1 a, C2 b, ... Cn z) and convert it into a context.  Blaach!

> ctype :: { QualType }
>       : context '=>' type             { QualType $1 $3 }
>       | type                          { QualType [] $1 }

> context :: { Context }
>       : btype                         {% checkContext $1 }

> types :: { [Type] }
>       : types ',' type                { $3 : $1 }
>       | type  ',' type                { [$3, $1] }

> simpletype :: { (Name, [Name]) }
>       : tycon tyvars                  { ($1,reverse $2) }

> tyvars :: { [Name] }
>       : tyvars tyvar                  { $2 : $1 }
>       | {- empty -}                   { [] }

-----------------------------------------------------------------------------
Datatype declarations

> constrs :: { [ConDecl] }
>       : constrs '|' constr            { $3 : $1 }
>       | constr                        { [$1] }

> constr :: { ConDecl }
>       : srcloc scontype               { ConDecl $1 (fst $2) (snd $2) }
>       | srcloc sbtype conop sbtype    { ConDecl $1 $3 [$2,$4] }
>       | srcloc con '{' '}'            { RecDecl $1 $2 [] }
>       | srcloc con '{' fielddecls '}' { RecDecl $1 $2 (reverse $4) }

> scontype :: { (Name, [BangType]) }
>       : btype                         {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c,map UnBangedTy ts) } }
>       | scontype1                     { $1 }

> scontype1 :: { (Name, [BangType]) }
>       : btype '!' atype               {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c,map UnBangedTy ts++
>                                                       [BangedTy $3]) } }
>       | scontype1 satype              { (fst $1, snd $1 ++ [$2] ) }

> satype :: { BangType }
>       : atype                         { UnBangedTy $1 }
>       | '!' atype                     { BangedTy   $2 }

> sbtype :: { BangType }
>       : btype                         { UnBangedTy $1 }
>       | '!' atype                     { BangedTy   $2 }

> fielddecls :: { [([Name],BangType)] }
>       : fielddecls ',' fielddecl      { $3 : $1 }
>       | fielddecl                     { [$1] }

> fielddecl :: { ([Name],BangType) }
>       : vars '::' stype               { (reverse $1, $3) }

> stype :: { BangType }
>       : type                          { UnBangedTy $1 }
>       | '!' atype                     { BangedTy   $2 }

> deriving :: { [QName] }
>       : {- empty -}                   { [] }
>       | 'deriving' qtycls             { [$2] }
>       | 'deriving' '('          ')'   { [] }
>       | 'deriving' '(' dclasses ')'   { reverse $3 }

> dclasses :: { [QName] }
>       : dclasses ',' qtycls           { $3 : $1 }
>       | qtycls                        { [$1] }

-----------------------------------------------------------------------------
Class declarations

> optcbody :: { [Decl] }
>       : 'where' decllist              {% checkClassBody $2 }
>       | {- empty -}                   { [] }

-----------------------------------------------------------------------------
Instance declarations

> optvaldefs :: { [Decl] }
>       : 'where' '{'  valdefs '}'      {% checkClassBody $3 }
>       | 'where' open valdefs close    {% checkClassBody $3 }
>       | {- empty -}                   { [] }

> valdefs :: { [Decl] }
>       : optsemis valdefs1 optsemis    {% checkRevDecls $2 }
>       | optsemis                      { [] }

> valdefs1 :: { [Decl] }
>       : valdefs1 semis valdef         { $3 : $1 }
>       | valdef                        { [$1] }

-----------------------------------------------------------------------------
Value definitions

> valdef :: { Decl }
>       : srcloc exp0b rhs optwhere     {% checkValDef $1 $2 $3 $4 }

> optwhere :: { [Decl] }
>       : 'where' decllist              { $2 }
>       | {- empty -}                   { [] }

> rhs   :: { Rhs }
>       : '=' exp                       {% do { e <- checkExpr $2;
>                                               return (UnGuardedRhs e) } }
>       | gdrhs                         { GuardedRhss  (reverse $1) }

> gdrhs :: { [GuardedRhs] }
>       : gdrhs gdrh                    { $2 : $1 }
>       | gdrh                          { [$1] }

> gdrh :: { GuardedRhs }
>       : srcloc '|' exp0 '=' exp       {% do { g <- checkExpr $3;
>                                               e <- checkExpr $5;
>                                               return (GuardedRhs $1 g e) } }

-----------------------------------------------------------------------------
Expressions

Note: The Report specifies a meta-rule for lambda, let and if expressions
(the exp's that end with a subordinate exp): they extend as far to
the right as possible.  That means they cannot be followed by a type
signature or infix application.  To implement this without shift/reduce
conflicts, we split exp10 into these expressions (exp10a) and the others
(exp10b).  That also means that only an exp0 ending in an exp10b (an exp0b)
can followed by a type signature or infix application.  So we duplicate
the exp0 productions to distinguish these from the others (exp0a).

> exp   :: { Exp }
>       : exp0b '::' srcloc ctype       { ExpTypeSig $3 $1 $4 }
>       | exp0                          { $1 }

> exp0 :: { Exp }
>       : exp0a                         { $1 }
>       | exp0b                         { $1 }

> exp0a :: { Exp }
>       : exp0b qop exp10a              { InfixApp $1 $2 $3 }
>       | exp10a                        { $1 }

> exp0b :: { Exp }
>       : exp0b qop exp10b              { InfixApp $1 $2 $3 }
>       | exp10b                        { $1 }

> exp10a :: { Exp }
>       : '\\' srcloc apats '->' exp    { Lambda $2 (reverse $3) $5 }
>       | 'let' decllist 'in' exp       { Let $2 $4 }
>       | 'if' exp 'then' exp 'else' exp { If $2 $4 $6 }

> exp10b :: { Exp }
>       : 'case' exp 'of' altslist      { Case $2 $4 }
>       | '-' fexp                      { NegApp $2 }
>       | 'do' stmtlist                 { Do $2 }
>       | fexp                          { $1 }

> fexp :: { Exp }
>       : fexp aexp                     { App $1 $2 }
>       | aexp                          { $1 }

> apats :: { [Pat] }
>       : apats apat                    { $2 : $1 }
>       | apat                          { [$1] }

> apat :: { Pat }
>       : aexp                          {% checkPattern $1 }

UGLY: Because patterns and expressions are mixed, aexp has to be split into
two rules: One right-recursive and one left-recursive. Otherwise we get two
reduce/reduce-errors (for as-patterns and irrefutable patters).

Even though the variable in an as-pattern cannot be qualified, we use
qvar here to avoid a shift/reduce conflict, and then check it ourselves
(as for vars above).

> aexp  :: { Exp }
>       : qvar '@' aexp                 {% do { n <- checkUnQual $1;
>                                               return (AsPat n $3) } }
>       | '~' aexp                      { IrrPat $2 }
>       | aexp1                         { $1 }

Note: The first two alternatives of aexp1 are not necessarily record
updates: they could be labeled constructions.

> aexp1 :: { Exp }
>       : aexp1 '{' '}'                 {% mkRecConstrOrUpdate $1 [] }
>       | aexp1 '{' fbinds '}'          {% mkRecConstrOrUpdate $1 (reverse $3) }
>       | aexp2                         { $1 }

According to the Report, the left section (e op) is legal iff (e op x)
parses equivalently to ((e) op x).  Thus e must be an exp0b.

> aexp2 :: { Exp }
>       : qvar                          { Var $1 }
>       | gcon                          { $1 }
>       | literal                       { Lit $1 }
>       | '(' exp ')'                   { Paren $2 }
>       | '(' texps ')'                 { Tuple (reverse $2) }
>       | '[' list ']'                  { $2 }
>       | '(' exp0b qop ')'             { LeftSection $2 $3  }
>       | '(' qopm exp0 ')'             { RightSection $2 $3 }
>       | '_'                           { WildCard }

> commas :: { Int }
>       : commas ','                    { $1 + 1 }
>       | ','                           { 1 }

> texps :: { [Exp] }
>       : texps ',' exp                 { $3 : $1 }
>       | exp ',' exp                   { [$3,$1] }

-----------------------------------------------------------------------------
List expressions

The rules below are little bit contorted to keep lexps left-recursive while
avoiding another shift/reduce-conflict.

> list :: { Exp }
>       : exp                           { List [$1] }
>       | lexps                         { List (reverse $1) }
>       | exp '..'                      { EnumFrom $1 }
>       | exp ',' exp '..'              { EnumFromThen $1 $3 }
>       | exp '..' exp                  { EnumFromTo $1 $3 }
>       | exp ',' exp '..' exp          { EnumFromThenTo $1 $3 $5 }
>       | exp '|' quals                 { ListComp $1 (reverse $3) }

> lexps :: { [Exp] }
>       : lexps ',' exp                 { $3 : $1 }
>       | exp ',' exp                   { [$3,$1] }

-----------------------------------------------------------------------------
List comprehensions

> quals :: { [Stmt] }
>       : quals ',' qual                { $3 : $1 }
>       | qual                          { [$1] }

> qual  :: { Stmt }
>       : pat srcloc '<-' exp           { Generator $2 $1 $4 }
>       | exp                           { Qualifier $1 }
>       | 'let' decllist                { LetStmt $2 }

-----------------------------------------------------------------------------
Case alternatives

> altslist :: { [Alt] }
>       : '{'  alts '}'                 { $2 }
>       | open alts close               { $2 }

> alts :: { [Alt] }
>       : optsemis alts1 optsemis       { reverse $2 }

> alts1 :: { [Alt] }
>       : alts1 semis alt               { $3 : $1 }
>       | alt                           { [$1] }

> alt :: { Alt }
>       : srcloc pat ralt optwhere      { Alt $1 $2 $3 $4 }

> ralt :: { GuardedAlts }
>       : '->' exp                      { UnGuardedAlt $2 }
>       | gdpats                        { GuardedAlts (reverse $1) }

> gdpats :: { [GuardedAlt] }
>       : gdpats gdpat                  { $2 : $1 }
>       | gdpat                         { [$1] }

> gdpat :: { GuardedAlt }
>       : srcloc '|' exp0 '->' exp      { GuardedAlt $1 $3 $5 }

> pat :: { Pat }
>       : exp0b                         {% checkPattern $1 }

-----------------------------------------------------------------------------
Statement sequences

As per the Report, but with stmt expanded to simplify building the list
without introducing conflicts.  This also ensures that the last stmt is
an expression.

> stmtlist :: { [Stmt] }
>       : '{'  stmts '}'                { $2 }
>       | open stmts close              { $2 }

> stmts :: { [Stmt] }
>       : 'let' decllist ';' stmts      { LetStmt $2 : $4 }
>       | pat srcloc '<-' exp ';' stmts { Generator $2 $1 $4 : $6 }
>       | exp ';' stmts                 { Qualifier $1 : $3 }
>       | ';' stmts                     { $2 }
>       | exp ';'                       { [Qualifier $1] }
>       | exp                           { [Qualifier $1] }

-----------------------------------------------------------------------------
Record Field Update/Construction

> fbinds :: { [FieldUpdate] }
>       : fbinds ',' fbind              { $3 : $1 }
>       | fbind                         { [$1] }

> fbind :: { FieldUpdate }
>       : qvar '=' exp                  { FieldUpdate $1 $3 }

-----------------------------------------------------------------------------
Variables, Constructors and Operators.

> gcon :: { Exp }
>       : '(' ')'               { unit_con }
>       | '[' ']'               { List [] }
>       | '(' commas ')'        { tuple_con $2 }
>       | qcon                  { Con $1 }

> var   :: { Name }
>       : varid                 { $1 }
>       | '(' varsym ')'        { $2 }

> qvar  :: { QName }
>       : qvarid                { $1 }
>       | '(' qvarsym ')'       { $2 }

> con   :: { Name }
>       : conid                 { $1 }
>       | '(' consym ')'        { $2 }

> qcon  :: { QName }
>       : qconid                { $1 }
>       | '(' gconsym ')'       { $2 }

> varop :: { Name }
>       : varsym                { $1 }
>       | '`' varid '`'         { $2 }

> qvarop :: { QName }
>       : qvarsym               { $1 }
>       | '`' qvarid '`'        { $2 }

> qvaropm :: { QName }
>       : qvarsymm              { $1 }
>       | '`' qvarid '`'        { $2 }

> conop :: { Name }
>       : consym                { $1 }
>       | '`' conid '`'         { $2 }

> qconop :: { QName }
>       : gconsym               { $1 }
>       | '`' qconid '`'        { $2 }

> op    :: { Op }
>       : varop                 { VarOp $1 }
>       | conop                 { ConOp $1 }

> qop   :: { QOp }
>       : qvarop                { QVarOp $1 }
>       | qconop                { QConOp $1 }

> qopm  :: { QOp }
>       : qvaropm               { QVarOp $1 }
>       | qconop                { QConOp $1 }

> gconsym :: { QName }
>       : ':'                   { list_cons_name }
>       | qconsym               { $1 }

-----------------------------------------------------------------------------
Identifiers and Symbols

> qvarid :: { QName }
>       : varid                 { UnQual $1 }
>       | QVARID                { Qual (ModuleName (fst $1)) (Ident (snd $1)) }

> varid :: { Name }
>       : VARID                 { Ident $1 }
>       | 'as'                  { Ident "as" }
>       | 'export'              { Ident "export" }
>       | 'hiding'              { Ident "hiding" }
>       | 'qualified'           { Ident "qualified" }
>       | 'safe'                { Ident "safe" }
>       | 'unsafe'              { Ident "unsafe" }

> qconid :: { QName }
>       : conid                 { UnQual $1 }
>       | QCONID                { Qual (ModuleName (fst $1)) (Ident (snd $1)) }

> conid :: { Name }
>       : CONID                 { Ident $1 }

> qconsym :: { QName }
>       : consym                { UnQual $1 }
>       | QCONSYM               { Qual (ModuleName (fst $1)) (Symbol (snd $1)) }

> consym :: { Name }
>       : CONSYM                { Symbol $1 }

> qvarsym :: { QName }
>       : varsym                { UnQual $1 }
>       | qvarsym1              { $1 }

> qvarsymm :: { QName }
>       : varsymm               { UnQual $1 }
>       | qvarsym1              { $1 }

> varsym :: { Name }
>       : VARSYM                { Symbol $1 }
>       | '-'                   { Symbol "-" }
>       | '!'                   { Symbol "!" }

> varsymm :: { Name } -- varsym not including '-'
>       : VARSYM                { Symbol $1 }
>       | '!'                   { Symbol "!" }

> qvarsym1 :: { QName }
>       : QVARSYM               { Qual (ModuleName (fst $1)) (Symbol (snd $1)) }

> literal :: { Literal }
>       : INT                   { Int $1 }
>       | CHAR                  { Char $1 }
>       | RATIONAL              { Frac $1 }
>       | STRING                { String $1 }

> srcloc :: { SrcLoc }  :       {% getSrcLoc }

-----------------------------------------------------------------------------
Layout

> open  :: { () }       :       {% pushCurrentContext }

> close :: { () }
>       : vccurly               { () } -- context popped in lexer.
>       | error                 {% popContext }

-----------------------------------------------------------------------------
Miscellaneous (mostly renamings)

> modid :: { ModuleName }
>       : CONID                 { ModuleName $1 }
>       | QCONID                { ModuleName (fst $1 ++ '.':snd $1) }

> tyconorcls :: { Name }
>       : conid                 { $1 }

> tycon :: { Name }
>       : conid                 { $1 }

> qtyconorcls :: { QName }
>       : qconid                { $1 }

> qtycls :: { QName }
>       : qconid                { $1 }

> tyvar :: { Name }
>       : varid                 { $1 }

-----------------------------------------------------------------------------

> {
> happyError :: P a
> happyError = fail "Parse error"

> -- | Parse of a string, which should contain a complete Haskell 98 module.
> parseModule :: String -> ParseResult Module
> parseModule = runParser parse

> -- | Parse of a string, which should contain a complete Haskell 98 module.
> parseModuleWithMode :: ParseMode -> String -> ParseResult Module
> parseModuleWithMode mode = runParserWithMode mode parse
> }
