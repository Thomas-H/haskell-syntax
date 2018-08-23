-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.ParseUtils
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for the Haskell parser.
--
-----------------------------------------------------------------------------

module Language.Haskell.ParseUtils (
          splitTyConApp         -- Type -> P (Name,[Type])
        , mkRecConstrOrUpdate   -- Exp -> [FieldUpdate] -> P Exp
        , checkPrec             -- Integer -> P Int
        , checkContext          -- Type -> P Context
        , checkAssertion        -- Type -> P Asst
        , checkDataHeader       -- QualType -> P (Context,Name,[Name])
        , checkClassHeader      -- QualType -> P (Context,Name,[Name])
        , checkInstHeader       -- QualType -> P (Context,QName,[Type])
        , checkPattern          -- Exp -> P Pat
        , checkExpr             -- Exp -> P Exp
        , checkValDef           -- SrcLoc -> Exp -> Rhs -> [Decl] -> P Decl
        , checkClassBody        -- [Decl] -> P [Decl]
        , checkUnQual           -- QName -> P Name
        , checkRevDecls         -- [Decl] -> P [Decl]
 ) where

import           Language.Haskell.ParseMonad
import           Language.Haskell.Pretty
import           Language.Haskell.Syntax

splitTyConApp :: Type -> P (Name,[Type])
splitTyConApp t0 = split t0 []
 where
        split :: Type -> [Type] -> P (Name,[Type])
        split (TyApp t u) ts = split t (u:ts)
        split (TyCon (UnQual t)) ts = return (t,ts)
        split _ _ = fail "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Various Syntactic Checks

checkContext :: Type -> P Context
checkContext (TyTuple ts) =
        mapM checkAssertion ts
checkContext t = do
        c <- checkAssertion t
        return [c]

-- Changed for multi-parameter type classes

checkAssertion :: Type -> P Asst
checkAssertion = checkAssertion' []
        where   checkAssertion' ts (TyCon c) = return (c,ts)
                checkAssertion' ts (TyApp a t) = checkAssertion' (t:ts) a
                checkAssertion' _ _ = fail "Illegal class assertion"


checkDataHeader :: QualType -> P (Context,Name,[Name])
checkDataHeader (QualType cs t) = do
        (c,ts) <- checkSimple "data/newtype" t []
        return (cs,c,ts)

checkClassHeader :: QualType -> P (Context,Name,[Name])
checkClassHeader (QualType cs t) = do
        (c,ts) <- checkSimple "class" t []
        return (cs,c,ts)

checkSimple :: String -> Type -> [Name] -> P ((Name,[Name]))
checkSimple kw (TyApp l (TyVar a)) xs = checkSimple kw l (a:xs)
checkSimple _kw (TyCon (UnQual t))   xs = return (t,xs)
checkSimple kw _ _ = fail ("Illegal " ++ kw ++ " declaration")

checkInstHeader :: QualType -> P (Context,QName,[Type])
checkInstHeader (QualType cs t) = do
        (c,ts) <- checkInsts t []
        return (cs,c,ts)

checkInsts :: Type -> [Type] -> P ((QName,[Type]))
checkInsts (TyApp l t) ts = checkInsts l (t:ts)
checkInsts (TyCon c)   ts = return (c,ts)
checkInsts _ _              = fail "Illegal instance declaration"

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: Exp -> P Pat
checkPattern e = checkPat e []

checkPat :: Exp -> [Pat] -> P Pat
checkPat (Con c) args = return (PApp c args)
checkPat (App f x) args = do
        x' <- checkPat x []
        checkPat f (x':args)
checkPat e [] = case e of
        Var (UnQual x)     -> return (PVar x)
        Lit l              -> return (PLit l)
        InfixApp l op r    -> do
                              l' <- checkPat l []
                              r' <- checkPat r []
                              case op of
                                 QConOp c -> return (PInfixApp l' c r')
                                 _          -> patFail
        Tuple es           -> do
                              ps <- mapM (\e' -> checkPat e' []) es
                              return (PTuple ps)
        List es            -> do
                              ps <- mapM (\e' -> checkPat e' []) es
                              return (PList ps)
        Paren e'           -> do
                              p <- checkPat e' []
                              return (PParen p)
        AsPat n e'         -> do
                              p <- checkPat e' []
                              return (PAsPat n p)
        WildCard           -> return PWildCard
        IrrPat e'          -> do
                              p <- checkPat e' []
                              return (PIrrPat p)
        RecConstr c fs     -> do
                              fs' <- mapM checkPatField fs
                              return (PRec c fs')
        NegApp (Lit l)     -> return (PNeg (PLit l))
        _ -> patFail

checkPat _ _ = patFail

checkPatField :: FieldUpdate -> P PatField
checkPatField (FieldUpdate n e) = do
        p <- checkPat e []
        return (PFieldPat n p)

patFail :: P a
patFail = fail "Parse error in pattern"

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: Exp -> P Exp
checkExpr e = case e of
        Var _                   -> return e
        Con _                   -> return e
        Lit _                   -> return e
        InfixApp e1 op e2       -> check2Exprs e1 e2 (flip InfixApp op)
        App e1 e2               -> check2Exprs e1 e2 App
        NegApp e1               -> check1Expr e1 NegApp
        Lambda loc ps e1        -> check1Expr e1 (Lambda loc ps)
        Let bs e1               -> check1Expr e1 (Let bs)
        If e1 e2 e3             -> check3Exprs e1 e2 e3 If
        Case e1 alts            -> do
                                   alts' <- mapM checkAlt alts
                                   e1' <- checkExpr e1
                                   return (Case e1' alts')
        Do stmts                -> do
                                   stmts' <- mapM checkStmt stmts
                                   return (Do stmts')
        Tuple es                -> checkManyExprs es Tuple
        List es                 -> checkManyExprs es List
        Paren e1                -> check1Expr e1 Paren
        LeftSection e1 op       -> check1Expr e1 (flip LeftSection op)
        RightSection op e1      -> check1Expr e1 (RightSection op)
        RecConstr c fields      -> do
                                   fields' <- mapM checkField fields
                                   return (RecConstr c fields')
        RecUpdate e1 fields     -> do
                                   fields' <- mapM checkField fields
                                   e1' <- checkExpr e1
                                   return (RecUpdate e1' fields')
        EnumFrom e1             -> check1Expr e1 EnumFrom
        EnumFromTo e1 e2        -> check2Exprs e1 e2 EnumFromTo
        EnumFromThen e1 e2      -> check2Exprs e1 e2 EnumFromThen
        EnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 EnumFromThenTo
        ListComp e1 stmts       -> do
                                   stmts' <- mapM checkStmt stmts
                                   e1' <- checkExpr e1
                                   return (ListComp e1' stmts')
        ExpTypeSig loc e1 ty    -> do
                                   e1' <- checkExpr e1
                                   return (ExpTypeSig loc e1' ty)
        _                       -> fail "Parse error in expression"

-- type signature for polymorphic recursion!!
check1Expr :: Exp -> (Exp -> a) -> P a
check1Expr e1 f = do
        e1' <- checkExpr e1
        return (f e1')

check2Exprs :: Exp -> Exp -> (Exp -> Exp -> a) -> P a
check2Exprs e1 e2 f = do
        e1' <- checkExpr e1
        e2' <- checkExpr e2
        return (f e1' e2')

check3Exprs :: Exp -> Exp -> Exp -> (Exp -> Exp -> Exp -> a) -> P a
check3Exprs e1 e2 e3 f = do
        e1' <- checkExpr e1
        e2' <- checkExpr e2
        e3' <- checkExpr e3
        return (f e1' e2' e3')

checkManyExprs :: [Exp] -> ([Exp] -> a) -> P a
checkManyExprs es f = do
        es' <- mapM checkExpr es
        return (f es')

checkAlt :: Alt -> P Alt
checkAlt (Alt loc p galts bs) = do
        galts' <- checkGAlts galts
        return (Alt loc p galts' bs)

checkGAlts :: GuardedAlts -> P GuardedAlts
checkGAlts (UnGuardedAlt e) = check1Expr e UnGuardedAlt
checkGAlts (GuardedAlts galts) = do
        galts' <- mapM checkGAlt galts
        return (GuardedAlts galts')

checkGAlt :: GuardedAlt -> P GuardedAlt
checkGAlt (GuardedAlt loc e1 e2) = check2Exprs e1 e2 (GuardedAlt loc)

checkStmt :: Stmt -> P Stmt
checkStmt (Generator loc p e) = check1Expr e (Generator loc p)
checkStmt (Qualifier e)       = check1Expr e Qualifier
checkStmt s@(LetStmt _)       = return s

checkField :: FieldUpdate -> P FieldUpdate
checkField (FieldUpdate n e) = check1Expr e (FieldUpdate n)

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SrcLoc -> Exp -> Rhs -> [Decl] -> P Decl
checkValDef srcloc lhs rhs whereBinds =
    case isFunLhs lhs [] of
         Just (f,es) -> do
                        ps <- mapM checkPattern es
                        return (FunBind [Match srcloc f ps rhs whereBinds])
         Nothing     -> do
                        lhs' <- checkPattern lhs
                        return (PatBind srcloc lhs' rhs whereBinds)

-- A variable binding is parsed as an PatBind.

isFunLhs :: Exp -> [Exp] -> Maybe (Name, [Exp])
isFunLhs (InfixApp l (QVarOp (UnQual op)) r) es = Just (op, l:r:es)
isFunLhs (App (Var (UnQual f)) e) es            = Just (f, e:es)
isFunLhs (App (Paren f) e) es                   = isFunLhs f (e:es)
isFunLhs (App f e) es                           = isFunLhs f (e:es)
isFunLhs _ _                                    = Nothing

-----------------------------------------------------------------------------
-- In a class or instance body, a pattern binding must be of a variable.

checkClassBody :: [Decl] -> P [Decl]
checkClassBody decls = do
        mapM_ checkMethodDef decls
        return decls

checkMethodDef :: Decl -> P ()
checkMethodDef (PatBind _ (PVar _) _ _) = return ()
checkMethodDef (PatBind loc _ _ _) =
        fail "illegal method definition" `atSrcLoc` loc
checkMethodDef _ = return ()

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: QName -> P Name
checkUnQual (Qual _ _)  = fail "Illegal qualified name"
checkUnQual (UnQual n)  = return n
checkUnQual (Special _) = fail "Illegal special name"

-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = return (fromInteger i)
checkPrec i | otherwise        = fail ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: Exp -> [FieldUpdate] -> P Exp
mkRecConstrOrUpdate (Con c) fs         = return (RecConstr c fs)
mkRecConstrOrUpdate e         fs@(_:_) = return (RecUpdate e fs)
mkRecConstrOrUpdate _         _        = fail "Empty record update"

-----------------------------------------------------------------------------
-- Reverse a list of declarations, merging adjacent FunBinds of the
-- same name and checking that their arities match.

checkRevDecls :: [Decl] -> P [Decl]
checkRevDecls = mergeFunBinds []
    where
        mergeFunBinds revDs [] = return revDs
        mergeFunBinds revDs (FunBind ms1@(Match _ name ps _ _:_):ds1) =
                mergeMatches ms1 ds1
            where
                arity = length ps
                mergeMatches ms' (FunBind ms@(Match loc name' ps' _ _:_):ds)
                    | name' == name =
                        if length ps' /= arity
                        then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                             `atSrcLoc` loc
                        else mergeMatches (ms++ms') ds
                mergeMatches ms' ds = mergeFunBinds (FunBind ms':revDs) ds
        mergeFunBinds revDs (d:ds) = mergeFunBinds (d:revDs) ds
