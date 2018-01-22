{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax where

import Data.List(intercalate)
import Control.Monad(forM_)
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Parser as P

data MEnv = MEnv 
  { menvGenerator :: Int
  , menvUserFun   :: (Map.Map String Int)
  , menvVarTable   :: (Set.Set String)
  }
newtype M a = M { runM :: S.State MEnv a } deriving (Functor, Applicative, Monad, S.MonadState MEnv)



data Func = Func String [String] Expr


-- todo: check built-in syntax

builtinFuns = 
  [ ("add",2)
  , ("sub",2)
  , ("mul",2)
  , ("div",2)
  , ("mod",2)
  , ("neg",1)
  , ("and",2)
  , ("or",2)
  , ("xor",2)
  , ("not",1)
  , ("eq",2)
  , ("neq",2)
  , ("lt",2)
  , ("gt",2)
  , ("le",2)
  , ("ge",2)
  , ("putint",1)
  , ("putc",1)
  , ("newline",0)
  ]

emptyMEnv = MEnv 0 (Map.fromList builtinFuns) (Set.empty)




data Expr 
  = ExprCompound [Expr] 
  | ExprInt Int
  | ExprVar String
  | ExprApply String [Expr]
  | ExprAssign String Expr
  | ExprBind String Expr Expr
  | ExprWhile Expr Expr
  | ExprBranch Expr Expr Expr
  deriving (Show)

genVar :: M String
genVar = do
  menv <- S.get
  let i = menvGenerator menv
  S.put $ menv{ menvGenerator = i + 1 }
  return ("_g" ++ show i)

localVar :: [String] -> M a -> M a
localVar names m = do
  menv <- S.get
  let varmap = menvVarTable menv
  S.put $ menv{ menvVarTable = (Set.union (Set.fromList names) varmap) }
  r <- m
  S.put $ menv
  return r

checkVar :: String -> M a -> M a
checkVar name m = do
  v <- menvVarTable <$> S.get
  if Set.member name v then m else error ("unknown variable: "++name)

addFun :: String -> Int -> M ()
addFun name ary = do
  menv <- S.get
  let fnmap = menvUserFun menv
  if Map.member name fnmap
    then error ("redefinition of function: " ++ name)
    else S.put $ menv{ menvUserFun = Map.insert name ary fnmap }

checkFun :: String -> Int -> M a -> M a
checkFun name ary m = do
  menv <- S.get
  case Map.lookup name (menvUserFun menv) of
    Just i
      | i == ary  -> m
      | otherwise -> error "arity error"
    Nothing       -> error "undefiend function"


mangleFun f = "m_" ++ mangleName f
mangleName xs = concatMap conv xs 
 where
  conv '_' = "_u_"
  conv '.' = "_d_"
  conv '+' = "_p_"
  conv '-' = "_m_"
  conv '*' = "_a_"
  conv '/' = "_s_"
  conv '%' = "_p_"
  conv '=' = "_e_"
  conv '>' = "_g_"
  conv '<' = "_l_"
  conv '!' = "_E_"
  conv '?' = "_q_"
  conv x = [x]

showPrototypes :: M [String]
showPrototypes = do
  menv <- S.get
  return $ map (uncurry proto) $ Map.toList (menvUserFun menv)
 where
  proto name arity = "int " ++ mangleFun name ++ "(" ++ intercalate "," (take arity $ repeat "int") ++ ");"


compileExpr :: String -> Expr -> M String
compileExpr out (ExprCompound xs) = do
  name <- genVar
  ss <- mapM (compileExpr name) xs
  return $ "{ int " ++ name ++ ";\n" ++ concat ss ++ "\n" ++ out ++ "=" ++ name ++ ";\n}\n"
compileExpr out (ExprInt i) = do
  return $ out ++ "=" ++ show i ++ ";\n"
compileExpr out (ExprVar s) = do
  checkVar s $ do
    return $ out ++ "=" ++ s ++ ";\n"
compileExpr out (ExprApply fn args) = do
  checkFun fn (length args) $ do
    names <- mapM (\ _ -> genVar) args
    let decls = concatMap (\ v -> "int " ++ v ++ ";\n") names
    binds <- mapM (\ (s, x) -> compileExpr s x) (zip names args)
    let call = out ++ "=" ++ mangleFun fn ++ "(" ++ intercalate "," names ++ ");\n"
    return $ "{\n" ++ decls ++ concat binds ++ call ++ "}\n"
compileExpr out (ExprAssign s x) = do
  ev <- compileExpr s x
  return $ ev ++ out ++ "=" ++ s ++ ";\n"
compileExpr out (ExprBind s x body) = do
  localVar [s] $ do
    let decl = "int " ++ s ++ ";\n"
    ev <- compileExpr s x
    b <- compileExpr out body
    return $ "{\n" ++ decl ++ ev ++ b ++ "}"
compileExpr out (ExprWhile x body) = do
  name <- genVar
  pred <- compileExpr name x 
  let decl = "int " ++ name ++ ";\n"
  ev <- compileExpr out body
  return $ "for (;;) {" ++ decl ++ pred ++ "if (" ++ name ++ ") {\n" ++ ev ++ "} else { break; }\n}\n"
compileExpr out (ExprBranch p tc ec) = do
  name <- genVar
  pred <- compileExpr name p
  tcev <- compileExpr out tc
  ecev <- compileExpr out ec
  return $ "{ int " ++ name ++ ";\n" ++ pred ++ "if (" ++ name ++ ") {\n" ++ tcev ++ "} else {\n" ++ ecev ++ "}\n}\n"


compileFun (Func name params body) = do
  let paramBinds = intercalate "," $ map (\ s -> "int " ++ s) params
  let decl = "int _out;\n"
  localVar params $ do
    ev <- compileExpr "_out" body
    return $ "int " ++ mangleFun name ++ "(" ++ paramBinds ++ ")\n{\n" ++ decl ++ ev ++ "return _out;\n}\n"





constructGlobalFunTable :: [Func] -> M ()
constructGlobalFunTable xs = forM_ xs $ \ (Func name params _) -> do
  addFun name (length params)





makeFuncSyntax :: P.Expr -> Func
makeFuncSyntax (P.ExprList (P.ExprAtom (P.AtomSym "defproc") : P.ExprAtom (P.AtomSym name) : P.ExprList params : body) ) = Func name paramsSyntax bodySyntax
 where
  paramsSyntax = makeParamSyntax params
  bodySyntax = ExprCompound $ map makeExprSyntax body
makeFuncSyntax _  = error "syntax error"

--makeParamSyntax (P.ExprList [P.ExprAtom (P.AtomSym s), P.ExprAtom (P.AtomSym "int")] : xs) = (s, TyInt) : makeParamSyntax xs
makeParamSyntax (P.ExprAtom (P.AtomSym s) : xs) = s : makeParamSyntax xs
makeParamSyntax [] = []
makeParamSyntax _ = error "syntax error"


makeExprSyntax :: P.Expr -> Expr
makeExprSyntax (P.ExprAtom (P.AtomInt i)) = ExprInt i
makeExprSyntax (P.ExprAtom (P.AtomSym s)) = ExprVar s
makeExprSyntax (P.ExprAtom _) = error "not yet implemented"
makeExprSyntax (P.ExprList (P.ExprAtom (P.AtomSym name) : args)) = makeAppExprSyntax name args
makeExprSyntax (P.ExprList _) = error "syntax error"

makeAppExprSyntax "begin" args = ExprCompound (map makeExprSyntax args)
makeAppExprSyntax "set!" args = case args of
  [P.ExprAtom (P.AtomSym s), x] -> ExprAssign s (makeExprSyntax x)
  _                             -> error "syntax error"
makeAppExprSyntax "if" args = case args of
  [pred, tc, ec] -> ExprBranch (makeExprSyntax pred) (makeExprSyntax tc) (makeExprSyntax ec)
  _              -> error "syntax error"
makeAppExprSyntax "while" args = case args of
  (pred:xs) -> ExprWhile (makeExprSyntax pred) (ExprCompound $ map makeExprSyntax xs)
  _         -> error "syntax error"
makeAppExprSyntax "let1" args = case args of
  (P.ExprAtom (P.AtomSym s) : x : xs) -> ExprBind s (makeExprSyntax x) (ExprCompound (map makeExprSyntax xs))
makeAppExprSyntax x args = ExprApply x (map makeExprSyntax args)






testCompileExpr x = S.evalState (runM (compileExpr "out" x)) emptyMEnv
testCompileFun x = S.evalState (runM (compileFun x)) emptyMEnv
test0 = Func "add" ["x", "y"] (ExprApply "add_int" [ExprVar "x", ExprVar "y"])
test1 src = unlines $ flip S.evalState emptyMEnv $ runM $ do
  let es = (map makeFuncSyntax (P.parse Nothing src))
  constructGlobalFunTable es
  xs <- mapM compileFun es
  return $  xs

compileSource file src = unlines $ flip S.evalState emptyMEnv $ runM $ do
  let es = (map makeFuncSyntax (P.parse file src))
  constructGlobalFunTable es
  ps <- showPrototypes
  xs <- mapM compileFun es
  return $ ps ++ ["\n"] ++ xs
  
