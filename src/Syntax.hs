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
  , menvUserFun   :: (Map.Map String [TypeName])
  , menvVarTable  :: (Map.Map String TypeName)
  }
newtype M a = M { runM :: S.State MEnv a } deriving (Functor, Applicative, Monad, S.MonadState MEnv)

data Type = TypeInt | TypeRecord [Bind]
type TypeName = String
type Bind = (String, TypeName)

data Func = Func [Bind] Expr

data Decl = DeclFunc String Func | DeclType String Type


-- todo: check built-in syntax

builtinFuns = 
  [ ("add",["int","int"])
  , ("sub",["int","int"])
  , ("mul",["int","int"])
  , ("div",["int","int"])
  , ("mod",["int","int"])
  , ("neg",["int"])
  , ("and",["int","int"])
  , ("or",["int","int"])
  , ("xor",["int","int"])
  , ("not",["int"])
  , ("eq",["int","int"])
  , ("neq",["int","int"])
  , ("lt",["int","int"])
  , ("gt",["int","int"])
  , ("le",["int","int"])
  , ("ge",["int","int"])
  , ("putint",["int"])
  , ("putc",["int"])
  , ("newline",[])
  ]

emptyMEnv = MEnv 0 (Map.fromList builtinFuns) (Map.empty)




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

localVar :: [Bind] -> M a -> M a
localVar names m = do
  menv <- S.get
  let varmap = menvVarTable menv
  S.put $ menv{ menvVarTable = (Map.union (Map.fromList names) varmap) }
  r <- m
  S.put $ menv
  return r

checkVar :: String -> M a -> M a
checkVar name m = do
  v <- menvVarTable <$> S.get
  if Map.member name v then m else error ("unknown variable: "++name)

addFun :: String -> [TypeName] -> M ()
addFun name pty = do
  menv <- S.get
  let fnmap = menvUserFun menv
  if Map.member name fnmap
    then error ("redefinition of function: " ++ name)
    else S.put $ menv{ menvUserFun = Map.insert name pty fnmap }

checkFun :: String -> Int -> M a -> M a
checkFun name i m = do
  menv <- S.get
  case Map.lookup name (menvUserFun menv) of
    Just ty
      | length ty == i -> m
      | otherwise -> error "arity does not match"
    Nothing       -> error "undefiend function"


mangleFun f = "m_" ++ mangleName f
mangleType t = "t_" ++ mangleName t
mangleVar v = "m_" ++ mangleName v

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
  proto name pty = "int " ++ mangleFun name ++ "(" ++ intercalate "," (map mangleType pty) ++ ");"


compileExpr :: String -> Expr -> M String
compileExpr out (ExprCompound xs) = do
  name <- genVar
  ss <- mapM (compileExpr name) xs
  return $ "{ int " ++ name ++ ";\n" ++ concat ss ++ "\n" ++ out ++ "=" ++ name ++ ";\n}\n"
compileExpr out (ExprInt i) = do
  return $ out ++ "=" ++ show i ++ ";\n"
compileExpr out (ExprVar s) = do
  checkVar s $ do
    return $ out ++ "=" ++ mangleVar s ++ ";\n"
compileExpr out (ExprApply fn args) = do
  checkFun fn (length args) $ do
    names <- mapM (\ _ -> genVar) args
    let decls = concatMap (\ v -> "int " ++ v ++ ";\n") names
    binds <- mapM (\ (s, x) -> compileExpr s x) (zip names args)
    let call = out ++ "=" ++ mangleFun fn ++ "(" ++ intercalate "," names ++ ");\n"
    return $ "{\n" ++ decls ++ concat binds ++ call ++ "}\n"
compileExpr out (ExprAssign s x) = do
  ev <- compileExpr (mangleVar s) x
  return $ ev ++ out ++ "=" ++ mangleVar s ++ ";\n"
compileExpr out (ExprBind s x body) = do
  localVar [(s,"int")] $ do
    let decl = "int " ++ mangleVar s ++ ";\n"
    ev <- compileExpr (mangleVar s) x
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


compileFun name (Func params body) = do
  let paramBinds = intercalate "," $ map (\ (v, t) -> mangleType t ++ " " ++ mangleVar v) params
  let decl = "int _out;\n"
  localVar params $ do
    ev <- compileExpr "_out" body
    return $ "int " ++ mangleFun name ++ "(" ++ paramBinds ++ ")\n{\n" ++ decl ++ ev ++ "return _out;\n}\n"

compileType name TypeInt = "typedef int " ++ mangleType name ++ ";\n"
compileType name (TypeRecord []) = "typedef int " ++ mangleType name ++ ";\n"
compileType name (TypeRecord _)  = "typedef struct " ++ mangleType name ++ " " ++ mangleType name ++ ";\n"

compileTypeBody name TypeInt = ""
compileTypeBody name (TypeRecord []) = ""
compileTypeBody name (TypeRecord xs) = "struct " ++ mangleType name ++ " {\n" ++ concatMap compileBind xs ++ "};\n"
 where
  compileBind (var, ty) = mangleType ty ++ " " ++ mangleVar var ++ ";\n"
  

compileDeclFunc (DeclFunc name fun) = compileFun name fun
compileDeclFunc (DeclType _ _) = return ""

compileDeclType (DeclFunc _ _) = return ""
compileDeclType (DeclType name ty) = return $ compileType name ty

compileDeclTypeBody (DeclFunc _ _) = return ""
compileDeclTypeBody (DeclType name ty) = return $ compileTypeBody name ty



constructGlobalFunTable :: [Decl] -> M ()
constructGlobalFunTable xs = forM_ xs $ \ decl -> do
  case decl of
    DeclFunc name (Func params _) -> addFun name (map snd params)
    _                             -> return ()




makeDeclSyntax :: P.Expr -> Decl
makeDeclSyntax (P.ExprList (P.ExprAtom (P.AtomSym "defproc") : P.ExprAtom (P.AtomSym name) : body)) = DeclFunc name (makeFuncSyntax body)
makeDeclSyntax (P.ExprList (P.ExprAtom (P.AtomSym "defrecord") : P.ExprAtom (P.AtomSym name) : body)) = DeclType name (makeRecordSyntax body)
makeDeclSyntax _ = error "syntax error"


makeFuncSyntax :: [P.Expr] -> Func
makeFuncSyntax (P.ExprList params : body) = Func paramsSyntax bodySyntax
 where
  paramsSyntax = makeParamSyntax params
  bodySyntax = ExprCompound $ map makeExprSyntax body
makeFuncSyntax _  = error "syntax error"

--makeParamSyntax (P.ExprList [P.ExprAtom (P.AtomSym s), P.ExprAtom (P.AtomSym "int")] : xs) = (s, TyInt) : makeParamSyntax xs
makeParamSyntax (P.ExprList [P.ExprAtom (P.AtomSym s), P.ExprAtom (P.AtomSym t)] : xs) = (s,t) : makeParamSyntax xs
makeParamSyntax [] = []
makeParamSyntax _ = error "syntax error"


makeRecordSyntax :: [P.Expr] -> Type
makeRecordSyntax decls = TypeRecord (makeFieldDeclSyntax decls)

makeFieldDeclSyntax [] = []
makeFieldDeclSyntax (P.ExprList [(P.ExprAtom (P.AtomSym var)), (P.ExprAtom (P.AtomSym ty))] : xs) = (var, ty) : makeFieldDeclSyntax xs
makeFieldDeclSyntax _ = error "syntax error"


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






--testCompileExpr x = S.evalState (runM (compileExpr "out" x)) emptyMEnv
--testCompileFun x = S.evalState (runM (compileFun x)) emptyMEnv
--test0 = Func "add" ["x", "y"] (ExprApply "add_int" [ExprVar "x", ExprVar "y"])
--test1 src = unlines $ flip S.evalState emptyMEnv $ runM $ do
--  let es = (map makeDeclSyntax (P.parse Nothing src))
--  constructGlobalFunTable es
--  xs <- mapM compileDecl es
--  return $  xs

compileSource file src = unlines $ flip S.evalState emptyMEnv $ runM $ do
  let es = (map makeDeclSyntax (P.parse file src))
  constructGlobalFunTable es
  ps <- showPrototypes
  xs <- mapM compileDeclType es
  xs' <- mapM compileDeclTypeBody es
  ys <- mapM compileDeclFunc es
  return $ xs ++ xs' ++ ps ++ ["\n"] ++ ys
  
