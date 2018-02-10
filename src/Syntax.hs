{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax where

import Data.List(intercalate)
import Control.Monad(forM_, forM)
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Parser as P

import Debug.Trace(trace)


data MEnv = MEnv 
  { menvGenerator :: Int
  , menvUserFun   :: (Map.Map String Func)
  , menvVarTable  :: (Map.Map String TypeName)
  }
newtype M a = M { runM :: S.State MEnv a } deriving (Functor, Applicative, Monad, S.MonadState MEnv)

data Type = TypeUnit | TypeInt | TypeRecord [Bind] deriving (Eq, Ord)
type TypeName = String
type Bind = (String, TypeName)

data Func = Func [Bind] TypeName Expr | ExternalFunc [TypeName] TypeName

data Decl = DeclFunc String Func | DeclType String Type


-- todo: check built-in syntax

builtinFuns = map (\ (name, pty, rty) -> (name, ExternalFunc pty rty))
  [ ("add",["int","int"],"int")
  , ("sub",["int","int"],"int")
  , ("mul",["int","int"],"int")
  , ("div",["int","int"],"int")
  , ("mod",["int","int"],"int")
  , ("neg",["int"],"int")
  , ("and",["int","int"],"int")
  , ("or",["int","int"],"int")
  , ("xor",["int","int"],"int")
  , ("not",["int"],"int")
  , ("eq",["int","int"],"int")
  , ("neq",["int","int"],"int")
  , ("lt",["int","int"],"int")
  , ("gt",["int","int"],"int")
  , ("le",["int","int"],"int")
  , ("ge",["int","int"],"int")
  , ("putint",["int"],"int")
  , ("putc",["int"],"int")
  , ("newline",[],"int")
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

typeOfVar :: String -> M TypeName
typeOfVar name = do
  v <- menvVarTable <$> S.get
  case Map.lookup name v of
    Just t  -> return t
    Nothing -> error ("unknown variable: "++name)

typeOfFunRet :: String -> M TypeName
typeOfFunRet name = do
  v <- menvUserFun <$> S.get
  case Map.lookup name v of
    Just (Func _ rty _)       -> return rty
    Just (ExternalFunc _ rty) -> return rty
    _                         -> error ("unknown function:" ++ name)

getFunTypes :: String -> M (Maybe ([TypeName], TypeName))
getFunTypes name = do
  v <- menvUserFun <$> S.get
  case Map.lookup name v of
    Just (Func ps rty _)       -> return $ Just (map snd ps, rty)
    Just (ExternalFunc ps rty) -> return $ Just (ps, rty)
    _                          -> return Nothing


addFun :: String -> Func -> M ()
addFun name fn = do
  menv <- S.get
  let fnmap = menvUserFun menv
  if Map.member name fnmap
    then error ("redefinition of function: " ++ name)
    else S.put $ menv{ menvUserFun = Map.insert name fn fnmap }


checkFun :: String -> Int -> M a -> M a
checkFun name i m = do
  menv <- S.get
  case Map.lookup name (menvUserFun menv) of
    Just (Func pty _ _)
      | length pty == i -> m
      | otherwise -> error "arity does not match"
    Just (ExternalFunc pty _)
      | length pty == i -> m
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
  return $ map proto $ Map.toList (menvUserFun menv)
 where
  proto (name, fn) = case fn of
    Func pty rty _   -> mangleType rty ++ " " ++ mangleFun name ++ "(" ++ intercalate "," (map (mangleType . snd) pty) ++ ");"
    --ExternalFun pty rty -> mangleType rty ++ " " ++ mangleFun name ++ "(" ++ intercalate "," (map mangleType pty) ++ ");"
    ExternalFunc _ _ -> ""

  
getExprType :: Expr -> M TypeName
getExprType (ExprCompound [])   = return "unit" 
getExprType (ExprCompound xs)   = getExprType (last xs)
getExprType (ExprInt _)         = return "int"
getExprType (ExprVar s)         = typeOfVar s
getExprType (ExprApply fn args) = typeOfFunRet fn
getExprType (ExprAssign s x)    = typeOfVar s
getExprType (ExprBind s x body) = getExprType body
getExprType (ExprWhile x body)  = return "unit"
getExprType (ExprBranch c tc ec) = do
  tcty <- getExprType tc
  ecty <- getExprType ec
  if ecty == tcty then return tcty else return "unit"


typeCheck :: TypeName -> Expr -> M Bool
typeCheck rty (ExprCompound []) = return $ rty == "unit"
typeCheck rty (ExprCompound xs) = do
  ps <- mapM (typeCheck "unit") xs
  p  <- typeCheck rty (last xs)
  return $ all id (p:ps)
typeCheck rty (ExprApply fn args) = do
  mty <- getFunTypes fn
  case mty of
    Nothing                       -> return False
    Just (xs, x)
      | length args /= length xs  -> return False
      | otherwise                 -> do
        ps <- forM (zip xs args) (uncurry typeCheck)
        let p = rty == "unit" || x == rty
        return $ all id (p:ps)
typeCheck rty (ExprAssign x y) = do
  vty <- typeOfVar x
  p <- (== vty) <$> getExprType y
  return $ p && (rty == "unit" || vty == rty)
typeCheck rty (ExprBind s x body) = do
  ty <- getExprType x
  localVar [(s, ty)] $ do
    typeCheck rty body
typeCheck rty (ExprWhile pred body) = do
  pp <- typeCheck "int" pred
  pb <- typeCheck rty body
  return (pp && pb)
typeCheck rty (ExprBranch pred tc ec) = do
  pp <- typeCheck "int" pred
  ptc <- typeCheck rty tc
  pec <- typeCheck rty ec
  return (pp && ptc && pec)
typeCheck rty e = do
  ty <- getExprType e
  return $ rty == "unit" || rty == ty


typeCheckFun (DeclFunc name (Func ps rty expr)) = do
  localVar ps $ do
    ok <- typeCheck rty expr
    if ok then return True else error $ "type error at " ++ name
typeCheckFun _ = return True



compileAssignOut Nothing v  = "(void)"++"(" ++ v ++ ");\n"
compileAssignOut (Just x) v = x ++ "=" ++ v ++ ";\n"


compileExpr :: Maybe String -> Expr -> M String
compileExpr out (ExprCompound xs) =
  case reverse xs of
    []     -> do
      -- assert (out == Nothing)
      return "{}\n"
    (y:ys) -> do
      ss <- forM (reverse ys) (compileExpr Nothing)
      s <- compileExpr out y
      return $ "{\n" ++ concat (ss++[s]) ++ "\n}\n"
compileExpr out (ExprInt i) = do
  return $ compileAssignOut out (show i)
compileExpr out (ExprVar s) = do
  checkVar s $ do
    return $ compileAssignOut out (mangleVar s)
compileExpr out (ExprApply fn args) = do
  checkFun fn (length args) $ do
    names <- mapM (\ _ -> genVar) args
    let decls = concatMap (\ v -> "int " ++ v ++ ";\n") names
    binds <- mapM (\ (s, x) -> compileExpr (Just s) x) (zip names args)
    let call = compileAssignOut out (mangleFun fn ++ "(" ++ intercalate "," names ++ ")")
    return $ "{\n" ++ decls ++ concat binds ++ call ++ "}\n"
compileExpr out (ExprAssign s x) = do
  ev <- compileExpr (Just $ mangleVar s) x
  return $ ev ++ compileAssignOut out (mangleVar s) 
compileExpr out (ExprBind s x body) = do
  ty <- getExprType x
  localVar [(s,ty)] $ do
    let decl = mangleType ty ++ " " ++ mangleVar s ++ ";\n"
    ev <- compileExpr (Just $ mangleVar s) x
    b <- compileExpr out body
    return $ "{\n" ++ decl ++ ev ++ b ++ "}"
compileExpr out (ExprWhile x body) = do
  name <- genVar
  pred <- compileExpr (Just name) x 
  let decl = "int " ++ name ++ ";\n"
  ev <- compileExpr out body
  return $ "for (;;) {" ++ decl ++ pred ++ "if (" ++ name ++ ") {\n" ++ ev ++ "} else { break; }\n}\n"
compileExpr out (ExprBranch p tc ec) = do
  name <- genVar
  pred <- compileExpr (Just name) p
  tcev <- compileExpr out tc
  ecev <- compileExpr out ec
  return $ "{ int " ++ name ++ ";\n" ++ pred ++ "if (" ++ name ++ ") {\n" ++ tcev ++ "} else {\n" ++ ecev ++ "}\n}\n"


compileFun name (Func params rty body) = do
  let paramBinds = intercalate "," $ map (\ (v, t) -> mangleType t ++ " " ++ mangleVar v) params
  let decl = mangleType rty ++ " _out;\n"
  localVar params $ do
    ev <- compileExpr (Just "_out") body
    return $ mangleType rty ++ " " ++ mangleFun name ++ "(" ++ paramBinds ++ ")\n{\n" ++ decl ++ ev ++ "return _out;\n}\n"

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
    DeclFunc name fn -> addFun name fn
    _                -> return ()


makeDeclSyntax :: P.Expr -> Decl
makeDeclSyntax (P.ExprList (P.ExprAtom (P.AtomSym "defproc") : P.ExprAtom (P.AtomSym name) : body)) = DeclFunc name (makeFuncSyntax body)
makeDeclSyntax (P.ExprList (P.ExprAtom (P.AtomSym "defrecord") : P.ExprAtom (P.AtomSym name) : body)) = DeclType name (makeRecordSyntax body)
makeDeclSyntax _ = error "syntax error"


makeFuncSyntax :: [P.Expr] -> Func
makeFuncSyntax (P.ExprList params : P.ExprAtom (P.AtomKey ":>") : P.ExprAtom (P.AtomSym rty) : body) = Func paramsSyntax rty bodySyntax
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
  typeok <- all id <$> mapM typeCheckFun es 
  if not typeok
    then do
      error "type error"
    else do
      xs <- mapM compileDeclType es
      xs' <- mapM compileDeclTypeBody es
      ys <- mapM compileDeclFunc es
      return $ xs ++ xs' ++ ps ++ ["\n"] ++ ys
  
