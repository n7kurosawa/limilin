{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Translate where

import Syntax


import Data.List(intercalate)
import Control.Monad(forM_, forM)
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Set as Set

-- import qualified Parser as P

import Debug.Trace(trace)


data MEnv = MEnv 
  { menvGenerator  :: Int
  , menvUserFun    :: (Map.Map String Func)
  , menvVarTable   :: (Map.Map String Type)
  , menvTypeAlias  :: (Map.Map String Type)
  , menvTypeEntity :: (Map.Map String Type)
  }
newtype M a = M { runM :: S.State MEnv a } deriving (Functor, Applicative, Monad, S.MonadState MEnv)


-- todo: check built-in syntax

builtinFuns = map (\ (name, pty, rty) -> (name, ExternalFunc (map TypeId pty) (TypeId rty)))
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

builtinTypes = [("int", TypeInt), ("unit", TypeUnit)]

emptyMEnv = MEnv 0 (Map.fromList builtinFuns) (Map.empty) (Map.fromList builtinTypes) (Map.empty)




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

typeOfVar :: String -> M Type
typeOfVar name = do
  v <- menvVarTable <$> S.get
  case Map.lookup name v of
    Just t  -> return t
    Nothing -> error ("unknown variable: "++name)

typeOfFunRet :: String -> M Type
typeOfFunRet name = do
  v <- menvUserFun <$> S.get
  case Map.lookup name v of
    Just (Func _ rty _)       -> return rty
    Just (ExternalFunc _ rty) -> return rty
    _                         -> error ("unknown function:" ++ name)

getFunTypes :: String -> M (Maybe ([Type], Type))
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

addNewType :: String -> Type -> M ()
addNewType name ty = do
  menv <- S.get
  let tymap = menvTypeEntity menv
  if Map.member name tymap
    then error ("redefiniton of type: " ++ name)
    else S.put $ menv{ menvTypeEntity = Map.insert name ty tymap }

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
mangleType TypeUnit = "int"
mangleType TypeInt = "int"
mangleType (TypeId t) = "t_" ++ mangleName t
mangleType _ = error "internal: mangleType"
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


unAliasType :: Type -> M Type
unAliasType t@(TypeId s) = do
  m <- menvTypeAlias <$> S.get
  case Map.lookup s m of
    Just u  -> unAliasType u
    Nothing -> return t
unAliasType t = return t

entityOfType :: Type -> M Type
entityOfType t = entityOfTypeWork =<< unAliasType t
entityOfTypeWork t@(TypeId s) = do
  m <- menvTypeEntity <$> S.get
  case Map.lookup s m of
    Just u -> entityOfType u
    Nothing -> error $ "error: type " ++ show s ++ " is  without concrete entity"
entityOfTypeWork t = return t

matchType :: Type -> Type -> M Bool
matchType ty1 ty2 = (==) <$> unAliasType ty1 <*> unAliasType ty2



getExprType :: Expr -> M Type
getExprType (ExprCompound [])   = return TypeUnit
getExprType (ExprCompound xs)   = getExprType (last xs)
getExprType (ExprInt _)         = return TypeInt
getExprType (ExprVar s)         = typeOfVar s
getExprType (ExprApply fn args) = typeOfFunRet fn
getExprType (ExprAssign s x)    = typeOfVar s
getExprType (ExprBind s x body) = do
  xty <- getExprType x
  localVar [(s,xty)] $ getExprType body
getExprType (ExprWhile x body)  = return TypeUnit
getExprType (ExprBranch c tc ec) = do
  tcty <- getExprType tc
  ecty <- getExprType ec
  if ecty == tcty then return tcty else return TypeUnit


typeCheck :: Type -> Expr -> M Bool
typeCheck rty (ExprCompound []) = matchType rty TypeUnit
typeCheck rty (ExprCompound xs) = do
  ps <- mapM (typeCheck TypeUnit) xs
  p  <- typeCheck rty (last xs)
  {- trace (show rty ++ "<->"++show xs ++ ":" ++ show ps) $ -}
  return $ all id (p:ps)
typeCheck rty (ExprApply fn args) = do
  mty <- getFunTypes fn
  case mty of
    Nothing                       -> return False
    Just (xs, x)
      | length args /= length xs  -> return False
      | otherwise                 -> do
        ps <- forM (zip xs args) (uncurry typeCheck)
        p <- (||) <$> (matchType rty TypeUnit) <*> (matchType rty x)
        return $ all id (p:ps)
typeCheck rty (ExprAssign x y) = do
  vty <- typeOfVar x
  p <- matchType vty =<< getExprType y
  q <- (||) <$> (matchType rty TypeUnit) <*> (matchType rty vty)
  return $ p && q
typeCheck rty (ExprBind s x body) = do
  ty <- getExprType x
  localVar [(s, ty)] $ do
    typeCheck rty body
typeCheck rty (ExprWhile pred body) = do
  pp <- typeCheck TypeInt pred
  pb <- typeCheck rty body
  return (pp && pb)
typeCheck rty (ExprBranch pred tc ec) = do
  pp <- typeCheck TypeInt pred
  ptc <- typeCheck rty tc
  pec <- typeCheck rty ec
  return (pp && ptc && pec)
typeCheck rty e = do
  ty <- getExprType e
  (||) <$> matchType rty TypeUnit <*> matchType rty ty


typeCheckFun (DeclFunc name (Func ps rty expr)) = do
  localVar ps $ do
    lty <- getExprType expr
    ok <- typeCheck rty expr
    if ok then return True else trace (show expr) $ error $ "type error at " ++ name
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

compileType name (TypeRecord []) = return $ "typedef int " ++ mangleType (TypeId name) ++ ";\n"
compileType name (TypeRecord _)  = return $ "typedef struct " ++ mangleType (TypeId name) ++ " " ++ mangleType (TypeId name) ++ ";\n"
compileType name t = return $ "typedef " ++ mangleType t ++ " " ++ mangleType (TypeId name) ++ ";\n"

compileTypeBody name (TypeRecord []) = return ""
compileTypeBody name (TypeRecord xs) = return $ "struct " ++ mangleType (TypeId name) ++ " {\n" ++ concatMap compileBind xs ++ "};\n"
 where
  compileBind (var, ty) = mangleType ty ++ " " ++ mangleVar var ++ ";\n"
compileTypeBody name _ = return ""
  

compileDeclFunc (DeclFunc name fun) = compileFun name fun
compileDeclFunc (DeclType _ _) = return ""

compileDeclType (DeclFunc _ _) = return ""
compileDeclType (DeclType name ty) = compileType name ty

compileDeclTypeBody (DeclFunc _ _) = return ""
compileDeclTypeBody (DeclType name ty) = compileTypeBody name ty



constructGlobalFunTable :: [Decl] -> M ()
constructGlobalFunTable xs = forM_ xs $ \ decl -> do
  case decl of
    DeclFunc name fn -> addFun name fn
    _                -> return ()

constructTypeTable :: [Decl] -> M ()
constructTypeTable xs = forM_ xs $ \ decl -> do
  case decl of
    DeclType name ty -> addNewType name ty
    _                -> return ()


compileSource file src = unlines $ flip S.evalState emptyMEnv $ runM $ do
  let es = parseToSyntax file src
  constructTypeTable es
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
  
