module Syntax where

import qualified Parser as P

data Type = TypeUnit | TypeInt | TypeRecord [Bind] deriving (Eq, Ord)
type TypeName = String
type Bind = (String, TypeName)

data Func = Func [Bind] TypeName Expr | ExternalFunc [TypeName] TypeName

data Decl = DeclFunc String Func | DeclType String Type

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

parseToSyntax file src = map makeDeclSyntax (P.parse file src)
