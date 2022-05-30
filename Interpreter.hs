module Interpreter where

import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Except
import System.IO
import Data.Char (toLower)
import Control.Monad.IO.Class
import qualified Lautaro.Abs as L
import qualified TypeChecker as TC
import qualified TableModule as A

---- STRUCTURES ----
data Value = VString String | VInt Int | VBool Bool | VFVoid | VArrFull [Value] | VArrString (A.Table String) | VArrInt (A.Table Int) | VArrBool (A.Table Bool) 
  deriving (Eq)

data Arg = ValArg String | RefArg String
  deriving (Show)

data Func = Func [Arg] [L.Stmt]
  deriving (Show)

type Pos = TC.Pos

posShow :: Pos -> String
posShow = TC.posShow

instance Show Value where
    show (VString str) = str
    show (VInt n) = show n
    show (VBool b) = (map toLower $ show b)
    show (VArrFull elems) = show elems
    show (VArrString tb) = show $ A.values tb
    show (VArrInt tb) = show $ A.values tb
    show (VArrBool tb) = show $ A.values tb
    show VFVoid = "void"

boolify :: Value -> Bool
boolify (VInt 0) = False
boolify (VInt _) = True
boolify (VBool False) = False
boolify (VBool True) = True
boolify (VString "") = False
boolify (VString _) = True
boolify _ = error "Cannot convert to bool condition"

data ErrorStruct = ErrSt {
  msg :: String,
  pos :: Pos}
    deriving Show

type Loc = Int

data Env = Env {
  env :: M.Map String Loc }
    deriving Show

data St = St {
  st :: M.Map Loc Value,
  functions :: M.Map String Func,
  newLoc :: Int}
    deriving Show

type InterpreterMonad = ExceptT ErrorStruct (StateT (St) (ReaderT Env IO))

data StmtReturnValue = VReturn Value | VBreak | VContinue | VBlank
  deriving (Show, Eq)

---- AUXILIARY ----
updateEnv :: String -> Loc -> Env -> Env
updateEnv tag newLoc en = en {env = M.insert tag newLoc (env en)}

incNewLoc :: St -> St
incNewLoc s = s{newLoc = 1 + newLoc s}

updateSt :: Loc -> Value -> St -> St
updateSt newLoc val s = s{st = M.insert newLoc val (st s)}

initState = St {st = M.empty, newLoc = 0, functions = M.empty}
initEnv = Env {env = M.empty}

prepareEnv :: [Arg] -> [L.Expr] -> Env -> InterpreterMonad Env
prepareEnv (ValArg tag : argsTemplate) (e : exprArgs) en = do
  val <- evalExp e
  newLocVal <- gets newLoc
  modify incNewLoc
  modify $ updateSt newLocVal val
  prepareEnv argsTemplate exprArgs (updateEnv tag newLocVal en)

prepareEnv (RefArg tag : argsTemplate) (L.EVar _ (L.Ident srcTag) : exprArgs) en = do
  curLoc <- asks (\en -> M.lookup srcTag (env en))
  prepareEnv argsTemplate exprArgs (updateEnv tag (fromJust curLoc) en)

prepareEnv (RefArg tag : argsTemplate) (_ : exprArgs) en = error "TypeChecker for this case failed!"

prepareEnv _ _ en = return en

convertTable :: Value -> Value 
convertTable arr@(VArrFull tab) = case (digBaseElem arr) of
  (VInt _) -> VArrInt $ A.createTableInt (breakArrayInt arr [] 0)
  (VString _) -> VArrString $ A.createTableString (breakArrayString arr [] 0)
  (VBool _) -> VArrBool $ A.createTableBool (breakArrayBool arr [] 0) 
  _ -> error "Interpreter failure!"

convertTable x = x

digBaseElem :: Value -> Value
digBaseElem (VArrFull tab) = digBaseElem $ head tab
digBaseElem elem@(VInt n) = elem
digBaseElem elem@(VString str) = elem
digBaseElem elem@(VBool b) = elem
digBaseElem _ = error "Interpreter failure!"

breakArrayInt :: Value -> [Int] -> Int -> [([Int], Int)]
breakArrayInt (VArrFull (h:t)) curDepth curPos = 
  breakArrayInt h (curPos : curDepth) 0 ++ breakArrayInt (VArrFull t) curDepth (curPos+1)
breakArrayInt (VArrFull []) curDepth curPos = []
breakArrayInt (VInt n) curDepth curPos = [(reverse curDepth, n)]

breakArrayBool :: Value -> [Int] -> Int -> [([Int], Bool)]
breakArrayBool (VArrFull (h:t)) curDepth curPos = 
  breakArrayBool h (curPos : curDepth) 0 ++ breakArrayBool (VArrFull t) curDepth (curPos+1)
breakArrayBool (VArrFull []) curDepth curPos = []
breakArrayBool (VBool n) curDepth curPos = [(reverse curDepth, n)]

breakArrayString :: Value -> [Int] -> Int -> [([Int], String)]
breakArrayString (VArrFull (h:t)) curDepth curPos = 
  breakArrayString h (curPos : curDepth) 0 ++ breakArrayString (VArrFull t) curDepth (curPos+1)
breakArrayString (VArrFull []) curDepth curPos = []
breakArrayString (VString n) curDepth curPos = [(reverse curDepth, n)]

---- EXPRESSIONS ----
evalExp :: L.Expr -> InterpreterMonad Value
evalExp (L.ELitInt _ n) = return $ VInt $ fromInteger n
evalExp (L.ELitTrue _) = return $ VBool True
evalExp (L.ELitFalse _) = return $ VBool False
evalExp (L.EString _ str) = return $ VString str
evalExp (L.Neg _ e) = do
  (VInt n) <- evalExp e
  return $ VInt $ -n

evalExp (L.EAdd pos e1 op e2) = do
  (VInt n) <- evalExp e1
  (VInt m) <- evalExp e2
  return $ VInt $ convertOp op n m where
    convertOp :: L.AddOp -> Int -> Int -> Int
    convertOp (L.Plus _) = (+)
    convertOp (L.Minus _) = (-)

evalExp (L.EMul pos e1 op e2) = do
  (VInt n) <- evalExp e1
  (VInt m) <- evalExp e2
  convertOp op n m where
    convertOp :: L.MulOp -> Int -> Int -> InterpreterMonad Value
    convertOp (L.Times _) a b = return $ VInt $ a * b
    convertOp (L.Div _) _ 0 = throwError $ ErrSt{ msg = "Division by zero is forbidden!", pos = pos }
    convertOp (L.Div _) a b = return $ VInt $ a `div` b

evalExp (L.ERel pos e1 op e2) = do
  val1 <- evalExp e1
  val2 <- evalExp e2
  return $ VBool $ case (val1, val2, op) of
    (VString str1, VString str2, L.EQU _) -> str1 == str2 
    (VString str1, VString str2, L.NE _) -> str1 /= str2
    (VBool b1, VBool b2, L.EQU _) -> b1 == b2 
    (VBool b1, VBool b2, L.NE _) -> b1 /= b2
    (VInt n, VInt m, _) -> convertOp op n m where
      convertOp :: L.RelOp -> Int -> Int -> Bool
      convertOp (L.EQU _) = (==)
      convertOp (L.NE _) = (/=)
      convertOp (L.LTH _) = (<)
      convertOp (L.GTH _) = (>)
      convertOp (L.LE _) = (<=)
      convertOp (L.GE _) = (>=)
    (_, _, _) -> error "Unknown relation" 

evalExp (L.EVar pos (L.Ident tag)) = do
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  return $ fromJust curVal

evalExp (L.EApp pos (L.Ident tag) expr_args) = do
  curSt <- get
  let (Func argsTemplate bodyStmts) = fromJust $ M.lookup tag (functions curSt)
  funcEnv <- prepareEnv argsTemplate expr_args initEnv
  funcResult <- local (\en -> funcEnv) (evalStmtBlock bodyStmts)
  case funcResult of
    VReturn res -> return res
    VBlank -> return $ VFVoid
    _ -> error "Interpreter failure!"

-- Array expression
evalExp (L.EArr pos elems) = do
  evalElems <- mapM evalExp elems
  return $ VArrFull evalElems

-- Getting length of n-th dimenstion
evalExp (L.ArrLen pos (L.Ident tag) arg') = do 
  VInt arg <- evalExp arg'
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  case curVal of
    Just (VArrInt tb) -> let result = A.getLen tb arg in if isNothing result 
      then throwError $ ErrSt{ msg = "Index out of bound!", pos = pos }
      else return $ VInt $ fromJust result
    Just (VArrString tb) -> let result = A.getLen tb arg in if isNothing result 
      then throwError $ ErrSt{ msg = "Index out of bound!", pos = pos }
      else return $ VInt $ fromJust result
    Just (VArrBool tb) -> let result = A.getLen tb arg in if isNothing result 
      then throwError $ ErrSt{ msg = "Index out of bound!", pos = pos }
      else return $ VInt $ fromJust result

-- Getting number of dimenstions
evalExp (L.ArrDim pos (L.Ident tag)) = do 
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  case fromJust curVal of
    VArrInt tb -> return $ VInt $ A.getDimensions tb
    VArrString tb -> return $ VInt $ A.getDimensions tb
    VArrBool tb -> return $ VInt $ A.getDimensions tb

-- Value from array
evalExp (L.ArrApp pos (L.Ident tag) args) = do
  evalArgs' <- mapM evalExp (map (\(L.ArrArg _ e) -> e) args)
  let evalArgs = map (\(VInt n) -> n) evalArgs'
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  case fromJust curVal of
    VArrInt tb -> return $ VInt $ A.getValue tb evalArgs
    VArrString tb -> return $ VString $ A.getValue tb evalArgs
    VArrBool tb -> return $ VBool $ A.getValue tb evalArgs

evalExp (L.StrLength pos (L.Ident tag)) = do 
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  return $ VInt $ length $ (\(VString str) -> str) (fromJust curVal)

evalExp (L.StrReverse pos (L.Ident tag)) = do
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  return $ VString $ reverse $ (\(VString str) -> str) (fromJust curVal)

evalExp (L.StrAppend pos (L.Ident tag) arg) = do
  VString evalArg <- evalExp arg
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  return $ VString $ (\(VString str) -> str)(fromJust curVal) ++ evalArg

evalExp (L.StrCut pos (L.Ident tag) argA argB) = do
  VInt indBegin <- evalExp argA
  VInt indEnd <- evalExp argB
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  let str = (\(VString s) -> s) (fromJust curVal)
  if (indBegin < 0 || length str <= indBegin)
    then throwError $ ErrSt{ msg = "Index out of bound!", pos = pos }
  else if (indEnd < 0 || length str <= indEnd)
    then throwError $ ErrSt{ msg = "Index out of bound!", pos = pos }
  else if (indBegin > indEnd)
    then throwError $ ErrSt{ msg = "Begin index cannot be larger than end", pos = pos}
  else return $ VString $ slice indBegin indEnd str where 
    slice :: Int -> Int -> String -> String
    slice from to xs = take (to - from + 1) (drop from xs)

evalExp (L.StrReplicate pos (L.Ident tag) arg') = do
  VInt arg <- evalExp arg'
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  let str = (\(VString s) -> s) (fromJust curVal)
  return $ VString $ concat $ replicate arg str

---- STATEMENTS ----
evalStmtBlock :: [L.Stmt] -> InterpreterMonad StmtReturnValue

evalStmtBlock (L.Empty _ : tstmts) = evalStmtBlock tstmts

evalStmtBlock (L.Skip _ : tstmts) = evalStmtBlock tstmts

evalStmtBlock (L.Decl pos varType (L.NoInit ipos (L.Ident tag)) : tstmts) =
    evalStmtBlock (L.Decl pos varType (L.Init ipos (L.Ident tag) $ TC.defaultLitExp varType) : tstmts)

evalStmtBlock (L.DeclConst pos varType (L.NoInit ipos (L.Ident tag)) : tstmts) =
    evalStmtBlock (L.DeclConst pos varType (L.Init ipos (L.Ident tag) $ TC.defaultLitExp varType) : tstmts)

evalStmtBlock ((L.DeclConst pos varType (L.Init ipos (L.Ident tag) e)) : tstmts) =
    evalStmtBlock (L.Decl pos varType (L.Init ipos (L.Ident tag) e) : tstmts)

evalStmtBlock (L.Decl _ _ (L.Init _ (L.Ident tag) e) : tstmts) = do
  valPre <- evalExp e
  newLocVal <- gets newLoc
  let val = convertTable valPre
  modify incNewLoc
  modify $ updateSt newLocVal val
  local (updateEnv tag newLocVal) (evalStmtBlock tstmts)

evalStmtBlock (L.VarSet pos (L.Ident tag) e : tstmts) = do
  valPre <- evalExp e
  let newVal = convertTable valPre
  curLoc <- asks (\en -> M.lookup tag (env en))
  modify $ updateSt (fromJust curLoc) newVal
  evalStmtBlock tstmts

evalStmtBlock (L.SReturn _ e : tstmts) = do
  val <- evalExp e
  return $ VReturn val

evalStmtBlock (L.SVoidReturn _ : tstmts) = return $ VReturn $ VFVoid

evalStmtBlock (L.SIf _ e_cond stmts : tstmts) = do
  val <- evalExp e_cond
  if boolify val then do
    flag <- evalStmtBlock stmts
    if flag == VBlank then
      evalStmtBlock tstmts
    else return flag
  else do
    evalStmtBlock tstmts

evalStmtBlock (L.SIfElse _ e_cond stmts else_stmts : tstmts) = do
  val <- evalExp e_cond
  if boolify val then do
    flag <- evalStmtBlock stmts
    if flag == VBlank then
      evalStmtBlock tstmts
    else return flag
  else do
    flag <- evalStmtBlock else_stmts
    if flag == VBlank then
      evalStmtBlock tstmts
    else return flag

evalStmtBlock (L.Brk _ : tstmts) = return VBreak

evalStmtBlock (L.Cnt _ : tstmts) = return VContinue

evalStmtBlock stmtWhile@(L.SWhile _ e_cond stmts : tstmts) = do
  val <- evalExp e_cond
  if boolify val then do
    flag <- evalStmtBlock stmts
    case flag of
      VBreak -> evalStmtBlock tstmts
      (VReturn v) -> return flag
      _ -> evalStmtBlock stmtWhile
  else do
    evalStmtBlock tstmts

evalStmtBlock (L.SPrint _ e : tstmts) = do
  val <- evalExp e
  liftIO $ putStr $ show val ++ "\n"
  evalStmtBlock tstmts

evalStmtBlock (L.SPut _ e : tstmts) = do
  val <- evalExp e
  liftIO $ putStr $ show val
  evalStmtBlock tstmts

evalStmtBlock (L.SAssert pos e_cond emess : tstmts) = do
  val <- evalExp e_cond
  mess <- evalExp emess
  if boolify val == False
    then throwError $ ErrSt{ msg = "Assertion failed: " ++ (\(VString str) -> str)mess, pos = pos }
  else evalStmtBlock tstmts

evalStmtBlock (L.SExp _ e : tstmts) = do
  _ <- evalExp e
  evalStmtBlock tstmts

evalStmtBlock ((L.ArrSet pos (L.Ident tag) args e):tstmts) = do
  newVal <- evalExp e
  evalArgs' <- mapM evalExp (map (\(L.ArrArg _ e) -> e) args)
  let evalArgs = map (\(VInt n) -> n) evalArgs'
  curLoc <- asks (\en -> M.lookup tag (env en))
  curVal <- gets (\s -> M.lookup (fromJust curLoc) (st s))
  case (fromJust curVal, newVal) of
    (VArrInt tb, VInt n) -> modify $ updateSt (fromJust curLoc) (VArrInt $ A.updateTable tb evalArgs n)
    (VArrString tb, VString str) -> modify $ updateSt (fromJust curLoc) (VArrString $ A.updateTable tb evalArgs str)
    (VArrBool tb, VBool b) -> modify $ updateSt (fromJust curLoc) (VArrBool $ A.updateTable tb evalArgs b)
    (_, _) -> error "Interpreter failure!"
  evalStmtBlock tstmts

evalStmtBlock [] = return VBlank

---- MAIN ----
evalProgramme :: L.Programme -> IO St
evalProgramme (L.Main _ funcs mainStms) = runReaderT (execStateT (runExceptT (catchError (eval mainStms) handleErr)) $ appendFunctions funcs initState) initEnv where

  handleErr :: ErrorStruct -> InterpreterMonad StmtReturnValue
  handleErr err = do
    curSt <- get
    liftIO $ hPutStrLn stderr $ "Runtime error!\n" ++ posShow (pos err) ++ msg err
    return VBlank

  eval :: [L.Stmt] -> InterpreterMonad StmtReturnValue
  eval stmts = evalStmtBlock stmts

  appendFunctions :: [L.TopDef] -> St -> St
  appendFunctions (L.FnDef pos (L.Ident fname) args retType bodyStmts : t) state =
    let curFunc = Func (map convertArg args) (bodyStmts ++ [L.SReturn pos $ TC.defaultLitExp retType]) in
    appendFunctions t $ state{ functions = M.insert fname curFunc (functions state)}

  appendFunctions (L.FnDefVoid _ (L.Ident fname) args bodyStmts : t) state =
    let curFunc = Func (map convertArg args) bodyStmts in
    appendFunctions t $ state{ functions = M.insert fname curFunc (functions state)}

  appendFunctions [] state = state

  convertArg :: L.Arg -> Arg
  convertArg (L.ArgVal _ _ (L.Ident tag)) = ValArg tag
  convertArg (L.ArgRef _ _ (L.Ident tag)) = RefArg tag

runProgramme :: L.Programme -> IO ()
runProgramme p = do
  evalProgramme p
  return ()
