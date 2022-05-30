module TypeChecker where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Functor.Identity ()

import qualified Data.Map as M
import Data.List (intercalate, nub)
import qualified Lautaro.Abs as L
import Data.Maybe (fromJust, isNothing)

---- STRUCTURES ----
data TypeCheckerEnv = Env {
    env :: M.Map String Type, -- functions and all declared variables
    constStatus :: M.Map String Bool, -- if variable is declared as const
    declaredVariables :: [String], -- variables declared in current block
    insideWhile :: Bool, -- while loop flag
    insideFunction :: Maybe Type } -- function indicator
    deriving Show

type TypeCheckerMonad a = ExceptT String (Reader TypeCheckerEnv) a

type Pos = L.BNFC'Position

---- AUXILIARY FUNCTIONS ----
appendObject :: String -> Type -> TypeCheckerEnv -> TypeCheckerEnv
appendObject tag obj oldEnv = oldEnv { env = M.insert tag obj (env oldEnv), declaredVariables = tag : declaredVariables oldEnv}

setFunction :: Type -> TypeCheckerEnv -> TypeCheckerEnv
setFunction retType oldEnv = oldEnv {insideFunction = Just retType}

setConst :: String -> Bool -> TypeCheckerEnv -> TypeCheckerEnv
setConst tag isConst oldEnv = oldEnv {constStatus = M.insert tag isConst (constStatus oldEnv)}

clearForNewScope :: TypeCheckerEnv -> TypeCheckerEnv
clearForNewScope oldEnv = oldEnv {declaredVariables = map fst $ filter (isFun . snd) $ M.toList $ env oldEnv} 

isFun :: Type -> Bool
isFun (TFun _ _) = True
isFun _ = False

enterWhile :: TypeCheckerEnv -> TypeCheckerEnv
enterWhile oldEnv = oldEnv {insideWhile = True}

isMember :: Eq t => t -> [t] -> Bool
isMember y = foldr (\x -> (||) (y == x)) False

initEnv :: TypeCheckerEnv
initEnv = Env { env = M.empty, constStatus = M.empty, declaredVariables = [], insideFunction = Nothing, insideWhile = False }

posShow :: Pos -> String
posShow (Just (line, col)) = show line ++ ":" ++ show col ++ ": "
posShow _ = "?:?: "

getFunStmts :: L.TopDef -> [L.Stmt]
getFunStmts (L.FnDef _ _ _ _ stmts) = stmts
getFunStmts (L.FnDefVoid _ _ _ stmts) = stmts

defaultLitExp :: L.Type -> L.Expr
defaultLitExp (L.Int ppos) = L.ELitInt ppos 0
defaultLitExp (L.Bool ppos) = L.ELitFalse ppos
defaultLitExp (L.Str ppos) = L.EString ppos ""
defaultLitExp _ = error "Typechecker failure!"

getArrayType :: Type -> Type 
getArrayType (TArr ty _) = ty 
getArrayType _ = error "Not array!"

---- TYPES -----
data Type
    = TInt
    | TString
    | TBool
    | TArr Type Int             -- Multidimensional array
    | TFun Type [(Type, Bool)]  -- function type (type arg1 isRef, type arg2 isRef, ...)
    | TFVoid                    -- auxiliary type for function
    deriving (Eq, Ord)

instance Show Type where
    show TInt = "int"
    show TString = "string"
    show TBool = "bool"
    show (TArr t d) = show t ++ concat (replicate d "[]")
    show TFVoid = "void"
    show (TFun retType args) = "function (" ++ intercalate ", " (map showFnArg args) ++ ") -> " ++ show retType where
        showFnArg (t, isRef) = (if isRef then "ref " else "") ++ show t

isArray :: Type -> Bool
isArray (TArr _ _) = True
isArray _ = False

convertType :: L.Type -> Type
convertType (L.Int _)    = TInt
convertType (L.Str _)    = TString
convertType (L.Bool _)   = TBool
convertType arr@(L.TArr _ t) = TArr (convertType' t) (countDims arr) where
    countDims :: L.Type -> Int
    countDims (L.TArr _ t'@(L.TArr _ _)) = 1 + countDims t'
    countDims _ = 1
    convertType' (L.TArr _ t) = convertType' t
    convertType' t = convertType t

assertType :: Pos -> Type -> Type -> String -> TypeCheckerMonad Type
assertType pos givenType expectedType actionDescr =
    if givenType == expectedType
        then return givenType
        else throwError $ posShow pos ++ "Types mismatch at " ++ actionDescr ++ " - expected: " ++ show expectedType ++ " got: " ++ show givenType
    where 
        sameArray (TArr t1 _) (TArr t2 _) = sameArray t1 t2
        sameArray a b = a == b

assertTypeLst :: Pos -> [Type] -> [Type] -> String -> TypeCheckerMonad Type
assertTypeLst pos (h1:t1) (h2:t2) actionDescr = do
    assertType pos h1 h2 actionDescr
    assertTypeLst pos t1 t2 actionDescr
assertTypeLst _ _ _ _ = return TFVoid

assertArray :: Pos -> L.Expr -> TypeCheckerMonad Type
assertArray _ (L.EArr pos args) = do
    case head args of
        (L.EArr pos' args') -> do
            mapM_ (assertDimension pos') args
            evalArgs <- mapM (assertArray pos') args
            assertTypeLst pos (tail evalArgs ++ [head evalArgs]) evalArgs "initialising subarrays in array"
            return $ head evalArgs
        exp -> do 
            evalArgs <- mapM getTypeExp args
            assertTypeLst pos (tail evalArgs ++ [head evalArgs]) evalArgs "initialising elements in array"
            return $ head evalArgs
    where
        assertDimension :: Pos -> L.Expr -> TypeCheckerMonad ()
        assertDimension _ (L.EArr _ _) = return ()
        assertDimension pos _ = throwError $ posShow pos ++ "Colliding elements dimension in array!"

assertArray pos exp = do
    expType <- getTypeExp exp
    return expType

checkRefMatch :: Pos -> (L.Expr, Bool) -> TypeCheckerMonad ()
checkRefMatch _ (_, False) = return ()
checkRefMatch _ ((L.EVar pos (L.Ident tag)), True) = do
    isConst <- asks (M.lookup tag . constStatus)
    if isConst == Just True
       then throwError $ posShow pos ++ "Passing const variable " ++ tag ++ " for function with referenced argument!"
       else return ()
checkRefMatch pos _ = throwError $ posShow pos ++ "Passing wrong expression for function with referenced argument!"

---- EXPRESSIONS ----
getTypeExp :: L.Expr -> TypeCheckerMonad Type
getTypeExp (L.ELitInt _ _) = return TInt
getTypeExp (L.ELitTrue _) = return TBool
getTypeExp (L.ELitFalse _) = return TBool
getTypeExp (L.EString _ _) = return TString

getTypeExp (L.Neg pos e) = do
    expVal <- getTypeExp e
    assertType pos expVal TInt "negating number"
    return TInt

getTypeExp (L.EAdd pos e1 _ e2) = do
    expVal1 <- getTypeExp e1
    assertType pos expVal1 TInt "first arg for arithmetics"
    expVal2 <- getTypeExp e2
    assertType pos expVal2 TInt "second arg for arithmetics"
    return TInt

getTypeExp (L.EMul pos e1 _ e2) = getTypeExp (L.EAdd pos e1 (L.Plus pos) e2)

getTypeExp (L.ERel pos e1 (L.NE ipos) e2) = getTypeExp (L.ERel pos e1 (L.EQU ipos) e2)

getTypeExp (L.ERel pos e1 (L.EQU _) e2) = do
    expVal1 <- getTypeExp e1
    expVal2 <- getTypeExp e2
    if isArray expVal1 || isArray expVal2 
        then throwError $ posShow pos ++ "Direct array comparison is forbidden!"
    else do
        assertType pos expVal1 expVal2 "equality operator"
        return TBool

getTypeExp (L.ERel pos e1 _ e2) = do
    expVal1 <- getTypeExp e1
    assertType pos expVal1 TInt "first arg for comparison"
    expVal2 <- getTypeExp e2
    assertType pos expVal2 TInt "second arg for comparison"
    return TBool

getTypeExp (L.EVar pos (L.Ident tag)) = do
    expType <- asks (M.lookup tag . env)
    if isNothing expType
        then throwError $ posShow pos ++ "Usage of undeclared variable " ++ tag ++ "!"
        else return $ fromJust expType

getTypeExp (L.EApp pos (L.Ident tag) args) = do
    expType <- asks (M.lookup tag . env)
    evalArgs <- mapM getTypeExp args
    case expType of
        Just (TFun retType argTypes) -> do
            if length args /= length argTypes
                then throwError $ posShow pos ++ "Incorrect number of arguments!"
                else do
                    assertTypeLst pos evalArgs (map fst argTypes) "function application!"
                    mapM_ (checkRefMatch pos) (zip args (map snd argTypes))
                    return retType
        _ -> throwError $ posShow pos ++ "Usage of undeclared function " ++ tag ++ "!"


-- Array expression
getTypeExp exp@(L.EArr pos args) = do
    arrType <- assertArray pos exp
    let arrDepth = breakArr exp
    return $ TArr arrType arrDepth where
        breakArr :: L.Expr -> Int
        breakArr (L.EArr _ args) = breakArr (head args) + 1
        breakArr _ = 0

-- Getting length of n-th dimenstion
getTypeExp (L.ArrLen pos (L.Ident tag) arg) = do
    expType <- getTypeExp arg
    assertType pos expType TInt "arguments for getting array's dimension length"
    arr <- asks (M.lookup tag . env)
    if isNothing arr 
        then throwError $ posShow pos ++ "Array " ++ tag ++ " is not declared!"
    else return TInt

-- Getting number of dimenstions
getTypeExp (L.ArrDim pos (L.Ident tag)) = do
    arr <- asks (M.lookup tag . env)
    if isNothing arr 
        then throwError $ posShow pos ++ "Array " ++ tag ++ " is not declared!"
    else return TInt

-- Value from array
getTypeExp (L.ArrApp pos (L.Ident tag) args) = do
    evalArgs <- mapM getTypeExp $ map (\(L.ArrArg _ arg) -> arg) args
    arr <- asks (M.lookup tag . env)
    assertTypeLst pos evalArgs (replicate (length evalArgs) TInt) "retreving element from array" 
    if isNothing arr 
        then throwError $ posShow pos ++ "Array " ++ tag ++ " is not declared!"
    else do 
        let (TArr _ arrDim) = fromJust arr
        if length evalArgs < arrDim 
            then throwError $ posShow pos ++ "Not enough arguments for this array!"
        else if length evalArgs > arrDim
            then throwError $ posShow pos ++ "Too many arguments for this array!"
        else return $ getArrayType $ fromJust arr

getTypeExp (L.StrLength pos (L.Ident tag)) = do 
    str <- asks (M.lookup tag . env)
    if isNothing str 
        then throwError $ posShow pos ++ "String " ++ tag ++ " is not declared!"
    else return TInt

getTypeExp (L.StrReverse pos (L.Ident tag)) = do
    str <- asks (M.lookup tag . env)
    if isNothing str 
        then throwError $ posShow pos ++ "String " ++ tag ++ " is not declared!"
    else return TString

getTypeExp (L.StrAppend pos (L.Ident tag) arg) = do
    expType <- getTypeExp arg
    assertType pos expType TString "appending string"
    str <- asks (M.lookup tag . env)
    if isNothing str 
        then throwError $ posShow pos ++ "String " ++ tag ++ " is not declared!"
    else return TString

getTypeExp (L.StrCut pos (L.Ident tag) argA argB) = do
    expTypeA <- getTypeExp argA
    expTypeB <- getTypeExp argB
    assertType pos expTypeA TInt "cutting string begin index"
    assertType pos expTypeB TInt "cutting string end index"
    str <- asks (M.lookup tag . env)
    if isNothing str 
        then throwError $ posShow pos ++ "String " ++ tag ++ " is not declared!"
    else return TString

getTypeExp (L.StrReplicate pos (L.Ident tag) arg) = do
    expType <- getTypeExp arg
    assertType pos expType TInt "replicating string"
    str <- asks (M.lookup tag . env)
    if isNothing str 
        then throwError $ posShow pos ++ "String " ++ tag ++ " is not declared!"
    else return TString

---- STATEMENTS ----
typeCheckStmtsBlock :: [L.Stmt] -> TypeCheckerMonad ()

typeCheckStmtsBlock ((L.Empty _):tstmts) = typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.Skip _):tstmts) = typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.Decl pos varType (L.NoInit ipos (L.Ident tag))):tstmts) = do 
    case varType of 
        (L.TArr _ _) -> throwError $ posShow ipos ++ "Initialising empty array is forbidden!"
        _ -> typeCheckStmtsBlock (L.Decl pos varType (L.Init ipos (L.Ident tag) $ defaultLitExp varType):tstmts)

typeCheckStmtsBlock ((L.Decl pos varType (L.Init _ (L.Ident tag) e)):tstmts) = do
    expType <- getTypeExp e
    assertType pos expType (convertType varType) $ "initialising variable " ++ tag
    alreadyDeclared <- asks (isMember tag . declaredVariables)
    if alreadyDeclared
        then throwError $ posShow pos ++ "Variable " ++ tag ++ " is already declared!"
    else local (appendObject tag expType . setConst tag False) (typeCheckStmtsBlock tstmts)

typeCheckStmtsBlock ((L.VarSet pos (L.Ident tag) e):tstmts) = do
    expType <- getTypeExp e
    curType <- asks (M.lookup tag . env)
    isConst <- asks (M.lookup tag . constStatus)
    if isNothing curType
        then throwError $ posShow pos ++ "Variable " ++ tag ++ " is not declared!"
    else if isConst == Just True
    then throwError $ posShow pos ++ "Constant " ++ tag ++ " cannot be modified!"
    else if expType /= fromJust curType
        then throwError $ posShow pos ++ "Cannot assign " ++ show expType ++ " to " ++ show (fromJust curType)
    else typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.SReturn pos e):tstmts) = do
    expType <- getTypeExp e
    functionType <- asks insideFunction
    case functionType of
      Nothing -> throwError $ posShow pos ++ "Return outside of function"
      Just ty -> if ty == expType
          then typeCheckStmtsBlock tstmts
          else throwError $ posShow pos ++ "Returning " ++ show expType ++ " in " ++ show ty ++ " function!"

typeCheckStmtsBlock ((L.SVoidReturn pos):tstmts) = do
    let expType = TFVoid
    functionType <- asks insideFunction
    case functionType of
      Nothing -> throwError $ posShow pos ++ "Return outside of function"
      Just ty -> if ty == expType
          then typeCheckStmtsBlock tstmts
          else throwError $ posShow pos ++ "Returning " ++ show expType ++ " in void function!"

typeCheckStmtsBlock ((L.SIf pos e_cond stmts):tstmts) = do
    condType <- getTypeExp e_cond
    if isArray condType
        then throwError $ posShow pos ++ "Array cannot be used as if statement condition!"
    else if isFun condType
        then throwError $ posShow pos ++ "Function cannot be used as while loop condition!"
    else do
        local clearForNewScope (typeCheckStmtsBlock stmts)
        typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.SIfElse pos e_cond stmts else_stmts):tstmts) = do
    condType <- getTypeExp e_cond
    if isArray condType
        then throwError $ posShow pos ++ "Array cannot be used as if-else statement condition!"
    else if isFun condType
        then throwError $ posShow pos ++ "Function cannot be used as while loop condition!"
    else do
        local clearForNewScope (typeCheckStmtsBlock stmts)
        local clearForNewScope (typeCheckStmtsBlock else_stmts)
        typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.SWhile pos e_cond stmts):tstmts) = do
    condType <- getTypeExp e_cond
    if isArray condType
        then throwError $ posShow pos ++ "Array cannot be used as while loop condition!"
    else if isFun condType
        then throwError $ posShow pos ++ "Function cannot be used as while loop condition!"
    else do
        local (clearForNewScope . enterWhile) (typeCheckStmtsBlock stmts)
        typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.Brk pos):tstmts) = do
    isInsideWhile <- asks insideWhile
    if isInsideWhile
        then typeCheckStmtsBlock tstmts
        else throwError $ posShow pos ++ "Loop manipulation outside of while loop!"

typeCheckStmtsBlock ((L.Cnt pos):tstmts) = typeCheckStmtsBlock (L.Brk pos:tstmts)

typeCheckStmtsBlock ((L.SPut pos e):tstmts) = typeCheckStmtsBlock ((L.SPrint pos e):tstmts)

typeCheckStmtsBlock ((L.SPrint pos e):tstmts) = do
    expType <- getTypeExp e
    case expType of
      TArr _ _ -> throwError $ posShow pos ++ "Cannot display array!"
      TFVoid -> throwError $ posShow pos ++ "Cannot display void result!"
      TFun _ _ -> throwError $ posShow pos ++ "Cannot display function!"
      _ -> typeCheckStmtsBlock tstmts

typeCheckStmtsBlock (L.SAssert pos e_cond emess : tstmts) = do
    condType <- getTypeExp e_cond
    messType <- getTypeExp emess
    assertType pos messType TString "assert message"
    if isArray condType
        then throwError $ posShow pos ++ "Array cannot be used as assert condition!"
    else if isFun condType
        then throwError $ posShow pos ++ "Function cannot be used as assert condition!"
    else typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.SExp pos e):tstmts) = do
    _ <- getTypeExp e
    typeCheckStmtsBlock tstmts

typeCheckStmtsBlock ((L.DeclConst pos varType (L.NoInit ipos (L.Ident tag))):tstmts) = do
    case varType of 
        (L.TArr _ _) -> throwError $ posShow ipos ++ "Initialising empty array is forbidden!"
        _ -> typeCheckStmtsBlock (L.DeclConst pos varType (L.Init ipos (L.Ident tag) $ defaultLitExp varType) : tstmts)

typeCheckStmtsBlock ((L.DeclConst pos varType (L.Init _ (L.Ident tag) e)):tstmts) = do
    expType <- getTypeExp e
    assertType pos expType (convertType varType) $ "initialising variable " ++ tag
    alreadyDeclared <- asks (isMember tag . declaredVariables)
    if alreadyDeclared
        then throwError $ posShow pos ++ "Variable " ++ tag ++ " already declared!"
        else local (appendObject tag expType . setConst tag True) (typeCheckStmtsBlock tstmts)

typeCheckStmtsBlock ((L.ArrSet pos (L.Ident tag) args e):tstmts) = do
    expType <- getTypeExp e
    curType <- asks (M.lookup tag . env)
    if isNothing curType
        then throwError $ posShow pos ++ "Array " ++ tag ++ " is not declared!"
    else do
        isConst <- asks (M.lookup tag . constStatus)
        evalArgs <- mapM getTypeExp (map (\(L.ArrArg _ x) -> x) args)
        let arrDim = ((\(TArr _ dim) -> dim) $ fromJust curType)
        assertTypeLst pos evalArgs (replicate (length evalArgs) TInt) "retreving element from array" 
        if length evalArgs < arrDim 
            then throwError $ posShow pos ++ "Not enough arguments for this array!"
        else if length evalArgs > arrDim
            then throwError $ posShow pos ++ "Too many arguments for this array!"
        else if isNothing curType
            then throwError $ posShow pos ++ "Array " ++ tag ++ " is not declared!"
        else if isConst == Just True
            then throwError $ posShow pos ++ "Constant array " ++ tag ++ " cannot be modified!"
        else if expType /= getArrayType (fromJust curType)
            then throwError $ posShow pos ++ "Cannot assign " ++ show expType ++ " as " ++ show (getArrayType $ fromJust curType) ++ " array element!"
        else typeCheckStmtsBlock tstmts

typeCheckStmtsBlock [] = return ()

---- RUN ----
runTypeCheck :: L.Programme -> Either String ()
runTypeCheck p@(L.Main _ funcs mainStms) =
    runReader (runExceptT (typeCheck p)) (readGlobals (concatMap getFunStmts funcs ++ mainStms) $ readFunctionsHeaders funcs initEnv)

getArgType :: L.Arg -> (String, (Type, Bool))
getArgType (L.ArgVal _ argType (L.Ident tag)) = (tag, (convertType argType, False))
getArgType (L.ArgRef _ argType (L.Ident tag)) = (tag, (convertType argType, True))

readFunctionsHeaders :: [L.TopDef] -> TypeCheckerEnv -> TypeCheckerEnv
readFunctionsHeaders ((L.FnDef pos (L.Ident fname) args retType stmts):t) env =
    readFunctionsHeaders t (appendObject fname curFn env) where
       curFn = TFun (convertType retType) $ map (snd . getArgType) args
readFunctionsHeaders ((L.FnDefVoid pos (L.Ident fname) args stmts):t) env =
    readFunctionsHeaders t (appendObject fname curFn env) where
       curFn = TFun TFVoid $ map (snd . getArgType) args
readFunctionsHeaders [] env = env

readGlobals :: [L.Stmt] -> TypeCheckerEnv -> TypeCheckerEnv
readGlobals (_ : t) env = env
readGlobals [] env = env

typeCheck :: L.Programme -> TypeCheckerMonad ()
typeCheck (L.Main _ funcs mainStms) = do
    mapM_ typeCheckFunction funcs
    typeCheckStmtsBlock mainStms
    return ()

typeCheckFunction :: L.TopDef -> TypeCheckerMonad ()
typeCheckFunction (L.FnDef pos (L.Ident fname) args retType stmts) = typeCheckFunctionUtil pos fname (map getArgType args) (convertType retType) stmts
typeCheckFunction (L.FnDefVoid pos (L.Ident fname) args stmts) = typeCheckFunctionUtil pos fname (map getArgType args) TFVoid stmts

typeCheckFunctionUtil :: Pos -> String -> [(String, (Type, Bool))] -> Type -> [L.Stmt] -> TypeCheckerMonad ()
typeCheckFunctionUtil pos fname args retType stmts = do 
    if length (nub (map (\(nm, (t, b)) -> nm) args)) /= length args
       then throwError $ posShow pos ++ "Function with a repeating argument name!"
    else if isArray retType 
        then throwError $ posShow pos ++ "Function cannot return an array (use referencing instead)!"
    else if isFun retType
        then throwError $ posShow pos ++ "Function can return only objects, not functions!"
        else local (clearForNewScope . setFunction retType . flip (foldr (uncurry appendObject)) (map (\(n, (t, b)) -> (n,t)) args)) $ typeCheckStmtsBlock stmts
