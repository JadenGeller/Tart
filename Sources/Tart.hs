{-# LANGUAGE MultiParamTypeClasses,
             TemplateHaskell,
             FlexibleInstances,
             UndecidableInstances,
             ViewPatterns,
             TupleSections #-}

import Prelude hiding (pi)
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh
import Control.Monad.Trans.Reader (mapReaderT)
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Morph (MFunctor, hoist, generalize)

-- # Data Types

-- ## Terms - representation that can be type checked

data CheckableTerm = TInferrable InferrableTerm
                   | TLambda (Bind (TermName) CheckableTerm)
type CheckableType = CheckableTerm
                          
data InferrableTerm = TStar
                    | TAnnotation CheckableTerm CheckableType
                    | TPi (Bind (TermName, Embed CheckableType) CheckableType)
                    | TVariable (TermName)
                    | TApplication InferrableTerm CheckableTerm

type TermName = Name InferrableTerm

$(derive [''CheckableTerm, ''InferrableTerm])

instance Alpha CheckableTerm
instance Alpha InferrableTerm
            
instance Subst InferrableTerm CheckableTerm where
instance Subst InferrableTerm InferrableTerm where
    isvar (TVariable v) = Just (SubstName v)
    isvar _             = Nothing
    
-- ### Conveniences
    
lambda :: String -> CheckableTerm -> CheckableTerm
lambda varName body = TLambda $ bind boundName body
    where boundName = (string2Name varName)

pi :: String -> CheckableTerm -> CheckableTerm -> InferrableTerm
pi varName varType bodyType = TPi $ bind boundName bodyType
    where boundName = ((string2Name varName), embed varType)

(-->) :: CheckableTerm -> CheckableTerm -> InferrableTerm
(-->) = pi "_"
infixr 1 -->

var :: String -> InferrableTerm
var = TVariable . string2Name

(@@) :: InferrableTerm -> CheckableTerm -> InferrableTerm
(@@) = TApplication
infixl 9 @@

inf :: InferrableTerm -> CheckableTerm
inf = TInferrable

-- ## Expressions - representation that can be evaluated

data Expr = EStar
          | EPi (Bind (Name Expr, Embed TypeExpr) TypeExpr)
          | ELambda (Bind (Name Expr) Expr)
          | EVariable (Name Expr)
          | EApplication Expr Expr
        
type TypeExpr = Expr

$(derive [''Expr])

instance Alpha Expr
            
instance Subst Expr Expr where
    isvar (EVariable v) = Just (SubstName v)
    isvar _             = Nothing

instance Show Expr where
    show = runLFreshM . show'
        where show' :: LFresh m => Expr -> m String
              show' EStar = return "*"
              show' (EPi term) = lunbind term $ \((varName, unembed -> varType), bodyType) -> do
                  varTypeString <- show' varType
                  bodyTypeString <- show' bodyType
                  if containsFree varName bodyType
                      then return $ "(" ++ name2String varName ++ ":" ++ varTypeString ++ ") -> " ++ bodyTypeString
                      else return $ "(" ++ varTypeString ++ ")" ++ " -> " ++ bodyTypeString
              show' (EVariable name) = return $ name2String name
              show' (EApplication func arg) = do
                  funcString <- show' func
                  argString <- show' arg
                  return $ "(" ++ funcString ++ ")" ++ " " ++ "(" ++ argString ++ ")"
              show' (ELambda term) = lunbind term $ \(varName, body) -> do
                  bodyString <- show' body
                  return $ "λ" ++ name2String varName 
                               ++ "." ++ bodyString
                
              containsFree :: Name Expr -> Expr -> Bool
              containsFree name term = name `elem` (fv term :: [Name Expr])

-- ## Values - representation that cannot be further evaluated

data Value = VStar
           | VPi (Bind (Name Expr, Embed TypeExpr) TypeExpr)
           | VLambda (Bind (Name Expr) Expr)
           | VNeutral NeutralValue

data NeutralValue = VVariable (Name Value)
                  | VApplication NeutralValue Value

type TypeValue = Value
        
$(derive [''Value, ''NeutralValue])

instance Alpha Value
instance Alpha NeutralValue

instance Show Value where
    show = show . expr
instance Show NeutralValue where
    show = show . VNeutral
    
-- # Small-Step Evaluation

step :: Expr -> EitherT Value LFreshM Expr
step EStar           = throwError VStar
step (EPi func)      = throwError (VPi func)
step (ELambda func)  = throwError (VLambda func)
step (EVariable var) = throwError (VNeutral $ VVariable $ translate var)
step (EApplication (EPi func) arg) = lift . lunbind func $ \((varName, _), bodyType) ->
    return $ subst varName arg bodyType
step (EApplication (ELambda func) arg) = lift . lunbind func $ \(varName, body) ->
    return $ subst varName arg body
step (EApplication func arg) = (step func >>= \func' -> return $ EApplication func' arg)
            `catchError` \_ -> (step arg  >>= \arg'  -> return $ EApplication func arg')

evaluate :: Expr -> LFreshM Value
evaluate expr = eitherT return evaluate (step expr)

instance MFunctor LFreshMT where
    hoist f = LFreshMT . (mapReaderT f) . unLFreshMT

expr :: Value -> Expr
expr VStar            = EStar
expr (VPi func)       = EPi func
expr (VLambda func)   = ELambda func
expr (VNeutral value) = expr' value
    where expr' :: NeutralValue -> Expr
          expr' (VVariable var) = EVariable (translate var)
          expr' (VApplication func arg) = EApplication (expr' func) (expr arg)

-- # Type Checking

type Context = [(TermName, TypeValue)]

type ExpectedType = TypeValue
type FoundType = TypeValue
data TypeError = TypeMismatchFoundPiType ExpectedType
               | TypeMismatch FoundType ExpectedType
               | ApplicationToNonPiType FoundType
               | VariableNotInScope String
               | UnableToDeduceHoleFromContext
            
instance Show TypeError where
    show (TypeMismatchFoundPiType expectedType) =
        "Type mismatch: expected " ++ show expectedType ++ " but found pi type"
    show (TypeMismatch foundType expectedType) =
        "Type mismatch: expected " ++ show expectedType ++ " but found " ++ show foundType
    show (ApplicationToNonPiType foundType) =
        "Function application requires function to be pi type, not " ++ show foundType
    show (VariableNotInScope name) =
        "Variable named " ++ show name ++ "is not in scope"
    show UnableToDeduceHoleFromContext =
        "Unable to deduce hole value from context"

elseThrowError :: Maybe a -> TypeError -> Except TypeError a
elseThrowError (Just x) _     = return x
elseThrowError Nothing  error = throwError error

checkEvalType :: Context -> CheckableType -> LFreshMT (Except TypeError) TypeValue
checkEvalType context termType = check context termType VStar >>= hoist generalize . evaluate

infer :: Context -> InferrableTerm -> LFreshMT (Except TypeError) (Expr, TypeValue)
infer _ TStar = return (EStar, VStar)
infer context (TAnnotation term termType) = do
    termType <- checkEvalType context termType
    term <- check context term termType
    return (term, termType)
infer context (TPi term) = lunbind term $ \((varName, unembed -> varType), bodyType) -> do
    varType <- checkEvalType context varType
    bodyType <- check ((varName, varType):context) bodyType VStar
    return (EPi $ bind (translate varName, (embed . expr) varType) bodyType, VStar)
infer context (TVariable name) = lift $ do
    varType <- (lookup name context `elseThrowError` VariableNotInScope (name2String name))
    return (EVariable (translate name), varType)
infer context (TApplication func arg) = do
    (func, funcType) <- infer context func
    case funcType of
        VPi funcType -> lunbind funcType $ \((varName, unembed -> varType), body) -> do
            varType <- hoist generalize (evaluate varType)
            arg <- check context arg varType
            let bodyType = subst varName arg body
            bodyType <- hoist generalize (evaluate bodyType)
            return (EApplication func arg, bodyType)
        actualType -> throwError $ ApplicationToNonPiType actualType

check :: Context -> CheckableTerm -> TypeValue -> LFreshMT (Except TypeError) Expr
check context (TInferrable term) expectedType = do
    (elab, actualType) <- infer context term 
    unless (expectedType `aeq` actualType) $ throwError $ TypeMismatch actualType expectedType
    return elab
check context (TLambda term) (VPi termType) = lunbind termType $ \((piVarName, unembed -> varType), bodyType) ->
                                              lunbind term $ \(lambdaVarName, body) -> do
    varType  <- hoist generalize (evaluate varType)
    bodyType <- hoist generalize (evaluate bodyType)
    body <- check ((translate piVarName, varType):(lambdaVarName, varType):context) body bodyType
    return $ ELambda $ bind (translate lambdaVarName) body
check _ (TLambda _) expectedType = throwError $ TypeMismatchFoundPiType expectedType

check' :: Context -> CheckableTerm -> CheckableType -> LFreshMT (Except TypeError) Expr
check' context term typeTerm = checkEvalType context typeTerm >>= check context term

-- # Examples

--foo = pi "t" (lambda "bar" $ (inf . var) "bar") $ inf $
--        (inf . var) "t" --> (inf . var) "t"

idTypeT = pi "t" (inf TStar) $ inf $
              (inf . var) "t" --> (inf . var) "t"

idT = lambda "t" $
          lambda "x" $
              (inf . var) "x"

callIdT = (TAnnotation idT $ inf idTypeT) @@ inf idTypeT @@ idT


-- 1) global variables


