{-# LANGUAGE MultiParamTypeClasses,
             TemplateHaskell,
             FlexibleInstances,
             UndecidableInstances,
             ViewPatterns,
             TupleSections #-}

import Prelude hiding (pi)
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh
import Unbound.LocallyNameless.Ops (unsafeUnbind)
import Control.Monad.Trans.Reader (mapReaderT)
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Morph (MFunctor, hoist, generalize)

-- # Data Types

-- ## Terms - representation that can be type checked

-- TODO: "Inferable" is spelled wrong.
data CheckableTerm = TInferrable InferrableTerm
                   | TLambda (Bind (TermName) CheckableTerm)
                   | TFix (Bind (TermName) CheckableTerm)
    deriving Show
type CheckableType = CheckableTerm
                          
data InferrableTerm = TStar
                    | TAnnotation CheckableTerm CheckableType
                    | TPi (Bind (TermName, Embed CheckableType) CheckableType)
                    | TVariable (TermName)
                    | TApplication InferrableTerm CheckableTerm
                    | TLet (Bind (TermName, Embed InferrableTerm) InferrableTerm)
    deriving Show
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
    where boundName = string2Name varName

letIn :: String -> InferrableTerm -> InferrableTerm -> InferrableTerm
letIn varName var body = TLet (bind (string2Name varName, embed var) body)

letIn' :: String -> CheckableType -> CheckableTerm -> InferrableTerm -> InferrableTerm
letIn' varName varType var body = letIn varName (TAnnotation var varType) body

pi :: String -> CheckableTerm -> CheckableTerm -> InferrableTerm
pi varName varType bodyType = TPi $ bind boundName bodyType
    where boundName = (string2Name varName, embed varType)

(-->) :: CheckableTerm -> CheckableTerm -> InferrableTerm
(-->) = pi "_"
infix 1 -->

var :: String -> InferrableTerm
var = TVariable . string2Name

(@@) :: InferrableTerm -> CheckableTerm -> InferrableTerm
(@@) = TApplication
infixl 9 @@

inf :: InferrableTerm -> CheckableTerm
inf = TInferrable

-- ### Program

--data Scope = Empty | Cons (Bind (TermName, Embed InferrableTerm) Scope)

--data TermProgram = TermProgram (TRec [(TermName, Embed InferrableTerm)])
--data ExprProgram = ExprProgram (TRec [(Name Expr, Embed Expr)])

-- ## Expressions - representation that can be evaluated

data Expr = EStar
          | EPi (Bind (Name Expr, Embed TypeExpr) TypeExpr)
          | ELambda (Bind (Name Expr) Expr)
          | EVariable (Name Expr)
          | EApplication Expr Expr
          | EFix (Bind (Name Expr) Expr)
          | ELet (Bind (Name Expr, Embed Expr) Expr)
        
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
              show' (EFix func) = lunbind func $ \(funcName, body) -> do
                  bodyString <- show' body
                  return $ "fix(λ" ++ name2String funcName
                                   ++ "." ++ bodyString ++ ")"
              show' (ELet term) = lunbind term $ \((varName, unembed -> var), body) -> do
                  varString <- show' var
                  bodyString <- show' body
                  return $ "(let " ++ name2String varName ++ " = " ++ varString ++ " in " ++ bodyString ++ ")"
                
              containsFree :: Name Expr -> Expr -> Bool
              containsFree name term = name `elem` (fv term :: [Name Expr])

-- ## Values - representation that cannot be further evaluated

data Value = VStar
           | VPi (Bind (Name Expr, Embed TypeExpr) TypeExpr)
           | VLambda (Bind (Name Expr) Expr)
           | VNeutral NeutralValue
           | VFix (Bind (Name Value) Value) -- evaluated body!

data NeutralValue = VVariable (Name Value)
                  | VApplication NeutralValue Value

type TypeValue = Value
        
$(derive [''Value, ''NeutralValue])

instance Alpha Value
instance Alpha NeutralValue

--instance Subst Expr Value where
--    isvar (EVariable v) = Just (SubstName v)
--    isvar _             = Nothing

instance Show Value where
    show = show . expr
instance Show NeutralValue where
    show = show . VNeutral
    
-- # Small-Step Evaluation

instance (LFresh m) => LFresh (EitherT a m) where
  lfresh = lift . lfresh
  avoid  = mapEitherT . avoid
  getAvoids = lift getAvoids

step :: Expr -> EitherT Value LFreshM Expr
step EStar           = throwError VStar
step (EPi func)      = throwError (VPi func)
step (ELambda func)  = throwError (VLambda func)
step (EVariable var) = throwError (VNeutral $ VVariable $ translate var)
step (EApplication (EPi func) arg) = lunbind func $ \((varName, _), bodyType) ->
    return $ subst varName arg bodyType
step (EApplication (ELambda func) arg) = lunbind func $ \(varName, body) ->
    return $ subst varName arg body
step (EApplication func arg) = (step func >>= \func' -> return $ EApplication func' arg)
            `catchError` \_ -> (step arg  >>= \arg'  -> return $ EApplication func arg')
step (EFix func) = lunbind func $ \(funcName, body) ->
    return $ subst funcName (EFix func) body
step (ELet term) = lunbind term $ \((varName, unembed -> var), body) -> 
    (step var >>= \var' -> return $ ELet $ bind (varName, embed var') body)
    `catchError` \_ -> return $ subst varName var body

evaluate :: Expr -> LFreshM Value
evaluate expr = eitherT return evaluate (step expr)

instance MFunctor LFreshMT where
    hoist f = LFreshMT . (mapReaderT f) . unLFreshMT

mapBound :: (Alpha t, Alpha v) => (t -> v) -> Bind (Name t) t -> Bind (Name v) v
mapBound f (unsafeUnbind -> (name, value)) = bind (translate name) (f value)

expr :: Value -> Expr
expr VStar            = EStar
expr (VPi func)       = EPi func
expr (VLambda func)   = ELambda func
expr (VNeutral value) = expr' value
    where expr' :: NeutralValue -> Expr
          expr' (VVariable var) = EVariable (translate var)
          expr' (VApplication func arg) = EApplication (expr' func) (expr arg)
expr (VFix value) = EFix $ mapBound expr value

-- # Type Checking

type Context = [(TermName, TypeValue)]

type ExpectedType = TypeValue
type FoundType = TypeValue
data TypeError = TypeMismatchFoundPiType ExpectedType Context
               | TypeMismatch FoundType ExpectedType
               | TypesNotEqual FoundType FoundType
               | ApplicationToNonPiType FoundType
               | FixReturnsNonPiType FoundType
               | VariableNotInScope String
               | UnableToDeduceHoleFromContext
            
instance Show TypeError where
    show (TypeMismatchFoundPiType expectedType context) =
        "Type mismatch: expected " ++ show expectedType ++ " but found pi type in context " ++ show context
    show (TypeMismatch foundType expectedType) =
        "Type mismatch: expected " ++ show expectedType ++ " but found " ++ show foundType
    show (TypesNotEqual firstType secondType) =
        "Type mismatch: expected " ++ show firstType ++ " and " ++ show secondType ++ " to match"
    show (ApplicationToNonPiType foundType) =
        "Function application requires function to be pi type, not " ++ show foundType
    show (FixReturnsNonPiType foundType) =
        "Fix expression must be a function type, but expected " ++ show foundType
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
    varType <- lookup name context `elseThrowError` VariableNotInScope (name2String name)
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
infer context (TLet term) = lunbind term $ \((varName, unembed -> var), body) -> do
    (var', _) <- infer context var -- FIXME: Duplicate work inferring var multiple times after subst.
    (body, bodyType) <- infer context (subst varName var body)
    return (ELet $ bind (translate varName, embed var') body, bodyType)
  
check :: Context -> CheckableTerm -> TypeValue -> LFreshMT (Except TypeError) Expr
check context (TInferrable term) expectedType = do
    (elab, actualType) <- infer context term 
    unless (expectedType `aeq` actualType) $ throwError $ TypeMismatch actualType expectedType
    return elab
check context (TLambda term) (VPi termType) = lunbind termType $ \((piVarName, unembed -> varType), bodyType) ->
                                              lunbind term $ \(lambdaVarName, body) -> do
    varType  <- hoist generalize (evaluate varType)
    let rename = subst piVarName (EVariable $ translate lambdaVarName)
    bodyType <- hoist generalize (evaluate $ rename bodyType)
    body <- check ((lambdaVarName, varType):context) body bodyType
    return $ ELambda $ bind (translate lambdaVarName) body
  
check context (TLambda _) expectedType = throwError $ TypeMismatchFoundPiType expectedType context
-- {t : *} -> ((t -> t) -> (t -> t)) -> (t -> t)
check context (TFix func) expectedType = do
  case expectedType of
      VPi funcType -> lunbind funcType $ \((_, unembed -> varType), bodyType) ->
                      lunbind func $ \(funcName, body) -> do
          varType  <- hoist generalize (evaluate varType)
          bodyType <- hoist generalize (evaluate bodyType)
          unless (varType `aeq` bodyType) $ throwError $ TypesNotEqual varType bodyType
          body <- check context body bodyType
          return $ EFix $ bind (translate funcName) body
      _ -> throwError $ FixReturnsNonPiType expectedType

check' :: Context -> CheckableTerm -> CheckableType -> LFreshMT (Except TypeError) Expr
check' context term typeTerm = checkEvalType context typeTerm >>= check context term

run :: InferrableTerm -> LFreshMT (Except TypeError) (Value, TypeValue)
run term = do
    (expr, typ) <- infer [] term 
    value <- hoist generalize (evaluate expr)
    return (value, typ)

-- # Examples

--foo = pi "t" (lambda "bar" $ (inf . var) "bar") $ inf $
--        (inf . var) "t" --> (inf . var) "t"

--program = 
--    letIn "id" (TAnnotation )

idT = TAnnotation idTerm (inf idType)
    where idTerm = lambda "T" $ lambda "x" $ 
                       inf (var "x")
          idType = pi "T" (inf TStar) $ inf $
                       inf (var "T") --> inf (var "T")              

boolT = pi "T" (inf TStar) $ inf $
            inf (var "T") --> inf (inf (var "T") --> inf (var "T"))

falseT = TAnnotation falseTerm (inf boolT)
    where falseTerm = lambda "T" $ lambda "t" $ lambda "f" $
                          inf $ var "f"

trueT = TAnnotation falseTerm (inf boolT)
    where falseTerm = lambda "T" $ lambda "t" $ lambda "f" $
                          inf $ var "t"

notT = TAnnotation notTerm (inf $ inf boolT --> inf boolT)
    where notTerm = lambda "b" $ lambda "T" $ lambda "t" $ lambda "f" $
                        inf $ var "b" @@ inf (var "T") @@ inf (var "f") @@ inf (var "t")

constT = TAnnotation constTerm (inf constType)
    where constTerm = lambda "T" $ lambda "x" $
                          inf $ var "x"
          constType = pi "T" (inf TStar) $ inf $ pi "V" (inf TStar) $ inf $
                          inf (var "T") --> inf (inf (var "V") --> inf (var "T"))

program = 
  letIn "nat" (pi "A" (inf TStar) $ inf $
                   inf (inf (var "A") --> inf (var "A")) --> inf (inf (var "A") --> inf (var "A"))) $
  letIn' "zero" (inf $ var "nat")
                (lambda "T" $ lambda "succ" $ lambda "zero" $
                     inf $ var "zero") $
  letIn' "succ" (inf $ inf (var "nat") --> inf (var "nat"))
                (lambda "n" $ lambda "T" $ lambda "succ" $ lambda "zero" $
                     inf $ var "succ" @@ inf (var "n" @@ inf (var "T") @@ inf (var "succ") @@ inf (var "zero"))) $
  var "succ" @@ (inf ((var "succ") @@ inf (var "zero")))
  

--test :: LFreshM CheckableType
--test = let (TPi term) = natT in lunbind term $ \((varName, unembed -> _), bodyType) -> do
--    return $ subst varName (var "Y") bodyType


--
--zeroT = TAnnotation zeroTerm (inf natT)
--    where zeroTerm = lambda "T" $ lambda "succ" $ lambda "zero" $
--                         inf $ var "zero"
--                   
--oneT = TAnnotation oneTerm (inf natT)
--    where oneTerm = lambda "T" $ lambda "succ" $ lambda "zero" $
--                        inf $ var "succ" @@ inf (var "zero")
--
--twoT = TAnnotation twoTerm (inf natT)
--    where twoTerm = lambda "T" $ lambda "succ" $ lambda "zero" $
--                        inf $ var "succ" @@ inf (var "succ" @@ inf (var "zero"))
--
--succT = TAnnotation succTerm (inf $ inf natT --> inf natT)
--    where succTerm = lambda "n" $ lambda "T" $ lambda "succ" $ lambda "zero" $
--                         inf $ var "succ" @@ inf (var "n" @@ inf (var "T") @@ inf (var "succ") @@ inf (var "zero")) 


--testSuccIsSuccType = runLFreshMT $ infer [] succAnnotT
--
--testSuccOneIsNat = runLFreshMT $ infer [] (TAnnotation (inf (succAnnotT @@ oneT)) (inf natTypeT))


--test = Program . trec $ [
--        (string2Name "Bool", embed $ TAnnotation (inf boolT) (inf TStar)),
--        (string2Name "true", embed $ TAnnotation constT (inf $ var "Bool"))
--    ]


--test = Program . trec $ [
--        (string2Name "id", embed $ TAnnotation idT (inf idTypeT)),
--        (string2Name "const", embed $ TAnnotation constT constT)
--    ]
  
-- Might loop forever in case of recursion or mutual recursion
--infer' :: Program -> LFreshMT (Except TypeError) [(TermName, (Expr, TypeValue)]
--infer' (Program (luntrec -> exprs)) = _

  
--callIdT = (TAnnotation idT $ inf idTypeT) @@ inf idTypeT @@ idT

--ExceptT (Identity (Right (λT.λsucc.λzero.(succ) ((((λT.λsucc.λzero.(succ) ((succ) (zero))) (T)) (succ)) (zero)),(A:*) -> ((A) -> A) -> (A) -> A)))
