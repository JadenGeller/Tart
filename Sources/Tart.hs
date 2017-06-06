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
import Control.Monad.Except hiding (fix)
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

fix :: String -> CheckableTerm -> CheckableTerm
fix funcName body = TFix (bind (string2Name funcName) body)

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

evaluate :: Expr -> LFreshM Value
evaluate EStar                   = return $ VStar
evaluate (EPi func)              = return $ VPi func
evaluate (ELambda func)          = return $ VLambda func
evaluate (EVariable var)         = return $ VNeutral $ VVariable $ translate var
evaluate (EApplication func arg) = do
    func <- evaluate func
    arg <- evaluate arg
    case func of
        VPi func -> lunbind func $ \((varName, _), bodyType) -> do
            evaluate $ subst varName (expr arg) bodyType
        VLambda func -> lunbind func $ \(varName, body) -> do
            evaluate $ subst varName (expr arg) body
        _ -> error "precondition failure"
evaluate (EFix func) = lunbind func $ \(funcName, body) -> do
    self <- evaluate (EFix func)
    evaluate $ subst funcName (expr self) body
evaluate (ELet term) = lunbind term $ \((varName, unembed -> var), body) -> do
    var <- evaluate var
    evaluate $ subst varName (expr var) body

-- ((a -> b) -> (a -> b)) -> (a -> b)

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
               | ApplicationToNonPiType Expr CheckableTerm FoundType
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
    show (ApplicationToNonPiType func arg foundType) =
        "Function application of " ++ show func ++ " to " ++ show arg ++ 
        " requires function to be pi type, not " ++ show foundType
    show (FixReturnsNonPiType foundType) =
        "Fix expression must be a function type, but expected " ++ show foundType
    show (VariableNotInScope name) =
        "Variable named " ++ show name ++ " is not in scope"
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
        actualType -> throwError $ ApplicationToNonPiType func arg actualType
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
check context (TFix func) expectedType = lunbind func $ \(funcName, body) -> do
    body <- check ((funcName, expectedType):context) body expectedType
    return $ EFix $ bind (translate funcName) body

check' :: Context -> CheckableTerm -> CheckableType -> LFreshMT (Except TypeError) Expr
check' context term typeTerm = checkEvalType context typeTerm >>= check context term

run :: InferrableTerm -> LFreshMT (Except TypeError) (Value, TypeValue)
run term = do
    (expr, typ) <- infer [] term 
    value <- hoist generalize (evaluate expr)
    return (value, typ)

-- # Examples

withCore :: InferrableTerm -> InferrableTerm
withCore term = 

  letIn' "id" (inf $ pi "T" (inf TStar) $ inf $
                   inf (var "T") --> inf (var "T"))
              (lambda "T" $ lambda "x" $ 
                   inf (var "x")) $

  letIn' "const" (inf $ pi "T" (inf TStar) $ inf $ pi "V" (inf TStar) $ inf $
                      inf (var "T") --> inf (inf (var "V") --> inf (var "T")))
                 (lambda "T" $ lambda "V" $ lambda "x" $ lambda "y" $
                      inf (var "x")) $

  letIn' "seq" (inf $ pi "T" (inf TStar) $ inf $ pi "V" (inf TStar) $ inf $
                      inf (var "T") --> inf (inf (var "V") --> inf (var "V")))
                 (lambda "T" $ lambda "V" $ lambda "x" $ lambda "y" $
                      inf (var "y")) $

  letIn "bool" (pi "T" (inf TStar) $ inf $
                    inf (var "T") --> inf (inf (var "T") --> inf (var "T"))) $
                
  letIn' "true" (inf $ var "bool")
                (lambda "T" $ inf $
                    var "const" @@ inf (var "T") @@ inf (var "T")) $
                                          
  letIn' "false" (inf $ var "bool")
                (lambda "T" $ inf $
                    var "seq" @@ inf (var "T") @@ inf (var "T")) $
                    
  letIn' "not" (inf $ inf (var "bool") --> inf (var "bool"))
               (lambda "b" $ lambda "T" $ lambda "t" $ lambda "f" $
                    inf $ var "b" @@ inf (var "T") @@ inf (var "f") @@ inf (var "t")) $
                                          
  letIn "nat" (pi "A" (inf TStar) $ inf $
                   inf (inf (var "A") --> inf (var "A")) --> inf (inf (var "A") --> inf (var "A"))) $
                  
  letIn' "zero" (inf $ var "nat")
                (lambda "T" $ lambda "succ" $ lambda "zero" $
                     inf $ var "zero") $
                    
  letIn' "succ" (inf $ inf (var "nat") --> inf (var "nat"))
                (lambda "n" $ lambda "T" $ lambda "succ" $ lambda "zero" $
                     inf $ var "succ" @@ inf (var "n" @@ inf (var "T") @@ inf (var "succ") @@ inf (var "zero"))) $
                    
  letIn' "absurd" (inf $ pi "A" (inf TStar) $ (inf $ var "A"))
                 (lambda "A" $ fix "x" $ inf $ var "x") $
  
  letIn' "pair" (inf $ pi "A" (inf TStar) $ inf $ pi "B" (inf TStar) $ inf TStar)
                (lambda "A" $ lambda "B" $ inf $ 
                     pi "C" (inf TStar) $ inf $
                         (inf $ (inf $ var "A") --> inf ((inf $ var "B") --> (inf $ var "C"))) --> inf (
                              var "C")) $

  letIn' "makePair" (inf $ pi "A" (inf TStar) $ inf $ pi "B" (inf TStar) $ inf $
                         inf (var "A") --> inf (
                         inf (var "B") --> inf (
                         var "pair" @@ inf (var "A") @@ inf (var "B"))))
                    (lambda "A" $ lambda "B" $ lambda "x" $ lambda "y" $
                         lambda "C" $ lambda "f" $ inf $
                             var "f" @@ inf (var "x") @@ inf (var "y")) $
                    
  letIn' "first" (inf $ pi "A" (inf TStar) $ inf $ pi "B" (inf TStar) $ inf $
                      (inf $ var "pair" @@ inf (var "A") @@ inf (var "B")) --> 
                      inf (var "A"))
                 (lambda "A" $ lambda "B" $ 
                     lambda "p" $ inf $
                         var "p" @@ inf (var "A") @@ inf (var "const" @@ inf (var "A") @@ inf (var "B"))) $

  letIn' "second" (inf $ pi "A" (inf TStar) $ inf $ pi "B" (inf TStar) $ inf $
                        (inf $ var "pair" @@ inf (var "A") @@ inf (var "B")) --> 
                        inf (var "B"))
                   (lambda "A" $ lambda "B" $ 
                       lambda "p" $ inf $
                           var "p" @@ inf (var "B") @@ inf (var "seq" @@ inf (var "A") @@ inf (var "B"))) $

  term
  
program = withCore $ var "succ" @@ (inf ((var "succ") @@ inf (var "zero")))
--program = withCore $ (var "false") @@ inf TStar
--program = withCore $ (var "false")
--program = withCore $
--    letIn' "test" (inf $ pi "A" (inf TStar) $ (inf TStar))
--                  (lambda "A" $ fix " x" $ inf $ TStar) $
--    var "test" @@ inf TStar

