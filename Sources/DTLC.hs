{-# LANGUAGE MultiParamTypeClasses,
             TemplateHaskell,
             FlexibleInstances,
             UndecidableInstances #-}

import Prelude hiding (pi)
import Unbound.LocallyNameless
--import Control.Monad.Trans.Maybe
--import Control.Applicative
import Control.Monad.Except

data Term = Type
          | Pi (Bind (Name Term, Embed Term) Term)
          | Var (Name Term) -- name indexed by what they refer to
          | App Term Term
          | Lambda (Bind (Name Term, Embed Term) Term)
    deriving (Show)
type Type = Term
            
$(derive [''Term]) -- derivices boilerplate instances

instance Alpha Term

instance Subst Term Term where
    isvar (Var v) = Just (SubstName v)
    isvar _            = Nothing
 
-- Indexing

-- TODO: This might be a good idea if you want to support good error messages.
--       I'm not sure how I ought to do this with bindings though.
--       Also, it'd be best if the mechanism worked equally well for a tree with
--       line numbers, etc.

--data IndexStep = AppFunc | AppArg | BindingType | BindingBody
--type Index =  [IndexStep]

--index :: Term -> Index -> Term
--index term [] = term
--index (App func _) (AppFunc:xs) = index func xs
--index (App _ arg)  (AppArg:xs)  = index arg xs
--index (Pi term)    (BindingType:xs) = do
--    ((binding, Embed argType), body) <- unbind term
    

-- Helpers
        
lambda :: String -> Type -> Term -> Term
lambda name typeAnnotation result = Lambda $ bind boundName result
    where boundName = ((string2Name name), Embed typeAnnotation)

pi :: String -> Type -> Term -> Term
pi name typeAnnotation result = Pi $ bind boundName result
    where boundName = ((string2Name name), Embed typeAnnotation)

var :: String -> Term
var = Var . string2Name

-- type checking
-- note: a mutually recursive design would be better in future

type Context = [(Name Term, Type)]

data TypeError = VariableNotInScopeError 
               | ExpectedFunction
               | TypeMismatchError Term Type Type
    deriving Show

elseThrowError :: Maybe a -> TypeError -> Except TypeError a
elseThrowError (Just x) _     = return x
elseThrowError Nothing  error = throwError error

check :: Context -> Term -> Type -> FreshMT (Except TypeError) ()
check context term foundType = do 
    expectedType <- infer context term
    if expectedType `aeq` foundType -- TODO: definitional eq
        then return ()
        else throwError $ TypeMismatchError term expectedType foundType

-- Determine the type of a term
infer :: Context -> Term -> FreshMT (Except TypeError) Type

-- Types have type "type".
-- This makes our language inconsistent as a logic
-- See: Girard's paradox
-- TODO: Use universes to make logic consistent.
infer _ Type = return Type

-- Check the type of a variable using the context.
-- TODO: Give a more informative error message.
infer context (Var name) = lift $ 
    lookup name context `elseThrowError` VariableNotInScopeError

infer context (Lambda term) = do
    ((binding, Embed argType), body) <- unbind term
    check context Type argType
    bodyType <- infer ((binding, argType):context) body
    return $ Pi (bind (binding, Embed argType) bodyType)

infer context (Pi term) = do
    ((binding, Embed argType), body) <- unbind term
    check context Type argType
    check ((binding, argType):context) Type body
    return Type

infer context (App func arg) = do
    expectedFuncType <- infer context func
    case expectedFuncType of
        Pi funcType -> do
            ((binding, Embed expectedArgType), body) <- unbind funcType
            check context arg expectedArgType
            return $ subst binding arg body
        _ -> throwError ExpectedFunction -- TODO: args?


--infer context (Pi term) = do
--    ((binding, Embed argType), body) <- unbind term
    

--infer _ Unit            = return UnitType

--infer _ (BoolLiteral _) = return BoolType


--infer context (Application func arg) = do
--    funcType <- infer context func
--    case funcType of
--        FunctionType argType resultType -> check context arg argType >> return resultType
--        _ -> lift . throwError $ UnexpectedApplicationToNonFunctionTypeError func funcType arg

--infer context (Lambda term) = do
--    ((binding, Embed argType), body) <- unbind term
--    bodyType <- infer ((binding, argType):context) body
--    return $ FunctionType argType bodyType

runTypeInfer :: Term -> Except TypeError Type
runTypeInfer term = runFreshMT (infer [] term)

--runTypeCheck :: Term -> Type -> Except TypeError ()
--runTypeCheck term unvalidatedType = runFreshMT (check [] term unvalidatedType)


---- Small-step evaluation
--
--step :: Term -> MaybeT FreshM Term
--step Unit = mzero
--step (BoolLiteral _) = mzero
--step (Variable _) = mzero
--step (Lambda _) = mzero
--step (Application (Lambda abstraction) rightTerm) = do
--        ((name, _typeAnnotation), leftTerm) <- unbind abstraction
--        return $ subst name rightTerm leftTerm
--step (Application leftTerm rightTerm) = 
--        let reduceLeft = do leftTerm' <- step leftTerm
--                            return $ Application leftTerm' rightTerm in
--        let reduceRight = do rightTerm' <- step rightTerm
--                             return $ Application leftTerm rightTerm' in
--        reduceLeft <|> reduceRight
--
--reduce :: Term -> FreshM Term
--reduce term = do
--        result <- runMaybeT (step term)
--        case result of
--            Just term' -> reduce term'
--            Nothing -> return term
--
--eval :: Term -> Term
--eval term = runFreshM (reduce term)
--

---- Example

id' = lambda "t" Type $ 
          lambda "x" (var "t") $ 
              var "x"

bool' = pi "x" Type $
            pi "_" (var "x") $
                pi "_" (var "x") $
                    var "x"

true' = lambda "x" Type $
            lambda "y" (var "x") $
                lambda "z" (var "x") $
                    var "y"


--
---- BoolType
--program = (Application
--              (Application
--                  (lambda "x" BoolType $
--                      lambda "y" UnitType $
--                          var "x")
--                  (BoolLiteral False))
--              Unit)
--
