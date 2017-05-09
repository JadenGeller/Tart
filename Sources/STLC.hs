{-# LANGUAGE MultiParamTypeClasses,
             TemplateHaskell,
             FlexibleInstances,
             UndecidableInstances
  #-}

import Unbound.LocallyNameless
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad

data Type = UnitType | BoolType | FunctionType Type Type
    deriving (Show, Eq)

data Term = Unit
          | BoolLiteral Bool
          | Variable (Name Term) -- name indexed by what they refer to
          | Application Term Term
          | Lambda (Bind (Name Term, Embed Type) Term)
    deriving Show
        
$(derive [''Type, ''Term]) -- derivices boilerplate instances

instance Alpha Type
instance Alpha Term
    -- automatically derived
    
instance Subst Term Type where
instance Subst Type Type where
instance Subst Term Term where
    isvar (Variable v) = Just (SubstName v)
    isvar _            = Nothing
 
-- Helpers
    
lambda :: String -> Type -> Term -> Term
lambda name typeAnnotation result = Lambda $ bind boundName result
    where boundName = ((string2Name name), Embed typeAnnotation)

var :: String -> Term
var = Variable . string2Name

-- Small-step evaluation

step :: Term -> MaybeT FreshM Term
step Unit = mzero
step (BoolLiteral _) = mzero
step (Variable _) = mzero
step (Lambda _) = mzero
step (Application (Lambda abstraction) rightTerm) = do
    ((name, _typeAnnotation), leftTerm) <- unbind abstraction
    return $ subst name rightTerm leftTerm
step (Application leftTerm rightTerm) = 
    let reduceLeft = do leftTerm' <- step leftTerm
                        return $ Application leftTerm' rightTerm in
    let reduceRight = do rightTerm' <- step rightTerm
                         return $ Application leftTerm rightTerm' in
    reduceLeft <|> reduceRight

reduce :: Term -> FreshM Term
reduce term = do
    result <- runMaybeT (step term)
    case result of
        Just term' -> reduce term'
        Nothing -> return term

eval :: Term -> Term
eval term = runFreshM (reduce term)

-- type checking
-- note: a mutually recursive design would be better in future

type Context = [(Name Term, Type)]

check :: Context -> Term -> Maybe Type -> FreshMT (Either String) Type
-- Check unit type
check _context Unit Nothing             = return UnitType
check _context Unit (Just UnitType)     = return UnitType
check _context Unit (Just expectedType) = fail $ "Expected unit, found " ++ show expectedType
-- Check bool type
check _context (BoolLiteral _) Nothing             = return BoolType
check _context (BoolLiteral _) (Just BoolType)     = return BoolType
check _context (BoolLiteral _) (Just expectedType) = fail $ "Expected bool, found " ++ show expectedType
-- Check variable type
check context (Variable name) expectedType = case (lookup name context, expectedType) of
    (Just foundType, Just expectedType) | foundType == expectedType -> return foundType
                                        | otherwise -> fail $ "Variable " ++ show name ++ " of type " ++ show foundType
                                                               ++ " was expected to be of type " ++ show expectedType
    (Just foundType, Nothing) -> return foundType
    (Nothing, _) -> fail $ "Reference to unbound variable " ++ show name
-- Check application type
check context (Application function argument) expectedType = do
    functionType <- check context function Nothing
    case (functionType, expectedType) of
        (FunctionType argumentType resultType, Just expectedResultType) | resultType == expectedResultType 
                                                                            -> check context argument (Just argumentType) >> return resultType
                                                                        | otherwise 
                                                                            -> fail $ "Function application results in type " ++ show resultType
                                                                                  ++ " instead of expected type " ++ show expectedResultType
        (FunctionType argumentType resultType, Nothing) -> check context argument (Just argumentType) >> return resultType
        (foundType, _) -> fail $ "Application of non-function type " ++ show foundType 
check context (Lambda abstraction) Nothing = do
    ((binding, Embed argType), body) <- unbind abstraction
    returnType <- check ((binding, argType):context) body Nothing
    return $ FunctionType argType returnType
check context (Lambda abstraction) (Just lambdaType@(FunctionType expectedArgumentType expectedResultType)) = do
    ((binding, Embed argType), body) <- unbind abstraction
    if (argType /= expectedArgumentType) 
        then fail $ "Expected lambda of type " ++ show lambdaType
        else check ((binding, argType):context) body (Just expectedResultType) >> return lambdaType
check _context (Lambda _) incorrectType = fail $ "Lambda incorrectly of type " ++ show incorrectType ++ " which is not a function type"
--check context (Application leftTerm rightTerm) = do
--    leftValid <- check context leftTerm (FunctionType )

runTypeCheck :: Term -> Either String Type
runTypeCheck term = runFreshMT (check [] term Nothing)

-- Example

test = Application (lambda "x" BoolType (lambda "x" BoolType Unit) ) (BoolLiteral False)

program = (Application 
    (Application 
        (lambda "x" BoolType $
            lambda "y" UnitType $
                var "x") 
        (BoolLiteral False))
    Unit)

