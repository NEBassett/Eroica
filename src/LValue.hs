module LValue
  (
    Value (..),
    LError (..),
    ThrowsError (..),
    IOThrowsError(..),
    LispParser(..),
    Env,
    trapError,
    extractValue,
    showv,
    showe
  ) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec
import Text.Parsec
import Data.IORef
import Data.Maybe
import qualified Data.Map as M


type LispParser m = ParsecT String () IOThrowsError m

type Env = IORef (M.Map String (IORef Value))

data Value = Symbol String
           | List [Value]
           | DottedList [Value] Value
           | Number Double
           | String String
           | Bool Bool
           | PrimitiveFunction (Env -> [Value] -> IOThrowsError Value)
           | Func {paramVars :: [String], varargs :: (Maybe String), body :: [Value], closure :: Env }
           | Stream String
           | LParser (LispParser Value)

           -- this error type is used by the compiler aswell for smooth monadic semantics
data LError = NumArgs Integer [Value] 
            | TypeMismatch String Value
            | Parser ParseError
            | BadSpecialForm String Value
            | NotFunction String Value
            | UnboundVar String String
            | WrongTime String Value
            | Default String

type ThrowsError = Either LError

type IOThrowsError = ExceptT LError IO

instance Show LError where show = showe
instance Show Value where show = showv
showe :: LError -> String
showe (UnboundVar message varname) = message ++ ": " ++ varname
showe (BadSpecialForm message form) = message ++ ": " ++ show form
showe (NotFunction message func) = message ++ ": " ++ showv func
showe (NumArgs expected found) = "Expected: " ++ show expected ++ " args; found values " ++ (unwords (map show found))
showe (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showe (Parser parseErr) = "Parse error at " ++ show parseErr
showe (WrongTime m v) = "Unsupported functionality: " ++ m ++ (show v)

showv :: Value -> String
showv (LParser _) = "<parser>"
showv (Symbol s) = s
showv (String s) = "\"" ++ s ++ "\""
showv (Number n) = show n
showv (Bool True) = "#t"
showv (Bool False) = "#f"
showv (List l) = "(" ++ (unwords (map showv l)) ++ ")"
showv (DottedList h l) = "(" ++ (unwords (map showv h)) ++ " . " ++ showv l ++ ")"
showv (PrimitiveFunction _) = "<primitive>"
showv (Stream s) = "stream: " ++ (show s)
showv (Func {paramVars = paramVars, varargs = vararg, body = body, closure = env}) =
  "(lambda (" ++ unwords (fmap show paramVars) ++ (unwrapVararg vararg) ++ ") ...)"
  where unwrapVararg m = fromMaybe "" $ (fmap (\x -> " . " ++ x)) m

showRest :: (String, Maybe Value) -> String
showRest (s, Nothing) = show s
showRest (s, Just v) = "(" ++ (show s) ++ " " ++ (show v) ++ ")"

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
