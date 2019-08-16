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
           | Prim (Env -> [Value] -> IOThrowsError Value)
           | Func {paramVars :: [String], varargs :: (Maybe String), body :: [Value], closure :: Env }
           | Stream String
           | LParser (LispParser Value)

           -- this error type is used by the compiler aswell for smooth monadic semantics
data LError = BadCall String [Value]
            | Parser ParseError
            | BadForm String Value
            | UnboundVar String String
            | NotFunc String Value
            | WrongTime String Value

type ThrowsError = Either LError

type IOThrowsError = ExceptT LError IO

instance Show LError where show = showe
instance Show Value where show = showv
showe :: LError -> String
showe (UnboundVar msg varname) = msg ++ ": " ++ varname
showe (BadForm msg form) = msg ++ ": " ++ show form
showe (NotFunc msg func) = msg ++ ": " ++ showv func
showe (BadCall msg vals) = "Bad call: expected " ++ msg ++ ", got: " ++ (unwords $ fmap show vals)
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
showv (Prim _) = "<primitive>"
showv (Stream s) = "stream: " ++ (show s)
showv (Func params vararg body env) =
  "(lambda (" ++ unwords (fmap show params) ++ (unwrapVararg vararg) ++ ") ...)"
  where unwrapVararg m = fromMaybe "" $ (fmap (\x -> " . " ++ x)) m

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
