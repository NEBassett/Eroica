module Lib
    ( eroicaMain,
      evalString,
      compileOne
    ) where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.List
import qualified Data.Map as Map
import LValue
import qualified LuaValue as LV
import LispReadEval
import LispEnv
import LispToLua
import System.IO


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

compileString :: Env -> String -> IO String
compileString macEnv expr = (runIOThrows $ liftM show $ (readExpr macEnv expr) >>= (expandMacro macEnv) >>= (\x -> liftThrows $ toLua x)) >>= (\x -> return $  LV.luaPrelude ++ x)

compileAndPrint :: Env -> String -> IO ()
compileAndPrint macEnv expr = (compileString macEnv expr) >>= putStrLn

evalString :: Env -> String -> IO String
evalString macEnv expr =
  runIOThrows $ liftM show $ (readExpr macEnv expr) >>= (expandMacro macEnv) >>= (eval macEnv)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint macEnv expr = evalString macEnv expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> ( a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if (pred result)
  then return ()
  else action result >> until_ pred prompt action

compileOne :: String -> IO ()
compileOne expr = join $ ((flip compileAndPrint (expr)) <$> macroEnv)

runRepl :: IO ()
runRepl = join $ ((until_ (== "quit") (readPrompt "Eroica: ")) . evalAndPrint) <$> macroEnv

eroicaMain :: IO ()
eroicaMain = do
  args <- getArgs
  case (length args) of
    0 -> runRepl
    1 -> compileOne $ args !! 0
    otherwise -> putStrLn "Usage: ./executable (lispExpression | nothing)"
