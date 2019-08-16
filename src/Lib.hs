module Lib
    ( eroicaMain,
      evalString
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

flStr :: String -> IO ()
flStr str = putStr str >> hFlush stdout

rPrompt :: String -> IO String
rPrompt str = flStr str >> getLine

evalString :: Env -> String -> IO String
evalString macEnv expr = runIOThrows $ liftM show $ (readExpr macEnv expr) >>= (expandMacro macEnv) >>= (eval macEnv)

replLoop :: IO ()
replLoop = macroEnv >>= (mainLoop . evalString)
  where mainLoop action = do
          input <- rPrompt "Eroica: "
          if input == "(quit)"
            then return ()
            else (action input) >>= putStrLn >> (mainLoop action)
          
  
eroicaMain :: IO ()
eroicaMain = replLoop
