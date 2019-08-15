module LispEnv
  (
    setVar,
    getVar,
    defineVar,
    bindVars,
    liftThrows,
    nullEnv,
    runIOThrows
  ) where


import LValue
import Control.Monad
import Control.Monad.Except
import Data.IORef

nullEnv :: IO Env
nullEnv = newIORef []
 
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left a) = throwError a
liftThrows (Right a) = return a
  
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound ref var = readIORef ref >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError Value
getVar ref var = do
  env <- liftIO $ readIORef ref
  maybe (throwError $ UnboundVar "Attempted to read unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> Value -> IOThrowsError Value
setVar ref var val = do env <- liftIO $ readIORef ref
                        maybe (throwError $ UnboundVar "Attempted to write unbound var" var) (liftIO . (flip writeIORef val)) (lookup var env)
                        return val

defineVar :: Env -> String -> Value -> IOThrowsError Value
defineVar ref var val = do
  alreadyDefined <- liftIO $ isBound ref var
  if alreadyDefined
    then setVar ref var val >> return val
    else liftIO $ do
         valRef <- newIORef val
         env <- readIORef ref
         writeIORef ref ((var, valRef) : env)
         return val

bindVars :: Env -> [(String, Value)] -> IO Env
bindVars ref bindings = readIORef ref >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
