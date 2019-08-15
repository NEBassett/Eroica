module LispEnv
  (
    set,
    get,
    define,
    bind,
    liftThrows,
    nullEnv,
    runIOThrows
  ) where


import LValue
import Control.Monad
import Control.Monad.Except
import Data.IORef
import qualified Data.Map as M

nullEnv :: IO Env
nullEnv = newIORef M.empty
 
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left a) = throwError a
liftThrows (Right a) = return a
  
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

bound :: Env -> String -> IO Bool
bound ioref name = readIORef ioref >>= (\x -> return $ maybe False (const True) $ M.lookup name x)

get :: Env -> String -> IOThrowsError Value
get ioref name = (liftIO $ readIORef ioref)
  >>= (\x -> maybe (throwError $ UnboundVar "Read on unbound variable" name)
             (liftIO . readIORef)
             (M.lookup name x))

set :: Env -> String -> Value -> IOThrowsError Value
set ioref name val =  ((liftIO $ readIORef ioref)
                       >>= (\x -> maybe (throwError $ UnboundVar "Write on unbound variable" name)
                                  (liftIO . (flip writeIORef val))
                                  (M.lookup name x))) >> (return val)
                     
define :: Env -> String -> Value -> IOThrowsError Value
define ioref name val =
  (liftIO $ bound ioref name)
  >>= (\isBound ->
         if isBound
         then (set ioref name val) >> return val
         else
           (liftIO $ (newIORef val) >>=
            (\ref -> (readIORef ioref) >>= (\env -> writeIORef ioref (M.insert name ref env)) >>
            return val)))

bind :: Env -> [(String, Value)] -> IO Env
bind ioref binds = readIORef ioref >>= (\env ->
                                          (foldM (\fMap (x,y) -> (newIORef y) >>= (\m -> return $ M.insert x m fMap)) env binds) >>= newIORef)
                                          
