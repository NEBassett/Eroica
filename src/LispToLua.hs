module LispToLua
  (
    toLua
  ) where

import qualified LValue as L
import LuaValue
import Control.Monad.Except

unsupportedFuncs = ["read", "read-char", "peek-char"]

isUnsupported (L.Symbol m) = m `elem` unsupportedFuncs
isUnsupported _ = False

cantBeFunc (L.Symbol m) = False
cantBeFunc (L.Func _ _ _ _) = False
cantBeFunc (L.PrimitiveFunction _) = False
canteBefunc _ = True

toLua :: L.Value -> L.ThrowsError LuaValue
toLua (L.Number m) = return $ Number m
toLua (L.Symbol m) = return $ Identifier m
toLua (L.String m) = return $ String m
toLua (L.Bool m) = return $ Bool m
toLua (L.Func params varargs body _) = (mapM toLua body) >>= (\x -> return $ Func params varargs x)
toLua (L.List ((L.Symbol "if"):cond:(L.List body):rest)) = do
  x <- toLua cond
  y <- mapM toLua body
  case rest of
    [m, L.List elseBody] -> do
      z <- toLua m
      w <- mapM toLua elseBody
      return $ If x y z w
    [] -> return $ If x y (Bool False) []
    m -> throwError $
         L.TypeMismatch "either empty list or else condition and body for if call" (L.List m)
toLua (L.List [L.Symbol "while", cond, (L.List body)]) = do
  x <- toLua cond
  y <- mapM toLua body
  return $ While x y
toLua (L.List [L.Symbol "define-var", name, value]) = do
  x <- toLua name
  y <- toLua value
  return $ Declaration x y
toLua (L.List [L.Symbol "set-var", name, value]) = do
  x <- toLua name
  y <- toLua value
  return $ Assignment x y
toLua (L.List [L.Symbol "num-for", var, start, finish, stride, (L.List body)]) = do
  q <- toLua var
  x <- toLua start
  y <- toLua finish
  z <- toLua stride
  w <- mapM toLua body
  return $ NumericFor q x y z w
toLua (L.List [L.Symbol "gen-for", (L.List vars), iter, (L.List body)]) = do
          x <- mapM toLua vars
          y <- toLua iter
          z <- mapM toLua body
          return $ GenericFor x y z
toLua (L.List [L.Symbol "array", L.List args])  = (mapM toLua args) >>= (\x -> return $ Array x)
toLua (L.List [L.Symbol "map", L.List args]) = (mapM toPairs args) >>= (\x -> return $ Map x)
  where toPairs (L.List pairs@[field, value]) = (mapM toLua pairs)
        toPairs m = throwError $ L.TypeMismatch "pair of values in the form (field value) in map call" m
toLua (L.List (L.Symbol "lambda" : L.List params : body)) = (mapM toLua body) >>= (\x -> return $ Func (fmap show params) Nothing x) 
toLua (L.List (L.Symbol "lambda" : L.DottedList params varargs : body)) = (mapM toLua body) >>= (\x -> return $ Func (fmap show params) (Just $ show varargs) x)
toLua (L.List [L.Symbol "index", tab, field]) = (toLua tab) >>= (\x -> (toLua field) >>= (\y -> return $ Index x  y))
toLua (L.List [L.Symbol "quote", form]) = case form of
                                            L.List ms -> (mapM toLua ms) >>= (\x -> return $ FunctionCall (Identifier "list") x)
                                            _ -> toLua form

toLua (L.List (func:args)) = if (isUnsupported func) then
                               (throwError $ L.WrongTime "Attempted to use compile time feature at runtime: " func)
                             else if (cantBeFunc func) then
                               (throwError $ L.NotFunction "Attempted to call something other than a function" func) 
                             else
                               (toLua func) >>= (\x -> (mapM toLua args) >>= (\y -> return $ FunctionCall x y))

