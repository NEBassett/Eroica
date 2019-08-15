module LispReadEval
  ( readExpr,
    eval,
    expandMacro,
    macroEnv,
    parseMacroCharacter
  ) where


import LValue
import Control.Monad.Except
import Control.Monad
import Text.Parsec
import Text.Parsec.Number
import LispEnv
import Data.Fixed

globals :: [(String, Value)]
globals =
  [
    ("nil", Symbol "nil"),
    ("parse-any-char", LParser (anyChar >>= (\x -> return $ String [x]))),
    ("parse-spaces", LParser (spaces >>= (\x -> return (Symbol "spaces")))),
    ("parse-digit", LParser (digit >>= (\x -> return (String [x])))),
    ("parse-symbol", LParser (symbol >>= (\x -> return (String [x])))),
    ("parse-letter", LParser (letter >>= (\x -> return (String [x])))),
    ("#t", Bool True),
    ("#f", Bool False)
  ]

prims :: [(String, Env -> [Value] -> IOThrowsError Value)]
prims =
  [
    ("read", liftOverStream readExprEval),
    ("peek-char", liftOverStream peekChar),
    ("read-char", liftOverStream readChar),
    ("cons", liftToPrimitive cons),
    ("car", liftToPrimitive car),
    ("cdr", liftToPrimitive cdr),
    ("+", addFunc),
    ("-", liftToBinaryOp toNumber fromNumber (-)), 
    ("*", liftToBinaryOp toNumber fromNumber (*)),
    ("/", liftToBinaryOp toNumber fromNumber (/)),
    ("mod", liftToBinaryOp toNumber fromNumber mod'),
    ("=", equalFunc),
    ("/=", unequalFunc),
    ("<", lFunc),
    (">", gFunc),
    ("<=", leqFunc),
    (">=", geqFunc),
    ("&&", liftToBinaryOp toBool fromBool (&&)),
    ("||", liftToBinaryOp toBool fromBool (||)),
    ("bind", bindLParsers),
    ("connect", connectLParsers),
    ("run-parser", runLParser),
    ("choice", choiceLParsers),
    ("try", tryLParser),
    ("<|>", fastChoiceLParsers),
    ("parse-char", charLParser),
    ("parse-string", stringLParser),
    ("many", manyLParser),
    ("one-of", oneOfLParser)
  ]

tryFuncs :: [Env -> [Value] -> IOThrowsError Value] -> String -> Env -> [Value] -> IOThrowsError Value
tryFuncs (m:ms) err env args = (eval env (List ((PrimitiveFunction m):args))) `catchError` (\_ -> tryFuncs ms err env args)
tryFuncs [] err env args = throwError $ TypeMismatch err (List args)

addFunc = tryFuncs [
  liftToBinaryOp toNumber fromNumber (+),
  liftToBinaryOp toString fromString (++)
                   ] "two values of addable type"

geqFunc = tryFuncs [
  liftToBinaryOp toNumber fromBool (>=),
  liftToBinaryOp toString fromBool (>=)
                   ] "two values of comparable type"
          
leqFunc = tryFuncs [
  liftToBinaryOp toNumber fromBool (<=),
  liftToBinaryOp toString fromBool (<=)
                   ] "two values of comparable type"
          
lFunc = tryFuncs [
  liftToBinaryOp toNumber fromBool (<),
  liftToBinaryOp toString fromBool (<)
                   ] "two values of comparable type"
        
gFunc = tryFuncs [
  liftToBinaryOp toNumber fromBool (>),
  liftToBinaryOp toString fromBool (>)
                   ] "two values of comparable type"
          
equalFunc = tryFuncs [
  liftToBinaryOp toNumber fromBool (==),
  liftToBinaryOp toString fromBool (==),
  liftToBinaryOp toBool fromBool (==),
  liftToBinaryOp toEqList fromBool (listEq)
                     ] "two values of comparable type"
          
unequalFunc = tryFuncs [
  liftToBinaryOp toNumber fromBool (/=),
  liftToBinaryOp toString fromBool (/=),
  liftToBinaryOp toBool fromBool (/=),
  liftToBinaryOp toEqList fromBool (\x y -> not (listEq x y))
                     ] "two values of comparable type"


listEq :: [Value] -> [Value] -> Bool
listEq ms ns = ((length ms) == (length ns)) && ((foldl (&&) True) (zipWith valComp ms ns))
  where valComp (String m) (String n) = m == n
        valComp (Symbol m) (Symbol n) = m == n
        valComp (Number m) (Number n) = m == n
        valComp (Bool m) (Bool n) = m == n
        valComp _ _ = False

toEqList :: Value -> IOThrowsError [Value]
toEqList (List ms) = mapM hasIdentity ms
  where hasIdentity m@(PrimitiveFunction _) = (throwError $ TypeMismatch "value with identity" m :: IOThrowsError Value)
        hasIdentity m@(LParser _) = throwError $ TypeMismatch "value with identity" m
        hasIdentity m@(Func _ _ _ _) = throwError $ TypeMismatch "value with identity" m
        hasIdentity m = return m
toEqList m = throwError $ TypeMismatch "list" m

toString :: Value -> IOThrowsError String
toString (String m) = return m
toString (Symbol m) = return m
toString m = throwError $ TypeMismatch "string" m

toNumber :: Value -> IOThrowsError Double
toNumber (Number m) = return m
toNumber m = throwError $ TypeMismatch "number" m

toBool :: Value -> IOThrowsError Bool
toBool (Bool m) = return m
toBool m = throwError $ TypeMismatch "bool" m

fromString = String
fromBool = Bool
fromNumber = Number
fromList = List

liftToBinaryOp :: (Value -> IOThrowsError f) -> (g -> Value) -> (f -> f -> g) -> (Env -> [Value] -> IOThrowsError Value)
liftToBinaryOp to from op = (\_ vals -> case vals of
                                          [a, b] -> do
                                            m <- to a
                                            n <- to b
                                            return $ from (op m n)
                                          ms -> throwError $ NumArgs 2 ms)
  
manyLParser :: Env -> [Value] -> IOThrowsError Value
manyLParser env [LParser m, combine, def] = return $ LParser $ (many m) >>= (\xs -> lift $ foldM binop def xs)
  where binop x y = eval env $ List [combine, x, y]
manyLParser env m = throwError $ TypeMismatch "parser, fold operator, and fold default" (List m)

tryLParser :: Env -> [Value] -> IOThrowsError Value
tryLParser env [LParser m] = return $ LParser $ try m
tryLParser env m = throwError $ TypeMismatch "parser" (List m)

stringLParser :: Env -> [Value] -> IOThrowsError Value
stringLParser env [String m] = return $ LParser $ (string m >>= (\x -> return $ String x))
stringLParser env m = throwError $ TypeMismatch "string" (List m)

oneOfLParser :: Env -> [Value] -> IOThrowsError Value
oneOfLParser env [String m] = return $ LParser $ (oneOf m >>= (\x -> return $ String [x]))
oneOfLParser env m = throwError $ TypeMismatch "string" (List m)

charLParser :: Env -> [Value] -> IOThrowsError Value
charLParser env [String [m]] = return $ LParser $ (char m >>= (\x -> return $ String [x]))
charLParser env m = throwError $ TypeMismatch "string containing a single character" (List m)

fastChoiceLParsers :: Env -> [Value] -> IOThrowsError Value
fastChoiceLParsers env parsers = (mapM unwrapParsers parsers) >>= (\x -> return $ LParser $ (foldl (<|>) parserZero (fmap toParser x)))
  where unwrapParsers val@(LParser p) = (return val :: IOThrowsError Value)
        unwrapParsers m = throwError $ TypeMismatch "every item to be a parser" m
        toParser (LParser p) = p

choiceLParsers :: Env -> [Value] -> IOThrowsError Value
choiceLParsers env parsers = (mapM unwrapParsers parsers) >>= (\x -> return $ LParser $ choice (fmap toParser x))
  where unwrapParsers val@(LParser p) = (return val :: IOThrowsError Value)
        unwrapParsers m = throwError $ TypeMismatch "a list of parsers " m
        toParser (LParser p) = p
        
bindLParsers :: Env -> [Value] -> IOThrowsError Value
bindLParsers env [LParser m, f] = return $ LParser $ m >>= (\x -> lift $ eval env (List [f, x]))
bindLParsers env m = throwError $ TypeMismatch "a parser and a function to bind it with" (List m)

connectLParsers :: Env -> [Value] -> IOThrowsError Value
connectLParsers env [LParser m, LParser n] = return $ LParser (m >> n)
connectLParsers env m = throwError $ TypeMismatch "two parsers to bind together" (List m)

runLParser :: Env -> [Value] -> IOThrowsError Value
runLParser env [LParser m, (Stream n)] = liftIntoEval (\x -> m) env n
runLParser env [LParser m, (Stream n), otherwise] = (liftIntoEval (\x -> m) env n) `catchError` (\x -> return $ otherwise)
runLParser env m = throwError $ TypeMismatch "parser and stream to parse" (List m)

equalPrim :: Env -> [Value] -> IOThrowsError Value
equalPrim _ [m, n] = equal m n
equalPrim _ l = throwError $ NumArgs 2 l

equal :: Value -> Value -> IOThrowsError Value
equal (List xs) (List ys) = foldr bAppend (return $ Bool True) (zipWith equal xs ys)
  where bAppend  n m = do
          (Bool x) <- n `catchError` (\_ -> return $ Bool False)
          (Bool y) <- m `catchError` (\_ -> return $ Bool False)
          return $ Bool $ x && y 
          
equal (Symbol m) (Symbol n) = return $ Bool $ m == n
equal (String m) (String n) = return $ Bool $ m == n
equal (Number m) (Number n) = return $ Bool $ m == n
equal (Bool m) (Bool n) = return $ Bool $ m == n
equal m n = throwError $ TypeMismatch "f g ; where f and g are of the same type" $ List [m, n]

liftToPrimitive :: ([Value] -> ThrowsError Value) -> (Env -> [Value] -> IOThrowsError Value)
liftToPrimitive func = (\_ vals -> liftThrows $ (func vals))

liftOverStream :: (Env -> String -> IOThrowsError Value) -> Env -> [Value] -> IOThrowsError Value
liftOverStream func env [(Stream v)] =  func env v
liftOverStream func _ xs = throwError $ TypeMismatch "stream" (List xs)

apply :: Env -> Value -> [Value] -> IOThrowsError Value
apply env (PrimitiveFunction func) args = func env args
apply env (Func params rest body closure) args =
  if (length params) > (length args)
  then
    throwError $ NumArgs (toInteger $ length params) args
  else
    (liftIO $ bindVars closure $ zip params args) >>= (bindRest rest) >>= evalBody
  where evalBody nEnv = liftM last $ mapM (eval nEnv) body
        bindRest (Just x) nEnv = liftIO $ bindVars nEnv $ [(x, List (drop (length params) args))]
        bindRest Nothing nEnv = return nEnv
apply env m args = throwError $ NotFunction "Attempted to call something other than a function" m

macroEnv :: IO Env
macroEnv = nullEnv >>= (flip bindVars $ fmap (\(x, y) -> (x, PrimitiveFunction y)) prims) >>= (flip bindVars globals) >>= (\x-> bindVars x [("parse-expr", LParser $ parseExpr x)])

car :: [Value] -> ThrowsError Value
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [other] = throwError $ TypeMismatch "pair" other
car other = throwError $ NumArgs 1 other

cdr :: [Value] -> ThrowsError Value
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList(_ : xs) x] = return $ DottedList xs x
cdr [other] = throwError $ TypeMismatch "pair" other
cdr other = throwError $ NumArgs 1 other

cons :: [Value] -> ThrowsError Value
cons [x, List[]] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x, y] = return $ DottedList [x] y
cons other = throwError $ NumArgs 2 other

makeFunc varargs env params body = return $ Func (fmap show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

expandMacro :: Env -> Value -> IOThrowsError Value
expandMacro env (List [Symbol "backquote", form]) = case form of
  List ms -> do
    evaledMs <- mapM commaEval ms
    return $ List [Symbol "quote", List evaledMs]
      where commaEval (List [Symbol "comma", m]) = eval env m
            commaEval m = return m
  m -> return $ List [Symbol "quote", m]
  
expandMacro env (List (Symbol "defmacro" : List (Symbol name : params) : body)) =
  fmap (\x -> List [Symbol "quote", Symbol "Macro successfully created"]) (makeNormalFunc env params body >>= defineVar env name)
  
expandMacro env (List (Symbol "defmacro" : DottedList (Symbol name : params) varargs : body)) =
  fmap  (\x -> List [Symbol "quote", Symbol "Macro successfully created"]) (makeVarArgs varargs env params body >>= defineVar env name)
  
expandMacro env (List (function : args)) = (do
  func <- eval env function
  (apply env func args) >>= (expandMacro env))
  `catchError`
  (\x -> fmap List (mapM (expandMacro env) (function : args)))
  
expandMacro env m = return m 

eval :: Env -> Value -> IOThrowsError Value
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Func _ _ _ _) = return val
eval env val@(PrimitiveFunction _) = return val
eval env val@(Stream _) = return val
eval env (Symbol sym) = getVar env sym

eval env (List (Symbol "lambda" : List params : body)) =
  makeNormalFunc env params body

eval env (List (Symbol "lambda" : DottedList params varargs : body)) = 
  makeVarArgs varargs env params body

eval env (List (Symbol "progn" : forms)) =
  foldl (\x y -> x >> (eval env y)) (return (Symbol "")) forms

eval env (List [Symbol "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       otherwise -> eval env conseq

eval env (List [Symbol "set-var", Symbol sym, form]) = eval env form >>= setVar env sym

eval env (List [Symbol "define-var", Symbol sym, form]) = eval env form >>= defineVar env sym

eval env (List [Symbol "quote", val]) = return val

eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply env func argVals
  
eval env m = throwError $ BadSpecialForm "Unknown special form: " m

isReaderMacro :: Value -> Bool
isReaderMacro (List [Symbol "reader-macro", (Func _ _ _ _)]) = True
isReaderMacro (List [Symbol "reader-macro", (PrimitiveFunction _)]) = True
isReaderMacro _ = False

isTerminating :: Value -> Bool
isTerminating (List [_, Bool x, _]) = x
isTerminating _ = False

symbol :: LispParser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

isNil (Symbol "nil") = True
isNil _ = False

parseMacroCharacter :: Env -> LispParser Value
parseMacroCharacter env = do
  pChar <- lookAhead (letter <|> symbol <|> digit)
  boundForm <- lift $ (getVar env [pChar] `catchError` (\x -> return $ String ""))
  (if (isReaderMacro boundForm) then anyChar else parserZero)
  currentStream <- getInput
  lift $ defineVar env "srcStream" (Stream currentStream)
  value <- lift $ (eval env (List ([((\(List [_, m]) -> m) boundForm), Symbol "srcStream"])))
  if (isNil value)
    then
    (do
      setInput currentStream
      parserZero)
    else
    (do
      newStream <- lift $ getVar env "srcStream"
      setInput ((\(Stream x) -> x) newStream) 
      return value)
  
parseString :: LispParser Value
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x


parseSymbol :: LispParser Value
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Symbol (first:rest)

parseNumber :: LispParser Value
parseNumber = do
  num <- natFloat
  return $ Number (either fromInteger id num)

evalRead :: Env -> Value -> LispParser Value
evalRead env (List [ Symbol "set-macro-character", Symbol macChar, form]) = do
  evaledForm <- lift $ eval env form
  lift $ defineVar env macChar $ List [Symbol "reader-macro", evaledForm]
  return $ List [Symbol "quote", Symbol "Macro character set"]
evalRead _ val = return val

parseExpr :: Env -> LispParser Value
parseExpr env =
  choice [
  try $ parseMacroCharacter env,
  parseSymbol <|>
  parseString <|>
  parseNumber <|> 
  parseQuoted env <|> 
  do
    char '('
    x <- try (parseList env) <|> (parseDottedList env)
    char ')'
    return x
         ] >>= (\x -> evalRead env x)

parseList :: Env -> LispParser Value
parseList env = liftM List $ sepBy (parseExpr env) spaces

parseDottedList :: Env -> LispParser Value
parseDottedList env = do
  head <- endBy (parseExpr env) spaces
  tail <- char '.' >> spaces >> (parseExpr env)
  return $ DottedList head tail

parseQuoted :: Env -> LispParser Value
parseQuoted env = do
  char '\''
  x <- (parseExpr env)
  return $ List [Symbol "quote", x]

parseBackquoted :: Env -> LispParser Value
parseBackquoted env = do
  char '`'
  x <- (parseExpr env)
  return $ List [Symbol "backquote", x]

parseCommad :: Env -> LispParser Value
parseCommad env = do
  char ','
  x <- (parseExpr env)
  return $ List [Symbol "comma", x]

liftIntoEval  :: (Env -> LispParser Value) -> Env -> String -> IOThrowsError Value
liftIntoEval parser env input = join $ (flip fmap) (runParserT evalParser () "" input)
                                (\x -> case x of
                                    Left err -> throwError $ Parser err
                                    Right val -> return val)
  where evalParser = do
          val <- (parser env)
          currentStream  <- getInput
          lift $ defineVar env "srcStream" (Stream currentStream)
          return val
          

readExprEval = liftIntoEval parseExpr
peekChar = liftIntoEval (\x -> lookAhead anyChar >>= (\y -> return $ String [y]))
readChar = liftIntoEval (\x -> anyChar >>= (\y -> return $ String [y]))


readExpr :: Env -> String -> IOThrowsError Value
readExpr env input = join $ (flip fmap) (runParserT (parseExpr env) () "" input)
                 (\x -> case x of
                           Left err -> throwError $ Parser err
                           Right val -> return val)
  
