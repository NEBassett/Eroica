{-# LANGUAGE QuasiQuotes #-}

module LuaValue
  (
    LuaValue (..),
    LuaError (..),
    ThrowsError (..),
    luaPrelude
  ) where

-- foreword: calling this a "LuaValue" is a bit of a misnomer, really what this module is to do is not at all
-- defining a lua ast as much as it is a matter of defining the ast of our output (some pragmatic ast)
-- which will then be shown into lua source 

import Data.List
import Control.Monad.Except
import Text.RawString.QQ

data LuaValue = Identifier String
              | String String
              | Number Double 
              | Bool Bool
              | FunctionCall LuaValue [LuaValue]
              | Index LuaValue LuaValue
              | Array [LuaValue]
              | Map [[LuaValue]]
              | Func {params :: [String], varargs :: Maybe String, body :: [LuaValue]}
              | Assignment LuaValue LuaValue
              | Declaration LuaValue LuaValue
              | Return LuaValue
              | While LuaValue [LuaValue]
              | If LuaValue [LuaValue] LuaValue [LuaValue]
              | NumericFor LuaValue LuaValue LuaValue LuaValue [LuaValue]
              | GenericFor [LuaValue] LuaValue [LuaValue]
              
              

data LuaError = Unsupported String
              | Default String

type ThrowsError = Either LuaError

luaPrelude :: String
luaPrelude = [r|
local __listClass = {type = "list"}

__listClass.__index = __listClass

local function car(lst)
  if type(lst) ~= "table" or lst.type ~= "list" then error("car must be called upon lists only") end
  if lst == nil then return nil end 
  return lst[1]
end

local function cdr(lst)
  if type(lst) ~= "table" or lst.type ~= "list" then error("cdr must be called upon lists only") end
  local m = lst[2]
  if m == nil then return m end
  setmetatable(m, __listClass)
  return lst[2]
end

__listClass.__eq = function(a, b)
  local function recursiveEq(m, n)
    local k = car(m)
    if car(n) ~= car(m) then return false end 
    if k == nil then return true end 
    return recursiveEq(cdr(m), cdr(n))
  end

  return recursiveEq(a,b)
end

local function __listImpl(x, ...)
  if x == nil then
    return nil
  end
  return {x, __listImpl(...)}
end

local function __dottedListImpl (x, y, ...)
  if y == nil then
    return x
  end
  return {y, __listImpl(...)}
end

local function list(...)
  local nList = __listImpl(...)
  setmetatable(nList, __listClass)
  return nList
end

local function dottedList(...)
  local nList = __dottedListImpl(...)
  setmetatable(nList, __listClass)
  return nList
end|] ++ "\n"

luaOps :: [(String, String)]
luaOps = [
  ("+", "+"),
  ("-", "-"),
  ("mod", "%"),
  ("/", "/"),
  ("*", "*")
         ]

isStatement :: LuaValue -> Bool
isStatement (Return _) = True
isStatement (Assignment _ _) = True
isStatement (Declaration _ _) = True
isStatement (While _ _) = True
isStatement (If _ _ _ _) = True
isStatement (NumericFor _ _ _ _ _) = True
isStatement (GenericFor _ _ _) = True
isStatement _ = False



showlv :: LuaValue -> String
showlv (Bool True) = "true"
showlv (Bool False) = "false"
showlv (Identifier m) = m
showlv (Number n) = show n
showlv (String m) = "\"" ++ m ++ "\""
showlv (Index m n) = showlv m ++ "[" ++ showlv n ++ "]"
showlv (Array args) = "{" ++ (intercalate ", " (fmap showlv args)) ++ "}"
showlv (Map args) = "{" ++ (intercalate "; " (fmap toAssignment args)) ++ "}"
  where toAssignment (a:b:tail) = showlv a ++ " = " ++ showlv b
showlv (FunctionCall (Identifier "progn") stmts) = (intercalate "\n" (fmap showlv stmts))
showlv (Return val) = "return" ++ (showlv val)
showlv (Assignment name val) = (showlv name) ++ " = " ++ (showlv val)
showlv (Declaration name val) = "local " ++ (showlv name) ++ " = " ++ (showlv val)
showlv (While cond body) =
  "while" ++ " (" ++ (showlv cond) ++ ") " ++ "do\n\t" ++ (intercalate "\n\t" (fmap showlv body)) ++ "\nend"
showlv (If cond body elseCond elseBody) =
  "if " ++ " (" ++ (showlv cond) ++ ") " ++ "then\n\t" ++ (intercalate "\n\t" (fmap showlv body)) ++ "\nelseif" ++
  " (" ++ (showlv elseCond) ++ ") " ++ "then\n\t" ++ (intercalate "\n\t" (fmap showlv elseBody)) ++ "\nend"
showlv (NumericFor var start finish stride body) =
  "for " ++ (showlv var) ++ " = " ++ (showlv start) ++ ", " ++ (showlv finish) ++ ", " ++ (showlv stride) ++
  " do\n\t" ++ (intercalate "\n\t" (fmap showlv body)) ++ "\nend"
showlv (GenericFor vars iterator body) =
  "for " ++ (intercalate ", " (fmap showlv vars)) ++ " in " ++ (showlv iterator) ++ " do\n" ++
  (intercalate "\n\t" (fmap showlv body)) ++ "\nend"
showlv (FunctionCall func args) = case (lookup (showlv func) luaOps) of
                                    Just op -> "(" ++ (intercalate op (fmap showlv args)) ++ ")" 
                                    Nothing -> (showlv func) ++ "(" ++ (intercalate ", " (fmap showlv args)) ++ ")"
showlv (Func params varargs body) = "(function" ++ "(" ++ (intercalate ", " params) ++ ", ...)\n" ++ varargsDef ++ "\t" ++ (intercalate "\n\t" (fmap showlv (init body))) ++ (lastStatement (last body)) ++ "\n" ++ "end)"
  where varargsDef = case varargs of
                       Just ident -> "local " ++ ident ++ " = list(...)\n"
                       Nothing -> ""
        lastStatement m = "\n\t" ++ case (isStatement m) of
                            True -> showlv m
                            False -> "return " ++ (showlv m)

showle :: LuaError -> String
showle (Unsupported m) = "Unsupported functionality: " ++ m
showle (Default m) = "General error: " ++ m

instance Show LuaValue where show = showlv
instance Show LuaError where show = showle
