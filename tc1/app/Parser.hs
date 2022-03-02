module Parser where

import Control.Monad
import Data.Void
import Data.Functor

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Syntax 

type Parser = Parsec Void String

l ∷ Parser a → Parser a
l = L.lexeme space

lstring ∷ String → Parser String
lstring = l . string 

parens ∷ Parser a → Parser a
parens = between (lstring "(") (lstring ")")

eBool ∷ Parser Expr
eBool = fmap EBool $ true' <|> false' where
  true' = string "true" $> True
  false' = string "false" $> False 

eInt ∷ Parser Expr 
eInt = EInt . read <$> some numberChar

var ∷ Parser String
var = (:) <$> letterChar <*> many (letterChar <|> numberChar)

eVar ∷ Parser Expr
eVar = EVar <$> var 

eLet ∷ Parser Expr 
eLet = liftM3 ELet 
  (lstring "let" *> l var) 
  (lstring "="   *> lexpr) 
  (lstring "in"  *> lexpr)

eApp ∷ Parser Expr
eApp = liftM2 EApp
  (choice [parens expr, eLam, eVar])
  lexpr

eLam ∷ Parser Expr
eLam = liftM2 ELam
  (lstring "λ" *> l var)
  (lstring "." *> lexpr)

expr ∷ Parser Expr
expr = choice [try eApp, eLam, eLet, eBool, eVar, eInt, parens expr]

lexpr ∷ Parser Expr
lexpr = l expr 

parseExpr ∷ String → Either (ParseErrorBundle String Void) Expr 
parseExpr = runParser lexpr "<input>"
