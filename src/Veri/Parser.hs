{-# LANGUAGE OverloadedStrings #-}
module Veri.Parser where

import Veri.AST

import Text.Parsec
import Text.Parsec.Char
import Data.Text (Text)
import Data.Void
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Char (isAscii, isAsciiLower)

type Parser a = Parsec String () a

parseVariable :: Parser VariableIdent
parseVariable = show <$> satisfy isAscii

parseAsterisk :: Parser Epsilon
parseAsterisk = char '*' >> (return Asterisk)

parseSort :: Parser Epsilon
parseSort = char '@' >> (return Sort)

wt :: Parser ()
wt = void (many (char ' ' <|> char '\t'))

parseApp :: Parser Epsilon
parseApp = do
    char '%' >> char '(' >> wt
    m <- parseEpsilon
    wt >> char ')' >> wt >> char '(' >> wt
    n <- parseEpsilon
    _ <- wt >> char ')'
    return $ App m n

parseLambda :: Parser Epsilon
parseLambda = do
    char '$' >> wt
    x <- parseVariable
    char ':' >> wt >> char '(' >> wt
    m <- parseEpsilon
    _ <- char ')' >> wt >> char '.' >> wt >> char '('
    n <- parseEpsilon
    _ <- wt >> char ')'
    return $ Lambda x m n

parsePi :: Parser Epsilon
parsePi = do
    char '?' >> wt
    x <- parseVariable
    char ':' >> wt >> char '(' >> wt
    m <- parseEpsilon
    _ <- char ')' >> wt >> char '.' >> wt >> char '('
    n <- parseEpsilon
    _ <- wt >> char ')'
    return $ Pi x m n

parseList :: Parser [Epsilon]
parseList = do
    e <- parseEpsilon
    es <- option [] (char ',' >> wt >> parseList)
    return ((e:es) :: [Epsilon])

parseConst :: Parser Epsilon
parseConst = do
    c <- many1 $ satisfy isAsciiLower
    char '[' >> wt
    v <- option [] parseList
    _ <- wt >> char ']'
    return $ Const c v

parseEpsilon' :: Parser Epsilon
parseEpsilon' = choice [
        parseVariable <&> Variable,
        parseAsterisk,
        parseSort,
        parseApp,
        parseLambda,
        parsePi,
        parseConst
    ]
parseParen :: Parser Epsilon
parseParen = do
    char '(' >> wt
    e <- parseEpsilon
    _ <- wt >> char ')'
    return e

parseEpsilon :: Parser Epsilon
parseEpsilon = parseParen <|> parseEpsilon'

