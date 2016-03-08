{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Hesh.Shell (sh, desugar) where

import Control.Applicative ((<$>), (*>), (<*))
import Data.Char (isSpace, isAlphaNum)
import Language.Haskell.TH (Q, location, Name, mkName, newName, stringL, litE, varE, listE, varP)
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ)
import Language.Haskell.TH.Syntax (Exp(..), Lit(..), Loc(..))
import System.Environment (getEnv)
import System.FilePath.Glob (namesMatching)
import System.Process (proc)
import Text.Parsec (parse)
import Text.Parsec.Char (space, spaces, char, string, noneOf, satisfy, lower, upper)
import Text.Parsec.Combinator (eof, sepEndBy1, many1, between)
import Text.Parsec.Pos (SourcePos, newPos)
import Text.Parsec.Prim (setPosition, many, (<|>), (<?>), try)
import Text.Parsec.String (Parser)

import qualified Hesh.Process

sh :: QuasiQuoter
sh = QuasiQuoter heshExp undefined undefined undefined

heshExp :: String -> Q Exp
heshExp s = do
  l <- location'
  case parse (setPosition l *> topLevel tokensP) "unknown" s of
    Left err -> error ("parse error: " ++ show err)
    Right tokens -> cmdExp tokens []

-- We need to be able to expand some arguments in the IO monad, so
-- each argument is expanded and bound until we finally evaluate the
-- command.
cmdExp :: [String] -> [Name] -> Q Exp
cmdExp [] vars = [| let tokens = concat $(listE (map varE (reverse vars)))
                    in Hesh.Process.cmd (head tokens) (tail tokens) |]
cmdExp (t:ts) vars = do
  case parse fragmentsP "unknown" t of
    Left err -> error ("parse error while parsing token \"" ++ t ++ "\": " ++ show err)
    Right fragments -> do
      -- We have a list of fragments. We need to expand all of the
      -- environment variables in the IO monad. The easiest way seems
      -- to be to just build a list of variables and use return for any
      -- non-monadic expansions.
      x <- newName "x"
      [| ($(fragmentsExp fragments [])) >>= \ $(varP x) -> $(cmdExp ts (x:vars)) |]

data Fragment = FragmentString String | FragmentIdentifier String | FragmentEnvVar String

-- This uses a similar recursive monad statement binding technique as cmdExp
fragmentsExp :: [Fragment] -> [Name] -> Q Exp
fragmentsExp [] vars = [| return [concat $(listE (map varE (reverse vars)))] |]
fragmentsExp (f:fs) vars = do
  x <- newName "y"
  [| $(fragmentExp f) >>= \ $(varP x) -> $(fragmentsExp fs (x:vars)) |]

fragmentExp :: Fragment -> Q Exp
fragmentExp (FragmentString s) = [| return $(litE (stringL s)) |]
fragmentExp (FragmentIdentifier i) = [| return $(varE (mkName i)) |]
fragmentExp (FragmentEnvVar e) = [| getEnv $(litE (stringL e)) |]

fragmentsP :: Parser [Fragment]
fragmentsP = many (try (variableP >>= \x -> case x of
                                              Left i -> return (FragmentIdentifier i)
                                              Right e -> return (FragmentEnvVar e))
               <|> (many1 (onlyEscapedP '$') >>= return . FragmentString))

-- Variables must match valid Haskell identifier names.
-- Any variable beginning with an uppercase character is assumed to be an environment variable.
-- Use of undefined variables throws an error.
-- Left => normal variable (can be referenced immediately)
-- Right => environment variable (must be looked up at runtime)
variableP :: Parser (Either String String)
variableP = do
  char '$'
  (try (between (char '{') (char '}') variable) <|> variable)
 where variable = (try identifier <|> envVariable)
       -- This doesn't cover all possible environment variable names, but it removes ambiguity.
       identifier = do
         x <- lower
         xs <- many (satisfy (\c -> isAlphaNum c || c == '\''))
         return (Left (x:xs))
       envVariable = do
         x <- upper
         xs <- many (satisfy (\c -> isAlphaNum c || c == '_'))
         return (Right (x:xs))

tokensP :: Parser [String]
tokensP = do
  spaces
  tokens <- sepEndBy1 token spaces
  return tokens
 where token = do parts <- many1 (try quotedP <|> unquotedP)
                  return (concat parts)

quotedP = do
  char '"'
  xs <- many (onlyEscapedP '"')
  char '"'
  return xs
 <?> "quoted string"

unquotedP = many1 (satisfy (\c -> not (isSpace c) && c /= '"'))

onlyEscapedP :: Char -> Parser Char
onlyEscapedP c = try (string ['\\', c] >> return c) <|> satisfy (/= c)

topLevel :: Parser a -> Parser a
topLevel p = spaces *> p <* (spaces *> eof)

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

-- The following parsers are for use by hesh before passing the script to the compiler.

desugar :: String -> String
desugar s =
  case parse sugarP "" s of
    Left err -> error ("parse error: " ++ show err)
    Right fragments -> concat fragments

sugarP :: Parser [String]
sugarP = many
  (   try shSugarP
  <|> try (string "$")
  <|> (many1 (noneOf "$"))
  )

-- syntactic sugar
-- $() => [sh| |]
shSugarP :: Parser String
shSugarP = do
  string "$("
  xs <- many (onlyEscapedP ')')
  char ')'
  return ("[sh|" ++ xs ++ "|]")
