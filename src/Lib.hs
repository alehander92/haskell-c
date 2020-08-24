module Lib
    ( compile
    ) where

-- import System.Path 
import System.FilePath
import System.Environment
import Data.Text as T
import Prelude hiding (lex)
import Control.Monad (liftM2)
import Control.Monad.Except (throwError)
-- TODO: System.Path paths

data Options = Options { path :: FilePath, outputPath :: FilePath }
  deriving Show

data TokenType = 
    Hash |
    Name |
    Whitespace | -- tab or space
    Lt |
    Gt |
    Dot |
    LParen |
    RParen |
    LCurly |
    RCurly |
    Text |
    Semicolon |
    Nl |
    End
    deriving Show

data ErrorType =
  LexError |
  OtherError

type Line = Int
type Column = Int

data Location = Location (Line, Column)
  deriving Show

data Token = Token (TokenType, T.Text)
  deriving Show

data Error = Error (ErrorType, T.Text, Location)

data State =
  Normal |
  InToken Token

lex :: String -> Either Error [Token]
lex text = lexTokens text Normal 0

-- based on https://stackoverflow.com/a/8062909/438099
-- credit to nponeccop 
combineTokens :: State -> [Token] -> Either Error [Token] -> Either Error [Token]
combineTokens state left (Left err) = Left err
combineTokens Normal left (Right tokens) = Right (left ++ tokens)
combineTokens (InToken token) left (Right tokens) = Right ([token] ++ left ++ tokens)

maybeTokenTypeOfChar :: Char -> Maybe TokenType
maybeTokenTypeOfChar c =
  case c of
    '<' -> Just Lt
    '>' -> Just Gt
    '#' -> Just Hash
    '(' -> Just LParen
    ')' -> Just RParen
    '{' -> Just LCurly
    '}' -> Just RCurly
    '\n' -> Just Nl
    '.' -> Just Dot
    _ -> Nothing

lexTokens :: String -> State -> Int -> Either Error [Token]
--lexTokens ('#' : right) state index =
--  combineTokens state [Token (Hash, pack "#")] (lexTokens right Normal $ index + 1)

-- post pass
lexTokens (' ' : right) state index =
  combineTokens state [Token (Whitespace, pack " ")] (lexTokens right Normal $ index + 1)

lexTokens ('\t' : right) state index =
  combineTokens state [Token (Whitespace, pack "\t")] (lexTokens right Normal $ index + 1)

lexTokens (c : right) Normal index =
  let tokenType = maybeTokenTypeOfChar c in
  case tokenType of
    Just t -> combineTokens Normal [Token (Lt, pack [c])] (lexTokens right Normal $ index + 1)
    Nothing -> lexOtherCharToken c right Normal index
      
lexTokens (c : right) (InToken (Token (inType, name))) index =
  let tokenType = maybeTokenTypeOfChar c in
  case tokenType of
    Just t -> combineTokens (InToken (Token (inType, name))) [Token (Lt, pack [c])] (lexTokens right Normal $ index + 1)
    Nothing -> lexOtherCharToken c right (InToken (Token (inType, name))) index

  -- combineTokens Normal [Token (Name, name)] (lexTokens right Normal index)


lexTokens [] state index =
  Right []


lexOtherCharToken :: Char -> String -> State -> Int -> Either Error [Token]

lexOtherCharToken c right (InToken (Token (Name, name))) index =
  if c >= 'a' && c <= 'z' then
    lexTokens right (InToken (Token (Name, name <> (pack [c])))) $ index + 1
  else
    Right [Token (Name, name)]

lexOtherCharToken c right state index =
  if c >= 'a' && c <= 'z' then
    lexTokens right (InToken (Token (Name, pack [c]))) $ index + 1
  else
    Right []

compileSingleProgram :: Options -> IO ()
compileSingleProgram options = do
  print "options"
  text <- readFile (path options)
  let res = lex text
  case res of
    Right tokens -> print tokens
    Left (Error (kind, err, location)) -> print err
  return ()

-- haskell-c -o print print.c
  

parseOptions :: [String] -> IO Options
parseOptions ["-o", outputPath, path] =
  return Options { path = path, outputPath = outputPath }
parseOptions [path] =
  fail "expected -o"
  -- Options { path = path, output }
parseOptions _ =
  fail "can't run those args"

compile :: IO ()
compile = do
    putStrLn "haskell-c"
    a <- getArgs
    options <- parseOptions a
    compileSingleProgram options
