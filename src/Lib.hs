module Lib
    ( compile
    ) where

-- import System.Path 
import System.FilePath
import System.Environment
import qualified Data.Text as T
import Prelude hiding (lex)
import Control.Monad (liftM2)
import Control.Monad.Except (throwError)
import Debug.Trace
-- TODO: System.Path paths

data Options = Options { path :: FilePath, outputPath :: FilePath }
  deriving Show

data TokenType = 
    Hash |
    TokenName |
    Whitespace | -- tab or space
    Lt |
    Gt |
    Dot |
    LParen |
    RParen |
    LCurly |
    RCurly |
    TokenText |
    Semicolon |
    Nl |
    End
    deriving Show

data AInstruction =
  -- load_arg 0
  -- call     printf
  -- load_string "peace"
  ALoadArg Int |
  ACall T.Text |
  ALoadString T.Text |
  ALoadStringConst Int
    deriving Show

type AConst = [AConstSection] -- deriving Show -- AConst { sections :: [AConstSection], map :: Map T.Text Int }

data AConstSection = AConstSection (Int, [T.Text]) deriving Show

data AIR = AIR (Node Location, AConst, [AInstruction]) deriving Show

-- Arch 

data AsmInstruction =
  AsmMov AsmLocation AsmLocation |
  AsmLoad AsmValue |
  AsmMovq AsmLocation |
  AsmLeaq AsmLocation AsmLocation |
  AsmCall AsmLocation

    deriving Show

data Asm = Asm (Node Location, [AsmInstruction]) deriving Show

data AsmObject = AsmObject (T.Text, [Asm]) deriving Show

data ErrorType =
  LexError |
  ParseError |
  AIRError |
  AsmError
    deriving Show

type AsmValue = Int
  -- deriving Show

data AsmLocation =
  ALReg Register |
  ALInt Int |
  ALLabel T.Text |
  ALAddress T.Text Register |
  ALSymbol T.Text
    deriving Show

data Register =
  AX |
  BX |
  CX |
  DX |
  IP |
  EAX |
  EBX |
  ECX |
  EDX |
  RIP |
  SP |
  RSP |
  DI |
  RDI
    deriving Show

newtype Line = Line Int deriving Show
newtype Column = Column Int deriving Show

data Location = Location (Line, Column)
  deriving Show

data Token = Token (TokenType, T.Text)
  deriving Show

data Node a = 
  Name T.Text a |
  Include T.Text a |
  List [Node a] a |
  Function (Node a) (Node a) [Node a] [Node a] a |
  Call (Node a) [Node a] a |
  TypeName T.Text a |
  -- C String , not haskell string or text
  Text T.Text a
    deriving Show

data Error = Error (ErrorType, T.Text, Location) deriving Show

data State =
  Normal |
  InToken Token
    deriving Show

pack :: String -> T.Text
pack = T.pack

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
    ';' -> Just Semicolon
    _ -> Nothing

lexTokens :: String -> State -> Int -> Either Error [Token]
--lexTokens ('#' : right) state index =
--  combineTokens state [Token (Hash, pack "#")] (lexTokens right Normal $ index + 1)

-- post pass
lexTokens (' ' : right) state index =
  combineTokens state [Token (Whitespace, pack " ")] (lexTokens right Normal $ index + 1)

lexTokens ('\t' : right) state index =
  combineTokens state [Token (Whitespace, pack "\t")] (lexTokens right Normal $ index + 1)

lexTokens ('"' : right) (InToken (Token (TokenText, text))) index =
  combineTokens (InToken (Token (TokenText, text))) [] (lexTokens right Normal $ index + 1)

lexTokens ('"' : right) Normal index =
  lexTokens right (InToken (Token (TokenText, (pack "")))) $ index + 1

lexTokens (c : right) (InToken (Token (TokenText, text))) index =
  lexTokens right (InToken (Token (TokenText, text <> pack [c]))) $ index + 1

-- "a" => Text a

lexTokens (c : right) Normal index =
  let tokenType = maybeTokenTypeOfChar c in
  case tokenType of
    Just t -> combineTokens Normal [Token (t, pack [c])] (lexTokens right Normal $ index + 1)
    Nothing -> lexOtherCharToken c right Normal index
      
lexTokens (c : right) (InToken (Token (inType, name))) index =
  let tokenType = maybeTokenTypeOfChar c in
  case tokenType of
    Just t -> combineTokens (InToken (Token (inType, name))) [Token (t, pack [c])] (lexTokens right Normal $ index + 1)
    Nothing -> lexOtherCharToken c right (InToken (Token (inType, name))) index

  -- combineTokens Normal [Token (Name, name)] (lexTokens right Normal index)

lexTokens [] state index =
  Right []


lexOtherCharToken :: Char -> String -> State -> Int -> Either Error [Token]

lexOtherCharToken c right (InToken (Token (TokenName, name))) index =
  if c >= 'a' && c <= 'z' then
    lexTokens right (InToken (Token (TokenName, name <> (pack [c])))) $ index + 1
  else
    Right [Token (TokenName, name)]

lexOtherCharToken c right state index =
  if c >= 'a' && c <= 'z' then
    lexTokens right (InToken (Token (TokenName, pack [c]))) $ index + 1
  else
    Right []

-- parse :: [Token] -> Line -> Column -> Either E
-- parser:
--   Include lib -> 
--     Hash ~ "include" ~ Whitespace ~ Lt ~ Var ~ Dot ~ "h" ~ Gt

--     "expected include after "

--     "e.g. include <stdio.h>"

--            "expected include after #" <>
--                                     "expected < after include : we dont support \" yet" <>
--                                                "expected lib.h" <>
--                                                    "expected .h "


combineAst :: Node Location -> Either Error (Node Location) -> Either Error (Node Location)
combineAst (List nodes location) (Right other) =
  -- compose from parser combinators ?
  Right $ List (nodes ++ [other]) $ location

combineAst node (Right other) =
  -- TODO take location
  Right $ List [node, other] $ Location((Line 0), (Column 0))

combineAst node (Left error) =
  Left error

-- Include("stdio.h")
parseNode :: [Token] -> Line -> Column -> Either Error (Node Location)

parseNode (Token (Hash, _) : 
      Token (TokenName, name) : Token (Whitespace, _) : Token (Lt, _) : Token (TokenName, lib) : Token (Dot, _) : Token (TokenName, ext) : Token (Gt, _) : right) line column =
  if name == pack "include" && ext == pack "h" then
    -- TODO line 
    combineAst (Include name $ Location (line, column)) $ parseNode right line column
  else
    Left $ Error (ParseError, pack "expected include", Location (line, column))

parseNode (Token (TokenName, typeName) : Token (Whitespace, _) : Token (TokenName, functionName) : Token (LParen, _) : Token (RParen, _) :
              Token (Whitespace, _) : Token(LCurly, _) : Token(Nl, _) : right) line column =
  let code = parseNode right line column in
  case code of
    Right node -> Right (Function 
      (TypeName typeName (Location ((Line 0), (Column 0))))
      (Name functionName (Location ((Line 0), (Column 0))))
      []
      [node]
      (Location (line, column)))
    Left error -> Left error

parseNode (Token (TokenName, call) : Token (LParen, _) : arg : Token (RParen, _) : Token (Semicolon, _) : right) line column =
  let argEither = parseNode [arg] (Line 0) (Column 0) in
  case argEither of
    Right argNode -> Right (Call (Name call (Location (line, column))) [argNode] (Location (line, column)))
    Left error -> Left error

parseNode ([Token (TokenText, arg) ]) line column =
  Right (Text arg $ Location (line, column))

-- TODO right } check
parseNode ( Token (RCurly, _) : right) line column =
  parseNode right line column

-- multiple nl , whitespace
parseNode (Token (Nl, _) : right) line column =
  parseNode right line column

parseNode (Token (Whitespace, _) : right) line column =
  parseNode right line column

parseNode tokens line column =
  Left $ Error (ParseError, pack ("expected valid code " ++ (show tokens)), Location (line, column))

parse :: [Token] -> Either Error (Node Location)
parse tokens = 
  parseNode tokens (Line 0) (Column 0)
  -- Right $ Name (pack "") $ Location (0, 0)

parseAndCompile :: Options -> [Token] -> IO ()
parseAndCompile options tokens = do
  let astEither = parse tokens
  case astEither of
    Right ast -> print ast
    Left (Error (kind, err, location)) -> print err
  case astEither of
    Right ast -> do
      generateIRAndCompile options ast
    Left _ ->
      print ""
  return ()

-- each module -> list of functions -> each functions, compile to IR ?
-- each IR 
-- in a future language my own or other can * generate cranelift, generate asm, generate c, or llvm stuff or my own ir
-- also keep in mind debug info

-- credits to the authors of those resources
-- (many other resources/people not mentioned)
-- https://idea.popcount.org/2013-07-24-ir-is-better-than-assembly/
-- https://cs.lmu.edu/~ray/notes/ir/
-- https://www.seas.harvard.edu/courses/cs153/2018fa/lectures/Lec23-SSA.pdf

-- use abstract ir for general optimizations
-- and e.g. x86_64 ir for arch optimizations

-- for now generate SSA single statement assignment
-- CFG control flow graph : probably makes it easier to define labels
-- and we can add other optional stuff there: checks, optimizations?

-- how to visualize
-- write some dot visualizers ? or just text ones for now, simpler
-- side by side

loadFunctions :: Node Location -> [Node Location]
loadFunctions (List nodes location) =
  foldl (\a b -> a ++ (loadFunctions b)) [] nodes

loadFunctions function @ (Function _ _ _ _ _) =
  [function]

loadFunctions _ =
  []

-- generate IR for functions
-- we need to first generate IR
-- then find out where branches happen and convert assignments into SSA
-- for now we dont have if, so just generate directly
-- asm and generate 
generateAIR :: Either Error (AConst, [AInstruction]) -> Node Location -> Either Error (AConst, [AInstruction])
generateAIR (Right (c, a)) (Call (Name name _) [] _) =
  Right $ (c, a ++ [ACall (trace (show name) name)])

generateAIR (Right (c, a)) (Call (Name name _) [arg] _) =
  let argIR = generateAIR (Right (c, [])) arg  in    
  case argIR of
    Right (c', ir) -> Right $ (c', a ++ ir ++ [ACall name])
    Left error -> Left error

generateAIR (Right (c, a)) (Text value _) =
  let id = length c in
  let c' = (c ++ [AConstSection (id, [value])]) in
  
  Right $ (c', a ++ [ALoadStringConst id])

generateAIR (Left error) _ =
  Left error

generateAIR (Right _) _ =
  -- TODO location
  Left $ Error (AIRError, pack "not supported", Location ( (Line 0), (Column 0)) )

generateAIRList :: [Node Location] -> Either Error (AConst, [AInstruction])
generateAIRList = foldl generateAIR $ Right ([], [])

generateIR :: Node Location -> Either Error AIR
generateIR (Function (TypeName typeName _) (Name functionName nameLocation) args code location) =
  let airList = generateAIRList code in
  case airList of
    Right (c, list) ->
      Right $ AIR ((Name functionName nameLocation), c, list)
    Left error ->
      Left error

generateIR _ =
  -- TODO location
  Left $ Error (AIRError, pack "expected function", Location ((Line 0), (Column 0)))

-- 



toStringLabel :: Int -> T.Text
toStringLabel id = pack $ ".String" ++ (show id)

-- https://stackoverflow.com/a/5329669/438099
generateAsmInstructions :: Either Error [AsmInstruction] -> AInstruction -> Either Error [AsmInstruction]
generateAsmInstructions (Left error) _ =
  Left error

generateAsmInstructions (Right a) (ACall name) =
  -- TODO location
  Right $ a ++ [AsmCall (ALSymbol name)]
  -- Left $ Error (AsmError, pack "call not supported", Location ((Line 0), (Column 0)))

generateAsmInstructions (Right a) (ALoadString id) =
  Left $ Error (AsmError, pack "load_string not expected here", Location ((Line 0), (Column 0)))

generateAsmInstructions (Right a) (ALoadStringConst id) =
  Right $ a ++ [AsmLeaq (ALAddress (toStringLabel id) RIP) (ALReg RDI)]

generateAsmFunction :: AIR -> Either Error Asm
generateAsmFunction (AIR (node, c, a)) =
  let instructions = (foldl generateAsmInstructions $ Right []) a :: (Either Error [AsmInstruction]) in 
  case instructions of
    Right instructions' -> Right $ Asm (node, instructions')
    Left error -> Left error

generateIRList :: [Node Location] -> Either Error [AIR]
generateIRList = mapM generateIR

generateAsmAndCompile :: Options -> [AIR] -> IO ()
generateAsmAndCompile _ air = do
  let a = mapM generateAsmFunction air
  print air
  case a of
    Right a' -> do
      let asmObject = AsmObject (pack "peace.asm", a')
      print asmObject
    Left error -> print error

generateIRAndCompile :: Options -> Node Location -> IO ()
generateIRAndCompile options ast = do
  let functions = loadFunctions ast
  let irList  = generateIRList functions
  case irList of
    Right irList' -> generateAsmAndCompile options irList'
    Left error -> print error
  
compileSingleProgram :: Options -> IO ()
compileSingleProgram options = do
  print "options"
  text <- readFile (path options)
  let res = lex text
  -- case res of
  --   Right tokens -> print tokens
  --   Left (Error (kind, err, location)) -> print err
  case res of
    Right tokens -> do
      _ <- parseAndCompile options tokens
      print ""
    Left _ -> do
      print ""
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

