module Lib where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Expr

kaiDef :: LanguageDef st
kaiDef = LanguageDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "%"
    , nestedComments  = True
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> char '_'
    , opStart         = opLetter kaiDef
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedNames   = ["class","import","function","return","if","then","else","break","continue","true","false","math","m","verbatim","v","do"]
    , reservedOpNames = ["+","-","*","/","^","="]
    , caseSensitive  = True
    }

kai :: TokenParser st
kai = (makeTokenParser kaiDef) { lexeme = id }

data KaiTyping = KaiTyping
    deriving Show

parseTyping :: Parser KaiTyping
parseTyping = return KaiTyping

data KaiScript = Class String
               | Import String
               | VarAssign KaiExpr KaiExpr
               | FunDec String [String] [KaiScript]
               | Expr KaiExpr
    deriving Show

parseKaiScript :: Parser KaiScript
parseKaiScript = try parseClass <|> try parseImport <|> try parseFunDec <|> try parseVarAssign <|> try parseExpr

parseClass :: Parser KaiScript
parseClass = Class <$> (reserved kai "class" >> parens kai (identifier kai))

parseImport :: Parser KaiScript
parseImport = Import <$> (reserved kai "import" >> parens kai (identifier kai))

parseVarAssign :: Parser KaiScript
parseVarAssign = do
    l <- parseKaiExpr
    reservedOp kai "="
    r <- parseKaiExpr
    return (VarAssign l r)

parseFunDec :: Parser KaiScript
parseFunDec = do
    reserved kai "function"
    i <- identifier kai
    as <- parens kai (commaSep kai $ identifier kai)
    b <- braces kai (parseKaiScript `sepBy` eol)
    return $ FunDec i as b

parseExpr :: Parser KaiScript
parseExpr = Expr <$> parseKaiExpr

eol :: Parser ()
eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

data KaiExpr = Val KaiVal
             | List [KaiExpr]
             | Dictionary [(String, KaiExpr)]
             | Var String
             | FunAppl String [KaiExpr]
             | Add KaiExpr KaiExpr
             | Subtract KaiExpr KaiExpr
             | Times KaiExpr KaiExpr
             | Divide KaiExpr KaiExpr
             | Raise KaiExpr KaiExpr
             | Lower KaiExpr KaiExpr
    deriving Show

parseKaiExpr :: Parser KaiExpr
parseKaiExpr = buildExpressionParser
    [ [ Infix (reservedOp kai "^" >> return Raise) AssocLeft
      , Infix (reservedOp kai "_" >> return Lower) AssocLeft ]
    , [ Infix (reservedOp kai "*" >> return Times) AssocLeft
      , Infix (reservedOp kai "/" >> return Divide) AssocLeft ]
    , [ Infix (reservedOp kai "+" >> return Add) AssocLeft
      , Infix (reservedOp kai "-" >> return Subtract) AssocLeft ]
    ] $ parens kai parseKaiExpr <|> parseKaiExprBit

parseKaiExprBit :: Parser KaiExpr
parseKaiExprBit = try parseDictionary <|> try parseList <|> try parseFunAppl <|> try parseVal <|> try parseVar

parseVal :: Parser KaiExpr
parseVal = Val <$> parseKaiVal

parseDictionary :: Parser KaiExpr
parseDictionary = (braces kai $ fmap Dictionary $ commaSep kai $ do
    i <- identifier kai
    colon kai
    v <- parseKaiExpr
    return (i,v)
    ) <?> "dictionary"

parseList :: Parser KaiExpr
parseList = (braces kai $ List <$> commaSep kai parseKaiExpr) <?> "list"

parseVar :: Parser KaiExpr
parseVar = Var <$> identifier kai

parseFunAppl :: Parser KaiExpr
parseFunAppl = do
    i <- identifier kai
    vs <- (try (parens kai $ commaSep kai parseKaiExpr)) <|> (do
        tv <- parseTypingVal
        return [Val tv])
    return $ FunAppl i vs

data KaiVal = Typing KaiTyping
            | Integer Integer
            | Float Double
            | String String
            | Bool Bool
            deriving Show

parseKaiVal :: Parser KaiVal
parseKaiVal =  try parseTypingVal <|> try parseFloat <|> try parseInteger <|> try parseString <|> try parseBool

parseTypingVal :: Parser KaiVal
parseTypingVal = Typing <$> brackets kai parseTyping

parseInteger :: Parser KaiVal
parseInteger = Integer <$> integer kai

parseFloat :: Parser KaiVal
parseFloat = Float <$> float kai

parseString :: Parser KaiVal
parseString = String <$> stringLiteral kai

parseBool :: Parser KaiVal
parseBool = (reserved kai "true" >> return (Bool True)) <|> (reserved kai "false" >> return (Bool False)) <?> "bool"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
