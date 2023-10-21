module Tokenizer (runParser, parseAST) where
import Control.Applicative (Alternative (..), optional)
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)

data AST
    = BinaryExpression BinaryOperator AST AST
    | BooleanExpression ComparisonOperator AST AST
    | UnaryExpression UnaryOperator AST
    | NumericLiteral Double
    | StringLiteral String
    | BooleanLiteral Bool
    | Identifier String
    | AssignmentStatement VarKind AST AST
    | FunctionCall String [AST]
    | FunctionDefinition String [String] [AST]
    | IfStatement AST [AST] (Maybe [AST])
    | ElseStatement [AST] (Maybe [AST])
    | WhileLoop AST [AST]
    | ReturnStatement AST
    deriving Show

data VarKind = Local | Global deriving Show

data BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Mod
    deriving Show
data ComparisonOperator
    = Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    deriving (Show)

data UnaryOperator
    = Negate
    | Increment
    | Decrement
    deriving Show

data Error
    = EndOfFile
    | Unexpected String
    | Missing String
    | Custom String
    | Empty
    deriving Show

newtype Parser a = Parser { runParser :: String -> Either Error (a, String)}


instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (output, rest) <- p input
        return (f output, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    (Parser l) <*> (Parser r) = Parser $ \input -> do
        (f, rest) <- l input
        (output', rest') <- r rest
        return (f output', rest')

instance Alternative Parser where
    empty = Parser $ \input -> Left $ Missing ("idk?( "  ++ (take 25 $ input) ++ "... )")
    (Parser l) <|> (Parser r)
        = Parser $ \input -> case l input of
            Left _ -> r input
            Right result -> Right result

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> case p input of
        Left err -> Left err
        Right (result, rest) -> runParser (f result) rest

charP :: Char -> Parser Char
charP p = Parser $ \input -> case input of
    (x:xs) -> case x == p of
        True -> Right (x, xs)
        False -> Left $ Unexpected [x]
    _ -> Left $ EndOfFile

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser [Char]
spanP p = Parser $ \input -> Right (span p input)

ws :: Parser String
ws = (spanP isSpace) <|> par

par :: Parser String
par = spanP (\x -> x == '(' || x == ')') 

sepBy sep element = (:) <$> element <*> many (sep *> element)

parseBooleanLiteral :: Parser AST
parseBooleanLiteral = (\x -> if x == "true"
                                then BooleanLiteral True
                                else BooleanLiteral False
                    ) <$> (stringP "true" <|> stringP "false")

parseStringLiteral :: Parser AST
parseStringLiteral = StringLiteral <$> (charP '"' *> spanP (/='"') <* charP '"')


parseNumericLiteral :: Parser AST
parseNumericLiteral = NumericLiteral <$> parseNumber
  where
    parseNumber :: Parser Double
    parseNumber = do
      integerPart <- spanP isDigit
      fractionalPart <- optionalFractionalPart
      let numberString = integerPart ++ maybe "" (\frac -> '.' : frac) fractionalPart
      case reads numberString of
        [(number, _)] -> return number
        _             -> empty

    optionalFractionalPart :: Parser (Maybe String)
    optionalFractionalPart = optional (charP '.' *> spanP isDigit)

parseLiteral :: Parser AST
parseLiteral = parseBooleanLiteral <|> parseStringLiteral <|> parseNumericLiteral

parseIdentifier :: Parser String
parseIdentifier =  do
  result <- spanP isAlphaNum
  if null result
    then empty
    else return result

parseIdentifier' :: Parser AST
parseIdentifier' = Identifier <$> spanP isAlphaNum

parseAssignment :: Parser AST
parseAssignment = local <|> global
    where 
        global = (\a b -> AssignmentStatement Global (Identifier a) b) <$> (ws *> parseIdentifier <* ws <* charP '=' <* ws) <*> (parseLiteral <|> parseBinaryExpression)
        local = (\a b -> AssignmentStatement Local (Identifier a) b) <$> (ws *> stringP "local" *> ws *> parseIdentifier <* ws <* charP '=' <* ws) <*> (parseLiteral <|> parseBinaryExpression)

parseFunctionCall :: Parser AST
parseFunctionCall = FunctionCall <$> (ws *> parseIdentifier <* charP '(' <* ws) <*> (sepBy (charP ',' <* ws) parseLiteral <* ws <* charP ')')

parseBinaryAdd :: Parser AST
parseBinaryAdd =  BinaryExpression Add <$> (ws *> (parseNumericLiteral <|> parseIdentifier') <* ws) <*> (charP '+' *> ws *> (parseNumericLiteral <|> parseIdentifier') <* ws)
parseBinarySub :: Parser AST
parseBinarySub =  BinaryExpression Subtract <$> (ws *> parseNumericLiteral <* ws) <*> (charP '-' *> ws *> parseNumericLiteral <* ws)
parseBinaryMul :: Parser AST
parseBinaryMul =  BinaryExpression Multiply <$> (ws *> parseNumericLiteral <* ws) <*> (charP '*' *> ws *> parseNumericLiteral <* ws)
parseBinaryMod :: Parser AST
parseBinaryMod =  BinaryExpression Mod <$> (ws *> parseNumericLiteral <* ws) <*> (charP '%' *> ws *> parseNumericLiteral <* ws)

parseBinaryExpression :: Parser AST
parseBinaryExpression = parseBinaryAdd <|> parseBinarySub <|> parseBinaryMul <|> parseBinaryMod

parseBooleanExpression :: Parser AST
parseBooleanExpression = (\x y z -> BooleanExpression (f y) x z)<$> (ws *> (parseLiteral <|> parseFunctionCall <|> parseIdentifier') <* ws) <*> (op <* ws) <*> ((parseLiteral <|> parseFunctionCall <|> parseIdentifier') <* ws)
    where
        op = stringP ">" <|> stringP ">=" <|> stringP "<" <|> stringP "<=" <|> stringP "==" <|> stringP "!="
        f ">" = GreaterThan
        f ">=" = GreaterThanOrEqual
        f "<" = LessThan
        f "<=" = LessThanOrEqual
        f "==" = Equal
        f "!=" = NotEqual

parseFunctionDefinition :: Parser AST
parseFunctionDefinition = (\funcname parnames block -> FunctionDefinition funcname parnames block)
        <$> (ws *> stringP "function" *> ws *> parseIdentifier <* charP '(' <* ws)
        <*> (sepBy (charP ',' <* ws) parseIdentifier <* ws <* charP ')' <* ws)
        <*> many parseAST <* ws <* stringP "end"
parseWhileLoop :: Parser AST
parseWhileLoop = (\x y -> WhileLoop x y)
        <$> (ws *> stringP "while" *> ws *> parseBooleanExpression <* ws <* stringP "do")
        <*> many parseAST <* ws <* stringP "end"

parseElseIfStatement :: Parser AST
parseElseIfStatement = (\x y z -> IfStatement x y (Just[z])) <$> (ws *> stringP "elseif" *> parseBooleanExpression <* ws <* stringP "then" <* ws)
                <*> (many parseAST <* ws)
                <*> (parseElseIfStatement <|> parseElseStatement)

parseElseStatement :: Parser AST
parseElseStatement = (\y z -> ElseStatement y Nothing) <$> (ws *> stringP "else" *> ws *> many parseAST <* ws)
                <*> (ws *> stringP "end" <* ws)
parseIfStatement :: Parser AST
parseIfStatement = (\x y z -> IfStatement x y (Just [z])) <$> (ws *> stringP "if" *> parseBooleanExpression <* ws <* stringP "then" <* ws)
                <*> (many parseAST <* ws)
                <*> (parseElseIfStatement <|> parseElseStatement)
parseSingleIfStatement :: Parser AST
parseSingleIfStatement = (\x y _ -> IfStatement x y Nothing) <$> (ws *> stringP "if" *> parseBooleanExpression <* ws <* stringP "then" <* ws)
                <*> (many parseAST <* ws)
                <*> (ws *> stringP "end" <* ws)

parseReturnStatement = ReturnStatement <$> (ws *> stringP "return" *> ws *> (parseLiteral <|> parseBinaryExpression <|> parseFunctionCall <|> parseIfStatement <|> parseIdentifier') <* ws)

parseAST :: Parser AST
parseAST = parseAssignment <|> parseFunctionCall <|> parseBinaryExpression <|> parseFunctionDefinition <|> parseBooleanExpression <|> parseWhileLoop <|> parseSingleIfStatement <|> parseIfStatement <|> parseReturnStatement