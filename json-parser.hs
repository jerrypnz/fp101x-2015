import Data.Char

-- Parser data type and monad implementation
data Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
  Parser f >>= k = Parser $ \inp ->
    [(v2, out2) | (v1, out1) <- f inp, (v2, out2) <- parse (k v1) out1]

  return v = Parser $ \inp -> [(v,inp)]


item :: Parser Char
item = Parser $ \inp -> case inp of
                          []     -> []
                          (x:xs) -> [(x,xs)]

failure :: Parser a
failure = Parser $ \inp -> []

(+++) :: Parser a -> Parser a -> Parser a
Parser p +++ Parser q = Parser $ \inp -> case p inp of
                                          []        -> q inp
                                          [(v,out)] -> [(v,out)]

parse               :: Parser a -> String -> [(a, String)]
parse (Parser p) inp = p inp

sat  :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit  = sat isDigit

char  :: Char -> Parser Char
char x = sat (x ==)

many  :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1  :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

string       :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- My JSON parser starts here --
data JsVal = JsObject [(String, JsVal)]
           | JsArray [JsVal]
           | JsString String
           | JsInteger Int
           | JsDouble Double
           deriving Show

skipSpace :: Parser String
skipSpace = (many $ sat isSpace)

escapeChar :: Parser Char
escapeChar  = do char '\\'
                 c <- item
                 return $ case c of
                   'n' -> '\n'
                   _   -> c

json :: Parser JsVal
json = jsInt +++ jsString +++ jsArray

jsString :: Parser JsVal
jsString  = do char '"'
               s <- (many $ escapeChar +++ sat ('"' /=))
               char '"'
               return $ JsString s

jsInt :: Parser JsVal
jsInt = do s  <- digit
           ss <- (many digit)
           return $ JsInteger (read (s:ss)::Int)

jsArray :: Parser JsVal
jsArray = do char '['
             skipSpace
             s <- json
             ss <- many (do skipSpace
                            char ','
                            skipSpace
                            json)
             char ']'
             return $ JsArray (s:ss)
