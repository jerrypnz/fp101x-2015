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
           | JsBoolean Bool
           | JsNull
           deriving Show

skipSpace :: Parser String
skipSpace = many $ sat isSpace

escapeChar :: Parser Char
escapeChar  = do char '\\'
                 c <- item
                 return $ case c of
                   'n'  -> '\n'
                   'r'  -> '\r'
                   'b'  -> '\b'
                   't'  -> '\t'
                   '\\' -> '\\'
                   _    -> c

commaSeparated  :: Parser a -> Parser [a]
commaSeparated p = do skipSpace
                      elems <- (do e  <- p
                                   es <- many (do skipSpace
                                                  char ','
                                                  skipSpace
                                                  p)
                                   return $ e:es) +++ return []
                      skipSpace
                      return elems

keyValuePair  :: Parser a -> Parser (String, a)
keyValuePair p = do skipSpace
                    JsString key <- jsString
                    skipSpace
                    char ':'
                    skipSpace
                    val <- p
                    return (key, val)

json :: Parser JsVal
json  = jsInt +++ jsString +++ jsArray +++ jsObject +++ jsBoolean +++ jsNull

jsNull :: Parser JsVal
jsNull  = do string "null"
             return JsNull

jsBoolean :: Parser JsVal
jsBoolean  = (do string "true"
                 return $ JsBoolean True) +++
             (do string "false"
                 return $ JsBoolean False)

jsString :: Parser JsVal
jsString  = do char '"'
               s <- (many $ escapeChar +++ sat ('"' /=))
               char '"'
               return $ JsString s

jsInt :: Parser JsVal
jsInt  = do s  <- digit
            ss <- (many digit)
            return $ JsInteger (read (s:ss)::Int)

jsArray :: Parser JsVal
jsArray  = do char '['
              elems <- commaSeparated json
              char ']'
              return $ JsArray elems

jsObject :: Parser JsVal
jsObject  = do char '{'
               kvs <- commaSeparated $ keyValuePair json
               char '}'
               return $ JsObject kvs
