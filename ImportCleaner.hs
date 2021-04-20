{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ImportCleaner where

import Text.Parsec.String
import Text.Parsec
import Control.Applicative (liftA2)
import Data.Bifunctor 
import Data.List (intercalate)

whitespace = " \t\n"

lexeme p = p <* many (oneOf whitespace)

identifier :: Parser String
identifier = lexeme $ many1 (alphaNum <|> oneOf "._")

exposing = lexeme $ string "exposing"

type IdentList = Maybe [IListItem]

data IListItem
    = Simple String
    | Recurs String IdentList
    deriving Show

importList :: Parser IdentList
importList =
 do lexeme (char '(')
    b <- importBody
    lexeme (char ')')
    return b
  where
    importBody :: Parser IdentList
    importBody = 
        lexeme (Nothing <$ string "..") <|> fmap Just (importIdentifier `sepBy` lexeme (char ','))
    importIdentifier :: Parser IListItem
    importIdentifier =
     do base <- identifier
        maybe (Simple base) (Recurs base) <$> optionMaybe importList

headerLine s =
 do lexeme (string s)
    x <- identifier
    exposing
    exports <- importList
    return (x, exports)

header = (,) <$> headerLine "module" <*> many (headerLine "import")


formatModule (name, exports) = "module " ++ name ++ " exposing " ++ formatIdentList exports

formatImport (name, imports) = "import " ++ name ++ " exposing " ++ formatIdentList imports

formatIdentList list = "( " ++ maybe ".." body list ++ " )"
    where body = intercalate " , " . map (\case
            Simple s -> s
            Recurs s list -> s ++ ' ':formatIdentList list)

formatAll m is = intercalate "\n" $ formatModule m : "\n" : map formatImport is

parseFile :: String -> IO ()
parseFile path =
  do
    text <- readFile path
    either print (putStrLn . uncurry formatAll) $ parse header path text
