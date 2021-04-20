{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ImportCleaner where

import Text.Parsec.String
import Text.Parsec
import Control.Applicative (liftA2)
import Data.Bifunctor 
import Data.List (nub, intercalate)
import qualified Data.Map.Strict as M

whitespace = " \t\n"

lexeme p = p <* many (oneOf whitespace)

identifier :: Parser String
identifier = lexeme $ many1 (alphaNum <|> oneOf "._")

exposing = lexeme $ string "exposing"

data IdentList 
    = All
    | List [IListItem]
    deriving (Show, Eq)

intoMaybe :: IdentList -> Maybe [IListItem]
intoMaybe All = Nothing
intoMaybe (List ls) = Just ls

data IListItem
    = Simple String
    | Recurs String IdentList
    deriving (Show, Eq)

intoTuple :: IListItem -> (String, Maybe IdentList)
intoTuple (Simple s) = (s, Nothing)
intoTuple (Recurs s list) = (s, Just list)

importList :: Parser IdentList
importList =
 do lexeme (char '(')
    b <- importBody
    lexeme (char ')')
    return b
  where
    importBody :: Parser IdentList
    importBody = 
        lexeme (All <$ string "..") <|> fmap List (importIdentifier `sepBy` lexeme (char ','))
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

cleanIdentList :: IdentList -> IdentList
cleanIdentList = 
    \case
        All -> All
        List list -> undefined 
            where
                m = M.fromListWith insertFn $ map intoTuple list
                insertFn :: Maybe IdentList -> Maybe IdentList -> Maybe IdentList
                insertFn _ (Just All) = Just All
                insertFn (Just All) _ = Just All
                insertFn Nothing o = o
                insertFn o Nothing = o
                insertFn (Just (List as)) (Just (List bs)) = Just . List . reverse . nub . reverse $ (as ++ bs)

formatModule :: (String, IdentList) -> String
formatModule (name, exports) = "module " ++ name ++ " exposing " ++ formatIdentList exports

formatImport :: (String, IdentList) -> String
formatImport (name, imports) = "import " ++ name ++ " exposing " ++ formatIdentList imports

formatIdentList :: IdentList -> String
formatIdentList list = "( " ++ maybe ".." body (intoMaybe list) ++ " )"
    where body = intercalate " , " . map ((\(s, ma) -> s ++ maybe "" ((' ':) . formatIdentList) ma) . intoTuple)

formatAll m is = intercalate "\n" $ formatModule m : "\n" : map formatImport is

parseFile :: String -> IO ()
parseFile path =
  do
    text <- readFile path
    either print (putStrLn . uncurry formatAll) $ parse header path text
