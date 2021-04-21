{-# LANGUAGE FlexibleContexts #-}
module ImportCleaner where

import Text.Parsec.String
import Text.Parsec
import Control.Applicative (liftA2)
import Data.Bifunctor 
import Data.List (nub, intercalate, sort, sortOn, isPrefixOf, groupBy)
import qualified Data.Map.Strict as M
import Data.Char (isUpper)
import Control.Arrow (Arrow((&&&)))

whitespace = " \t\n"

lexeme p = p <* many (oneOf whitespace)

identifier :: Parser String
identifier = lexeme $ many1 (alphaNum <|> oneOf "._")

exposing = lexeme $ string "exposing"

data IdentList 
    = All
    | List [IListItem]
    deriving (Show, Eq)

instance Ord IdentList where
    All <= (List _) = False
    _ <= _ = True

intoMaybe :: IdentList -> Maybe [IListItem]
intoMaybe All = Nothing
intoMaybe (List ls) = Just ls

fromMaybe :: Maybe [IListItem] -> IdentList
fromMaybe = maybe All List

data IListItem
    = Simple String
    | Recurs String IdentList
    deriving (Show, Eq)

instance Ord IListItem where
    (Recurs a xs) <= (Recurs b ys)
        | ys < xs = False
        | ys > xs = True
        | a < b = False -- swapped to get alpha order
        | otherwise = True
    (Recurs _ _) <= (Simple _) = False
    (Simple _) <= (Recurs _ _) = True
    (Simple (ah:at)) <= (Simple (bh:bt))
        | isUpper ah && not (isUpper bh) = False
        | not (isUpper ah) && isUpper bh = True
    (Simple a) <= (Simple b) = a >= b -- swapped to get alpha order

type HeaderLine = (String, IdentList)

intoTuple :: IListItem -> (String, Maybe IdentList)
intoTuple (Simple s) = (s, Nothing)
intoTuple (Recurs s list) = (s, Just list)

fromTuple :: (String, Maybe IdentList) -> IListItem
fromTuple (s, mi) = maybe (Simple s) (Recurs s) mi

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
cleanIdentList All = All
cleanIdentList (List ls) = List . reverse . sort . unique . map fromTuple . M.toList . makeMap $ ls
    where
        makeMap = M.fromListWith combineFn . map intoTuple
        combineFn :: Maybe IdentList -> Maybe IdentList -> Maybe IdentList
        combineFn _ (Just All) = Just All
        combineFn (Just All) _ = Just All
        combineFn Nothing (Just x) = Just $ cleanIdentList x
        combineFn (Just x) Nothing = Just $ cleanIdentList x
        combineFn (Just (List as)) (Just (List bs)) = Just . cleanIdentList . List $ as ++ bs

unique = reverse . nub . reverse

cleanLines :: [HeaderLine] -> [HeaderLine]
cleanLines = sortOn (groupNumber &&& fst) . map (second cleanIdentList) . M.toList . M.fromListWith combineFn
    where
        combineFn All _ = All
        combineFn _ All = All
        combineFn (List a) (List b) = List (a ++ b)

groupNumber :: HeaderLine -> Int        
groupNumber (s, _)
    | "Html" `isPrefixOf` s = -1
    | s `elem` ["Utils", "Either"] = 1
    | "Vector" `isPrefixOf` s = 2
    | "Basics.Extra" == s = 3
groupNumber _ = 0

formatModule :: HeaderLine -> String
formatModule (name, exports) = "module " ++ name ++ " exposing " ++ formatIdentList exports

formatImport :: HeaderLine -> String
formatImport (name, imports) = "import " ++ name ++ " exposing " ++ formatIdentList imports

formatIdentList :: IdentList -> String
formatIdentList list = "( " ++ maybe ".." body (intoMaybe list) ++ " )"
    where body = intercalate " , " . map ((\(s, ma) -> s ++ maybe "" ((' ':) . formatIdentList) ma) . intoTuple)

formatAll m = (++) (formatModule m ++ "\n\n") . ungroup . number . cleanLines
    where
        number = map (groupNumber &&& formatImport) 
        ungroup = intercalate "\n\n" . map (intercalate "\n" . map snd) . groupBy (\a b -> fst a == fst b)

parseFile :: String -> IO ()
parseFile path =
 do text <- readFile path
    either print (putStrLn . uncurry formatAll) $ parse header path text
