{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module ImportCleaner where

import Text.Parsec.String ( Parser )
import Text.Parsec
import Control.Applicative (liftA2)
import Data.Bifunctor ( Bifunctor(second) ) 
import Data.List (find, isSuffixOf, nub, intercalate, sort, sortOn, isPrefixOf, groupBy)
import qualified Data.Map.Strict as M
import Data.Char (isUpper)
import Control.Arrow ( Arrow((&&&)) )
import System.Directory ( getDirectoryContents )

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
    All <= (List _) = True
    _ <= _ = False

intoMaybe :: IdentList -> Maybe [IListItem]
intoMaybe All = Nothing
intoMaybe (List ls) = Just ls

data IListItem
    = Simple String
    | Recurs String IdentList
    deriving (Show, Eq)

instance Ord IListItem where
    (Recurs a xs) <= (Recurs b ys)
        | ys < xs = True
        | ys > xs = False
        | otherwise = a <= b
    (Recurs _ _) <= (Simple _) = True
    (Simple _) <= (Recurs _ _) = False
    (Simple a@(ah:at)) <= (Simple b@(bh:bt))
        | isUpper ah && not (isUpper bh) = True
        | not (isUpper ah) && isUpper bh = False
        | otherwise = a <= b

type HeaderLine = (String, Maybe IdentList)

intoTuple :: IListItem -> HeaderLine
intoTuple (Simple s) = (s, Nothing)
intoTuple (Recurs s list) = (s, Just list)

fromTuple :: HeaderLine -> IListItem
fromTuple (s, mi) = maybe (Simple s) (Recurs s) mi

importList :: Parser IdentList
importList =
 do lexeme (char '(')
    b <- lexeme (All <$ string "..") <|> fmap List (importIdentifier `sepBy` lexeme (char ','))
    lexeme (char ')')
    return b
  where
    importIdentifier =
     do base <- identifier
        maybe (Simple base) (Recurs base) <$> optionMaybe importList

headerLine :: String -> Parser HeaderLine
headerLine s =
 do lexeme (string s)
    x <- identifier
    exports <- optionMaybe (exposing *> importList)
    return (x, exports)

header = (,) <$> headerLine "module" <*> many (headerLine "import")

elmFile = (,) <$> header <*> many anyChar

cleanIdentList :: IdentList -> IdentList
cleanIdentList All = All
cleanIdentList (List ls) = List . sort . unique . map fromTuple . M.toList . makeMap $ ls
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
cleanLines = sortOn (groupNumber &&& fst) . map (second $ fmap cleanIdentList) . M.toList . M.fromListWith combineFn
    where
        combineFn Nothing x = x
        combineFn x Nothing = x
        combineFn (Just All) _ = Just All
        combineFn _ (Just All) = Just All
        combineFn (Just (List a)) (Just (List b)) = Just (List (a ++ b))

groupNumber :: HeaderLine -> Int        
groupNumber (s, _) = maybe 0 snd $ find ((\f -> f s) . fst)
    [ (("Html" `isPrefixOf`), -1)
    , ((`elem` ["Utils", "Either"]), 1)
    , (("Tuple" ==), 2)
    , (("List" `isPrefixOf`), 2)
    , (("Vector" `isPrefixOf`), 2)
    , (("Basics.Extra" ==), 3)
    ]

formatModule :: HeaderLine -> String
formatModule (name, exports) = "module " ++ name ++ maybe "" ((++) " exposing " . formatIdentList) exports

formatImport :: HeaderLine -> String
formatImport (name, imports) = "import " ++ name ++ maybe "" ((++) " exposing " . formatIdentList) imports

formatIdentList :: IdentList -> String
formatIdentList list = "(" ++ maybe ".." ((\s -> " " ++ s ++ " ") . body) (intoMaybe list) ++ ")"
    where body = intercalate " , " . map ((\(s, ma) -> s ++ maybe "" ((' ':) . formatIdentList) ma) . intoTuple)

formatAll m = ((formatModule m ++ "\n\n") ++) . ungroup . number . cleanLines
    where
        number = map (groupNumber &&& formatImport) 
        ungroup = intercalate "\n\n" . map (intercalate "\n" . map snd) . groupBy (\a b -> fst a == fst b)

cleanFile path =
 do text <- readFile path
    case parse elmFile path text of
        Left err -> 
         do putStrLn $ "Error parsing file: " ++ path
            print err
        Right (h, body) ->
         do let formatted = uncurry formatAll h
            let !_body = body  -- force eval before write
            writeFile path (formatted ++ "\n\n" ++ _body)

cleanDirectory :: FilePath -> IO ()
cleanDirectory dirPath =
 do paths <- getDirectoryContents dirPath
    let files = filter (isSuffixOf ".elm") paths
    let file_paths = map ((dirPath ++ "/") ++) files
    sequence_ (cleanFile <$> file_paths)

main = cleanDirectory "src"
