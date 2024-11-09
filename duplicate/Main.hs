{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)

data TRS = TRS {
    funs :: [(String, Int)],
    rules :: [Rule]
} deriving (Show, Eq, Ord)

data Term = Var String
          | Fun String [Term]
          deriving (Show, Eq, Ord)

type Rule = (Term, Term)

getFunsInRule :: Rule -> [String]
getFunsInRule (lhs, rhs) = nub (getFunsInTerm lhs ++ getFunsInTerm rhs)

getFunsInTerm :: Term -> [String]
getFunsInTerm (Var _) = []
getFunsInTerm (Fun f args) = f : concatMap getFunsInTerm args

getRuleStructure :: Rule -> String
getRuleStructure rule@(lhs, rhs) = 
    let
        getTermStructure :: Term -> String
        getTermStructure (Var v) = "V"
        getTermStructure (Fun _ args) = "F" ++ show (length args) ++ 
                                      concatMap getTermStructure args

        vars = nub (collectVars lhs ++ collectVars rhs)
        varMap = Map.fromList (zip vars (map (\i -> "x" ++ show i) [1..]))

        normalizeForStructure :: Term -> Term
        normalizeForStructure (Var v) = Var (varMap Map.! v)
        normalizeForStructure (Fun _ args) = 
            Fun "F" (map normalizeForStructure args)

        normLhs = normalizeForStructure lhs
        normRhs = normalizeForStructure rhs
    in show (normLhs, normRhs)

parseTRS :: String -> TRS
parseTRS input = 
    let ls = filter (not.isPrefixOf ";") (lines (filter (/= '\r') input))
        funLines = filter (isPrefixOf "(fun") ls
        ruleLines = filter (isPrefixOf "(rule") ls
        parsedFuns = map parseFun funLines
        parsedRules = mapMaybe parseRule ruleLines
    in TRS parsedFuns parsedRules

parseFun :: String -> (String, Int)
parseFun s = 
    let parts = words (filter (\c -> c /= '(' && c /= ')') s)
    in (parts !! 1, read (parts !! 2))

parseRule :: String -> Maybe Rule
parseRule s = do
    let withoutOuterParens = init (tail (trim s))
        afterRule = drop 4 withoutOuterParens
        terms = splitTermsInRule (trim afterRule)
    case terms of
        [lhs, rhs] -> Just (parseTerm lhs, parseTerm rhs)
        _ -> Nothing

splitTermsInRule :: String -> [String]
splitTermsInRule = go 0 "" []
  where
    go :: Int -> String -> [String] -> String -> [String]
    go depth acc terms [] = 
        if null acc 
        then reverse terms 
        else reverse (reverse acc : terms)
    go depth acc terms (c:cs) = case c of
        '(' -> go (depth + 1) (c:acc) terms cs
        ')' -> let newDepth = depth - 1
               in if newDepth == 0 
                  then go newDepth "" (reverse (c:acc) : terms) cs
                  else go newDepth (c:acc) terms cs
        ' ' | depth == 0 && not (null acc) -> go depth "" (reverse acc : terms) cs
        _ -> go depth (c:acc) terms cs

parseTerm :: String -> Term
parseTerm s = 
    let s' = trim s in
    if not ('(' `elem` s')
    then Var s'
    else 
        let withoutParens = init (tail s')
            parts = splitTermsInRule withoutParens
            funName = head parts
            args = map parseTerm (tail parts)
        in Fun funName args

trim :: String -> String
trim = dropWhile isSpace.reverse.dropWhile isSpace.reverse

normalizeTRS :: TRS -> TRS
normalizeTRS trs = 
    let
        getFunPattern :: String -> String
        getFunPattern fname = concat
            [getRuleStructure rule | rule <- rules trs,
             fname `elem` getFunsInRule rule]

        funsByArityAndStructure = Map.fromListWith (++) 
            [(arity, [(name, getFunPattern name)]) 
            | (name, arity) <- funs trs]

        sortedFuns = Map.map (sortBy (\a b -> compare (snd a) (snd b))) 
                    funsByArityAndStructure

        funMap = Map.fromList 
            [(name, "f" ++ show arity ++ "_" ++ show rank)
            | (arity, names) <- Map.toList sortedFuns
            , ((name, _), rank) <- zip names [1..]]

        normalizedRules = sort (map (normalizeRule funMap) (rules trs))
        normalizedFuns = sort (map (\(f,a) -> (Map.findWithDefault f f funMap, a)) 
                        (funs trs))
    in TRS normalizedFuns normalizedRules

normalizeRule :: Map.Map String String -> Rule -> Rule
normalizeRule funMap (lhs, rhs) = 
    let vars = nub (collectVars lhs ++ collectVars rhs)
        varMap = Map.fromList (zip vars (map (\i -> "x" ++ show i) [1..]))
    in (normalizeTerm funMap varMap lhs, normalizeTerm funMap varMap rhs)

collectVars :: Term -> [String]
collectVars (Var v) = [v]
collectVars (Fun _ ts) = nub (concatMap collectVars ts)

normalizeTerm :: Map.Map String String -> Map.Map String String -> Term -> Term
normalizeTerm funMap varMap term = case term of
    Var v -> Var (Map.findWithDefault v v varMap)
    Fun f ts -> Fun (Map.findWithDefault f f funMap) 
                    (map (normalizeTerm funMap varMap) ts)

areEquivalent :: FilePath -> FilePath -> BS.ByteString -> BS.ByteString -> Bool
areEquivalent path1 path2 bs1 bs2 = 
    let trs1 = parseTRS (BSC.unpack bs1)
        trs2 = parseTRS (BSC.unpack bs2)
        norm1 = normalizeTRS trs1
        norm2 = normalizeTRS trs2
    in  (norm1 == norm2)

findAriFiles :: FilePath -> IO [FilePath]
findAriFiles dir = do
    entries <- listDirectory dir
    let fullPaths = map (dir </>) entries
    files <- forM fullPaths (\path -> do
        isFile <- doesFileExist path
        isDir <- doesDirectoryExist path
        if isFile && takeExtension path == ".ari"
            then return [path]
            else if isDir
                then findAriFiles path
                else return [])
    return (concat files)

groupByContent :: [(FilePath, BS.ByteString)] -> [[FilePath]]
groupByContent files = 
    let findGroup :: [(FilePath, BS.ByteString)] -> [(FilePath, BS.ByteString)] -> [FilePath]
        findGroup [] acc = map fst acc
        findGroup ((fp, content):rest) acc =
            if null acc || areEquivalent fp (fst (head acc)) content (snd (head acc))
            then findGroup rest ((fp, content):acc)
            else findGroup rest acc

        findAllGroups :: [(FilePath, BS.ByteString)] -> [[FilePath]]
        findAllGroups [] = []
        findAllGroups remaining = 
            let group = findGroup remaining []
                notInGroup = filter (\(fp, _) -> not (fp `elem` group)) remaining
            in if null group
               then []
               else group : findAllGroups notInGroup

    in filter (\g -> length g > 1) (findAllGroups files)

printOutput :: [[FilePath]] -> IO ()
printOutput [] = return ()
printOutput [group] = mapM_ putStrLn group
printOutput (group:groups) = do
    mapM_ putStrLn group
    putStrLn ""
    printOutput groups

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            files <- findAriFiles dir
            fileContents <- mapM (\f -> do
                content <- BS.readFile f
                return (f, content)) files
            let groups = groupByContent fileContents
            printOutput groups
        _ -> putStrLn "Error: Wrong input"