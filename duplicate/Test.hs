{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import System.IO
import Text.Printf
import Data.Time.Clock
import Data.Hashable

data TRS = TRS {
    funs :: [(String, Int)],
    rules :: [Rule],
    hashValue :: Int
} deriving (Show, Eq, Ord)

data Term = Var String
          | Fun String [Term]
          deriving (Show, Eq, Ord)

instance Hashable Term where
    hashWithSalt salt (Var v) = salt `hashWithSalt` (0::Int) `hashWithSalt` v
    hashWithSalt salt (Fun f args) = salt `hashWithSalt` (1::Int) `hashWithSalt` f `hashWithSalt` args

instance Hashable TRS where
    hashWithSalt _ = hashValue

type Rule = (Term, Term)

data TRSError = 
    InvalidFunctionName String Int    
    | InvalidVariableName String Int    
    | InvalidArity String Int String       
    | InvalidFunctionStructure String Int String 
    | InvalidRuleStructure Int String   
    deriving (Show, Eq)

collectVars :: Term -> [String]
collectVars (Var v) = [v]
collectVars (Fun _ ts) = nub (concatMap collectVars ts)

parseFun :: String -> (String, Int)
parseFun s = 
    let parts = words (filter (\c -> c /= '(' && c /= ')') s)
    in (parts !! 1, read (parts !! 2))

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
    if head s' /= '(' || last s' /= ')'
    then Var s'
    else
        let withoutParens = init (tail s')
            parts = splitTermsInRule withoutParens
            funName = head parts
            args = map parseTerm (tail parts)
        in Fun funName args

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

checkFunctionNames :: [(Int, String)] -> [TRSError]
checkFunctionNames lines = 
    [ InvalidFunctionName fname lineNum 
    | (lineNum, line) <- lines,
        let parts = words line,
        length parts >= 2,
        let fname = takeWhile (/= ')') (dropWhile (=='(') (parts !! 1)),
        any (\c -> c `elem` ['(', ')', ';']) fname || null fname
    ]

checkArities :: [(Int, String)] -> [TRSError]
checkArities lines =
    [ InvalidArity fname lineNum arityStr
    | (lineNum, line) <- lines,
        isPrefixOf "(fun" line,
        let parts = words line,
        length parts >= 3,
        let fname = takeWhile (/= ')') (dropWhile (=='(') (parts !! 1)),
        let arityStr = takeWhile (/= ')') (parts !! 2),
        let parseResult = reads arityStr :: [(Int, String)],
        null parseResult || case parseResult of
                            [(n, "")] -> n < 0 
                            _ -> True
    ]

checkFunctionStructures :: [(Int, String)] -> [TRSError]
checkFunctionStructures lines =
    [ InvalidFunctionStructure fname lineNum line
    | (lineNum, line) <- lines,
        isPrefixOf "(fun" line,
        let parts = words line,
        not (length parts == 3),
        let fname = if length parts >= 2 
                then takeWhile (/= ')') (dropWhile (=='(') (parts !! 1))
                else ""
    ]

checkRuleStructures :: [(Int, String)] -> [TRSError]
checkRuleStructures lines = 
    [ InvalidRuleStructure lineNum line
    | (lineNum, line) <- lines,
        isPrefixOf "(rule" line,
        let withoutOuterParens = init (tail (trim line))
            afterRule = drop 4 withoutOuterParens
            terms = splitTermsInRule (trim afterRule)
        in case terms of
            [_, _] -> False
            _ -> True
    ]

checkVariableNames :: [(Int, String)] -> [TRSError]
checkVariableNames lines = 
    [ InvalidVariableName var lineNum
    | (lineNum, line) <- lines,
        isPrefixOf "(rule" line,
        let rule = parseRule line,
        Just (lhs, rhs) <- [rule],
        let vars = nub (collectVars lhs ++ collectVars rhs),
        var <- vars,
        any (\c -> c `elem` ['(', ')', ';']) var
    ]

getFunsInRule :: Rule -> [String]
getFunsInRule (lhs, rhs) = getFunsInTerm lhs ++ getFunsInTerm rhs

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

reportProgress :: String -> Int -> Int -> IO ()
reportProgress phase current total = do
    let percentage = (fromIntegral current / fromIntegral total * 100) :: Double
    printf "\r%s: %d/%d (%.2f%%) " phase current total percentage
    hFlush stdout
    when (current == total) (putStrLn "")

parseRule :: String -> Maybe Rule
parseRule s = do
    let withoutOuterParens = init (tail (trim s))
        afterRule = drop 4 withoutOuterParens
        terms = splitTermsInRule (trim afterRule)
    case terms of
        [lhs, rhs] -> Just (parseTerm lhs, parseTerm rhs)
        _ -> Nothing

checkTRS :: String -> [TRSError]
checkTRS input = 
    let ls = zip [1..] (filter (not . isPrefixOf ";") (lines (filter (/= '\r') input)))
        funLines = filter (\l -> isPrefixOf "(fun" (snd l)) ls
        ruleLines = filter (\l -> isPrefixOf "(rule" (snd l)) ls
    in concat [
            checkFunctionNames funLines,
            checkArities funLines,
            checkFunctionStructures funLines,
            checkRuleStructures ruleLines,
            checkVariableNames ruleLines
        ]

parseTRS :: String -> TRS
parseTRS input = 
    let ls = filter (not.isPrefixOf ";") (lines (filter (/= '\r') input))
        funLines = filter (isPrefixOf "(fun") ls
        ruleLines = filter (isPrefixOf "(rule") ls
        parsedFuns = map parseFun funLines
        parsedRules = mapMaybe parseRule ruleLines
        initialHash = hashWithSalt 0 parsedFuns `hashWithSalt` parsedRules
    in TRS parsedFuns parsedRules initialHash

normalizeTRS :: TRS -> TRS
normalizeTRS trs = 
    let rulesWithFuns = [(rule, getFunsInRule rule) | rule <- rules trs]
        
        getFunPattern fname = concat [getRuleStructure rule | (rule, funs) <- rulesWithFuns, 
                                fname `elem` funs] ++ 
                    show [funs | (rule, funs) <- rulesWithFuns,
                        fname `elem` funs]

        funsByArityAndStructure = Map.fromListWith (++) 
            [(arity, [(name, getFunPattern name)]) 
            | (name, arity) <- funs trs]

        sortedFuns = Map.map (sortBy (\a b -> compare (snd a) (snd b))) 
                    funsByArityAndStructure

        funMap = Map.fromList 
            [(name, "f" ++ show arity ++ "_" ++ show rank)
            | (arity, names) <- Map.toList sortedFuns, ((name, _), rank) <- zip names [1..]]

        normalizedRules = sort (map (normalizeRule funMap) (rules trs))
        normalizedFuns = sort (map (\(f,a) -> (Map.findWithDefault f f funMap, a)) 
                        (funs trs))
        newHash = hashWithSalt 0 normalizedFuns `hashWithSalt` normalizedRules
    in TRS normalizedFuns normalizedRules newHash

normalizeRule :: Map.Map String String -> Rule -> Rule
normalizeRule funMap (lhs, rhs) = 
    let vars = nub (collectVars lhs ++ collectVars rhs)
        varMap = Map.fromList (zip vars (map (\i -> "x" ++ show i) [1..]))
    in (normalizeTerm funMap varMap lhs, normalizeTerm funMap varMap rhs)

normalizeTerm :: Map.Map String String -> Map.Map String String -> Term -> Term
normalizeTerm funMap varMap term = case term of
    Var v -> if Map.member v funMap
            then Fun (funMap Map.! v) []
            else Var (Map.findWithDefault v v varMap)
    Fun f ts -> Fun (Map.findWithDefault f f funMap) 
                    (map (normalizeTerm funMap varMap) ts)

groupByBasename :: [FilePath] -> [[FilePath]]
groupByBasename files =
    let baseNames = map takeFileName files
        groupMap = foldl' (\acc (path, base) -> 
            Map.insertWith (++) base [path] acc) Map.empty (zip files baseNames)
    in filter (\g -> length g > 1) (Map.elems groupMap)

groupByContentMap :: [(FilePath, BS.ByteString)] -> Map.Map [FilePath] TRS
groupByContentMap files = 
    let normalizedFiles = map (\(fp, content) -> 
            (fp, normalizeTRS (parseTRS (BSC.unpack content)))) files
        
        buildGroups [] acc = acc
        buildGroups ((fp, trs):rest) acc =
            let exists = Map.foldlWithKey (\found key val -> 
                    if val == trs then Just key else found) Nothing acc
                acc' = case exists of
                    Just key -> Map.insert (fp:key) trs (Map.delete key acc)
                    Nothing -> Map.insert [fp] trs acc
            in buildGroups rest acc'
    in Map.filterWithKey (\k _ -> length k > 1) (buildGroups normalizedFiles Map.empty)

groupByContent :: [(FilePath, BS.ByteString)] -> [[FilePath]]
groupByContent files = 
    let trsMap = groupByContentMap files
    in Map.keys trsMap

findMissedDuplicates :: [[FilePath]] -> Map.Map [FilePath] TRS -> [[FilePath]]
findMissedDuplicates nameGroups contentGroups =
    let contentPaths = concat (Map.keys contentGroups)
        isInContentDups path = path `elem` contentPaths
        
        potentialMissed = filter (\group -> 
            not (all isInContentDups group) && 
            not (all (not . isInContentDups) group)) nameGroups
    in potentialMissed

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

processWithChecks :: FilePath -> BS.ByteString -> IO (Maybe TRS)
processWithChecks filepath content = do
    let input = BSC.unpack content
    case checkTRS input of
        [] -> do
            let result = parseTRS input
            return (Just result)
        errors -> do
            putStrLn ("\nErrors in file: " ++ filepath)
            reportErrors errors
            return Nothing

reportErrors :: [TRSError] -> IO ()
reportErrors [] = return ()
reportErrors errors = do
    putStrLn "Found errors in TRS:"
    forM_ errors (\error -> case error of
        InvalidFunctionName fname line ->
            printf "Line %d: Invalid function name '%s' (contains invalid characters)\n" 
                line fname
        InvalidVariableName vname line ->
            printf "Line %d: Invalid variable name '%s' (contains invalid characters)\n" 
                line vname
        InvalidArity fname line arity ->
            printf "Line %d: Invalid arity %s for function '%s'\n" 
                line arity fname
        InvalidFunctionStructure fname line content ->
            printf "Line %d: Invalid function declaration structure: %s\n" 
                line content
        InvalidRuleStructure line content ->
            printf "Line %d: Invalid rule structure: %s\n" 
                line content)

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
            startTime <- getCurrentTime
            files <- findAriFiles dir
            putStrLn ("Found " ++ show (length files) ++ " .ari files")
            let nameGroups = groupByBasename files
            fileContents <- forM (zip [1..] files) (\(idx, f) -> do
                reportProgress "Reading files" idx (length files)
                content <- BS.readFile f
                processed <- processWithChecks f content
                return (case processed of 
                    Just _ -> Just (f, content)
                    Nothing -> Nothing))
            let validFiles = mapMaybe id fileContents
            reportProgress "Processing" (length validFiles) (length validFiles)

            let contentGroups = groupByContentMap validFiles
                dupGroups = groupByContent validFiles

            let missedGroups = findMissedDuplicates nameGroups contentGroups
            
            if null missedGroups
                then putStrLn "\nNo files found with same name but different duplicate status."
                else do
                    putStrLn "\nFiles with same name but different duplicate status:"
                    printOutput missedGroups
            
            endTime <- getCurrentTime
            let duration = diffUTCTime endTime startTime
            printf "\nProcessing completed in %.2f seconds\n" 
                (realToFrac duration :: Double)
        
        _ -> putStrLn "Error: Wrong input"
