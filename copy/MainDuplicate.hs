{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -prof -fprof-auto -rtsopts #-}
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

type Rule = (Term, Term, Maybe Int)

data TRSError = 
    InvalidFunctionName String Int    
    | InvalidVariableName String Int    
    | InvalidArity String Int String       
    | InvalidFunctionStructure String Int String 
    | InvalidRuleStructure Int String   
    deriving (Show, Eq)

getFunsInRule :: Rule -> [String]
getFunsInRule (lhs, rhs, _) = getFunsInTerm lhs ++ getFunsInTerm rhs

getFunsInTerm :: Term -> [String]
getFunsInTerm (Var _) = []
getFunsInTerm (Fun f args) = f : concatMap getFunsInTerm args

getRuleStructure :: Rule -> String
getRuleStructure (lhs, rhs, cost) = 
    let getTermStructure (Var v) = "V"
        getTermStructure (Fun _ args) = "F" ++ show (length args) ++ 
                                      concatMap getTermStructure args

        vars = nub (collectVars lhs ++ collectVars rhs)
        varMap = Map.fromList (zip vars (map (\i -> "x" ++ show i) [1..]))

        normalizeForStructure (Var v) = Var (varMap Map.! v)
        normalizeForStructure (Fun _ args) = Fun "F" (map normalizeForStructure args)

        normLhs = normalizeForStructure lhs
        normRhs = normalizeForStructure rhs
        costStr = maybe "" (\c -> ":cost" ++ show c) cost
    in show (normLhs, normRhs) ++ costStr

reportProgress :: String -> Int -> Int -> IO ()
reportProgress phase current total = do
    let percentage = (fromIntegral current / fromIntegral total * 100) :: Double
    printf "\r%s: %d/%d (%.2f%%) " phase current total percentage
    hFlush stdout
    when (current == total) (putStrLn "")

generateOutputNames :: FilePath -> (FilePath, FilePath, FilePath)
generateOutputNames dir = 
    let baseName = map replaceSlash (takeBaseName dir)
        duplicateFile = "duplicate_files_" ++ baseName ++ ".txt"
        errorFile = "errors_" ++ baseName ++ ".txt"
        performanceFile = "performance_" ++ baseName ++ ".txt"
    in (duplicateFile, errorFile, performanceFile)
  where
    replaceSlash '/' = '_'
    replaceSlash '\\' = '_'
    replaceSlash c = c

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
          afterRule = dropWhile isSpace (drop 4 withoutOuterParens)
          terms = splitTermsInRule (trim afterRule)
      in not (case terms of
          [_, _] -> True
          [_, _, ":cost 0"] -> True
          _ -> False)
    ]

checkVariableNames :: [(Int, String)] -> [TRSError]
checkVariableNames lines = 
    [ InvalidVariableName var lineNum
    | (lineNum, line) <- lines,
      isPrefixOf "(rule" line,
      let rule = parseRule line,
      Just (lhs, rhs, _) <- [rule],
      let vars = nub (collectVars lhs ++ collectVars rhs),
      var <- vars,
      any (\c -> c `elem` ['(', ')', ';']) var
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

parseFun :: String -> (String, Int)
parseFun s = 
    let parts = words (filter (\c -> c /= '(' && c /= ')') s)
    in (parts !! 1, read (parts !! 2))

parseRule :: String -> Maybe Rule
parseRule s = 
    let withoutOuterParens = init (tail (trim s))
        afterRule = dropWhile isSpace (drop 4 withoutOuterParens)
        terms = splitTermsInRule afterRule
    in case terms of
        [lhs, rhs] -> Just (parseTerm lhs, parseTerm rhs, Nothing)
        [lhs, rhs, cost] | trim cost == ":cost 0" -> Just (parseTerm lhs, parseTerm rhs, Just 0)
        _ -> Nothing

splitAtCost :: String -> (String, Maybe String)
splitAtCost input = 
    case break (==':') (reverse input) of
        (rev_rest, ':':rev_terms) | "cost" `isPrefixOf` (reverse rev_rest) -> 
            let terms = reverse rev_terms
                costValue = trim (drop 4 (reverse rev_rest))
            in (trim terms, Just (":cost " ++ costValue))
        _ -> (trim input, Nothing)

splitTermsInRule :: String -> [String]
splitTermsInRule input = 
    case splitAtCost input of
        (termPart, Just costPart) -> parseTerms termPart ++ [costPart]
        (termPart, Nothing) -> parseTerms termPart
  where
    parseTerms = go 0 "" []
      where
        go depth acc terms [] = 
            if null acc 
            then reverse terms 
            else reverse (trim (reverse acc) : terms)
        go depth acc terms (c:cs) = case c of
            '(' -> go (depth + 1) (c:acc) terms cs
            ')' -> let newDepth = depth - 1
                   in if newDepth == 0 
                      then go newDepth "" (trim (reverse (c:acc)) : terms) cs
                      else go newDepth (c:acc) terms cs
            ' ' | depth == 0 -> 
                if null acc
                then go depth "" terms cs
                else go depth "" (trim (reverse acc) : terms) cs
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
trim = dropWhile isSpace.reverse.dropWhile isSpace.reverse

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
normalizeRule funMap (lhs, rhs, cost) = 
    let vars = nub (collectVars lhs ++ collectVars rhs)
        varMap = Map.fromList (zip vars (map (\i -> "x" ++ show i) [1..]))
    in (normalizeTerm funMap varMap lhs, 
        normalizeTerm funMap varMap rhs,
        cost)

collectVars :: Term -> [String]
collectVars (Var v) = [v]
collectVars (Fun _ ts) = nub (concatMap collectVars ts)

normalizeTerm :: Map.Map String String -> Map.Map String String -> Term -> Term
normalizeTerm funMap varMap term = case term of
    Var v -> if Map.member v funMap
            then Fun (funMap Map.! v) []
            else Var (Map.findWithDefault v v varMap)
    Fun f ts -> Fun (Map.findWithDefault f f funMap) 
                    (map (normalizeTerm funMap varMap) ts)

reportErrors :: Handle -> [TRSError] -> IO ()
reportErrors handle errors = do
    hPutStrLn handle "Found errors in TRS:"
    forM_ errors (\error -> case error of
        InvalidFunctionName fname line ->
            hPrintf handle "Line %d: Invalid function name '%s' (contains invalid characters)\n" 
                line fname
        InvalidVariableName vname line ->
            hPrintf handle "Line %d: Invalid variable name '%s' (contains invalid characters)\n" 
                line vname
        InvalidArity fname line arity ->
            hPrintf handle "Line %d: Invalid arity %s for function '%s'\n" 
                line arity fname
        InvalidFunctionStructure fname line content ->
            hPrintf handle "Line %d: Invalid function declaration structure: %s\n" 
                line content
        InvalidRuleStructure line content ->
            hPrintf handle "Line %d: Invalid rule structure: %s\n" 
                line content)

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
    let normalizedFiles = map (\(fp, content) -> 
            (fp, normalizeTRS (parseTRS (BSC.unpack content)))) files
        buildGroups [] groups = groups
        buildGroups ((fp, trs):rest) groups =
            let groups' = if HM.member trs groups
                         then HM.adjust (fp:) trs groups
                         else HM.insert trs [fp] groups
            in buildGroups rest groups'
        groups = buildGroups normalizedFiles HM.empty
        dupGroups = HM.elems groups
    in filter (\g -> length g > 1) dupGroups

printOutputToHandle handle [group] = mapM_ (hPutStrLn handle) group
printOutputToHandle handle (group:groups) = do
    mapM_ (hPutStrLn handle) group
    hPutStrLn handle ""
    printOutputToHandle handle groups

processWithChecks :: Handle -> FilePath -> BS.ByteString -> IO (Maybe TRS)
processWithChecks handle filepath content = do
    let input = BSC.unpack content
    case checkTRS input of
        [] -> do
            let result = parseTRS input
            return (Just result)
        errors -> do
            hPutStrLn handle ("\nErrors in file: " ++ filepath)
            reportErrors handle errors
            return Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            let (duplicateFile, errorFile, performanceFile) = generateOutputNames dir
            
            errorHandle <- openFile errorFile WriteMode
            hSetBuffering errorHandle LineBuffering

            startTime <- getCurrentTime
            files <- findAriFiles dir
            putStrLn ("Found " ++ show (length files) ++ " .ari files")
            fileContents <- forM (zip [1..] files) (\(idx, f) -> do
                reportProgress "Reading files" idx (length files)
                content <- BS.readFile f
                processed <- processWithChecks errorHandle f content
                return (case processed of 
                    Just _ -> Just (f, content)
                    Nothing -> Nothing))
            let validFiles = mapMaybe id fileContents
            reportProgress "Processing" (length validFiles) (length validFiles)
            
            let groups = groupByContent validFiles
            withFile duplicateFile WriteMode (\dupHandle -> do
                printOutputToHandle dupHandle groups)
            
            endTime <- getCurrentTime
            let duration = diffUTCTime endTime startTime
            printf "\nProcessing completed in %.2f seconds\n" 
                (realToFrac duration :: Double)
            printf "Results written to %s\n" duplicateFile
            printf "Errors written to %s\n" errorFile
            printf "Performance information written to %s\n" performanceFile
            
            hClose errorHandle
        
        _ -> putStrLn "Error: Wrong input"
