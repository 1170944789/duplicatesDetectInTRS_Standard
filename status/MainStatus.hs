{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -prof -fprof-auto -rtsopts #-}
module Main where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe, catMaybes)
import System.IO
import Text.Printf
import Data.Time.Clock
import qualified Data.Csv as CSV
import qualified Data.Vector as VEC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM

data Status = 
    CertifiedYES
    | CertifiedNO
    | YES
    | NO
    | Conflict
    | CertifiedConflict
    | Unknown
    deriving (Show, Eq, Ord)

data PathType = XmlPath | AriPath
    deriving (Show, Eq, Ord)

data Result = Result {
    benchmarkPath :: String,
    solver :: String,
    result :: String,
    certified :: Bool,
    year :: String,
    category :: String,
    pathType :: PathType
} deriving (Show, Eq)

allCategories :: [String]
allCategories = [
    "TRS_Standard",
    "SRS_Standard",
    "TRS_Innermost",
    "TRS_Outermost",
    "TRS_Relative",
    "SRS_Relative",
    "Runtime_Complexity_Full_Rewriting",
    "Runtime_Complexity_Innermost_Rewriting",
    "Derivational_Complexity_Full_Rewriting",
    "Derivational_Complexity_Innermost_Rewriting",
    "SRS_Cycle"
  ]

extractCategoryFromPath :: String -> Maybe String
extractCategoryFromPath path = 
    let pathParts = splitDirectories (normalise path)
        firstPart = case pathParts of
                      ("." : part : _) -> Just part
                      (part : _) -> Just part
                      _ -> Nothing
    in firstPart >>= (\p -> if p `elem` allCategories then Just p else Nothing)

countStatuses :: Map.Map (FilePath, String) Status -> Map.Map Status Int
countStatuses statusMap =
    let statusList = Map.elems statusMap
    in foldr (\s -> Map.insertWith (+) s 1) Map.empty statusList

findInfoFiles :: FilePath -> IO [FilePath]
findInfoFiles basePath = do
    dirs <- listDirectory basePath
    let infoDirs = filter (\d -> "_info" `isSuffixOf` d && not ("certified" `isInfixOf` d)) dirs
    csvFiles <- forM infoDirs (\dir -> do
        let fullDir = basePath </> dir
        subDirs <- listDirectory fullDir
        allCsvs <- forM subDirs (\subDir -> do
            let jobDir = fullDir </> subDir
            isDir <- doesDirectoryExist jobDir
            if isDir
            then do
                files <- listDirectory jobDir
                return [jobDir </> f | f <- files, takeExtension f == ".csv"]
            else
                return [])
        return (concat allCsvs))
    return (concat csvFiles)

findCertInfoFiles :: FilePath -> IO [FilePath]
findCertInfoFiles basePath = do
    dirs <- listDirectory basePath
    let certInfoDirs = filter (\d -> "certified" `isInfixOf` d && "_info" `isInfixOf` d) dirs
    csvFiles <- forM certInfoDirs (\dir -> do
        let fullDir = basePath </> dir
        subDirs <- listDirectory fullDir
        allCsvs <- forM subDirs (\subDir -> do
            let jobDir = fullDir </> subDir
            isDir <- doesDirectoryExist jobDir
            if isDir
            then do
                files <- listDirectory jobDir
                return [jobDir </> f | f <- files, takeExtension f == ".csv"]
            else
                return [])
        return (concat allCsvs))
    return (concat csvFiles)

parseAndFilterInfoCSV :: Bool -> String -> Handle -> [(String, String, PathType)] -> FilePath -> IO [Result]
parseAndFilterInfoCSV isCertified yearStr logHandle validPaths path = do
    content <- BL.readFile path
    case CSV.decodeByName content of
        Left err -> do
            hPutStrLn logHandle ("Error parsing CSV " ++ path ++ ": " ++ err)
            return []
        Right (_, records) -> do
            let allResults = catMaybes (map (parseCSVRecord isCertified yearStr) (VEC.toList records))
                samplePaths = take 5 (map benchmarkPath allResults)
                pathFiltered = filter (\r -> any (\(origPath, cat, pType) -> 
                                                   benchmarkPath r == origPath && 
                                                   (cat == category r || cat == "") &&
                                                   pType == pathType r) validPaths) allResults
            
            hPutStrLn logHandle ("From " ++ path ++ " (" ++ yearStr ++ "):")
            hPutStrLn logHandle ("  Total records: " ++ show (VEC.length records))
            hPutStrLn logHandle ("  Valid results: " ++ show (length allResults))
            hPutStrLn logHandle ("  Sample benchmark paths: " ++ show samplePaths)
            
            when (length allResults > length pathFiltered && length allResults > 0) (do
                let rejectedByPath = filter (\r -> not (any (\(origPath, cat, pType) -> 
                                                              benchmarkPath r == origPath && 
                                                              (cat == category r || cat == "") &&
                                                              pType == pathType r) validPaths)) 
                                           (take 5 allResults)
                hPutStrLn logHandle ("  Some paths rejected by path filter: " ++ 
                                 show (map (\r -> (benchmarkPath r, category r, pathType r)) rejectedByPath)))
            hPutStrLn logHandle ("  After path filter: " ++ show (length pathFiltered))
            return pathFiltered

parseCSVRecord :: Bool -> String -> CSV.NamedRecord -> Maybe Result
parseCSVRecord isCertified yearStr record = do
    benchmarkField <- HM.lookup "benchmark" record
    solverField <- HM.lookup "solver" record
    resultField <- HM.lookup "result" record
    let benchPath = BSC.unpack benchmarkField
        solverName = BSC.unpack solverField
        resultValue = BSC.unpack resultField
        certResult = if isCertified
                     then case HM.lookup "certification-result" record of
                         Just cert -> BSC.unpack cert /= "NONE"
                         Nothing -> False
                     else False
        cat = case extractCategoryFromPath benchPath of
                Just c -> c
                Nothing -> ""
        pType = case takeExtension benchPath of
                ".xml" -> XmlPath
                ".ari" -> AriPath
                _ -> XmlPath
    if resultValue `elem` ["YES", "NO"] 
    then return (Result benchPath solverName resultValue certResult yearStr cat pType)
    else Nothing

buildPathMapping :: [(FilePath, [(String, String, PathType)])] -> Map.Map (String, String, PathType) FilePath
buildPathMapping mappings =
    let allPairs = concatMap (\(copyPath, origPaths) -> 
                             [((origPath, cat, pType), copyPath) | (origPath, cat, pType) <- origPaths]) mappings
    in Map.fromList allPairs

groupResultsByCategoryAndCopyPath :: [Result] -> Map.Map (String, String, PathType) FilePath -> [((FilePath, String), [Result])]
groupResultsByCategoryAndCopyPath results pathMapping =
    let withCopyPath = mapMaybe (\r -> do
                        copyPath <- Map.lookup (benchmarkPath r, category r, pathType r) pathMapping
                        return ((copyPath, category r), r)) results
        groupedMap = foldl' (\acc ((path, cat), result) -> 
                           Map.insertWith (++) (path, cat) [result] acc) 
                     Map.empty withCopyPath
    in Map.toList groupedMap

computeStatus :: [Result] -> Status
computeStatus results = 
    let certYES = any (\r -> result r == "YES" && certified r) results
        certNO = any (\r -> result r == "NO" && certified r) results
        uncertYES = any (\r -> result r == "YES" && not (certified r)) results
        uncertNO = any (\r -> result r == "NO" && not (certified r)) results
    in 
        if certYES && certNO then CertifiedConflict
        else if certYES then CertifiedYES
        else if certNO then CertifiedNO
        else if uncertYES && uncertNO then Conflict
        else if uncertYES then YES
        else if uncertNO then NO
        else Unknown

findAriFiles :: FilePath -> IO [FilePath]
findAriFiles dir = do
    exists <- doesDirectoryExist dir
    if not exists
    then return []
    else do
        contents <- listDirectory dir
        let properContents = filter (\name -> name /= "." && name /= "..") contents
        paths <- forM properContents (\name -> do
            let path = dir </> name
            isDir <- doesDirectoryExist path
            if isDir
            then findAriFiles path
            else if takeExtension path == ".ari"
                 then return [path]
                 else return [])
        return (concat paths)

extractFilenames :: String -> [(String, String, PathType)]
extractFilenames content = 
    let contentLines = lines content
        xtcLines = filter (\l -> isPrefixOf "; @xtcfilename" l) contentLines
        origAriLines = filter (\l -> isPrefixOf "; @origariname" l) contentLines
        
        extractPathWithCategory :: String -> String -> Int -> PathType -> (String, String, PathType)
        extractPathWithCategory line prefix prefixLen pathType = 
            let start = drop prefixLen line
                noQuotes = if isPrefixOf "\"" start && isSuffixOf "\"" start
                          then init (tail start)
                          else start
                noPrefix = if isPrefixOf "./" noQuotes
                          then drop 2 noQuotes
                          else noQuotes
                trimmedPath = trim noPrefix
                category = case extractCategoryFromPath trimmedPath of
                            Just c -> c
                            Nothing -> ""
            in (trimmedPath, category, pathType)
        
        xtcPaths = map (\l -> extractPathWithCategory l "; @xtcfilename" 15 XmlPath) xtcLines
        origAriPaths = map (\l -> extractPathWithCategory l "; @origariname" 15 AriPath) origAriLines
            
    in nub (xtcPaths ++ origAriPaths)

extractPossibleBenchmarkPaths :: FilePath -> String -> [(String, String, PathType)]
extractPossibleBenchmarkPaths ariPath content = 
    let paths = extractFilenames content
    in if null paths
       then []
       else paths

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

updateAriFilesWithStatus :: [FilePath] -> Map.Map (FilePath, String) Status -> Handle -> IO Int
updateAriFilesWithStatus ariFiles statusMap logHandle = do
    let total = length ariFiles
    results <- forM (zip [1..] ariFiles) (\(idx, path) -> do
        reportProgress "Updating files" idx total
        let relevantStatuses = Map.filterWithKey (\(p, _) _ -> p == path) statusMap
        if Map.null relevantStatuses
        then updateSingleAriFileWithCategories path [] logHandle
        else updateSingleAriFileWithCategories path (Map.toList relevantStatuses) logHandle)
    return (sum results)

updateSingleAriFileWithCategories :: FilePath -> [((FilePath, String), Status)] -> Handle -> IO Int
updateSingleAriFileWithCategories path categoryStatuses logHandle = do
    content <- BS.readFile path
    let contentStr = BSC.unpack content
        contentLines = lines contentStr
        (tagLines, restLines) = span (isPrefixOf "; @") contentLines
        
        cleanedTagLines = filter (\line -> not (isPrefixOf "; @status" line)) tagLines
        newStatusTags = map (\((_, cat), status) -> 
                              "; @status_" ++ cat ++ " " ++ show status) categoryStatuses
        finalStatusTags = if null categoryStatuses 
                          then ["; @status Unknown"] 
                          else newStatusTags
        
        newContentStr = unlines (cleanedTagLines ++ finalStatusTags ++ restLines)
    
    BS.writeFile path (BSC.pack newContentStr)
    hPrintf logHandle "Updated %s with %d category statuses\n" path (length finalStatusTags)
    return 1

reportProgress :: String -> Int -> Int -> IO ()
reportProgress phase current total = do
    let percentage = (fromIntegral current / fromIntegral total * 100) :: Double
    printf "\r%s: %d/%d (%.2f%%) " phase current total percentage
    hFlush stdout
    when (current == total) (putStrLn "")

main :: IO ()
main = do
    args <- getArgs
    case args of
        (tpdbPath:resultsPaths) -> do

            if null resultsPaths
            then putStrLn "At least one competition result path is required."
            else do
                startTime <- getCurrentTime
                
                let logFile = "status_computation.log"
                    statusFile = "benchmark_statuses.csv"
                logHandle <- openFile logFile WriteMode
                statusHandle <- openFile statusFile WriteMode
                hSetBuffering logHandle LineBuffering
                hSetBuffering statusHandle LineBuffering
                
                putStrLn "Finding ARI files in TPDB_COPY..."
                ariFiles <- findAriFiles tpdbPath
                putStrLn ("Found " ++ show (length ariFiles) ++ " .ari files")
                
                putStrLn "Reading ARI file contents and extracting filenames..."
                ariMappings <- forM (zip [1..] ariFiles) (\(idx, path) -> do
                    reportProgress "Analyzing ARI files" idx (length ariFiles)
                    content <- BS.readFile path
                    let possiblePaths = extractPossibleBenchmarkPaths path (BSC.unpack content)
                    return (path, possiblePaths))
                
                let pathMapping = buildPathMapping ariMappings
                    allOriginalPaths = [origPath | ((origPath, _, _), _) <- Map.toList pathMapping]
                
                let sampleOrigPaths = take 10 allOriginalPaths
                hPutStrLn logHandle ("Total ARI files in TPDB_COPY: " ++ show (length ariFiles))
                hPutStrLn logHandle ("Total possible original paths: " ++ show (length (nub allOriginalPaths)))
                hPutStrLn logHandle ("Sample original paths from ARI files: " ++ show sampleOrigPaths)
                
                let ariFilesWithoutPaths = [path | (path, paths) <- ariMappings, null paths]
                hPutStrLn logHandle ("ARI files without path entries: " ++ show (length ariFilesWithoutPaths))
                forM_ (take 20 ariFilesWithoutPaths) (\path -> 
                    hPutStrLn logHandle ("  " ++ path))
                when (length ariFilesWithoutPaths > 20) (
                    hPutStrLn logHandle ("  ... and " ++ show (length ariFilesWithoutPaths - 20) ++ " more"))
                
                putStrLn "Processing multiple competition result directories..."
                allResults <- forM (zip [1..] resultsPaths) (\(yearIdx, resultsPath) -> do
                    let yearStr = "Year_" ++ show yearIdx
                    putStrLn ("Processing results from " ++ yearStr ++ ": " ++ resultsPath)
                    
                    infoFiles <- findInfoFiles resultsPath
                    certInfoFiles <- findCertInfoFiles resultsPath
                    
                    hPutStrLn logHandle (yearStr ++ ": Found " ++ show (length infoFiles) ++ " info files")
                    hPutStrLn logHandle (yearStr ++ ": Found " ++ show (length certInfoFiles) ++ " certified info files")
                    
                    let origPathsWithCategories = [(origPath, cat, pType) | ((origPath, cat, pType), _) <- Map.toList pathMapping]
                    
                    infoResults <- fmap concat (mapM (parseAndFilterInfoCSV False yearStr logHandle origPathsWithCategories) infoFiles)
                    certResults <- fmap concat (mapM (parseAndFilterInfoCSV True yearStr logHandle origPathsWithCategories) certInfoFiles)
                    
                    return (infoResults ++ certResults))
                
                let combinedResults = concat allResults
                hPutStrLn logHandle ("Total filtered results across all years: " ++ show (length combinedResults))
                
                let mergedResults = groupResultsByCategoryAndCopyPath combinedResults pathMapping
                hPutStrLn logHandle ("Grouped results for " ++ show (length mergedResults) ++ " TPDB_COPY benchmarks by category")
                
                let statusMap = Map.fromList [((copyPath, cat), computeStatus results) 
                                             | ((copyPath, cat), results) <- mergedResults]
                
                let conflictBenchmarks = [((path, cat), results) | ((path, cat), results) <- mergedResults, 
                           let stat = computeStatus results,
                           stat == Conflict || stat == CertifiedConflict]

                hPutStrLn logHandle "\nDetailed conflict analysis:"
                forM_ conflictBenchmarks (\((path, cat), results) -> do
                    let status = computeStatus results
                    hPutStrLn logHandle ("\nBenchmark with " ++ show status ++ " in category " ++ cat ++ ": " ++ path)

                    let yesResults = filter (\r -> result r == "YES") results
                        noResults = filter (\r -> result r == "NO") results
                        
                    hPutStrLn logHandle "YES results:"
                    forM_ yesResults (\r -> 
                        hPrintf logHandle "  %s: solver=%s, certified=%s, year=%s, category=%s, pathType=%s\n" 
                            (benchmarkPath r) (solver r) (show (certified r)) (year r) (category r) (show (pathType r)))
                    
                    hPutStrLn logHandle "NO results:"
                    forM_ noResults (\r -> 
                        hPrintf logHandle "  %s: solver=%s, certified=%s, year=%s, category=%s, pathType=%s\n" 
                            (benchmarkPath r) (solver r) (show (certified r)) (year r) (category r) (show (pathType r))))

                putStrLn "Updating ARI files with status information..."
                updatedCount <- updateAriFilesWithStatus ariFiles statusMap logHandle
                
                hPutStrLn statusHandle "benchmark,category,status"
                forM_ (Map.toList statusMap) (\((path, cat), status) -> do
                    hPrintf statusHandle "%s,%s,%s\n" path cat (show status))
                
                let statusCounts = countStatuses statusMap
                hPutStrLn logHandle "\nStatus statistics:"
                forM_ (Map.toList statusCounts) (\(status, count) -> do
                    hPrintf logHandle "%s: %d\n" (show status) count)
                
                let ariFilesWithStatus = Map.keysSet (Map.mapKeys fst statusMap)
                    missingStatusFiles = filter (\path -> not (path `elem` ariFilesWithStatus)) ariFiles
                hPutStrLn logHandle ("\nBenchmarks without status: " ++ show (length missingStatusFiles))
                forM_ (take 50 missingStatusFiles) (\path -> do
                    hPutStrLn logHandle ("  " ++ path))
                when (length missingStatusFiles > 50) (
                    hPutStrLn logHandle ("  ... and " ++ show (length missingStatusFiles - 50) ++ " more"))
                
                endTime <- getCurrentTime
                let duration = diffUTCTime endTime startTime
                printf "\nProcessing completed in %.2f seconds\n" 
                    (realToFrac duration :: Double)
                printf "Updated %d/%d ARI files with status information\n" 
                    updatedCount (length ariFiles)
                printf "Results logged to %s\n" logFile
                printf "Status summary written to %s\n" statusFile
                
                putStrLn "\nStatus statistics:"
                forM_ (Map.toList statusCounts) (\(status, count) -> do
                    printf "%s: %d\n" (show status) count)
                
                hClose logHandle
                hClose statusHandle
        _ -> putStrLn "Usage: program <TPDB-path> <Results-path1> <Results-path2> ..."
