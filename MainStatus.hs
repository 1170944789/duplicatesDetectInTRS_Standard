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

data Result = Result {
    benchmarkPath :: String,
    solver :: String,
    result :: String,
    certified :: Bool
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

countStatuses :: Map.Map FilePath Status -> Map.Map Status Int
countStatuses statusMap =
    let statusList = Map.elems statusMap
    in foldr (\s -> Map.insertWith (+) s 1) Map.empty statusList

findInfoFiles :: FilePath -> IO [FilePath]




findCertInfoFiles :: FilePath -> IO [FilePath]




parseAndFilterInfoCSV :: Bool -> Handle -> [String] -> FilePath -> IO [Result]




isBenchmarkInKnownCategories :: Result -> Bool
isBenchmarkInKnownCategories result =
    let path = benchmarkPath result
    in any (\cat -> (cat ++ "/") `isPrefixOf` path) allCategories

parseCSVRecord :: Bool -> CSV.NamedRecord -> Maybe Result
parseCSVRecord isCertified record = do
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
    if resultValue `elem` ["YES", "NO"] 
    then return (Result benchPath solverName resultValue certResult)
    else Nothing

buildPathMapping :: [(FilePath, [String])] -> Map.Map String FilePath
buildPathMapping mappings =
    let allPairs = concatMap (\(copyPath, origPaths) -> 
                             [(origPath, copyPath) | origPath <- origPaths]) mappings
    in Map.fromList allPairs

groupResultsByCopyPath :: [Result] -> Map.Map String FilePath -> [(FilePath, [Result])]
groupResultsByCopyPath results pathMapping =
    let withCopyPath = mapMaybe (\r -> do
                        copyPath <- Map.lookup (benchmarkPath r) pathMapping
                        return (copyPath, r)) results
        groupedMap = foldl' (\acc (path, result) -> 
                           Map.insertWith (++) path [result] acc) 
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

extractPossibleBenchmarkPaths :: FilePath -> String -> [String]




extractTags :: String -> [String]
extractTags content = 
    let contentLines = lines content
        tagLines = filter (\l -> isPrefixOf "; @tag" l) contentLines
    in map (trim . drop 7) tagLines

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

extractBasePathFromAri :: FilePath -> String
extractBasePathFromAri path =
    let parts = splitDirectories path
        fileName = takeBaseName (last parts)
        parentDir = if length parts >= 2
                    then parts !! (length parts - 2)
                    else ""
        (finalParentDir, finalFileName) = 
            if parentDir `elem` map (takeFileName . joinPath . splitDirectories) allCategories
            then
                if length parts >= 3
                then (parts !! (length parts - 3), fileName)
                else (parentDir, fileName)
            else (parentDir, fileName)
    in finalParentDir ++ "/" ++ finalFileName

updateAriFilesWithStatus :: [FilePath] -> Map.Map FilePath Status -> Handle -> IO Int
updateAriFilesWithStatus ariFiles statusMap logHandle = do
    let total = length ariFiles
    results <- forM (zip [1..] ariFiles) (\(idx, path) -> do
        reportProgress "Updating files" idx total
        case Map.lookup path statusMap of
            Just status -> updateSingleAriFile path status logHandle
            Nothing -> return 0)
    return (sum results)

updateSingleAriFile :: FilePath -> Status -> Handle -> IO Int
updateSingleAriFile path status logHandle = do
    content <- BS.readFile path
    let contentStr = BSC.unpack content
        contentLines = lines contentStr
        (tagLines, restLines) = span (isPrefixOf "; @") contentLines
        existingStatus = any (isPrefixOf "; @status") tagLines
        statusTag = "; @status " ++ show status
        newTagLines = if existingStatus
                     then map (\line -> if isPrefixOf "; @status" line 
                                       then statusTag 
                                       else line) tagLines
                     else tagLines ++ [statusTag]
        newContentStr = unlines (newTagLines ++ restLines)
    BS.writeFile path (BSC.pack newContentStr)
    hPrintf logHandle "Updated %s with status %s\n" path (show status)
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
        [tpdbPath, resultsPath] -> do
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
            
            putStrLn "Reading ARI file contents and extracting possible original paths..."
            ariMappings <- forM (zip [1..] ariFiles) (\(idx, path) -> do
                reportProgress "Analyzing ARI files" idx (length ariFiles)
                content <- BS.readFile path
                let possiblePaths = extractPossibleBenchmarkPaths path (BSC.unpack content)
                return (path, possiblePaths))
            
            let pathMapping = buildPathMapping ariMappings
                allOriginalPaths = Map.keys pathMapping
            
            let sampleOrigPaths = take 10 allOriginalPaths
            hPutStrLn logHandle ("Total ARI files in TPDB_COPY: " ++ show (length ariFiles))
            hPutStrLn logHandle ("Total possible original paths: " ++ show (length allOriginalPaths))
            hPutStrLn logHandle ("Sample original paths from ARI files: " ++ show sampleOrigPaths)
            
            putStrLn "Finding competition result files..."
            infoFiles <- findInfoFiles resultsPath
            certInfoFiles <- findCertInfoFiles resultsPath
            
            hPutStrLn logHandle ("Found " ++ show (length infoFiles) ++ " info files")
            hPutStrLn logHandle ("Found " ++ show (length certInfoFiles) ++ " certified info files")
            
            putStrLn "Parsing and filtering competition results..."
            infoResults <- concat (mapM (parseAndFilterInfoCSV False logHandle allOriginalPaths) infoFiles)
            certResults <- concat (mapM (parseAndFilterInfoCSV True logHandle allOriginalPaths) certInfoFiles)
            
            let allResults = infoResults ++ certResults
            hPutStrLn logHandle ("Total filtered results: " ++ show (length allResults))            
            
            let mergedResults = groupResultsByCopyPath allResults pathMapping
            hPutStrLn logHandle ("Grouped results for " ++ show (length mergedResults) ++ " TPDB_COPY benchmarks")
            
            let statusMap = Map.fromList [(copyPath, computeStatus results) 
                                         | (copyPath, results) <- mergedResults]
            
            putStrLn "Updating ARI files with status information..."
            updatedCount <- updateAriFilesWithStatus ariFiles statusMap logHandle
            
            hPutStrLn statusHandle "benchmark,status"
            forM_ (Map.toList statusMap) (\(path, status) -> do
                hPrintf statusHandle "%s,%s\n" path (show status))
            
            let statusCounts = countStatuses statusMap
            hPutStrLn logHandle "\nStatus statistics:"
            forM_ (Map.toList statusCounts) (\(status, count) -> do
                hPrintf logHandle "%s: %d\n" (show status) count)
            
            let missingStatusFiles = filter (\path -> not (Map.member path statusMap)) ariFiles
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
            
        _ -> putStrLn "Usage: Program <TPDB-path> <Results-path>"
