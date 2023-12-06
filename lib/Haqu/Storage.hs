module Haqu.Storage where

import System.Directory (listDirectory, doesDirectoryExist, createDirectory, doesFileExist, removeFile)
import System.FilePath (takeExtension, (</>))


data Quiz = MkQuiz {
    qId :: Int,
    name :: String,
    description :: String,
    link :: String
} deriving (Show)


getQuizzesFromData :: FilePath -> IO [FilePath]
getQuizzesFromData directory = do
    files <- listDirectory directory

    let filteredFiles = filter (\file -> takeExtension file == ".txt") files
    -- append directorypath to all files
    let fullFilePaths = map (directory </>) filteredFiles
    return fullFilePaths


getQuizOverviews :: FilePath -> IO [String]
getQuizOverviews path = do
    filePaths <- getQuizzesFromData path
    files <- parseFiles filePaths
    return files
    -- TODO from here, read files and build overview..


parseFiles :: [FilePath] -> IO [String]
parseFiles [] = do
    return []
parseFiles (f:fs) = do
    file <- readFile f
    otherfiles <- parseFiles fs
    return (file : otherfiles)
    
    
-- MkQuiz {qId=1, name="Test", description="Yada", link="loc"}