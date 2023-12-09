module Haqu.Storage where

import System.Directory (listDirectory, doesDirectoryExist, createDirectory, doesFileExist, removeFile)
import System.FilePath (takeExtension, dropExtensions, (</>))


data QuizOverview = MkQuizOverview {
    qId :: [Char],
    name :: String,
    desc :: String,
    link :: String
} deriving (Show)


getQuizFilesFromData :: FilePath -> IO [FilePath]
getQuizFilesFromData directory = do
    files <- listDirectory directory
    let filteredFiles = filter (\file -> takeExtension file == ".txt") files
    return filteredFiles


getQuizDicts :: FilePath -> IO [(FilePath, String)]
getQuizDicts path = do
    files <- getQuizFilesFromData path
    fileContents <- parseFiles (map (path </>) files)
    let splitString = map lines fileContents
    let quizIds = map dropExtensions files
    let dictQuiz = zip quizIds fileContents

    return dictQuiz

getQuizOverviews :: FilePath -> IO [QuizOverview]
getQuizOverviews path = do
    quizDicts <- getQuizDicts path
    -- TODO create quiz overviews
    return [MkQuizOverview {qId= "d", name="td", desc="td", link="td"}]

createQuizOverview :: [([Char], String)] -> [QuizOverview]
createQuizOverview [] = []
createQuizOverview [x] = [MkQuizOverview {qId= fst x, name="td", desc="td", link="td"}]
createQuizOverview ((id, content):xs) = MkQuizOverview {qId=id, name="td", desc="td", link="td"} : createQuizOverview xs


parseFiles :: [FilePath] -> IO [String]
parseFiles [] = do
    return []
parseFiles (f:fs) = do
    file <- readFile f
    otherfiles <- parseFiles fs
    return (file : otherfiles)


-- MkQuiz {qId=1, name="Test", description="Yada", link="loc"}