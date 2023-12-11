module Haqu.Storage where

import System.Directory (listDirectory)
    --doesDirectoryExist, createDirectory, doesFileExist, removeFile
import Data.List (find)


data QuizOverview = MkQuizOverview {
    qId :: String,
    name :: String,
    desc :: String,
    link :: String
} deriving (Show)


getQuizFilesFromData :: FilePath -> IO [FilePath]
getQuizFilesFromData directory = do
    files <- listDirectory directory
    -- TODO filter broken, fix
    let filteredFiles = filter (\file -> getExtension file == ".txt") files
    return filteredFiles


getQuizDicts :: FilePath -> IO [(FilePath, [String])]
getQuizDicts path = do
    files <- getQuizFilesFromData path
    fileContents <- parseFiles (map ((path ++) . ("/"++)) files)
    let quizIds = map removeFileExtension files
    let splitNewLine = map lines fileContents
    let dictQuiz = zip quizIds splitNewLine

    return dictQuiz


getQuizOverviews :: FilePath -> IO [QuizOverview]
getQuizOverviews path = do
    quizDicts <- getQuizDicts path
    let quizoverviews = createQuizOverview quizDicts

    return quizoverviews


createQuizOverview :: [([Char], [String])] -> [QuizOverview]
createQuizOverview [] = []
createQuizOverview [(quizId, content)] =
    [MkQuizOverview {
        qId=quizId,
        name= findValue "NAME:" content,
        desc= findValue "DESC:" content,
        link="/quiz/" ++ quizId ++ "/start"}]
    where
        findValue :: String -> [String] -> String
        findValue key = maybe "Key not found" (drop (length key)) . find (startsWith key)

        startsWith :: Eq a => [a] -> [a] -> Bool
        startsWith prefix x = take (length prefix) x == prefix
createQuizOverview (x:xs) = createQuizOverview [x] ++ createQuizOverview xs


-- TODO fix this
getExtension :: FilePath -> FilePath
getExtension filePath =
  case reverse filePath of
    [] -> ""
    (x:xs) -> if x /= '.' && elem '.' filePath
              then x : getExtension (reverse xs)
              else ""


removeFileExtension :: FilePath -> FilePath
removeFileExtension path = 
    reverse (dropWhile (/='.') (reverse path))


parseFiles :: [FilePath] -> IO [String]
parseFiles [] = do
    return []
parseFiles (f:fs) = do
    file <- readFile f
    otherfiles <- parseFiles fs
    return (file : otherfiles)
