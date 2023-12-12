module Haqu.Storage where

import System.Directory (listDirectory)
    --doesDirectoryExist, createDirectory, doesFileExist, removeFile
import Data.List (find)

import Haqu.Models ( QuizOverview(..) )



-- create
createPlayerFile :: String -> String -> ()
createPlayerFile quizId playername = 
    let a = "a" in () 


-- read
getQuizFilesFromData :: FilePath -> IO [FilePath]
getQuizFilesFromData directory = do
    files <- listDirectory directory
    let filteredFiles = filter (\file -> getExtension file == "txt") files
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

    return $ reverse quizoverviews


getNameById :: String -> IO String
getNameById quizId = do
    file <- readFile ("./data/" ++ quizId ++ ".txt")
    return $ findValue "NAME:" (lines file)


--helper methods

createQuizOverview :: [([Char], [String])] -> [QuizOverview]
createQuizOverview [] = []
createQuizOverview [(quizId, content)] =
    [MkQuizOverview {
        qId=quizId,
        name= findValue "NAME:" content,
        desc= findValue "DESC:" content,
        link="/quiz/" ++ quizId ++ "/start"}]        
createQuizOverview (x:xs) = createQuizOverview [x] ++ createQuizOverview xs


findValue :: String -> [String] -> String
findValue key =
    maybe "Key not found" (drop (length key)) . find (startsWith key)


startsWith :: Eq a => [a] -> [a] -> Bool
startsWith prefix x = take (length prefix) x == prefix


getExtension :: FilePath -> FilePath
getExtension filePath =
    takeWhile (/= '.') (fst (break (=='/') (reverse filePath)))


removeFileExtension :: FilePath -> FilePath
removeFileExtension = takeWhile (/='.')


parseFiles :: [FilePath] -> IO [String]
parseFiles [] = do
    return []
parseFiles (f:fs) = do
    file <- readFile f
    otherfiles <- parseFiles fs
    return (file : otherfiles)
