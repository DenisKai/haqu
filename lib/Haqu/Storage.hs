module Haqu.Storage where

import System.Directory (listDirectory, doesDirectoryExist, createDirectory)
import Data.List (find, isPrefixOf, elemIndices)
import Data.Char (isDigit)
--import Haqu.Models

data QuizOverview = MkQuizOverview {
    qId :: String,
    name :: String,
    desc :: String,
    link :: String
} deriving (Show)

data Answer = BoolVal Bool | IntVal Int deriving (Show)

data Question = MkQuestion {
    qType :: String,
    question :: String,
    answerTexts :: [String],
    answer :: Answer
} deriving (Show)


-- create
createPlayerFile :: String -> String -> IO ()
createPlayerFile quizId playername = do
    let dirPath = "./data/" ++ quizId
    let filePath = dirPath ++ "/" ++ playername ++ ".txt"

    dirExists <- doesDirectoryExist dirPath
    if not dirExists
        then createDirectory dirPath
        else return ()

    appendFile filePath ""


-- questions
readQuizFileById :: FilePath -> IO [Question]
readQuizFileById fileId = do
    content <- readFile ("./data/" ++ fileId ++ ".txt")

    -- drop everything before fist question
    let trimmed = dropWhile (not . isPrefixOf "TYPE:") (lines content)
    let filtered = filter (/="") trimmed

    let qIndices = drop 1 $ elemIndices True (map (startsWith "TYPE:") filtered)
    let splitList = splitAtIndices qIndices filtered

    let questions = createQuestions splitList

    return questions


splitAtIndices :: [Int] -> [String] -> [[String]]
splitAtIndices [] _ = []
splitAtIndices [x] content =
    let (first, second) = splitAt x content
    in first : [second]
splitAtIndices (x:xs) content =
    let (firstH, secondH) = splitAt x content
    in firstH : splitAtIndices (map (subtract x) xs) secondH


createQuestions :: [[String]] -> [Question]
createQuestions [] = []
createQuestions (x:xs) =
    let q_Type= findValue "TYPE:" x
        makeQuestion = MkQuestion {
        qType= q_Type,
        question= findValue "Q:" x,
        answerTexts= if q_Type == "SINGLECHOICE"
            then findAllValues "A:" x
            else [],
        answer= stringToAnswer $ findValue "S:" x
    }
    in makeQuestion : createQuestions xs


stringToAnswer :: String -> Answer
stringToAnswer str
    | str == "True" = BoolVal True
    | str == "False" = BoolVal False
    | all isDigit str = IntVal $ read str
    | otherwise = IntVal $ -1


-- overviews
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


createQuizOverview :: [([Char], [String])] -> [QuizOverview]
createQuizOverview [] = []
createQuizOverview [(quizId, content)] =
    [MkQuizOverview {
        qId=quizId,
        name= findValue "NAME:" content,
        desc= findValue "DESC:" content,
        link="/quiz/" ++ quizId ++ "/start"}]
createQuizOverview (x:xs) = createQuizOverview [x] ++ createQuizOverview xs


-- helper methods
findAllValues :: String -> [String] -> [String]
findAllValues key = map (drop (length key)) . filter (startsWith key)


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
