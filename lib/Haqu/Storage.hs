module Haqu.Storage where

import System.Directory
    (
      listDirectory,
      doesDirectoryExist,
      createDirectory,
      doesFileExist,
      removeFile)
import Data.List (find, isPrefixOf, elemIndices, transpose)
import Data.Char (isDigit)
import Haqu.Models
    ( Question(..),
      Answer(..),
      QuizOverview(..),
      QuizStats(..),
      Correct,
      AnswerVal,
      QuestionId,
      Player )


-- statistics
getQuizStatisticsByQuizId :: String -> IO QuizStats
getQuizStatisticsByQuizId quizId = do
    results <- getResultsByQuizId quizId

    let answersPerPlayer = map snd results
    let tansposedAnswers = transpose answersPerPlayer
    let statsPerQuestion = map buildStatsPerQuestion tansposedAnswers

    return $ MkQuizStats {
        playerAnswers= results,
        resultsQuestion= statsPerQuestion
    }


buildStatsPerQuestion :: [((QuestionId, AnswerVal), Correct)] -> (QuestionId, Int)
buildStatsPerQuestion [] = (-1, 0)
buildStatsPerQuestion (((quId, _), correct):as) =
    (quId, correct + snd (buildStatsPerQuestion as))


getResultsByQuizId :: String -> IO [(Player, [((QuestionId, AnswerVal), Correct)])]
getResultsByQuizId quizdId = do
    answerFiles <- getAnswerFilesByQuizid quizdId
    let paths = map (("./data/"++ quizdId ++ "/") ++) answerFiles

    panswers <- loadAnswers paths
    let players = map removeFileExtension answerFiles
    let questionIdAnswer = map (concatQuestionIdToAnswer . lines) panswers
    quiz <- readFile ("./data/" ++ quizdId ++ ".txt")
    let trueAnswers = findAllValues "S:" $ lines quiz
    let playerAnswerList = map (checkAnsToSol trueAnswers) questionIdAnswer
    let zipped = zip players playerAnswerList

    return zipped


checkAnsToSol :: [String] -> [(QuestionId, AnswerVal)] -> [((QuestionId, AnswerVal), Correct)]
checkAnsToSol _ [] = []
checkAnsToSol sol ((quI, ans): as) = let
    elemSol = sol!!quI
    correct = if ans == elemSol
        then 1
        else 0
    in ((quI,ans), correct) : checkAnsToSol sol as


concatQuestionIdToAnswer :: [String] -> [(QuestionId, AnswerVal)]
concatQuestionIdToAnswer [] = []
concatQuestionIdToAnswer (x:xs) = let
    index = read $ takeWhile (/=':') x
    panswer = drop 1 $ dropWhile (/=':') x
    others = concatQuestionIdToAnswer xs
    in (index, panswer) : others


loadAnswers :: [FilePath] -> IO [String]
loadAnswers [] = do
    return [[]]
loadAnswers (x:xs) = do
    aCon <- readFile x
    rest <- loadAnswers xs
    let joined = aCon : rest
    return $ filter (not . null) joined


getAnswerFilesByQuizid :: String -> IO [FilePath]
getAnswerFilesByQuizid quizId = do
    files <- listDirectory ("./data/" ++ quizId ++ "/")
    let filteredFiles = filter (\file -> getExtension file == "txt") files
    return filteredFiles


-- player
createPlayerFile :: String -> String -> IO ()
createPlayerFile quizId playername = do
    let dirPath = "./data/" ++ quizId
    let filePath = dirPath ++ "/" ++ playername ++ ".txt"

    dirExists <- doesDirectoryExist dirPath
    if not dirExists
        then createDirectory dirPath
        else return ()

    exists <- doesFileExist filePath
    if exists
        then do
            removeFile filePath
        else do
            return ()
    appendFile filePath ""


-- questions
readQuizFileById :: FilePath -> IO [Question]
readQuizFileById fileId = do
    content <- readFile ("./data/" ++ fileId ++ ".txt")
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


-- answers
updateAnswerFile :: String -> String -> String -> String -> IO ()
updateAnswerFile quizId questionNo player panswer = do
    let fp = "./data/" ++ quizId ++ "/" ++ player ++ ".txt"
    if questionNo == "0"
        then removeFile fp
        else return ()

    _ <- appendFile fp (questionNo ++ ":" ++ panswer ++ "\n")

    return ()


-- quiz overviews
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
doesQuizExist :: String -> IO Bool
doesQuizExist quizId = do
    doesFileExist ("./data/" ++ quizId ++ ".txt")


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
