-- | This module has the functionality for the persistence off all data in it.
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


{-|
    Get the statistics from a specific quiz.
    Maps the answer given per player and the amount of correct answers per question.
    Returns the results in a QuizStats Object.
-}
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


{-| 
    Takes a list of one question, answers and if they're correct.
    Builds the statistics of how many times the question was answered correctly
    and puts it into a Tuple of QuestionId and how many times it was answered correctly.
-}
buildStatsPerQuestion :: [((QuestionId, AnswerVal), Correct)] -> (QuestionId, Int)
buildStatsPerQuestion [] = (-1, 0)
buildStatsPerQuestion (((quId, _), correct):as) =
    (quId, correct + snd (buildStatsPerQuestion as))


{-| 
    Builds the results for each player participating in a quiz, identified by its ID.
    Reads all answers files and transforms the data to the format of a list:
    - Tuple that hold player and inner list of the players answers to each question.
    - Correct is if the question is answered correctly
    
    Returns the zipped data of playername and questions per player 
    (reverse so to keep order of answers).
-}
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

    return $ reverse zipped


{-| 
    Checks a players answers against given questions solution.
    Returns a list of given answers and if they were answered correctly.
-}
checkAnsToSol :: [String] -> [(QuestionId, AnswerVal)] -> [((QuestionId, AnswerVal), Correct)]
checkAnsToSol _ [] = []
checkAnsToSol sol ((quI, ans): as) = let
    elemSol = sol!!quI
    correct = if ans == elemSol
        then 1
        else 0
    in ((quI,ans), correct) : checkAnsToSol sol as


{-| 
    Splits the given String-answer to a Tuple of question Id and Answervalue.
-}
concatQuestionIdToAnswer :: [String] -> [(QuestionId, AnswerVal)]
concatQuestionIdToAnswer [] = []
concatQuestionIdToAnswer (x:xs) = let
    index = read $ takeWhile (/=':') x
    panswer = drop 1 $ dropWhile (/=':') x
    others = concatQuestionIdToAnswer xs
    in (index, panswer) : others


{-| 
    Loads the answer-files of given list of filepath.
    Returns their content in a list of strings.
-}
loadAnswers :: [FilePath] -> IO [String]
loadAnswers [] = do
    return [[]]
loadAnswers (x:xs) = do
    aCon <- readFile x
    rest <- loadAnswers xs
    let joined = aCon : rest
    return $ filter (not . null) joined


{-|
    Returs the existing files (as list of full dynamic paths) for a specific quiz.
    Identified by Quiz Id.
-}
getAnswerFilesByQuizid :: String -> IO [FilePath]
getAnswerFilesByQuizid quizId = do
    files <- listDirectory ("./data/" ++ quizId ++ "/")
    let filteredFiles = filter (\file -> getExtension file == "txt") files
    return filteredFiles


{-|
    Creates a players answer file for a specific quiz.
    If the answers-directory doesn't exist, creates the corresponding directory.

    Takes the quizId and the players name.
-}
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
        then do removeFile filePath
        else do return ()
    appendFile filePath ""


{-|
    Reads a quiz file by given quiz id.
    Returns a list of questions from corresponding quiz.
-}
getQuestionsFromQuizById :: FilePath -> IO [Question]
getQuestionsFromQuizById fileId = do
    content <- readFile ("./data/" ++ fileId ++ ".txt")
    let trimmed = dropWhile (not . isPrefixOf "TYPE:") (lines content)
    let filtered = filter (/="") trimmed

    -- Removes the first index of where to split the questions due to be always index 0.
    let qIndices = drop 1 $ elemIndices True (map (startsWith "TYPE:") filtered)
    let splitList = splitAtIndices qIndices filtered

    let questions = createQuestions splitList

    return questions


{-|
    Split a list at given indices.
    Indices are a list of int (containing the indices).
    List of strings is filecontent.
-}
splitAtIndices :: [Int] -> [String] -> [[String]]
splitAtIndices [] _ = []
splitAtIndices [x] content =
    let (first, second) = splitAt x content
    in first : [second]
splitAtIndices (x:xs) content =
    let (firstH, secondH) = splitAt x content
    in firstH : splitAtIndices (map (subtract x) xs) secondH


{-|
    Takes list of quiz-file-contents.
    Creates a Question out of the contents of a quiz file.
    The contents of the quiz are cut up into the lines of the corresponding quiz file.
-}
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


{-|
    Converts a answer-string to Answer type. 
    If its a Bool -> Boolval, else IntVal; 
    If it isn't a digit or bool Intval -> -1 / invalid answer
-}
stringToAnswer :: String -> Answer
stringToAnswer str
    | str == "True" = BoolVal True
    | str == "False" = BoolVal False
    | all isDigit str = IntVal $ read str
    | otherwise = IntVal $ -1


{-|
    Updates the answerfile of a player.
    Identiied by quizId, the answered question, the player and their answer.
    Starts a new player-answer file if the question id is the first of quiz (question id == 0).
-}
updateAnswerFile :: String -> String -> String -> String -> IO ()
updateAnswerFile quizId questionNo player panswer = do
    let fp = "./data/" ++ quizId ++ "/" ++ player ++ ".txt"
    if questionNo == "0"
        then removeFile fp
        else return ()

    _ <- appendFile fp (questionNo ++ ":" ++ panswer ++ "\n")
    return ()


-- | Returns all quiz-files (.txt) of given folder/path 
getQuizFilesFromData :: FilePath -> IO [FilePath]
getQuizFilesFromData directory = do
    files <- listDirectory directory
    let filteredFiles = filter (\file -> getExtension file == "txt") files
    return filteredFiles


{-|
    Creates dictionaries of all quizzes of given folder.
    Key of dict is the quiz Id, Value is the contents of the quiz, split by \n
-}
getQuizDicts :: FilePath -> IO [(FilePath, [String])]
getQuizDicts path = do
    files <- getQuizFilesFromData path
    fileContents <- parseFiles (map ((path ++) . ("/"++)) files)
    let quizIds = map removeFileExtension files
    let splitNewLine = map lines fileContents
    let dictQuiz = zip quizIds splitNewLine

    return dictQuiz


-- | Gets the overviews of all quizzes in a given folder
getQuizOverviews :: FilePath -> IO [QuizOverview]
getQuizOverviews path = do
    quizDicts <- getQuizDicts path
    let quizoverviews = createQuizOverview quizDicts

    return $ reverse quizoverviews


-- | Get the name if quiz by its given id-parameter
getNameById :: String -> IO String
getNameById quizId = do
    file <- readFile ("./data/" ++ quizId ++ ".txt")
    return $ findValue "NAME:" (lines file)


{-|
    Creates a list of QuizOverview with given list of tuples.
    Containing quizId and the content of the quiz.
-}
createQuizOverview :: [([Char], [String])] -> [QuizOverview]
createQuizOverview [] = []
createQuizOverview [(quizId, content)] =
    [MkQuizOverview {
        qId=quizId,
        name= findValue "NAME:" content,
        desc= findValue "DESC:" content,
        link="/quiz/" ++ quizId ++ "/start"}]
createQuizOverview (x:xs) = createQuizOverview [x] ++ createQuizOverview xs


-- Helper Functions Section
-- | Checks if quiz with given Id exists
doesQuizExist :: String -> IO Bool
doesQuizExist quizId = do
    doesFileExist ("./data/" ++ quizId ++ ".txt")


-- | Checks if solution directory of given quiz Id exists
doesDirExist :: String -> IO ()
doesDirExist quizId = do
    dirExists <- doesDirectoryExist ("./data/" ++ quizId)
    if dirExists
        then return ()
        else createDirectory ("./data/" ++ quizId)


-- | Finds all keys in a list of strings and returns corresponding values
findAllValues :: String -> [String] -> [String]
findAllValues key = map (drop (length key)) . filter (startsWith key)


-- | Finds a single key in a list of strings and returns value
findValue :: String -> [String] -> String
findValue key =
    maybe "Key not found" (drop (length key)) . find (startsWith key)


{-| 
    Checks if type a starts with given prefix (of same type).
    Returns Bool True if prefix exists.
-}
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith prefix x = take (length prefix) x == prefix


-- | Catches the extension of given FilePath/file and returns it
getExtension :: FilePath -> FilePath
getExtension filePath =
    takeWhile (/= '.') (fst (break (=='/') (reverse filePath)))


-- | Removes the extension of given FilePath/file and returns filename
removeFileExtension :: FilePath -> FilePath
removeFileExtension = takeWhile (/='.')


{-|
    Reads all files in given list of Filepaths.
    Returns a list of Strings containing the files contents.
-}
parseFiles :: [FilePath] -> IO [String]
parseFiles [] = do
    return []
parseFiles (f:fs) = do
    file <- readFile f
    otherfiles <- parseFiles fs
    return (file : otherfiles)
