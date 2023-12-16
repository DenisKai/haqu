module Haqu.Models where


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


type Player = String
type QuestionId = Int
type AnswerVal = String
type Result = Int
type Correct = Int
type QuestionAnswer = (String, String)


data QuizStats = MkQuizStats {
    playerAnswers :: [(Player, [((QuestionId, AnswerVal), Correct)])],
    resultsQuestion :: [(QuestionId, Result)]
} deriving (Show)
 