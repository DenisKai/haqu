-- | This module contains the models used in the haqu application
module Haqu.Models where

-- | Html Type for readabiliy (String)
type Html = String
-- | Player type for better readability of QuizStats data
type Player = String
-- | QuestionId for better readability of QuizStats data
type QuestionId = Int
-- | This type represents the value of given answer
type AnswerVal = String
-- | Represents the sum of correct answers for a given question
type Result = Int
-- | represents if a question has been asnwered correctly 0->False, 1->True
type Correct = Int


-- | Data type to represent all possible answer types of a question 
data Answer = BoolVal Bool | IntVal Int deriving (Show)


{-|
    Data type to represent the overview of a quiz, for the homepage of haqu
    qId-> Quiz Id
    name-> Quiz Name
    desc-> Quiz' description
    link-> Link to the startpage of the quiz
-}
data QuizOverview = MkQuizOverview {
    qId :: String,
    name :: String,
    desc :: String,
    link :: String
} deriving (Show)


{-|
    Data type to represent a questio of a quiz.
    qType-> Type of question (Singlechoice or FalseTrue)
    question-> Question text
    answerTexts-> possible answers to question
    answer-> solution of question
-}
data Question = MkQuestion {
    qType :: String,
    question :: String,
    answerTexts :: [String],
    answer :: Answer
} deriving (Show)


{-|
    Data tye to represent statistics of a quiz
    playerAnswers-> List of all players that answered the quiz.
        -> Contains a detailed list of each given answer for each question (id) 
           and if it was correct

    resultsQuestion-> representation of the statistics for each question of the quiz
        -> ordered by question id and how many answers were correct (over all players)
-}
data QuizStats = MkQuizStats {
    playerAnswers :: [(Player, [((QuestionId, AnswerVal), Correct)])],
    resultsQuestion :: [(QuestionId, Result)]
} deriving (Show)
 