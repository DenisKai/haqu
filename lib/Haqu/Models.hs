module Haqu.Models where


data QuizOverview = MkQuizOverview {
    qId :: String,
    name :: String,
    desc :: String,
    link :: String
} deriving (Show)