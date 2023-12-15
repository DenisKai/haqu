module Haqu.View where

import Data.List (intersperse)
import Haqu.Storage (QuizOverview (..), Question (qType, answerTexts, question))


type Html = String


-- general
generateUnorderedList :: String -> Html
generateUnorderedList = ea "ul" []



--- header
generateHeader :: Html
generateHeader = e "H1" "haqu"


--- h1 - title
generateTitle :: String -> Html
generateTitle = e "H2"


-- landing page
generateOverviewHtml :: QuizOverview -> Html
generateOverviewHtml quiz =
  e "li" (
    ea "b" [] ("[" ++ qId quiz ++ "] ")
    ++ ea "b" [] (name quiz ++ ": ")
    ++ desc quiz ++ " "
    ++ ea "a" [("href", link quiz)] "Start quiz"
  )


-- quiz start
generateFormHtml :: String -> Html
generateFormHtml quizId = let
    breakTag = e "br" []
    labelinput = ea "label" [("for", "player")] "Please enter your name:"
    textinput = ea "input" [
        ("type", "text"), 
        ("id", "player"), 
        ("name", "player")] ""
    submit = ea "input" [
        ("type", "submit"), 
        ("value", "Start Quiz")] ""
    form = ea "form" [
        ("action", "/quiz/" ++ quizId ++ "/start"), 
        ("method", "post")] 
        (labelinput ++ breakTag ++ textinput ++ breakTag ++ submit)
    in form


-- questions
generateQuestionHtml :: String -> String -> String -> Question -> Html
generateQuestionHtml q_Id qNo player quest = let
    qText = generateTitle (question quest)
    answers = generateAnswersHtml (qType quest) (answerTexts quest)
    submit = ea "input" [
        ("type", "submit"), 
        ("value", "Submit Answer")] ""
    form = ea "form" [
        ("action", "/quiz/" ++ q_Id ++ "/" ++ qNo ++ "?player=" ++ player), 
        ("method", "post")] 
        (answers ++ submit)
    in qText ++ form


-- answers
generateAnswersHtml :: String -> [String] -> Html
generateAnswersHtml q_Type answers = if q_Type == "SINGLECHOICE"
  then generateSingelChoiceAnswerHtml 0 answers
  else generateFalseTrueAnswerHtml


generateSingelChoiceAnswerHtml :: Int -> [String] -> Html
generateSingelChoiceAnswerHtml _ [] = []
generateSingelChoiceAnswerHtml aNo (x:xs) = let
  breakHtml = e "br" []
  inputHtml = ea "input" [
    ("type", "radio"), 
    ("id", show aNo),
    ("name", "answer"),
    ("value", show aNo)] ""
  labelHtml = ea "label" [("for", show aNo)] x
  in inputHtml ++ labelHtml ++ breakHtml ++ generateSingelChoiceAnswerHtml (aNo + 1) xs


generateFalseTrueAnswerHtml :: Html
generateFalseTrueAnswerHtml =
  ea "input" [("type", "radio"), ("id", "true"), ("name", "answer"), ("value", "True")] ""
  ++ ea "label" [("for", "true")] "True"
  ++ e "br" []
  ++ ea "input" [("type", "radio"), ("id", "false"), ("name", "answer"), ("value", "False")] ""
  ++ ea "label" [("for", "false")] "False"
  ++ e "br" []

-- results



-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []


ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
