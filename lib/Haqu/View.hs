module Haqu.View where

import Data.List (intersperse)
import Haqu.Models
    ( Question(answerTexts, question, qType),
      QuizOverview(link, qId, name, desc),
      QuizStats (resultsQuestion, playerAnswers), QuestionId, Result, Player, AnswerVal, Correct)


type Html = String


-- general
generateUnorderedList :: String -> Html
generateUnorderedList = ea "ul" []


generateHeadHtml :: Html
generateHeadHtml = let
  meta = ea "meta" [("charset", "utf-8")] ""
  linkTag = ea "link" [("rel", "stylesheet"), ("href", "styles.css")] ""
  in ea "head" [] (meta ++ linkTag)


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
  ea "input"
    [("type", "radio"), ("id", "true"), ("name", "answer"), ("value", "True")] ""
  ++ ea "label" [("for", "true")] "True"
  ++ e "br" []
  ++ ea "input"
    [("type", "radio"), ("id", "false"), ("name", "answer"), ("value", "False")] ""
  ++ ea "label" [("for", "false")] "False"
  ++ e "br" []


-- results
generateTableHtml :: QuizStats -> Html
generateTableHtml stats = if length (playerAnswers stats) > 0
  then let
    th = ea "tr" [] $ e "th" "Player" ++ generateTableHeaderHtml (resultsQuestion stats)
    tRow = generateTableRowHtml (playerAnswers stats)
    tFooter = generateTableFooter (length $ playerAnswers stats) (resultsQuestion stats)
    table = ea "table" [] (th ++ tRow ++ tFooter)
    in table
  else generateTitle "Keine Statistik verfügbar."


generateTableHeaderHtml :: [(QuestionId, Result)] -> Html
generateTableHeaderHtml [] = ""
generateTableHeaderHtml ((questionId, _):xs) =
  ea "th" [] ("Q" ++ show questionId) ++ generateTableHeaderHtml xs


generateTableRowHtml :: [(Player, [((QuestionId, AnswerVal), Correct)])] -> Html
generateTableRowHtml [] = ""
generateTableRowHtml ((player, answers):xs) = let
  answersTag = concatMap (\((_, answer), correct) -> generateRow answer correct) answers
  playerTag = ea "th" [] player
  in ea "tr" [] (playerTag ++ answersTag) ++ generateTableRowHtml xs


generateRow :: AnswerVal -> Correct -> Html
generateRow answer correct = let
  style = if correct == 1
    then "correct"
    else "wrong"
  in ea "td" [("class", style)] answer


generateTableFooter :: Int -> [(QuestionId, Result)] -> Html
generateTableFooter noPlayers results = let
  resultTag =
    concatMap (\(_, result) -> ea "td" [] (show result ++ " / " ++ show noPlayers)) results
  statisticTH = ea "th" [] "Statistics"
  in ea "tr" [] (statisticTH ++ resultTag)


-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []


ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"