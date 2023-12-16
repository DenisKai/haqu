-- | This modules contains the functionality for all the HTML-specific rendering of haqu
module Haqu.View where

import Data.List (intersperse)
import Haqu.Models
    ( Question(answerTexts, question, qType),
      QuizOverview(link, qId, name, desc),
      QuizStats (resultsQuestion, playerAnswers),
        QuestionId,
        Result,
        Player,
        AnswerVal,
        Correct, 
        Html)


-- | Generates an unordered list wrapper, with given content in it
generateUnorderedList :: String -> Html
generateUnorderedList = ea "ul" []


-- | Generates the <head> tag of the HTML page and connects the stylesheet
generateHeadHtml :: Html
generateHeadHtml = let
  meta = ea "meta" [("charset", "utf-8")] ""
  linkTag = ea "link" [("rel", "stylesheet"), ("href", "styles.css")] ""
  in ea "head" [] (meta ++ linkTag)


-- | Generates the "haqu" header of the website
generateHeader :: Html
generateHeader = e "H1" "haqu"


-- | Generates a H2 element for the given input
generateTitle :: String -> Html
generateTitle = e "H2"


-- | Generates the HTML for a given QuizOverview
generateOverviewHtml :: QuizOverview -> Html
generateOverviewHtml quiz =
  e "li" (
    ea "b" [] ("[" ++ qId quiz ++ "] ")
    ++ ea "b" [] (name quiz ++ ": ")
    ++ desc quiz ++ " "
    ++ ea "a" [("href", link quiz)] "Start quiz"
  )


-- | Generates the HTML for the quiz start page, containing the proper HTML-form
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


{-|
  Generates the HTML-Form for a question of a quiz.
  Specifies where to Submit the POST request with player (name), quiz Id and question Id
-}
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


{-|
  Helper unction to generate the answers for a question.
  Takes questiontype and list of possible answers as input.
-}
generateAnswersHtml :: String -> [String] -> Html
generateAnswersHtml q_Type answers = if q_Type == "SINGLECHOICE"
  then generateSingelChoiceAnswerHtml 0 answers
  else generateFalseTrueAnswerHtml


{-|
  Helper function to generate the HTML of a SingleChoice question
  Taking the number of the correct answer and a list of possible answer texts.
-}
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


-- | Helper function to generate the HTML of a FalseTrue question
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


{-|
  Wrapper to generate the whole table of the result page, for specific quiz. 
  Takes QuizStats as input.
-}
generateTableHtml :: QuizStats -> Html
generateTableHtml stats = if length (playerAnswers stats) > 0
  then let
    th = ea "tr" [] $ e "th" "Player" ++ generateTableHeaderHtml (resultsQuestion stats)
    tRow = generateTableRowHtml (playerAnswers stats)
    tFooter = generateTableFooter (length $ playerAnswers stats) (resultsQuestion stats)
    table = ea "table" [] (th ++ tRow ++ tFooter)
    in table
  else generateTitle "Keine Statistik verfügbar."


{-|
  Generates the result pages - table's header. 
  Input is a list of questions or the corresponding quiz.
-}
generateTableHeaderHtml :: [(QuestionId, Result)] -> Html
generateTableHeaderHtml [] = ""
generateTableHeaderHtml ((questionId, _):xs) =
  ea "th" [] ("Q" ++ show questionId) ++ generateTableHeaderHtml xs


-- | Generates all the rows for the players of the quiz and their given answers
generateTableRowHtml :: [(Player, [((QuestionId, AnswerVal), Correct)])] -> Html
generateTableRowHtml [] = ""
generateTableRowHtml ((player, answers):xs) = let
  answersTag = concatMap (\((_, answer), correct) -> generateRow answer correct) answers
  playerTag = ea "th" [] player
  in ea "tr" [] (playerTag ++ answersTag) ++ generateTableRowHtml xs


-- | Generates a players answers row and the adds the correct CSS style
generateRow :: AnswerVal -> Correct -> Html
generateRow answer correct = let
  style = if correct == 1
    then "correct"
    else "wrong"
  in ea "td" [("class", style)] answer


{-|
  Generates the footer of the results page - table.
  Takes the amoung of players and the statistics per question as input.
-}
generateTableFooter :: Int -> [(QuestionId, Result)] -> Html
generateTableFooter noPlayers results = let
  resultTag =
    concatMap (\(_, res) -> ea "td" [] (show res ++ " / " ++ show noPlayers)) results
  statisticTH = ea "th" [] "Statistics"
  in ea "tr" [] (statisticTH ++ resultTag)


-- | Wrapper function to build simple Html
e :: String -> Html -> Html
e tag = ea tag []


-- | Function to build HTML-Tags with classes/attributes and chidren (inner tags/values)
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"