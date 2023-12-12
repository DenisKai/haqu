module Haqu.View where

import Data.List (intersperse)
import Haqu.Models (QuizOverview (..))

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
    labelinput = ea "label" [("for", "playername")] "Please enter your name:"
    textinput = ea "input" [
        ("type", "text"), 
        ("id", "playername"), 
        ("name", "playername")] ""
    submit = ea "input" [
        ("type", "submit"), 
        ("value", "Start Quiz")] ""
    form = ea "form" [
        ("action", "/quiz/" ++ quizId ++ "/start"), 
        ("method", "post")] 
        (labelinput ++ breakTag ++ textinput ++ breakTag ++ submit)
    in form
    



-- questions



-- results



-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []


ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
