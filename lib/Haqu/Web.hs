{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Data.List (intersperse)
import Haqu.Storage

type Html = String


main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" homeAction
  get  "/quiz/:id/start" startAction
  post "/quiz/:id/start" postQuizStart

  -- questions
  get "/quiz/:id/:question?player=name" quizQuestionByIdForPlayer
  post "/quiz/:id/:question?player=name" postQuestionByIdForPlayer

  -- results
  get "/quiz/:id/result" getQuizResultsById


styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"


homeAction :: ActionM ()
homeAction = do
    liftIO (putStrLn "DEBUG: Home Action Called")
    let title = e "H1" "haqu"
    quizoverviews <- liftIO $ getQuizOverviews "./data/"
    let listEntries = map generateOverviewHtml quizoverviews
    let concatEntries = concat listEntries    
    let htmlOverviews = ea "ul" [] concatEntries 
    htmlString $ title ++ htmlOverviews


startAction :: ActionM ()
startAction = do
  pathId <- captureParam "id"
  name <- liftIO $ getNameById pathId
  let form = ea "form"
  htmlString $ e "H1" "haqu"
  

postQuizStart :: ActionM ()
postQuizStart = do
  htmlString $ e "to" "do"


quizQuestionByIdForPlayer :: ActionM ()
quizQuestionByIdForPlayer = do
  htmlString $ e "To" "Do"


postQuestionByIdForPlayer :: ActionM ()
postQuestionByIdForPlayer = do
  htmlString $ e "To" "Do"


getQuizResultsById :: ActionM ()
getQuizResultsById = do
  htmlString $ e "To" "Do"


generateOverviewHtml :: QuizOverview -> Html
generateOverviewHtml quiz = 
  e "li" (
    ea "b" [] ("[" ++ qId quiz ++ "] ")
    ++ ea "b" [] (name quiz ++ ": ")
    ++ desc quiz ++ " " 
    ++ ea "a" [("href", link quiz)] "Start quiz"
  )


htmlString :: String -> ActionM ()
htmlString = html . LT.pack


-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []


ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
