{-# LANGUAGE OverloadedStrings #-}
-- | This Module handles the web (routes, actions, redirects) of the application  
module Haqu.Web where


import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import qualified Data.Text.Lazy as LT
import Haqu.Storage
  ( createPlayerFile,
    getNameById,
    getQuizOverviews,
    getQuizStatisticsByQuizId,
    getQuestionsFromQuizById,
    updateAnswerFile, doesQuizExist, doesDirExist,
  )
import Haqu.View
  ( generateFormHtml,
    generateHeader,
    generateOverviewHtml,
    generateQuestionHtml,
    generateTableHtml,
    generateTitle,
    generateUnorderedList, generateHeadHtml,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty
import Haqu.Models (QuizOverview(..), Html)


-- | Entrypoint for the scotty web server, starts server and creates routes
main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  -- css
  get (regex "^/(.*)/styles\\.css$") styles
  get "/" homeAction
  -- quiz start
  get "/quiz/:quiz/start" startAction
  post "/quiz/:quiz/start" postQuizStart
  -- questions
  get "/quiz/:quiz/:question?player=" quizQuestionByIdForPlayer
  post "/quiz/:quiz/:question?player=" postQuestionByIdForPlayer
  -- results
  get "/quiz/:quiz/result" getQuizResultsById
  -- redirect
  notFound redirectToHome


-- | Redirects traffic to the homepage
redirectToHome :: ActionM ()
redirectToHome = redirect $ LT.pack "/"


-- | Fetch stylesheet from files when called
styles :: ActionM ()
styles = do
  setHeader "Content-Type" "text/css"
  file "static/styles.css"


-- | Homepage action, loads all existing quizzez and renders according HTML-Overview
homeAction :: ActionM ()
homeAction = do
  liftIO (putStrLn "DEBUG: Home Action Called")
  quizoverviews <- liftIO $ getQuizOverviews "./data/"

  if length quizoverviews > 0
    then do
      let listEntries = map generateOverviewHtml quizoverviews
      let concatEntries = concat listEntries
      let htmlOverviews = generateUnorderedList concatEntries
      htmlString $ generateHeader ++ htmlOverviews
    else do
      htmlString $ generateHeader ++ generateTitle "No quizzes available"


{-| 
  Handles the creation of the "Start-Quiz" page. Renders form to start quiz and get player data
  Checks if the quiz exists, if not redirects to homepage
-} 
startAction :: ActionM ()
startAction = do
  quizId <- captureParam "quiz"

  checkQuizExists quizId

  quizName <- liftIO $ getNameById quizId
  let title = generateTitle ("Starting " ++ quizName)
  let form = generateFormHtml quizId
  htmlString $ generateHeader ++ title ++ form


{-| 
  Catches the path-variables and POST-Body from players that start the quiz.
  Gets players name and creates a playerfile in the persistence layer.
  Redirects to the first question of the quiz 
-}
postQuizStart :: ActionM ()
postQuizStart = do
  quizId <- captureParam "quiz"
  player <- formParam "player"

  -- create player file here
  _ <- liftIO $ createPlayerFile quizId player

  -- redirect to first question
  redirect $ LT.pack ("/quiz/" ++ quizId ++ "/" ++ "0" ++ "?player=" ++ player)


{-| 
  Renders a quizquestion (fetched by ID) for a specific player.
  If the quiz doesn't exist, redirects player to homepage
  Prepares a form for the question.
  Redirects the player to the results-page of the quiz, if no more question are available
-}
quizQuestionByIdForPlayer :: ActionM ()
quizQuestionByIdForPlayer = do
  quizId <- captureParam "quiz"
  checkQuizExists quizId

  qNo <- captureParam "question?player="
  player <- queryParam "player"

  questions <- liftIO $ getQuestionsFromQuizById quizId
  if qNo < length questions
    then
      htmlString $
        generateHeader
          ++ generateQuestionHtml quizId (show qNo) player (questions !! qNo)
    else redirect $ LT.pack ("/quiz/" ++ quizId ++ "/result")


{-| 
  Fetches the path-variables and POST body from a submitted answer by a player.
  Updates the answer file of the player with he newly submitted answer.
  Redirects the player to the next question of the quiz.
-}
postQuestionByIdForPlayer :: ActionM ()
postQuestionByIdForPlayer = do
  quizId <- captureParam "quiz"
  qNo <- captureParam "question?player="
  player <- queryParam "player"
  answer <- formParam "answer"

  let qNoInt = read qNo :: Int
  _ <- liftIO $ updateAnswerFile quizId qNo player answer

  let path = "/quiz/" ++ quizId ++ "/" ++ show (qNoInt + 1) ++ "?player=" ++ player
  redirect $ LT.pack path


{-| 
  Renders a quizzes results page, identified by Quiz ID.
  Fetches path variables.
  Checks if quiz exists, if not: redirects to homepage.
-}
getQuizResultsById :: ActionM ()
getQuizResultsById = do
  quizId <- captureParam "quiz"
  checkResultExists quizId
  statistics <- liftIO $ getQuizStatisticsByQuizId quizId
  quizOverviews <- liftIO $ liftIO $ getQuizOverviews "./data/"
  let quizOverview = head $ filter (\quiz -> qId quiz == quizId) quizOverviews

  htmlString $ generateHeadHtml
    ++ generateHeader
    ++ generateTitle ("Results: " ++ name quizOverview)
    ++ desc quizOverview
    ++ generateTableHtml statistics


-- | Helper function: Checks if result directory exists
checkResultExists :: String -> ActionM ()
checkResultExists quizId = do
  liftIO $ doesDirExist quizId

-- | Helper function: checks if quiz exists, if not redirect to homepage
checkQuizExists :: String -> ActionM ()
checkQuizExists quizId = do
  qExists <- liftIO $ doesQuizExist quizId
  if qExists
    then return ()
    else redirect $ LT.pack "/"


-- | Takes string and packs it into an Scotty-Action
htmlString :: String -> ActionM ()
htmlString = html . LT.pack


-- | Wrapper function to build simple Html
e :: String -> Html -> Html
e tag = ea tag []


-- | Function to build HTML-Tags with classes/attributes and chidren (inner tags/values)
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where
    attrsHtml [] = []
    attrsHtml as = " " : intersperse " " (map attrHtml as)
    attrHtml (key, value) = key ++ "='" ++ value ++ "'"
