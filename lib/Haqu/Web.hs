{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where


import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import qualified Data.Text.Lazy as LT
import Haqu.Storage
  ( createPlayerFile,
    getNameById,
    getQuizOverviews,
    getQuizStatisticsByQuizId,
    readQuizFileById,
    updateAnswerFile, doesQuizExist,
  )
import Haqu.View
  ( Html,
    generateFormHtml,
    generateHeader,
    generateOverviewHtml,
    generateQuestionHtml,
    generateTableHtml,
    generateTitle,
    generateUnorderedList, generateHeadHtml,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty
import Haqu.Models (QuizOverview(..))


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


redirectToHome :: ActionM ()
redirectToHome = redirect $ LT.pack "/"


styles :: ActionM ()
styles = do
  setHeader "Content-Type" "text/css"
  file "static/styles.css"


homeAction :: ActionM ()
homeAction = do
  liftIO (putStrLn "DEBUG: Home Action Called")
  quizoverviews <- liftIO $ getQuizOverviews "./data/"
  let listEntries = map generateOverviewHtml quizoverviews
  let concatEntries = concat listEntries
  let htmlOverviews = generateUnorderedList concatEntries
  htmlString $ generateHeader ++ htmlOverviews


startAction :: ActionM ()
startAction = do
  quizId <- captureParam "quiz"

  checkQuizExists quizId
  
  quizName <- liftIO $ getNameById quizId
  let title = generateTitle ("Starting " ++ quizName)
  let form = generateFormHtml quizId
  htmlString $ generateHeader ++ title ++ form


postQuizStart :: ActionM ()
postQuizStart = do
  quizId <- captureParam "quiz"
  player <- formParam "player"

  -- create player file here
  _ <- liftIO $ createPlayerFile quizId player

  -- redirect to first question
  redirect $ LT.pack ("/quiz/" ++ quizId ++ "/" ++ "0" ++ "?player=" ++ player)


quizQuestionByIdForPlayer :: ActionM ()
quizQuestionByIdForPlayer = do
  quizId <- captureParam "quiz"
  checkQuizExists quizId

  qNo <- captureParam "question?player="
  player <- queryParam "player"

  questions <- liftIO $ readQuizFileById quizId
  if qNo < length questions
    then
      htmlString $
        generateHeader
          ++ generateQuestionHtml quizId (show qNo) player (questions !! qNo)
    else redirect $ LT.pack ("/quiz/" ++ quizId ++ "/result")


postQuestionByIdForPlayer :: ActionM ()
postQuestionByIdForPlayer = do
  quizId <- captureParam "quiz"
  qNo <- captureParam "question?player="
  player <- queryParam "player"
  answer <- formParam "answer"

  let qNoInt = read qNo :: Int
  _ <- liftIO $ updateAnswerFile quizId qNo player answer

  redirect $ LT.pack ("/quiz/" ++ quizId ++ "/" ++ show (qNoInt + 1) ++ "?player=" ++ player)


getQuizResultsById :: ActionM ()
getQuizResultsById = do
  quizId <- captureParam "quiz"
  statistics <- liftIO $ getQuizStatisticsByQuizId quizId
  quizOverviews <- liftIO $ liftIO $ getQuizOverviews "./data/"
  let quizOverview = head $ filter (\quiz -> qId quiz == quizId) quizOverviews

  htmlString $ generateHeadHtml
    ++ generateHeader 
    ++ generateTitle ("Results: " ++ name quizOverview)
    ++ desc quizOverview 
    ++ generateTableHtml statistics


checkQuizExists :: String -> ActionM ()
checkQuizExists quizId = do
  qExists <- liftIO $ doesQuizExist quizId
  if qExists
    then return ()
    else redirect $ LT.pack "/"


htmlString :: String -> ActionM ()
htmlString = html . LT.pack


-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []


ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where
    attrsHtml [] = []
    attrsHtml as = " " : intersperse " " (map attrHtml as)
    attrHtml (key, value) = key ++ "='" ++ value ++ "'"