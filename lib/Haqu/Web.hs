{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Haqu.Storage
    ( getQuizOverviews,
      getNameById, createPlayerFile, readQuizFileById, updateAnswerFile, getQuizStatisticsByQuizId)
import Haqu.View
    ( Html,
      generateUnorderedList,
      generateHeader,
      generateTitle,
      generateOverviewHtml,
      generateFormHtml,
      generateQuestionHtml, generateTableHtml )
import Data.List (intersperse)



main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" homeAction
  get  "/quiz/:quiz/start" startAction
  post "/quiz/:quiz/start" postQuizStart

  -- questions
  get "/quiz/:quiz/:question?player=" quizQuestionByIdForPlayer
  post "/quiz/:quiz/:question?player=" postQuestionByIdForPlayer

  -- results
  get "/quiz/:quiz/result" getQuizResultsById


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
  pathId <- captureParam "quiz"
  quizName <- liftIO $ getNameById pathId
  let title = generateTitle ("Starting " ++ quizName)
  let form = generateFormHtml pathId
  htmlString $ generateHeader ++ title ++ form


postQuizStart :: ActionM ()
postQuizStart = do
  qId <- captureParam "quiz"
  player <- formParam "player"

  -- create player file here
  _ <- liftIO $ createPlayerFile qId player

  -- redirect to first question
  redirect $ LT.pack ("/quiz/" ++ qId ++ "/" ++ "0" ++ "?player=" ++ player)


quizQuestionByIdForPlayer :: ActionM ()
quizQuestionByIdForPlayer = do
  qId <- captureParam "quiz"
  qNo <- captureParam "question?player="
  player <- queryParam "player"

  questions <- liftIO $ readQuizFileById qId
  if qNo < length questions
    then htmlString $ generateHeader
      ++ generateQuestionHtml qId (show qNo) player (questions!!qNo)
    else redirect $ LT.pack ("/quiz/" ++ qId ++ "/result")


postQuestionByIdForPlayer :: ActionM ()
postQuestionByIdForPlayer = do
  qId <- captureParam "quiz"
  qNo <- captureParam "question?player="
  player <- queryParam "player"
  answer <- formParam "answer"

  let qNoInt = read qNo :: Int
  _ <- liftIO $ updateAnswerFile qId qNo player answer

  redirect $ LT.pack ("/quiz/" ++ qId ++ "/" ++  show (qNoInt + 1) ++ "?player=" ++ player)


getQuizResultsById :: ActionM ()
getQuizResultsById = do
  qId <- captureParam "quiz"
  statistics <- liftIO $ getQuizStatisticsByQuizId qId
  

  htmlString $ generateHeader ++ generateTitle "Results: placeholder" ++ generateTableHtml


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