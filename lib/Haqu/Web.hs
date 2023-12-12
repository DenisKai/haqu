{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Haqu.Storage
    ( getQuizOverviews,
      getNameById )
import Haqu.View

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
    quizoverviews <- liftIO $ getQuizOverviews "./data/"
    let listEntries = map generateOverviewHtml quizoverviews
    let concatEntries = concat listEntries    
    let htmlOverviews = generateUnorderedList concatEntries 
    htmlString $ generateHeader ++ htmlOverviews


startAction :: ActionM ()
startAction = do
  pathId <- captureParam "id"
  quizName <- liftIO $ getNameById pathId
  let title = generateTitle quizName
  let form = generateFormHtml pathId
  htmlString $ generateHeader ++ title ++ form
  

postQuizStart :: ActionM ()
postQuizStart = do
  qId <- captureParam "id"
  playername <- formParam "playername"

  -- create player file here

  -- redirect to first question
  redirect $ LT.pack ("/quiz/" ++ qId ++ "/" ++ "0" ++ "?player=" ++ playername)


quizQuestionByIdForPlayer :: ActionM ()
quizQuestionByIdForPlayer = do
  qId <- captureParam "id"
  -- TODO get right para
  questionNo <- captureParam "question"
  player <- queryParam "player"


  htmlString $ e "To" (qId ++ questionNo ++ player)


postQuestionByIdForPlayer :: ActionM ()
postQuestionByIdForPlayer = do
  htmlString $ e "To" "Do"


getQuizResultsById :: ActionM ()
getQuizResultsById = do
  htmlString $ e "To" "Do"


htmlString :: String -> ActionM ()
htmlString = html . LT.pack
