{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Data.List (intersperse)
import Haqu.Storage
import GHC.IO (unsafePerformIO)

type Html = String


main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" homeAction


styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"


homeAction :: ActionM ()
homeAction = do
    liftIO (putStrLn "DEBUG: Home Action Called")
    let title = e "H1" "haqu"

    -- check if unsafePerformIO doesn't bring side effects
    let quizoverviews = unsafePerformIO (getQuizOverviews "./data/")
    let listEntries = map generateOverviewHtml quizoverviews
    let concatEntries = concat listEntries
    -- ok: let singleHtml = generateOverviewHtml MkQuizOverview{qId="1", name="aa", desc="bb", link="cc"}
    
    let htmlOverviews = ea "ul" [] concatEntries 
    htmlString $ title ++ htmlOverviews


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
