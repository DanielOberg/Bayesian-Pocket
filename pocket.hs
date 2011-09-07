{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Network.URI
import Data.List
import Data.Char
import System.Environment
import Control.Monad (forM_, mapM_)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans

import System.Console.Haskeline


import Spider
import Bayesian

main = do
  args <- getArgs
  sources <- loadLinks
  dataset <- load_data
  let uris = catMaybes (map (parseURI) sources)
  links <- mapM (getLinks) (uris)
  runInputT defaultSettings $ readEvalLoop links (sources, dataset)
  return ()

readEvalLoop :: [[(String, String)]] -> ([String], ClassifierData) -> InputT IO ()
readEvalLoop links (s, d) = do
  linknrs <- numberUrls d (concat links)
  mstr <- getInputLine "% "
  case mstr of
       Nothing -> return ()
       Just "q" -> return ()
       Just str -> do
            (s', d') <- checkArg (words str) s d linknrs
            readEvalLoop links (s', d')

numberUrls d links = do
  let linkclasses = map (\(t, l) -> (classify d t, t, l)) links
  let linknrs = Map.fromList $ zip [1..] linkclasses
  return linknrs

printUrls linknrs = do
  mapM_ (\(nr, (cat, title, url)) -> outputStrLn ("[" ++ show nr ++ "] " ++ show cat ++ ": " ++ title)) (Map.toList linknrs)

checkArg :: [String] -> [String] -> ClassifierData -> Map Int (Category, String, String) -> InputT IO ([String], ClassifierData)
checkArg ("add-source":v) s d ns = do
  let s' = v ++ s
  lift $ saveLinks (s')
  return (s', d)
checkArg ("remove-source":str:_) s d ns = do
  let s' = filter (not . (isInfixOf str)) s
  lift $ saveLinks (s')
  return (s', d)
checkArg ("good":nstr:_) s d ns = do
  readAndTrain s d ns nstr Good
checkArg ("all-good":nstrs) s d ns = do
  (s', d') <- foldM (\(ts, td) nstr -> readAndTrain ts td ns nstr Good) (s, d) nstrs
  let bads = Map.keys ns \\ catMaybes (map (maybeRead) nstrs)
  (s'', d'') <- foldM (\(ts, td) nr -> trainAndPrint ts td ns nr Bad) (s', d') bads
  return (s'',d'')
checkArg ("bad":nstr:_) s d ns = do
  readAndTrain s d ns nstr Bad
checkArg ("all":_) s d ns = do
  printUrls ns
  return (s, d)
checkArg ("titles":_) s d ns = do
  let good = filter (\(cat, _, _) -> cat == Good) (Map.elems ns)
  mapM_ (\(_, title, link) -> outputStrLn (title ++ "\t\t" ++ link)) good
  return (s, d)
checkArg _ s d _ = do
  outputStrLn "Commands: add-source, remove-source, good, bad, all, titles, all-good"
  return (s, d)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

readAndTrain s d ns nstr (c :: Category) = do
  let maybenr = maybeRead nstr :: Maybe Int
  case maybenr of
       Just nr -> do
         trainAndPrint s d ns nr c
       Nothing -> return (s,d)

trainAndPrint s d ns nr c = do
  case Map.lookup nr ns of
       Just (cat, title, url) -> do
          outputStrLn $ (show c) ++ " " ++ title
          let d' = train d title c
          lift $ save_data d'
          return (s, d')
       Nothing -> return (s, d)

