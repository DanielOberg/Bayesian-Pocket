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


import Spider
import Bayesian

main = do
  args <- getArgs
  sources <- loadLinks
  dataset <- load_data
  let uris = catMaybes (map (parseURI) sources)
  links <- mapM (getLinks) (uris)
  readEvalLoop links (sources, dataset)
  return ()

readEvalLoop links (s, d) = do
  linknrs <- printUrls d (concat links)
  str <- getLine
  unless (str == "quit") $ do
    (s', d') <- checkArg (words str) s d linknrs
    readEvalLoop links (s', d')

printUrls d links = do
  let linkclasses = map (\(t, l) -> (classify d t, t, l)) links
  let linknrs = Map.fromList $ zip [1..] linkclasses
  mapM_ (\(nr, (cat, title, url)) -> putStrLn ("[" ++ show nr ++ "] " ++ show cat ++ ": " ++ title)) (Map.toList linknrs)
  return linknrs

--checkArg :: [String] -> [String] -> ClassifierData -> IO ([String], ClassifierData)
checkArg ("add-source":v) s d ns = do
  let s' = v ++ s
  saveLinks (s')
  return (s', d)
checkArg ("remove-source":str:_) s d ns = do
  let s' = filter (not . (isInfixOf str)) s
  saveLinks (s')
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
checkArg _ s d _ = do
  putStrLn "Commands: add-source, remove-source, good, bad"
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
          putStrLn $ (show c) ++ " " ++ title
          let d' = train d title c
          save_data d'
          return (s, d')
       Nothing -> return (s, d)

