{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Network.URI
import Data.List
import Data.Char
import System.Environment
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Trans
import System.Process
import qualified Data.Text as T
import Control.Concurrent

import Control.Concurrent.STM

import System.Console.Haskeline
import System.Log.Logger( 
         Priority(WARNING),
         rootLoggerName,
         setHandlers,
         updateGlobalLogger,
         warningM )
import System.Log.Handler.Growl ( addTarget, growlHandler )

import Spider
import Bayesian

main = do
  args <- getArgs
  sources <- loadLinks
  dataset <- load_data
  let uris = mapMaybe parseURI sources
  links <- mapM getLinks uris
  tlinks <- atomically $ newTVar $ links
  tsources <- atomically $ newTVar $ sources
  tdataset <- atomically $ newTVar $ dataset
  forkIO $ waitAndUpdateLinks tlinks tsources tdataset
  runInputT defaultSettings $ readEvalLoop tlinks (tsources, tdataset)
  return ()

readEvalLoop :: TVar [[(String, String)]] -> (TVar [String], TVar ClassifierData) -> InputT IO ()
readEvalLoop tlinks (ts, td) = do
  dp <- lift $ readTVarIO td
  links <- lift $ readTVarIO tlinks
  linknrs <- numberUrls dp (concat links)
  mstr <- getInputLine "% "
  case mstr of
       Nothing -> return ()
       Just str | str == "q" || str == "quit" || str == "exit" -> return ()
                | otherwise -> do
                    s <- lift $ readTVarIO ts
                    d <- lift $ readTVarIO td
                    (s', d', upd) <- checkArg (words str) s d linknrs
                    lift $! atomically $! writeTVar td d'
                    case upd of
                         False ->
                            readEvalLoop tlinks (ts, td)
                         True -> do
                            let uris = mapMaybe parseURI s'
                            links' <- lift $ mapM getLinks uris
                            lift $! atomically $! writeTVar tlinks links'
                            lift $! atomically $! writeTVar ts s'
                            readEvalLoop tlinks (ts, td)

numberUrls d links = do
  let linkclasses = map (\(t, l) -> (classify d t, t, l)) links
  let linknrs = Map.fromList $ zip [1..] linkclasses
  return linknrs

printUrls linknrs = 
  mapM_ 
    (\(nr, (cat, title, url)) -> 
       outputStrLn ("[" ++ padNrTo 3 nr ++ "] " ++ padTo 4 ' ' (show cat) ++ ": " ++ padTo 80  ' ' title)) 
       (Map.toList linknrs)

printTitlesOnly linknrs = 
  mapM_ 
    (\(nr, (cat, title, url)) -> 
       outputStrLn (title))
       (Map.toList linknrs)

printLinksOnly linknrs = 
  mapM_ 
    (\(nr, (cat, title, url)) -> 
       outputStrLn (url))
       (Map.toList linknrs)

padTo nr space text = text ++ [space] ++ (replicate nr' space)
  where
    nr' = nr - (length text)

padNrTo le nr = (replicate nr' '0') ++ text
  where
    nr' = le - (length text)
    text = show nr


checkArg :: [String] -> [String] -> ClassifierData -> Map Int (Category, String, String) -> InputT IO ([String], ClassifierData, Bool)
checkArg (com:args) s d ns 
  | com == "add-source " = do
      let s' = removeDuplicates (args ++ s)
      lift $ saveLinks s'
      return (s', d, s /= s')
  | com == "remove-source" = do
      let s' = filter (\li -> and (map (\si -> not (hasWord si li)) args)) s
      lift $ saveLinks s'
      return (s', d, False)
  | com == "good" || com == "add" = do
      let ns' = getLinksFromStr ns args
      d' <- trainAndSave d ns' Good
      return (s, d', False)
  | com == "bad" || com == "rm" = do
      let ns' = getLinksFromStr ns args
      d' <- trainAndSave d ns' Bad
      return (s, d', False)
  | com == "all-good" = do
      let nsGood = getLinksFromStr ns args
      d' <- trainAndSave d nsGood Good
      let nsBad = Map.difference ns nsGood
      d'' <- trainAndSave d' nsBad Bad
      return (s,d'', False)
  | com == "seems-ok" || com == "ok" = do
      let nsGood = filterNotCat Bad $ ns
      d' <- trainAndSave d nsGood Good
      let nsBad = Map.difference ns nsGood
      d'' <- trainAndSave d' nsBad Bad
      return (s,d'', False)
  | com == "all" = do
      printUrls ns
      return (s, d, False)
  | com == "show" || com == "ls" = do
      let nsGood = filterNotCat Bad $ ns
      printUrls nsGood
      return (s, d, False)
  | com == "threshold" || com == "limit" = do
      case maybeRead (head args) of
           Just i -> do
             let d' = set_threshold d Good i
             return (s, d', False)
           Nothing ->
              return (s, d, False)
  | com == "titles" = do
      let nsGood = filterNotCat Bad $ ns
      printTitlesOnly nsGood
      return (s, d, False)
  | com == "links" = do
      let nsGood = filterNotCat Bad $ ns
      printLinksOnly nsGood
      return (s, d, False)
  | com == "search" = do
      let nsGood = getLinksFromStr ns args
      printUrls nsGood
      return (s, d, False)
  | com == "open" = do
      let nsGood = getLinksFromStr ns args
      mapM_ (\(_, title, link) -> lift $! system ("open " ++ link)) (Map.elems nsGood)
      return (s, d, False)
  | com == "update" = do
      return (s, d, True)
checkArg _ s d _  = do
      outputStrLn "Commands: add-source link, remove-source searchterm, good 3, bad 4, all, show, all-good 1 2 3, open searchterm"
      return (s, d, False)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

trainUntil d ns c = rd
  where
    rd = foldl (trainUntil) d (Map.elems ns)
    trainUntil d' (_, title, _) = until (\d'' -> (c == (classify d'' title))) (\dt -> trainWithSmallSet dt title c) d'

trainAndSave d ns (c :: Category) = do
  let d'' = trainUntil d ns c
  lift $ save_data d''
  outputStrLn $ padTo 90 '-' $ (show c) ++ " training"
  printUrls ns
  outputStrLn $ padTo 90 '-' ""
  return d''

hasWord :: String -> String -> Bool
hasWord a b = T.isInfixOf (T.toCaseFold $ T.pack a) (T.toCaseFold $ T.pack b)

getLinksFromStr ns str = foldl (\m s -> Map.union (Map.union (fromNr s) (fromSearchTerm s)) m) Map.empty str
  where
    fromNr s = fromMaybe Map.empty (fmap (\i -> Map.filterWithKey (\k _ -> k == i) ns) (maybeRead s :: Maybe Int))
    fromSearchTerm s = if (Map.null (fromNr s)) then (Map.filter (\(_, s', _) -> hasWord s s') ns) else Map.empty

filterNotCat cat = Map.filter (\(c, _, _) -> c /= cat)

sendGrowlMessage :: String -> IO ()
sendGrowlMessage msg = do
  handler  <- growlHandler "leave" WARNING
  handlerT <- addTarget "127.0.0.1" handler
  updateGlobalLogger rootLoggerName (setHandlers [handlerT])
  warningM "pocket" msg

removeDuplicates s = Set.toList $ Set.fromList s

updateLinks tlinks tsources tdata = do
  sources <- readTVarIO tsources
  links <- readTVarIO tlinks
  d <- readTVarIO tdata
  let uris = mapMaybe parseURI sources
  links' <- mapM getLinks uris
  case links' == links of
       True -> return ()
       False -> do
          let linkdiff = diffByTitles links links'
          atomically $ writeTVar tlinks links'
          mapM_ (sendGrowlMessage) (filterBad d linkdiff)

waitAndUpdateLinks tlinks tsources tdata = do
  threadDelay $ 1000000 * 60 * 15 -- 15 min
  updateLinks tlinks tsources tdata
  waitAndUpdateLinks tlinks tsources tdata

diffByTitles links links' = linkdiff
  where
    linkdiff = (concatFst links) \\ (concatFst links')
    concatFst l = map (fst)  (concat l)

filterBad d = filter (\title -> (classify d title) /= Bad)
