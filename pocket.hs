{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Network.URI
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans
import System.Process
import qualified Data.Text as T
import Control.Concurrent
import System.Directory
import System.FilePath
import Text.Regex.PCRE.Light
import qualified Data.ByteString.Char8 as B


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

main :: IO ()
main = do
  appdata_path <- getAppUserDataDirectory "pocket"
  createDirectoryIfMissing False appdata_path
  sources <- loadFrom (combine appdata_path "pocket_links.txt")
  dataset <- load_from (combine appdata_path "pocket_data.txt")
  let uris = mapMaybe parseURI sources
  links <- mapM getLinks uris
  tlinks <- atomically $ newTVar $ links
  tsources <- atomically $ newTVar $ sources
  tdataset <- atomically $ newTVar $ dataset
  _ <- forkIO $ waitAndUpdateLinks tlinks tsources tdataset
  runInputT defaultSettings $ readEvalLoop tlinks (tsources, tdataset)
  return ()

readEvalLoop :: TVar [[(String, String)]] -> (TVar [String], TVar ClassifierData) -> InputT IO ()
readEvalLoop tlinks (ts, td) = do
  dp <- lift $ readTVarIO td
  links <- fmap concat $ lift $ readTVarIO tlinks --fmap (filterLinks .  concat) $ lift $ readTVarIO tlinks
  linknrs <- numberUrls dp (links)
  mstr <- getInputLine "% "
  case mstr of
       Nothing -> return ()
       Just str | str == "q" || str == "quit" || str == "exit" -> return ()
                | otherwise -> do
                    s <- lift $ readTVarIO ts
                    d <- lift $ readTVarIO td
                    (s', d', upd) <- controller (words str) s d linknrs
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

numberUrls :: (Ord k, Num k, Monad m, Enum k) 
           => ClassifierData 
           -> [(String, t)] 
           -> m (Map k (Category, String, t))
numberUrls d links = do
  let linkclasses = map (\(t, l) -> (classify d t, t, l)) links
  let linknrs = Map.fromList $ zip [1..] linkclasses
  return linknrs

printUrls :: (MonadIO m, Show a1, Show a) 
          => Map a (a1, [Char], t) 
          -> InputT m ()
printUrls linknrs = 
  mapM_ 
    (\(nr, (cat, title, _)) -> 
       outputStrLn (cutLine ("[" ++ padNrTo 3 nr ++ "] " ++ padTo 4 ' ' (show cat) ++ ": " ++ title)))
       (Map.toList linknrs)

printTitlesOnly :: MonadIO m 
                => Map t (t1, String, t2) 
                -> InputT m ()
printTitlesOnly linknrs = 
  mapM_ 
    (\(_, (_, title, _)) -> 
       outputStrLn (title))
       (Map.toList linknrs)

printLinksOnly :: MonadIO m 
               => Map t (t1, t2, String) 
               -> InputT m ()
printLinksOnly linknrs = 
  mapM_ 
    (\(_, (_, _, url)) -> 
       outputStrLn (url))
       (Map.toList linknrs)

padTo :: Int -> a -> [a] -> [a]
padTo nr space text = text ++ [space] ++ (replicate nr' space)
  where
    nr' = nr - (length text)

padNrTo :: Show a => Int -> a -> [Char]
padNrTo le nr = (replicate nr' '0') ++ text
  where
    nr' = le - (length text)
    text = show nr

filterLinks strs = filter (\(title, _) -> match r (B.pack title) [] /= Nothing) strs
  where
    r = compile "(S\\d?\\dE\\d\\d)|(\\d?\\dx\\d\\d)" [caseless]

lineSeperator :: String -> String
lineSeperator = padTo 80 '-'

cutLine :: String -> String
cutLine s = take 80 s

controller :: [String] 
           -> [String] 
           -> ClassifierData 
           -> Map Int (Category, String, String) 
           -> InputT IO ([String], ClassifierData, Bool)
controller (com:args) s d ns 
  | com == "add-source" = do
      let s' = removeDuplicates (args ++ s)
      appdata_path <- lift $ getAppUserDataDirectory "pocket"
      lift $ save_to (combine appdata_path "pocket_links.txt") s'
      return (s', d, s /= s')
  | com == "remove-source" = do
      let s' = filter (\li -> and (map (\si -> not (hasWord si li)) args)) s
      appdata_path <- lift $ getAppUserDataDirectory "pocket"
      lift $ save_to (combine appdata_path "pocket_links.txt") s'
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
      let nsBad = Map.difference ns nsGood
      d' <- trainAndSave d nsBad Bad
      d'' <- trainAndSave d' nsGood Good
      return (s,d'', False)
  | com == "seems-ok" || com == "ok" = do
      let nsGood = filterNotCat Bad ns
      let nsBad = Map.difference ns nsGood
      d' <- trainAndSave d nsBad Bad
      d'' <- trainAndSave d' nsGood Good
      return (s,d'', False)
  | com == "all" = do
      printUrls ns
      return (s, d, False)
  | com == "show" || com == "ls" = do
      printUrls (filterNotCat Bad ns)
      return (s, d, False)
  | com == "threshold" || com == "limit" = do
      case maybeRead (head args) of
           Just i -> do
             let d' = set_threshold d Good i
             return (s, d', False)
           Nothing ->
              return (s, d, False)
  | com == "titles" = do
      printTitlesOnly (filterNotCat Bad ns)
      return (s, d, False)
  | com == "links" = do
      printLinksOnly (filterNotCat Bad ns)
      return (s, d, False)
  | com == "search" = do
      printUrls (getLinksFromStr ns args)
      return (s, d, False)
  | com == "open" = do
      let nsGood = getLinksFromStr ns args
      mapM_ (\(_, _, link) -> lift $! system ("open " ++ link)) (Map.elems nsGood)
      return (s, d, False)
  | com == "update" = do
      return (s, d, True)
controller _ s d _  = do
      outputStrLn "Commands: add-source link, remove-source searchterm, good 3, bad 4, all, show, all-good 1 2 3, open searchterm"
      return (s, d, False)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

trainUntil :: ClassifierData
           -> Map k (t, String, t1)
           -> Category
           -> ClassifierData
trainUntil d ns c = rd
  where
    rd = foldl (trainUntil') d (Map.elems ns)
    trainUntil' d' (_, title, _) = until (\d'' -> (c == (classify d'' title))) (\dt -> trainWithSmallSet dt title c) d'

trainAndSave :: (Show a, Show a1) 
             => ClassifierData
             -> Map a (a1, String, t)
             -> Category
             -> InputT IO ClassifierData
trainAndSave d ns (c :: Category) = do
  let d'' = trainUntil d ns c
  outputStrLn $ lineSeperator $ (show c) ++ " training"
  printUrls ns
  outputStrLn $ lineSeperator ""
  appdata_path <- lift $ getAppUserDataDirectory "pocket"
  lift $ saveTo (combine appdata_path "pocket_data.txt") d''
  return d''

hasWord :: String -> String -> Bool
hasWord a b = T.isInfixOf (T.toCaseFold $ T.pack a) (T.toCaseFold $ T.pack b)

getLinksFromStr :: Map Int (t, String, t1) -> [String] -> Map Int (t, String, t1)
getLinksFromStr ns str = foldl (\m s -> Map.union (Map.union (fromNr s) (fromSearchTerm s)) m) Map.empty str
  where
    fromNr s = fromMaybe Map.empty (fmap (\i -> Map.filterWithKey (\k _ -> k == i) ns) (maybeRead s :: Maybe Int))
    fromSearchTerm s = if (Map.null (fromNr s)) then (Map.filter (\(_, s', _) -> hasWord s s') ns) else Map.empty

filterNotCat :: (Ord k, Eq a) 
             => a -> Map k (a, t, t1) -> Map k (a, t, t1)
filterNotCat cat = Map.filter (\(c, _, _) -> c /= cat)

sendGrowlMessage :: String -> IO ()
sendGrowlMessage msg = do
  handler  <- growlHandler "leave" WARNING
  handlerT <- addTarget "127.0.0.1" handler
  updateGlobalLogger rootLoggerName (setHandlers [handlerT])
  warningM "pocket" msg

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates s = Set.toList $ Set.fromList s

updateLinks :: TVar [[(String, String)]]
            -> TVar [String]
            -> TVar ClassifierData
            -> IO ()
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

waitAndUpdateLinks :: TVar [[(String, String)]]
                   -> TVar [String]
                   -> TVar ClassifierData
                   -> IO b
waitAndUpdateLinks tlinks tsources tdata = do
  threadDelay $ 1000000 * 60 * 15 -- 15 min
  updateLinks tlinks tsources tdata
  waitAndUpdateLinks tlinks tsources tdata

diffByTitles :: Eq a 
             => [[(a, b)]] -> [[(a, b1)]] -> [a]
diffByTitles links links' = linkdiff
  where
    linkdiff = (concatFst links) \\ (concatFst links')
    concatFst l = map (fst)  (concat l)

filterBad :: ClassifierData -> [String] -> [String]
filterBad d = filter (\title -> (classify d title) /= Bad)
