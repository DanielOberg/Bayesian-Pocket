{-# LANGUAGE OverloadedStrings #-}

module Bayesian
  (
    train
  , trainWithSmallSet
  , classify
  , classify_with_default
  , load_from
  , save_to
  , default_data
  , set_threshold
  , get_threshold
  , get_features
  , ClassifierData
  , Category(..)
  ) where


import System.IO
import Control.Exception
import Data.Maybe
import Data.Ord
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Safe(tailSafe)


-------------------------------- DATA TYPES ---------------------------------

-- | Categories, add some more if you wish
data Category = Good | Bad deriving (Eq, Show, Ord, Enum, Read, Bounded)

-- | The ClassifierData is all the data we operate on, contained into one structure
data ClassifierData = ClassifierData
     { fc :: Map (T.Text, Category) Int -- ^ sum of feature per category
     , cc :: Map Category Int           -- ^ total sum of each category
     , thresholds :: Map Category Float -- ^ threshold to pass for category classification
     , weight_of_assumption :: Float    -- ^ weight
     , assumed_probability  :: Float    -- ^ ap
     } deriving (Eq, Show, Read)

-------------------------------- DEFAULT VALUES -----------------------------

default_weight :: Float
default_weight   = 1.0
default_ap :: Float
default_ap       = 0.5
default_thresholds :: Map Category Float
default_thresholds = Map.fromList [(Good, 0.3), (Bad, 0.1)]
default_data :: ClassifierData
default_data     = ClassifierData Map.empty Map.empty default_thresholds default_weight default_ap

-------------------------------- PUBLIC FUNCTIONS ---------------------------

-- | Train our dataset to recognize the features as category
--
--   > train default_data  "Mail me money" Bad
train :: ClassifierData -> [Char] -> Category -> ClassifierData
train d item cat = inc_cc d' cat
  where
    features = get_large_feature_set item
    d' = Set.fold (\f ld -> inc_fc ld f cat) d features

trainWithSmallSet :: ClassifierData
                      -> [Char]
                      -> Category
                      -> ClassifierData
trainWithSmallSet d item cat = inc_cc d' cat
  where
    features = get_large_feature_set item
    d' = Set.fold (\f ld -> inc_fc ld f cat) d features

-- | Classify the string into a category and categorise as Bad if unsure
--
--   > classify sample "Mail me money"
--   Bad
classify :: ClassifierData -> String -> Category
classify d item = classify_with_default d item Bad

-- | Classify the string into a category and categorise as 'def' if unsure
--
--   > classify_with_default sample  "Mail me money" Bad
--   Bad
classify_with_default :: ClassifierData -> String -> Category -> Category
classify_with_default d item def = if breaksThreshold then def else best_cat
  where
    probs = [(prob d item cat, cat) | cat <- all_categories]
    (best_p, best_cat) = maximumBy (comparing fst) probs
    breaksThreshold = or [p*(get_threshold d best_cat) > best_p | (p, _) <- probs]

-- | Load dataset from file
load_from :: FilePath -> IO ClassifierData
load_from f = do
  str <- catch (readFile f)
               (\e -> do
                   let err = show (e :: IOException)
                   hPutStr stderr ("Saving to: " ++ f)
                   return $ show default_data)
  return (read str :: ClassifierData)

-- | Save dataset to file
save_to :: Show a => FilePath -> a -> IO ()
save_to f d = do
  writeFile f (show d)


-------------------------------- PRIVATE FUNCTIONS -------------------------

-- | Split words into features (tokens).
--
-- Examples
--
--   > getwords "Donald Donald"
--   Map.fromList [(T.pack "donald",1)]
--
get_features :: [Char] -> Set T.Text
get_features doc = Set.fromList words'
  where
    splitTokens = "!#$%&'()*+,./:;<=>?@\^_`{|}~\t\r\n\v\f[] "
    words' = [T.toCaseFold (T.pack s) | s <- splitOneOf splitTokens doc,
                                            length s > 2, length s < 20]

-- | Split words into features (tokens). This works on small sets by combining words
--
-- Examples
--
--   > getwords "Donald Donald"
--   Map.fromList [(T.pack "donald",1)]
--
get_large_feature_set :: [Char] -> Set T.Text
get_large_feature_set doc = Set.fromList (w ++ w')
  where
    splitTokens = "!#$%&'()*+,./:;<=>?@\^_`{|}~\t\r\n\v\f[] "
    w = [T.toCaseFold (T.pack s) | s <- splitOneOf splitTokens doc,
                                            length s > 0, length s < 20]
    w' = zipWith (\x y -> T.unwords (y:[x])) (tailSafe w) w

add_one :: Num a => a -> a -> a
add_one _ y  = y + 1

inc_fc :: ClassifierData -> T.Text -> Category -> ClassifierData
inc_fc d f cat = d { fc = fc' }
  where
    fc'  = Map.insertWith (add_one) (f, cat) 1 (fc d)

inc_cc :: ClassifierData -> Category -> ClassifierData
inc_cc d cat   = d { cc = cc' }
  where
    cc' = Map.insertWith (add_one) cat 1 (cc d)

count_fc :: ClassifierData -> T.Text -> Category -> Int
count_fc d f cat = fromMaybe 0 (Map.lookup (f, cat) (fc d))

count_cc :: ClassifierData -> Category -> Int
count_cc d cat = fromMaybe 0 (Map.lookup cat (cc d))

count_total :: ClassifierData -> Int
count_total d   = Map.fold (+) 0 (cc d)

all_categories :: [Category]
all_categories = [(minBound :: Category) .. ]

fprob :: Fractional a => ClassifierData -> T.Text -> Category -> a
fprob d f cat
  | count_cc d cat == 0 = 0.0
  | otherwise = fromIntegral (count_fc d f cat) / fromIntegral (count_cc d cat)

weightedprob :: ClassifierData -> T.Text -> Category -> (T.Text -> Category -> Float) -> Float -> Float -> Float
weightedprob d f cat prf weight ap = ((weight*ap)+(totals*basicprob))/(weight+totals)
  where
    basicprob = prf f cat
    totals = fromIntegral (sum [count_fc d f c | c <- all_categories])

docprob :: ClassifierData -> String -> Category -> Float
docprob d item cat = product [weightedprob d f cat (fprob d) (weight_of_assumption d) (assumed_probability d) | f <- Set.toList features]
  where
    features = get_large_feature_set item

prob :: ClassifierData -> String -> Category -> Float
prob d item cat = docp * catp
  where
    catp    = fromIntegral (count_cc d cat) / fromIntegral (count_total d)
    docp    = docprob d item cat

set_threshold :: ClassifierData
              -> Category
              -> Float
              -> ClassifierData
set_threshold d cat n = d { thresholds = Map.insert cat n (thresholds d) }

get_threshold :: ClassifierData -> Category -> Float
get_threshold d cat = fromMaybe 0.0 (Map.lookup cat (thresholds d))

{-
example_training :: ClassifierData -> ClassifierData
example_training da = foldl (\d (s, c) -> train d s c) da trainingdata
  where
    trainingdata = [("Nobody owns the water.", Good),
                    ("the quick rabbit jumps fences", Good),
                    ("buy pharmaceuticals now", Bad),
                    ("make quick money at the online casino", Bad),
                    ("the quick brown fox jumps", Good)]

sample :: ClassifierData
sample = example_training default_data
-}
