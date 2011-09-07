{-# LANGUAGE OverloadedStrings #-}

module Bayesian(train, classify, classify_with_default) where

import Data.Maybe
import Data.Ord
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T


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

default_filename = "pocket_data.txt" :: FilePath
default_weight   = 1.0
default_ap       = 0.5
default_thresholds = Map.fromList [(Good, 0.5), (Bad, 0.5)]
default_data     = ClassifierData Map.empty Map.empty default_thresholds default_weight default_ap

-------------------------------- PUBLIC FUNCTIONS ---------------------------

-- | Train our dataset to recognise the features as category
--
--   > train default_data  "Mail me money" Bad
train d item cat = inc_cc d' cat
  where
    features = get_features item
    d' = Set.fold (\f ld -> inc_fc ld f cat) d features 

-- | Classify the string into a category and categorise as Good if unsure
--
--   > classify sample "Mail me money"
--   Bad
classify :: ClassifierData -> String -> Category
classify d item = classify_with_default d item Good

-- | Classify the string into a category and categorise as 'def' if unsure
--
--   > classify_with_default sample  "Mail me money" Bad
--   Bad
classify_with_default :: ClassifierData -> String -> Category -> Category
classify_with_default d item def = if breaksThreshold then def else best_cat
  where
    probs = [(prob d item cat, cat) | cat <- all_categories]
    (best_p, best_cat) = maximumBy (comparing fst) probs
    breaksThreshold = or [p*(get_threshold d best_cat) > best_p | (p, c) <- probs]

-- | Load dataset from file
load_from f = do 
  str <- readFile f
  return (read str :: ClassifierData)

-- | Save dataset to file
save_to f d = do
  writeFile f (show d)

load_data = load_from default_filename
save_data d = save_to default_filename d

-------------------------------- PRIVATE FUNCTIONS -------------------------

-- | Split words into features (tokens).
--
-- Examples
--
--   > getwords "Donald Donald"
--   Map.fromList [(T.pack "donald",1)]
--
get_features doc = Set.fromList words
  where
    splitTokens = "!#$%&'()*+,./:;<=>?@\^_`{|}~\t\r\n\v\f[] "
    words = [T.toCaseFold (T.pack s) | s <- splitOneOf splitTokens doc, 
                                            length s > 2, length s < 20]

add_one x y  = x + 1

inc_fc d f cat = d { fc = fc' }
  where
    fc'  = Map.insertWith (add_one) (f, cat) 1 (fc d)

inc_cc d cat   = d { cc = cc' }
  where
    cc' = Map.insertWith (add_one) cat 1 (cc d) 

count_fc d f cat = fromMaybe 0 (Map.lookup (f, cat) (fc d))

count_cc d cat = fromMaybe 0 (Map.lookup cat (cc d))

count_total d   = Map.fold (+) 0 (cc d) 

all_categories = [(minBound :: Category) .. ]

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
    features = get_features item

prob :: ClassifierData -> String -> Category -> Float
prob d item cat = docp * catp
  where
    catp    = fromIntegral (count_cc d cat) / fromIntegral (count_total d)
    docp    = docprob d item cat

set_threshold d cat n = d { thresholds = Map.insert cat n (thresholds d) }

get_threshold d cat = fromMaybe 0.0 (Map.lookup cat (thresholds d))

example_training da = foldl (\d (s, c) -> train d s c) da trainingdata
  where
    trainingdata = [("Nobody owns the water.", Good), 
                    ("the quick rabbit jumps fences", Good),
                    ("buy pharmaceuticals now", Bad),
                    ("make quick money at the online casino", Bad),
                    ("the quick brown fox jumps", Good)]

sample = example_training default_data

main = do

