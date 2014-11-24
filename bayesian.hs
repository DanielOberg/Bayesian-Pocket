{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayesian
  (
    train
  , trainWithSmallSet
  , classify
  , classifyWithDefault
  , loadBayesianFrom
  , saveBayesianTo
  , defaultData
  , setThreshold
  , getThreshold
  , getFeatures
  , ClassifierData
  , Category(..)
  ) where


import           Control.Exception
import           Data.List
import           Data.List.Split
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set          (Set)
import qualified Data.Set          as Set
import qualified Data.Text         as T
import           Safe              (tailSafe)
import           System.IO


-------------------------------- DATA TYPES ---------------------------------

-- | Categories, add some more if you wish
data Category = Good | Bad deriving (Eq, Show, Ord, Enum, Read, Bounded)

-- | The ClassifierData is all the data we operate on, contained into one structure
data ClassifierData = ClassifierData
     { fc                 :: Map (T.Text, Category) Int -- ^ sum of feature per category
     , cc                 :: Map Category Int           -- ^ total sum of each category
     , thresholds         :: Map Category Float -- ^ threshold to pass for category classification
     , weightOfAssumption :: Float    -- ^ weight
     , assumedProbability :: Float    -- ^ ap
     } deriving (Eq, Show, Read)

-------------------------------- DEFAULT VALUES -----------------------------

defaultWeight :: Float
defaultWeight   = 1.0
defaultAp :: Float
defaultAp       = 0.5
defaultThresholds :: Map Category Float
defaultThresholds = Map.fromList [(Good, 0.3), (Bad, 0.1)]
defaultData :: ClassifierData
defaultData     = ClassifierData Map.empty Map.empty defaultThresholds defaultWeight defaultAp

-------------------------------- PUBLIC FUNCTIONS ---------------------------

-- | Train our dataset to recognize the features as category
--
--   > train default_data  "Mail me money" Bad
train :: ClassifierData -> String -> Category -> ClassifierData
train d item cat = incCc d' cat
  where
    features = getLargeFeatureSet item
    d' = Set.fold (\f ld -> incFc ld f cat) d features

trainWithSmallSet :: ClassifierData
                      -> String
                      -> Category
                      -> ClassifierData
trainWithSmallSet d item cat = incCc d' cat
  where
    features = getLargeFeatureSet item
    d' = Set.fold (\f ld -> incFc ld f cat) d features

-- | Classify the string into a category and categorise as Bad if unsure
--
--   > classify sample "Mail me money"
--   Bad
classify :: ClassifierData -> String -> Category
classify d item = classifyWithDefault d item Bad

-- | Classify the string into a category and categorise as 'def' if unsure
--
--   > classify_with_default sample  "Mail me money" Bad
--   Bad
classifyWithDefault :: ClassifierData -> String -> Category -> Category
classifyWithDefault d item def = if breaksThreshold then def else best_cat
  where
    probs = [(prob d item cat, cat) | cat <- allCategories]
    (best_p, best_cat) = maximumBy (comparing fst) probs
    breaksThreshold = or [p * getThreshold d best_cat > best_p | (p, _) <- probs]

-- | Load dataset from file
loadBayesianFrom :: FilePath -> IO ClassifierData
loadBayesianFrom f = do
  str <- catch (readFile f)
               (\(_ :: IOException) -> do
                   hPutStr stderr ("Saving to: " ++ f)
                   return $ show defaultData)
  return (read str :: ClassifierData)

-- | Save dataset to file
saveBayesianTo :: Show a => FilePath -> a -> IO ()
saveBayesianTo f d = writeFile f (show d)


-------------------------------- PRIVATE FUNCTIONS -------------------------

-- | Split words into features (tokens).
--
-- Examples
--
--   > getwords "Donald Donald"
--   Map.fromList [(T.pack "donald",1)]
--
getFeatures :: String -> Set T.Text
getFeatures doc = Set.fromList words'
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
getLargeFeatureSet :: String -> Set T.Text
getLargeFeatureSet doc = Set.fromList (w ++ w')
  where
    splitTokens = "!#$%&'()*+,./:;<=>?@\^_`{|}~\t\r\n\v\f[] "
    w = [T.toCaseFold (T.pack s) | s <- splitOneOf splitTokens doc,
                                            not (null s), length s < 20]
    w' = zipWith (\x y -> T.unwords (y:[x])) (tailSafe w) w

addOne :: Num a => a -> a -> a
addOne _ y  = y + 1

incFc :: ClassifierData -> T.Text -> Category -> ClassifierData
incFc d f cat = d { fc = fc' }
  where
    fc'  = Map.insertWith addOne (f, cat) 1 (fc d)

incCc :: ClassifierData -> Category -> ClassifierData
incCc d cat   = d { cc = cc' }
  where
    cc' = Map.insertWith addOne cat 1 (cc d)

countFc :: ClassifierData -> T.Text -> Category -> Int
countFc d f cat = fromMaybe 0 (Map.lookup (f, cat) (fc d))

countCc :: ClassifierData -> Category -> Int
countCc d cat = fromMaybe 0 (Map.lookup cat (cc d))

countTotal :: ClassifierData -> Int
countTotal d   = Map.fold (+) 0 (cc d)

allCategories :: [Category]
allCategories = [(minBound :: Category) .. ]

fprob :: Fractional a => ClassifierData -> T.Text -> Category -> a
fprob d f cat
  | countCc d cat == 0 = 0.0
  | otherwise = fromIntegral (countFc d f cat) / fromIntegral (countCc d cat)

weightedprob :: ClassifierData -> T.Text -> Category -> (T.Text -> Category -> Float) -> Float -> Float -> Float
weightedprob d f cat prf weight ap = ((weight*ap)+(totals*basicprob))/(weight+totals)
  where
    basicprob = prf f cat
    totals = fromIntegral (sum [countFc d f c | c <- allCategories])

docprob :: ClassifierData -> String -> Category -> Float
docprob d item cat = product [weightedprob d f cat (fprob d) (weightOfAssumption d) (assumedProbability d) | f <- Set.toList features]
  where
    features = getLargeFeatureSet item

prob :: ClassifierData -> String -> Category -> Float
prob d item cat = docp * catp
  where
    catp    = fromIntegral (countCc d cat) / fromIntegral (countTotal d)
    docp    = docprob d item cat

setThreshold :: ClassifierData
              -> Category
              -> Float
              -> ClassifierData
setThreshold d cat n = d { thresholds = Map.insert cat n (thresholds d) }

getThreshold :: ClassifierData -> Category -> Float
getThreshold d cat = fromMaybe 0.0 (Map.lookup cat (thresholds d))

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
