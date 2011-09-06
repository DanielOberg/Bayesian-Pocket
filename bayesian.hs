{-# LANGUAGE OverloadedStrings #-} 
module Bayesian where

import Text.Regex.Posix

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Regex
import qualified Data.Text as T

import Data.List.Split

import Maybe

import Data.Ord
import Data.List

-- | Split words into tokens (features)
--
-- Examples
--
--   > getwords "Donald Donald"
--   Map.fromList [(T.pack "donald",1)]
--
getwords doc = Set.fromList words
  where
    splittTokens = "!#$%&'()*+,./:;<=>?@\^_`{|}~\t\r\n\v\f[] "
    words = [T.toCaseFold (T.pack s) | s <- splitOneOf splittTokens doc, length s > 2, length s < 20]


data Category = Good | Bad deriving (Eq, Show, Ord, Enum)

data ClassifierData = ClassifierData { 
                        fc :: Map (T.Text, Category) Int, -- feature (token) count
                        cc :: Map Category Int, -- Sum
                        thresholds :: Map Category Float
                      } deriving (Eq, Show, Ord)

getfeatures = getwords

addOne x y  = x + 1


incf d f cat = d { fc = fc' }
  where
    fc'  = Map.insertWith (addOne) (f, cat) 1 (fc d)

incc d cat   = d { cc = cc' }
  where
    cc' = Map.insertWith (addOne) cat 1 (cc d) 


fcount d f cat = fromMaybe 0 (Map.lookup (f, cat) (fc d))

catcount d cat = fromMaybe 0 (Map.lookup cat (cc d))

totalcount d   = Map.fold (+) 0 (cc d) 

categories = enumFrom Good

train d item cat = incc d' cat
  where
    features = getfeatures item
    d' = Set.fold (\f ld -> incf ld f cat) d features 

fprob d f cat 
  | catcount d cat == 0 = 0
  | otherwise = fromIntegral (fcount d f cat) / fromIntegral (catcount d cat) 


default_weight = 1.0
default_ap = 0.5

weightedprob d f cat prf weight ap = ((weight*ap)+(totals*basicprob))/(weight+totals)
  where
    basicprob = prf f cat
    totals = fromIntegral (sum [fcount d f c | c <- categories])

docprob d item cat = product [weightedprob d f cat (fprob d) | f <- Set.toList features]
  where
    features = getfeatures item

prob d item cat = docprob * catprob
  where
    catprob = fromIntegral (catcount d cat) / fromIntegral (totalcount d)
    docprob = fromIntegral docprob d item cat

default_thresholds = Map.fromList [(Good, 1.0), (Bad, 3.0)]

setthreshold d cat n = d { thresholds = Map.insert cat n (thresholds d) }

getthreshold d cat = fromMaybe 0.0 (Map.lookup cat (thresholds d))


classify d item def = if breaksThreshold then def else best_cat
  where
    probs = [(fromIntegral (prob d item cat), cat) | cat <- categories]
    (best_p, best_cat) = maximumBy (comparing fst) probs
    breaksThreshold = or [p*(getthreshold d best_cat) > best_p | (p, c) <- probs]
    
