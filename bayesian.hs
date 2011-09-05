{-# LANGUAGE OverloadedStrings #-} 
module Bayesian where

import Text.Regex.Posix

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Regex
import qualified Data.Text as T

import Data.List.Split

-- | Split words into tokens
--
--   > getwords "Donald Donald"
--   Map.fromList [(T.pack "donald",1)]
--   
getwords doc = Map.fromList [(w,1) | w <- words]
  where
    splittTokens = "!#$%&'()*+,./:;<=>?@\^_`{|}~\t\r\n\v\f[] "
    words = [T.toCaseFold (T.pack s) | s <- splitOneOf splittTokens doc, length s > 2, length s < 20]


