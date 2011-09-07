{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}

module Spider (getLinks, loadLinks, saveLinks) where

import Text.XML.HXT.Core
import Text.Printf
import Network.URI
import Network.HTTP
import Data.List
import Data.Char
import System.Environment
import Control.Monad (forM_, mapM_)
import qualified Data.Text as T
import Data.Maybe

selectAnchors = 
  atTagCase "a"
  >>> (getAttrValue "href" &&& 
       -- The title may be broken up into multiple text nodes.
       -- So, we collect it as a list and then lift 'concat' 
       -- to combine it.
       (listA (deep isText >>> getText) >>> arr concat))

urlLink = "http://eztv.it/"

-- case-insensitive tag matching
atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper

parseHTML = readString [ withValidate no
                       , withParseHTML yes
                       , withWarnings no
                       ]

-- Pretend to be a user of Mozilla Firefox, because Google
-- will not display results for unknown user agents.
userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667" 

get :: URI -> IO String
get uri = do
  let req = Request uri GET [] ""
  eresp <- simpleHTTP $ insertHeader HdrUserAgent userAgent req
  case eresp of
    Left er -> error $ show er
    Right res -> return $ rspBody res

main = do
  args <- getArgs
  let uris = catMaybes (map (parseURI) args)
  links <- mapM (getLinks) uris
  mapM_ (print) (concat links)

getLinks :: URI -> IO [(String, String)]
getLinks uri = do 
  body  <- get uri
  links <- runX (parseHTML body >>> selectAnchors)
  return $ filterBadLinks (show uri) links

filterBadLinks :: String -> [(String, String)] -> [(String, String)]
filterBadLinks base links = [(strip title, strip absolute) | (url, title) <- links
                            , length title > 2
                            , length title < 100
                            , let (Just absolute) = (expandURIString url base)
                            , not $ isPrefixOf "javascript" absolute
                            ]

strip str = T.unpack (T.strip (T.pack str))

-- | Load links from file
loadFrom f = do 
  str <- catch (readFile f) (\_ -> return $ show ([] :: [String]))
  return (read str :: [String])

-- | Save dataset to file
saveTo f d = do
  writeFile f (show d)

loadLinks = loadFrom default_filename
saveLinks d = saveTo default_filename d

default_filename = "pocket_links.txt" :: FilePath


