{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}

{-
example :: IO ()
example = do
  args <- getArgs
  let uris = catMaybes (map (parseURI) args)
  links <- mapM (getLinks) uris
  mapM_ (print) (concat links)
-}


module Spider (getLinks, loadFrom, saveTo) where

import Text.XML.HXT.Core
import Network.URI
import Network.HTTP
import Data.List
import Data.Char
import qualified Data.Text as T
import Data.Tree.NTree.TypeDefs (NTree)

selectAnchors :: ArrowXml cat 
              => cat (NTree XNode) (String, [Char])
selectAnchors = 
  atTagCase "a"
  >>> (getAttrValue "href" &&& 
       -- The title may be broken up into multiple text nodes.
       -- So, we collect it as a list and then lift 'concat' 
       -- to combine it.
       (listA (deep isText >>> getText) >>> arr concat))

text :: ArrowXml cat 
     => cat (NTree XNode) String
text = getChildren >>> getText

selectRSSLinks :: ArrowXml cat => cat (NTree XNode) (String, String)
selectRSSLinks = atTagCase "item" >>>
  proc x -> do
    title <- text <<< atTagCase "title" -< x
    link <- text <<< atTagCase "link" -< x
    returnA -< (link, title)

-- case-insensitive tag matching
atTagCase :: ArrowXml a 
          => [Char] -> a (NTree XNode) XmlTree
atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper

parseHTML :: String -> IOStateArrow s b XmlTree
parseHTML = readString [ withValidate no
                       , withParseHTML yes
                       , withWarnings no
                       ]

parseRSS :: String -> IOStateArrow s b XmlTree
parseRSS = readString [ withValidate no
                       , withWarnings no
                       ]

-- Pretend to be a user of Mozilla Firefox, because Google
-- will not display results for unknown user agents.
userAgent :: String
userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667" 

get :: URI -> IO String
get uri = do
  let req = Request uri GET [] ""
  eresp <- simpleHTTP $ insertHeader HdrUserAgent userAgent req
  case eresp of
    Left er -> error $ show er
    Right res -> return $ rspBody res



getLinks :: URI -> IO [(String, String)]
getLinks uri = do 
  body  <- get uri
  links <- runX (parseHTML body >>> selectAnchors)
  case null links of
       True -> do
          rsslinks <- runX (parseRSS body >>> selectRSSLinks)
          return $ filterBadLinks (show uri) rsslinks
       False -> do
          return $ filterBadLinks (show uri) links


filterBadLinks :: String -> [(String, String)] -> [(String, String)]
filterBadLinks base links = [(strip title, strip absolute) | (url, title) <- links
                            , length title > 2
                            , length title < 100
                            , let (Just absolute) = (expandURIString url base)
                            , not $ isPrefixOf "javascript" absolute
                            ]

strip :: String -> String
strip str = T.unpack (T.strip (T.pack str))

-- | Load links from file
loadFrom :: FilePath -> IO [String]
loadFrom f = do 
  str <- catch (readFile f) (\_ -> return $ show ([] :: [String]))
  return (read str :: [String])

-- | Save dataset to file
saveTo :: Show a => FilePath -> a -> IO ()
saveTo f d = do
  writeFile f (show d) 
