{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-
example :: IO ()
example = do
  args <- getArgs
  let uris = catMaybes (map (parseURI) args)
  links <- mapM (getLinks) uris
  mapM_ (print) (concat links)
-}


module Spider (getLinks, loadFrom, saveTo) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char
import           Data.List
import           Data.String
import qualified Data.Text                  as T
import           Data.Tree.NTree.TypeDefs   (NTree)
import           Network.HTTP.Conduit       as Conduit
import           Network.HTTP.Types.Header
import           Network.URI
import           Text.XML.HXT.Core

selectAnchors :: ArrowXml cat
              => cat (NTree XNode) (String, String)
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
          => String -> a (NTree XNode) XmlTree
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
userAgent :: Data.String.IsString a => a
userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667"

setConnectionClose :: Conduit.Request -> Conduit.Request
setConnectionClose req = req{requestHeaders = ("Connection", "close") : requestHeaders req}

simpleHttpWithAgent :: MonadIO m => String -> m L.ByteString
simpleHttpWithAgent url = liftIO $ withManager $ \man -> do
  req <- liftIO $ parseUrl url
  let custom_header = ("User-Agent", userAgent) :: Network.HTTP.Types.Header.Header
  let request = req { requestHeaders = [custom_header] }
  responseBody <$> httpLbs (setConnectionClose request) man

get :: URI -> IO String
get uri = do
  eresp <- try (simpleHttpWithAgent $ show uri)
  case eresp of
    Left (er :: IOException) -> error $ show er
    Right res -> return $ L.unpack res



getLinks :: URI -> IO [(String, String)]
getLinks uri = do
  body  <- get uri
  links <- runX (parseHTML body >>> selectAnchors)
  if null links then
      (do rsslinks <- runX (parseRSS body >>> selectRSSLinks)
          return $ filterBadLinks (show uri) rsslinks)
       else return $ filterBadLinks (show uri) links


filterBadLinks :: String -> [(String, String)] -> [(String, String)]
filterBadLinks base links = [(strip title, strip absolute) | (url, title) <- links
                            , length title > 2
                            , length title < 100
                            , let (Just absolute) = expandURIString url base
                            , not $ isPrefixOf "javascript" absolute
                            ]

strip :: String -> String
strip str = T.unpack (T.strip (T.pack str))

-- | Load links from file
loadFrom :: FilePath -> IO [String]
loadFrom f = do
  str <- catch (readFile f) (\(_ :: IOException) -> return $ show ([] :: [String]))
  return (read str :: [String])

-- | Save dataset to file
saveTo :: Show a => FilePath -> a -> IO ()
saveTo f d = writeFile f (show d)
