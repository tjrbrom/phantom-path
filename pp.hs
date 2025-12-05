{-# LANGUAGE OverloadedStrings #-}

-- | PhantomPath
-- A minimal WAI/Warp server that generates deterministic synthetic HTML pages.
-- 
-- Each request produces a page with:
--   * a generated title
--   * a generated body
--   * a small set of recursive links
--
-- All output depends only on the request path and user-agent.
-- Known search-engine bots receive a simple benign page.
-- No storage or external state is used.

module Main (main) where

import Control.Concurrent       (threadDelay)
import Data.Bits                (xor)
import Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Lazy     as L
import Data.Char                (toLower, toUpper)
import Network.HTTP.Types       (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Random
import Text.Printf              (printf)

-- | Configuration
port, minDelayMs, maxDelayMs, wordsPerPage, linksPerPage :: Int
port         = 8080
minDelayMs   = 200
maxDelayMs   = 1200
wordsPerPage = 120
linksPerPage = 5

whitelist :: [ByteString]
whitelist =
  [ "googlebot", "bingbot", "slurp", "duckduckbot", "baiduspider", "yandex" ]

lexicon :: [ByteString]
lexicon =
  [ "alpha","beta","gamma","delta","epsilon","theta","lambda","omega"
  , "node","leaf","matrix","vector","thread","loop","signal","cipher"
  , "poly","meta","datum","cache","flux","pulse","core","edge","span"
  , "trace","echo","void","forge","garden","lab","random","seed","orbit"
  , "phase","byte","glyph","quark","axiom","mote","kernel"
  ]

-------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = do
  putStrLn $ "PhantomPath listening on http://0.0.0.0:" <> show port
  run port application

-------------------------------------------------------------------------------
-- | WAI application
application :: Application
application req send = case rawPathInfo req of
    "/robots.txt" -> send robotsTxt
    path ->
      if isWhitelisted (userAgent req)
        then send (benignResponse path)
        else do
          applyDelay path
          send (syntheticResponse path (userAgent req))

userAgent :: Request -> ByteString
userAgent req = maybe "" (C.map toLower) $ lookup "User-Agent" (requestHeaders req)

isWhitelisted :: ByteString -> Bool
isWhitelisted ua = any (`C.isInfixOf` ua) whitelist

-------------------------------------------------------------------------------
-- | Responses
robotsTxt :: Response
robotsTxt = responseLBS status200 [("Content-Type","text/plain")] "User-agent: *\nDisallow: /\n"

benignResponse :: ByteString -> Response
benignResponse path = responseLBS status200
  [("Content-Type","text/html; charset=utf-8"), ("Cache-Control","public, max-age=86400")]
  (L.fromStrict $ mconcat
    [ "<!doctype html><html><body>"
    , "<h1>Index</h1>"
    , "<p>Path: ", path, "</p>"
    , "<p>PhantomPath benign mode</p>"
    , "</body></html>"
    ])

syntheticResponse :: ByteString -> ByteString -> Response
syntheticResponse path ua = responseLBS status200
  [("Content-Type","text/html; charset=utf-8"), ("Cache-Control","no-store, no-cache, must-revalidate")]
  (L.fromStrict $ buildPage path ua)

-------------------------------------------------------------------------------
-- | Request delay
applyDelay :: ByteString -> IO ()
applyDelay path = do
  let g = mkStdGen $ hashBytes path
      (d, _) = randomR (minDelayMs, maxDelayMs) g
  threadDelay (d * 1000)

hashBytes :: ByteString -> Int
hashBytes = B.foldl' (\h c -> h * 131 + fromEnum c) 7

-------------------------------------------------------------------------------
-- | Page generation
buildPage :: ByteString -> ByteString -> ByteString
buildPage path ua =
  let g0 = mkStdGen $ hashBytes path `xor` hashBytes ua
      (gTitle, gRest) = split g0
      (gBody, gLinks) = split gRest
  in mconcat
    [ "<!doctype html><html><head><meta charset='utf-8'>"
    , "<title>", titleFor path gTitle, "</title>"
    , "</head><body>"
    , "<h1>", titleFor path gTitle, "</h1>"
    , "<p>", bodyFor path gBody, "</p>"
    , "<hr/><ul>", linksFor path gLinks, "</ul>"
    , "</body></html>"
    ]

titleFor :: ByteString -> StdGen -> ByteString
titleFor path g =
  let (i1, g') = randomR (0, length lexicon-1) g
      (i2, _)  = randomR (0, length lexicon-1) g'
      w1 = lexicon !! i1
      w2 = capitalize (lexicon !! i2)
  in path <> " — " <> w1 <> w2

bodyFor :: ByteString -> StdGen -> ByteString
bodyFor path g =
  let (wordsList, _) = drawWords wordsPerPage g
  in C.unwords (wordsList <> ["—", "path:", path])

drawWords :: Int -> StdGen -> ([ByteString], StdGen)
drawWords n g0 = go n g0 []
  where
    go 0 g acc = (reverse acc, g)
    go k g acc =
      let (i, g') = randomR (0, length lexicon-1) g
      in go (k-1) g' (lexicon !! i : acc)

linksFor :: ByteString -> StdGen -> ByteString
linksFor path g = mconcat $ map mk $ take linksPerPage (tokenStream g)
  where
    mk t =
      let href = if C.null path || C.last path == '/' then path <> t else path <> "/" <> t
      in "<li><a href='" <> href <> "'>" <> t <> "</a></li>"

tokenStream :: StdGen -> [ByteString]
tokenStream g = map (C.pack . take 6 . printf "%x" . abs) (randoms g :: [Int])

capitalize :: ByteString -> ByteString
capitalize bs = case C.uncons bs of
  Nothing -> bs
  Just (c, r) -> C.cons (toUpper c) r
