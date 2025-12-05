{-# LANGUAGE OverloadedStrings #-}

-- PhantomPath
-- ------------
-- A compact experimental WAI server that synthesizes deterministic
-- HTML pages and deep link structures based on request paths.
--
-- The intent is neutral: generate recursive synthetic routes for
-- experimental crawler behaviour analysis, latency modelling or
-- synthetic traffic tests.
--
-- The code stays intentionally simple and explicit:
-- - minimal dependency footprint
-- - pure deterministic generation
-- - no partials, no hidden globals
-- - predictable PRNG usage

module Main (main) where

import           Control.Concurrent       (threadDelay)
import           Data.Bits                (xor)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Lazy     as L
import           Data.Char                (toLower, toUpper)
import           Network.HTTP.Types       (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           System.Random
import           Text.Printf              (printf)

-------------------------------------------------------------------------------
-- Configuration

port :: Int
port = 8080

minDelayMs, maxDelayMs :: Int
minDelayMs = 200
maxDelayMs = 1200

wordsPerPage :: Int
wordsPerPage = 120

linksPerPage :: Int
linksPerPage = 5

whitelist :: [ByteString]
whitelist =
    [ "googlebot"
    , "bingbot"
    , "slurp"
    , "duckduckbot"
    , "baiduspider"
    , "yandex"
    ]

lexicon :: [ByteString]
lexicon =
    [ "alpha","beta","gamma","delta","epsilon","theta","lambda","omega"
    , "node","leaf","matrix","vector","thread","loop","signal","cipher"
    , "poly","meta","datum","cache","flux","pulse","core","edge","span"
    , "trace","echo","void","forge","garden","lab","random","seed","orbit"
    , "phase","byte","glyph","quark","axiom","mote","kernel"
    ]

-------------------------------------------------------------------------------
-- Entry

main :: IO ()
main = do
    putStrLn $ "PhantomPath listening on http://0.0.0.0:" <> show port
    run port application

-------------------------------------------------------------------------------
-- Application

application :: Application
application req send = case rawPathInfo req of
    "/robots.txt" ->
        send robotsTxt
    path ->
        if isWhitelisted (ua req)
            then send (benignResponse path)
            else do
                applyDelay path
                send (syntheticResponse path (ua req))

ua :: Request -> ByteString
ua req = maybe "" (C.map toLower) (lookup "User-Agent" (requestHeaders req))

isWhitelisted :: ByteString -> Bool
isWhitelisted u =
    any (`C.isInfixOf` u) whitelist

-------------------------------------------------------------------------------
-- Responses

robotsTxt :: Response
robotsTxt =
    responseLBS status200
        [("Content-Type", "text/plain")]
        "User-agent: *\nDisallow: /\n"

benignResponse :: ByteString -> Response
benignResponse path =
    responseLBS status200
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("Cache-Control", "public, max-age=86400")
        ]
        (L.fromStrict $
            "<!doctype html><html><body>"
         <> "<h1>Index</h1>"
         <> "<p>Path: " <> path <> "</p>"
         <> "<p>PhantomPath benign mode</p>"
         <> "</body></html>"
        )

syntheticResponse :: ByteString -> ByteString -> Response
syntheticResponse path agent =
    let html = buildPage path agent
    in responseLBS status200
        [ ("Content-Type","text/html; charset=utf-8")
        , ("Cache-Control","no-store, no-cache, must-revalidate")
        ]
        (L.fromStrict html)

-------------------------------------------------------------------------------
-- Delay

applyDelay :: ByteString -> IO ()
applyDelay p = do
    let seed = foldHash p
        g    = mkStdGen seed
        (d, _) = randomR (minDelayMs, maxDelayMs) g
    threadDelay (d * 1000)

foldHash :: ByteString -> Int
foldHash = B.foldl' (\h c -> h * 131 + fromEnum c) 7

-------------------------------------------------------------------------------
-- Page generation

buildPage :: ByteString -> ByteString -> ByteString
buildPage path agent =
    let seed = foldHash path `xor` foldHash agent
        g0   = mkStdGen seed
        (g1, g2) = split g0
        title = titleFor path g1
        body  = bodyFor path g1
        links = linksFor path g2
    in mconcat
        [ "<!doctype html><html><head>"
        , "<meta charset='utf-8'>"
        , "<title>", title, "</title>"
        , "</head><body>"
        , "<h1>", title, "</h1>"
        , "<p>", body, "</p>"
        , "<hr/><ul>", links, "</ul>"
        , "</body></html>"
        ]

titleFor :: ByteString -> StdGen -> ByteString
titleFor path g =
    let (i1, g') = randomR (0, length lexicon - 1) g
        (i2, _)  = randomR (0, length lexicon - 1) g'
        w1 = lexicon !! i1
        w2 = capitalize (lexicon !! i2)
    in path <> " — " <> w1 <> w2

bodyFor :: ByteString -> StdGen -> ByteString
bodyFor path g =
    let (ws, _) = drawWords wordsPerPage g
    in C.unwords (ws <> ["—", "path:", path])

drawWords :: Int -> StdGen -> ([ByteString], StdGen)
drawWords n g0 = go n g0 []
  where
    go 0 g acc = (reverse acc, g)
    go k g acc =
        let (i, g') = randomR (0, length lexicon - 1) g
        in go (k - 1) g' (lexicon !! i : acc)

linksFor :: ByteString -> StdGen -> ByteString
linksFor path g =
    let toks = take linksPerPage (tokenStream g)
    in mconcat (map mk toks)
  where
    mk t =
        let href = if C.null path || C.last path == '/'
                     then path <> t
                     else path <> "/" <> t
        in "<li><a href='" <> href <> "'>" <> t <> "</a></li>"

tokenStream :: StdGen -> [ByteString]
tokenStream g = map (C.pack . fmt) (randoms g :: [Int])
  where
    fmt x = take 6 (printf "%x" (abs x))

capitalize :: ByteString -> ByteString
capitalize bs =
    case C.uncons bs of
        Nothing     -> bs
        Just (c, r) -> C.cons (toUpper c) r
