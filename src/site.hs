--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import qualified Clay (render)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as TL (unpack)

import qualified Css
import qualified Svg

main :: IO ()
main = hakyll $ do
    create ["css/generated.css"] $ do
        route idRoute
        compile $ makeItem $ TL.unpack $ Clay.render Css.genCss

    create ["images/stuff0.svg"] $ do
        route idRoute
        compile $ makeItem $ T.unpack $ Svg.genSvg 0.0

    create ["images/stuff1.svg"] $ do
        route idRoute
        compile $ makeItem $ T.unpack $ Svg.genSvg 4.5 -- 4.5 = number of entries + wait half a second

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "index.html" $ do
        route   idRoute
        compile $ getResourceBody >>= loadAndApplyTemplate "templates/default.html" defaultContext >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
