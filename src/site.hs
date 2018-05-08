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

    match "css/theme/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile getResourceBody

    match "lib/js/*" $ do
        route   idRoute
        compile getResourceBody

    match "lib/font/league-gothic/*" $ do
        route   idRoute
        compile getResourceLBS

    match "lib/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "plugin/*/*.js" $ do
        route   idRoute
        compile getResourceBody

    match "index.html" $ do
        route   idRoute
        compile $ do
            slides <- loadAll "slides/*.html"
            let context = listField "slides" defaultContext (return slides) `mappend`
                          defaultContext
            getResourceBody >>= applyAsTemplate context >>= loadAndApplyTemplate "templates/default.html" context >>= relativizeUrls

    match "slides/*.html" $ do
        route   idRoute

        let context = functionField "make_fragment" (\[index] _ -> return $"class=\"fragment\" data-fragment-index=" ++ index) `mappend`
                      constField "color_codegen" "#f6ba4a" `mappend`
                      constField "color_reflection" "#49f749" `mappend`
                      constField "color_tmp" "#f7f749" `mappend`
                      constField "color_declint" "#008f8f" `mappend`
                      constField "cpptype_writeablebuffer" "WriteableBuffer" `mappend`
                      defaultContext

        compile $ getResourceBody
            >>= applyAsTemplate context
            >>= loadAndApplyTemplate "templates/slide.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
