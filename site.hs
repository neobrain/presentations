--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import qualified Text.Regex.Posix as Regex

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
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

    match "lib/css/*" $ do
        route   idRoute
        compile getResourceBody

    match "lib/font/league-gothic/*" $ do
        route   idRoute
        compile getResourceLBS

    match "plugin/highlight/highlight.js" $ do
        route   idRoute
        compile getResourceBody

    match "slides/*.html_" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/slide.html"    postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            -- TODO: Sort by name or something.
            slides <- loadAll "slides/*.html"
            let indexCtx =
                    listField "slides" postCtx (return slides) `mappend`
                    constField "title" "Home"                `mappend`
                    -- $code("filename", "step")$
                    functionField "code" (\(file:step:[]) _ -> getCompiledCodeSnippet file step) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    -- Code snippets get a suffix that allows us to progressively enable individual parts of the snippet instead of showing everything at once
    -- (We need to do this ourselves because apparently that's something highlight.js cannot do currently :( )
    match "code/*" $ do
        route idRoute
        compile getResourceBody

getCompiledCodeSnippet file_path step =
    fmap itemBody $ loadBody (fromFilePath file_path) >>= makeItem >>= applyAsTemplate codeCtx >>= relativizeUrls
    -- Provide various utility macros:
    -- * ifStepEqual only includes the second argument if its first argument is equal to the step provided in the parent context
    where codeCtx = functionField "ifStepEqual" (\(number:content:tail) _ -> if number == step then return content else return "") `mappend`
                    defaultContext


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
