--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (forM, when)
import           Hakyll

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

    match "plugin/*/*.js" $ do
        route   idRoute
        compile getResourceBody

    match "slides/*.html" $ do
        route $ idRoute -- setExtension "html"

        let indexCtx =
                    -- Insert code snippet and progress through the given transition points (code snippet template will be evaluated for each step individually)
                    functionField "code" (\(file:steps) _ -> if null steps then (getCompiledCodeSnippet file "0") else do
                        let group_open_str = "<span style=\"position:relative\">"
                        let group_close_str = "</span>"
                        result <- fmap concat $ forM (zip steps (drop 1 steps ++ [""])) $ \(step, next_step) -> do
                            let open_str = if (step == "") then "" else ("<span class=\"fragment fade-in\" style=\"position:absolute; width:1000px; transform:translate(-50%, 0%)\" data-fragment-index=" ++ step ++ ">") ++
                                           -- If this is not the last step, auto-hide it on the next step
                                           if (next_step == "") then mempty else ("<span class=\"fragment fade-out\" data-fragment-index=" ++ next_step ++ ">")
                            snippet <- getCompiledCodeSnippet file step
                            let close_str = (if (step == "") then "" else "</span>") ++ if (next_step == "") then "" else "</span>"
                            return $ open_str ++ snippet ++ close_str
                        return $ group_open_str ++ result ++ group_close_str
                        ) `mappend`
                    functionField "make_fragment" (\[index] _ -> return $"class=\"fragment\" data-fragment-index=" ++ index) `mappend`
                    defaultContext

        compile $ getResourceBody -- >>= pandocCompiler
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/slide.html"    indexCtx
            -- >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            -- TODO: Sort by name or something.
            slides <- loadAll "slides/*.html"
            let indexCtx =
                    listField "slides" postCtx (return slides) `mappend`
                    -- Insert code snippet and progress through the given transition points (code snippet template will be evaluated for each step individually)
                    functionField "code" (\(file:steps) _ -> do
                        let group_open_str = "<span style=\"position:relative\">"
                        let group_close_str = "</span>"
                        result <- fmap concat $ forM (zip steps (drop 1 steps ++ [""])) $ \(step, next_step) -> do
                            let open_str = if (step == "") then "" else ("<span class=\"fragment fade-in\" style=\"position:absolute; width:1000px; transform:translate(-50%, 0%)\" data-fragment-index=" ++ step ++ ">") ++
                                           -- If this is not the last step, auto-hide it on the next step
                                           if (next_step == "") then mempty else ("<span class=\"fragment fade-out\" data-fragment-index=" ++ next_step ++ ">")
                            snippet <- getCompiledCodeSnippet file step
                            let close_str = (if (step == "") then "" else "</span>") ++ if (next_step == "") then "" else "</span>"
                            return $ open_str ++ snippet ++ close_str
                        return $ group_open_str ++ result ++ group_close_str
                        ) `mappend`
                    functionField "make_fragment" (\[index] _ -> return $"class=\"fragment\" data-fragment-index=" ++ index) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    -- Code snippet templates (with their own set of macros)
    match "code/*.html" $ do
        route idRoute
        compile getResourceBody

getCompiledCodeSnippet :: String -> String -> Compiler String
getCompiledCodeSnippet file_path step =
    fmap itemBody $ loadBody (fromFilePath file_path) >>= makeItem >>= applyAsTemplate codeCtx >>= relativizeUrls
    -- Provide various utility macros:
    -- * stepEquals_N will be set if the current step is N
    -- * template("string"): prints the given string enclosed in "<>" but properly HTML-escaped
    where codeCtx = constField ("stepEquals_" ++ step) "1" `mappend`
                    (mconcat $ map (flip constField "1" . ("stepGreaterEquals_" ++) . show) [0..(if step == "" then -1 else (read step :: Int))]) `mappend`
                    (mconcat $ map (flip constField "1" . ("stepLessEquals_" ++) . show) [(if step == "" then 21 else (read step :: Int))..20]) `mappend` -- Hardcoded to not go higher than 20 for now
                    functionField "template" (\[str] _ -> return $ "&lt;" ++ str ++ "&gt;") `mappend`
                    functionField "after" (\[step] _ -> return $ fadein_str step) `mappend`
                    functionField "endafter" (\_ _ -> return "</span>") `mappend`
                    functionField "only" (\(step:maybe_until_step) _ -> do
                        let (until_step:[]) = maybe_until_step -- TODO: Support implicit "maybe_until_step = step + 1"!
                        return $ fadein_str step ++ fadeout_str until_step) `mappend`
                    functionField "endonly" (\_ _ -> return "</span></span>") `mappend`
                    defaultContext
          fadein_str step = "<span class=\"fragment fade-in\" data-fragment-index=" ++ step ++ "\">"
          fadeout_str step = "<span class=\"fragment fade-out\" data-fragment-index=" ++ step ++ "\">"
          fadein_abspos_str step = "<span class=\"fragment fade-in\" style=\"position:absolute\" data-fragment-index=" ++ step ++ ">"
          fadeout_abspos_str step = "<span class=\"fragment fade-out\" style=\"position:absolute\" data-fragment-index=" ++ step ++ ">"


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
