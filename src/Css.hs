{-# LANGUAGE OverloadedStrings #-}
module Css (genCss) where

import Clay
--import qualified Clay.Selector (selectorFromText)
import Data.Monoid ((<>))
import Control.Monad (forM_)
import qualified Data.Text as T (pack)
import qualified TextShow as T (showt)
import Data.String (fromString)

spinKeyFrameName = "spin-gears"

css = do
  ".spin-gears" ? do
    textDecoration none
    --fontSize $ px 36
    animation (fromString spinKeyFrameName :: AnimationName) (sec 2) linear 0 infinite normal none
    display inlineBlock
  ".spin-gears" # before ? content (stringContent "⚙") -- Gear character
  keyframesFromTo (T.pack spinKeyFrameName) (transform $ rotate $ deg 0) (transform $ rotate $ deg 360)

  -- Animation that moves the IPC command block to the right
  let mystuffAnimationName1 = "cssAnimation"
  let mystuffAnimationName2 = "cssAnimation2"

  -- Animation of filling in the kernel IPC command block elements one-by-one
  let ipcCopyAnimationName = "copyIpcCmdBlockAnim"
  let numEntries = 4
  ".copied-ipc-cmd-block" ? do
    animations [(fromString ipcCopyAnimationName :: AnimationName, sec $ fromIntegral numEntries, stepsStart numEntries, 0, iterationCount 1, normal, forwards),
                (fromString mystuffAnimationName1 :: AnimationName, sec 1, easeInOut, sec $ fromIntegral (numEntries + 1), iterationCount 1, normal, forwards)]
    overflow hidden
  let ipcFontSize = em 1
  let ipcRowHeight = 1.4 *@ ipcFontSize
  fontSize ipcFontSize
  lineHeight ipcRowHeight
  keyframes (T.pack ipcCopyAnimationName) [
                                           --(0.0, height $ em 0),
                                           --(99.9, height $ fromIntegral numEntries *@ ipcRowHeight),
                                           --(100.0, height $ pct 100)
                                           ]
  keyframesFromTo (T.pack mystuffAnimationName1) (transform $ translateX $ em 0) (transform $ translateX $ em 13)
  keyframesFromTo (T.pack mystuffAnimationName2) (transform $ translateX $ em 13) (transform $ translateX $ em 26)

  let cmdHighlightEntryAnimationName = "HighlightCmdBlockEntry"
  let genCmdBufEntryClass idx =
        do
          let name = ".original-ipc-cmd-block-entry" <> T.showt idx
          element name ?
            animations [(fromString cmdHighlightEntryAnimationName :: AnimationName, sec 0.5, easeOut, sec $ fromIntegral idx, iterationCount 2, alternate, forwards)]
  forM_ [0..numEntries] genCmdBufEntryClass
  keyframes (T.pack cmdHighlightEntryAnimationName) [(0.0, color inherit), (100.0, color blue)] --(50.0, color blue), (100.0, color inherit)]

genCss :: Css
genCss = css
