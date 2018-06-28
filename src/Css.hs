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
  let ipcCommandKernelBufferPopulationBegin = 0.0

  let numEntries = 8 :: Int
  let cmdHighlightEntryAnimationName = "HighlightCmdBlockEntry"
  let cmdHighlightEntryAnimationNameKernel = "HighlightKernelCmdBlockEntry"
  let makeIpcBufferElements namePrefix keyframeName initialColor idx =
        do
          let name = namePrefix <> T.showt idx
          let anims = animations [(fromString keyframeName :: AnimationName, sec 4, easeOut,
                                  sec $ ipcCommandKernelBufferPopulationBegin + fromIntegral idx, iterationCount 1, normal, forwards)]
          element name ? do
            color initialColor
            anims
          keyframes (T.pack keyframeName) [(0.0, color initialColor), (25.0, color blue), (50.0, color $ rgb 0xff 0xff 0xff), (100.0, color $ rgb 0xff 0xff 0xff)]
  let genCmdBufEntryClass idx =
        do
          makeIpcBufferElements ".original-ipc-cmd-block-entry" cmdHighlightEntryAnimationName (rgb 0xff 0xff 0xff) idx
          makeIpcBufferElements ".kernel-ipc-cmd-block-entry" cmdHighlightEntryAnimationNameKernel (rgb 0x22 0x22 0x22) idx

  forM_ [0..numEntries] genCmdBufEntryClass


  -- Animation that moves the IPC command block to the right
  let ipcCommandBufferMoveToServiceBegin = ipcCommandKernelBufferPopulationBegin + fromIntegral (numEntries + 1)
  let ipcCommandBufferMoveToServiceDur = 1
  let mystuffAnimationName1 = "cssAnimation"
  let mystuffAnimationName2 = "cssAnimation2"

  -- Animation of filling in the kernel IPC command block elements one-by-one
  ".copied-ipc-cmd-block" ? do
    animations [(fromString mystuffAnimationName1 :: AnimationName, sec (fromIntegral (numEntries + 1) * ipcCommandBufferMoveToServiceDur), easeInOut, sec ipcCommandBufferMoveToServiceBegin, iterationCount 1, normal, forwards)]
    overflow hidden
  let ipcFontSize = em 1
  let ipcRowHeight = 1.4 *@ ipcFontSize
  fontSize ipcFontSize
  lineHeight ipcRowHeight
  let distance = em 16
  keyframes (T.pack mystuffAnimationName1) [(0.00, transform $ translateX $ em 0),
                                            (20.0, transform $ translateX distance),
                                            (80.0, transform $ translateX distance),
                                            (100.0, transform $ translateX distance)
                                            --(100.0, transform $ translateX $ em 0)
                                            ]

  let spinGearsAnimationBegin = ipcCommandBufferMoveToServiceBegin + ipcCommandBufferMoveToServiceDur + 1.0
  let spinGearsAnimationDur = 6
  ".spin-gears" ? do
    textDecoration none
    --fontSize $ px 36
    animation (fromString spinKeyFrameName :: AnimationName) (sec spinGearsAnimationDur) linear (sec spinGearsAnimationBegin) (iterationCount 1) normal none
    display inlineBlock
    opacity 0.0 -- Will be blended in once the animation starts
  ".spin-gears" # before ? content (stringContent "⚙") -- Gear character
  keyframes (T.pack spinKeyFrameName)  [(0, opacity 0.0),
                                        (16.6, do
                                          transform $ rotate $ deg 0
                                          opacity 1.0),
                                        (83.3, do
                                          transform $ rotate $ deg 300
                                          opacity 1.0),
                                        (100.0, do
                                          transform $ rotate $ deg 360
                                          opacity 0.0)]

  ".fadeoutArrow" ? animation (fromString "fadeoutArrow" :: AnimationName) (sec 1) linear (sec 7) (iterationCount 1) normal forwards
  ".fadeoutArrow2" ? animation (fromString "fadeoutArrow" :: AnimationName) (sec 2) linear (sec 8.5) (iterationCount 1) normal forwards
  keyframes (T.pack "fadeoutArrow")  [(0, opacity 1.0),
                                        (83.3, opacity 1.0),
                                        (100.0, opacity 0.0)]

  ".animated-slide" ? opacity 1.0

  -- To be used with animated slides:
  -- Make this element cover everything on this slide after N seconds
  -- That way we will not spoil any of the content that may already have started playing 5 slides ago to the audience
  ".link-for-reload" ? do
    top $ px (-10)
    left $ px 0
    width $ pct 1000
    height $ px 1000
    position absolute
    display block
    background $ rgb 0x22 0x22 0x22
    zIndex 5000
    opacity 0.0
    animation (fromString "hideContent" :: AnimationName) (sec 1) linear (sec 20) (iterationCount 1) normal forwards
    keyframesFromTo "hideContent" (opacity 0.0) (opacity 1.0)

genCss :: Css
genCss = css
