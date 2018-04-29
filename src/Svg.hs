{-# LANGUAGE OverloadedStrings #-}
module Svg where

import Data.Monoid ((<>))
import qualified Data.Text as T (Text)
import qualified TextShow as T (showt)
import qualified Data.Text.Lazy as TL (toStrict)
import Lucid.Svg

svg contents = do
  doctype_
  with (svg11_ contents) [width_ "100", height_ "70"]

rect_' :: Term arg result => arg -> result
rect_' = term "rect"

animateMotion_' :: Term arg result => arg -> result
animateMotion_' = term "animateMotion"

animatedArrow initialDelay = do
  path_ [id_ "mypath", class_ "path", fill_ "#FFFFFF00", stroke_ "#ffffff", stroke_width_ "5", stroke_miterlimit_ "10",
         d_ "M0,20  a100,100 0 0 1 100,0", -- endpoint radius axis-rotation flag1 flag2 endpoint
         stroke_dasharray_ "0,5 10,10 10,10 10,10 10,10 10,10 10,10 10,200",
         stroke_dashoffset_ "140"]
  let initialAnimBegin = T.showt initialDelay <> "s"
  let initialAnimDur = "0.95s"
  let secondaryAnimDur = "0.195s" -- initial duration / 5
  animate_ [xlinkHref_ "#mypath", attributeName_ "stroke-dashoffset", from_ "140", to_ "40", begin_ initialAnimBegin, dur_ initialAnimDur,
            id_ "initialAnimation", fill_ "freeze"]
  animate_ [xlinkHref_ "#mypath", attributeName_ "stroke-dashoffset", additive_ "sum", from_ "0", to_ "-20", begin_ "initialAnimation.end", dur_ secondaryAnimDur,
           fill_ "freeze",
            repeatCount_ "indefinite"]
  g_ $ do
    rect_ [width_ "10", height_ "5", fill_ "#FFFFFF", transform_ "rotate(125)"]
    rect_ [width_ "5", height_ "10", fill_ "#FFFFFF", transform_ "rotate(145)"]
    animateMotion_' [begin_ initialAnimBegin, dur_ "1s", repeatCount_ "indefinite", rotate_ "auto"] (mpath_ [xlinkHref_ "#mypath"])

genSvg :: Double -> T.Text
genSvg initialDelay = TL.toStrict $ prettyText $ svg $ animatedArrow initialDelay
