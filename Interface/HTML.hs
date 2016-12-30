{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : MathML interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}
module Interface.HTML (
  MathML(..),
  htmlHeader4MathML,
  htmlFooter4MathML
  ) where

import qualified Data.Text.Lazy as T

-- | `Math` is a class of types whose terms can be translated into a MathML source (in Data.Text.Lazy). 
-- `toMathML` method translates a Typeset term into a TeX source (in Data.Text.Lazy).
class MathML a where
  toMathML :: a -> T.Text

htmlHeader4MathML :: T.Text
htmlHeader4MathML = "\
    \<?xml version='1.0'?>\
    \<?xml-stylesheet type='text/xsl' href='http://www.w3.org/Math/XSL/mathml.xsl'?>\
    \<html xmlns='http://www.w3.org/1999/xhtml'>\
    \<head>\
    \  <meta charset='UTF-8'>\
    \  <title>lightblue parse tree</title>\
    \  <style>body {\
    \    font-size: 1em;\
    \    }\
    \  </style>\
    \  <script type='text/javascript'\
    \    src='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'>\
    \  </script>\
    \  <script type='text/x-mathjax-config'>\
    \    MathJax.Hub.Config({\
    \      tex2jax: {\
    \        inlineMath: [['$','$'], ['\\(','\\)']],\
    \        processEscapes: true\
    \        },\
    \      CommonHTML: { matchFontHeight: false },\
    \      displayAlign: left,\
    \      displayIndent: 2em\
    \      });\
    \    MathJax.Hub.Config({\
    \      'HTML-CSS': {\
    \      availableFonts: [],\
    \      preferredFont: null,webFont: 'Neo-Euler'}});\
    \  </script>\
    \  </head><body>"

htmlFooter4MathML :: T.Text
htmlFooter4MathML = "</body></html>"