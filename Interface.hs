{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Interface
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module Interface (
  Style(..),
  headerOf,
  interimOf,
  footerOf,
  printNodes,
  posTagger,
  printNumeration,
  treebankBuilder
  ) where

import qualified Data.Char as C           --base
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified System.IO as S           --base
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import qualified Parser.Japanese.Lexicon as LEX
import qualified Interface.Text as T
import qualified Interface.TeX as TEX
import qualified Interface.HTML as HTML
import qualified Interface.XML as X
import qualified DTS.UDTT as DTS
import qualified DTS.UDTTwithName as VN
import qualified DTS.Prover as Prover
import qualified DTS.Prover.Judgement as Ty

{- Some functions for pretty printing Chart/Nodes -}

-- | values of lightblue -s option
data Style = HTML | TEXT | XML | TEX deriving (Eq,Show)

instance Read Style where
  readsPrec _ r = 
    [(HTML,s) | (x,s) <- lex r, map C.toLower x == "html"]
    ++ [(TEXT,s) | (x,s) <- lex r, map C.toLower x == "text"]
    ++ [(TEX,s) | (x,s) <- lex r, map C.toLower x == "tex"]
    ++ [(XML,s) | (x,s) <- lex r, map C.toLower x == "xml"]

-- | header in style
headerOf :: Style -> String
headerOf style = case style of
  HTML -> HTML.htmlHeader4MathML
  TEXT -> replicate 100 '-'
  XML  -> "<?xml version='1.0' encoding='UTF-8'?><root><document id='d0'><sentences>"
  TEX  -> ""

-- | interim in style
interimOf :: Style -> String
interimOf style = case style of
  HTML -> "<hr size='15' />"
  TEXT -> replicate 100 '-'
  XML  -> ""
  TEX  -> ""

-- | footer in style
footerOf :: Style -> String
footerOf style = case style of
  HTML -> HTML.htmlFooter4MathML
  TEXT -> ""
  XML  -> "</sentences></document></root>"
  TEX  -> ""

-- | prints a CCG node (=i-th parsing result for a sid-th sentence) in a specified style (=HTML|text|XML|TeX)
printNodes :: S.Handle -> Style -> Int -> T.Text -> Bool -> [CCG.Node] -> IO()
printNodes handle HTML sid sentence typecheck nodes = do
  mapM_ (T.hPutStr handle) ["<p>[s",T.pack $ show sid,"] ",sentence,"</p>"]
  mapM_ (\(node,ith) -> 
          do
          mapM_ (T.hPutStr handle) [
            "<p>[parse ",
            T.pack $ show ith,
            ": score=",CCG.showScore node,
            "]</p>",
            HTML.startMathML,
            HTML.toMathML node,
            HTML.endMathML
            ]
          if typecheck 
             then do
                  T.hPutStrLn handle $ HTML.startMathML;
                  let trees = map Ty.utreeToMathML $ Prover.checkFelicity (CCG.sig node) [] (CCG.sem node);
                      -- T.hPutStrLn handle $ DTS.toVerticalMathML $ do
                      --   t1 <- Ty.checkFelicity (CCG.sig node) [] (CCG.sem node);
                      --   t2 <- Ty.aspElim t1
                      --   t3 <- Ty.getTerm t2
                      --   return $ DTS.betaReduce $ Ty.repositP t3
                  if null trees 
                    then return ()
                    else T.hPutStrLn handle $ head trees
                  T.hPutStrLn handle $ HTML.endMathML
             else return ()
        ) $ zip nodes ([0..]::[Int])

printNodes handle TEXT sid sentence _ nodes = do
  mapM_ (T.hPutStr handle) [
            "[s",
            T.pack $ show sid,
            "] ",
            sentence,
            "\n"
            ]
  mapM_ (\(node,ith) -> do
          mapM_ (T.hPutStr handle) [
            "[parse ",
            T.pack $ show ith,
            "]\n",
            T.toText node,
            T.pack $ interimOf TEXT,
            "\n"
            ]
        ) $ zip nodes ([0..]::[Int])

printNodes handle XML sid sentence _ nodes = do
  mapM_ (T.hPutStr handle) ["<sentence id='s", T.pack $ show sid, "'>", sentence]
  mapM_ (\(node,ith) ->
    T.hPutStr handle $ X.node2XML sid ith False sentence node
    ) $ zip nodes ([0..]::[Int])
  T.hPutStrLn handle "</sentence>"

printNodes handle TEX sid sentence _ nodes =
  mapM_ (\(node,ith) -> 
          do
          mapM_ (T.hPutStr handle) [
            "\\noindent\\kern-2em\\scalebox{",
            TEX.scaleboxsize sentence,
            "}{\\ensuremath{", 
            -- TEX.toTeX $ CCG.sem node,
            "[s",
            T.pack $ show sid,
            "-",
            T.pack $ show ith,
            "] ",
            sentence,
            "\\\\",
            TEX.toTeX node,
            "}}\\medskip"
            ]
        ) $ zip nodes ([0..]::[Int])

-- | prints CCG nodes (=a parsing result) in a \"part-of-speech tagger\" style
posTagger :: S.Handle -> Style -> [CCG.Node] -> IO()
posTagger handle XML = mapM_ ((T.hPutStrLn handle) . (X.node2XML 0 0 True T.empty))
posTagger handle style = mapM_ (\node -> mapM_ (T.hPutStrLn handle) $ node2PosTags style node)

-- | A subroutine for `posTagger` function
node2PosTags :: Style -> CCG.Node -> [T.Text]
node2PosTags style node =
  case CCG.daughters node of
    [] -> [printLexicalItem style node]
    dtrs -> [t | dtr <- dtrs, t <- node2PosTags style dtr]

printLexicalItem :: Style -> CCG.Node -> T.Text
printLexicalItem style node = case style of
  TEXT -> T.concat [CCG.pf node, "\t", T.toText (CCG.cat node), " \t", T.toText (CCG.sem node), "\t", CCG.source node, "\t[", CCG.showScore node, "]"]
  TEX  -> TEX.toTeX node
  HTML -> T.concat $ [HTML.startMathML, HTML.toMathML node, HTML.endMathML]
  XML  -> X.node2XML 0 0 True (CCG.pf node) node

-- | prints the numeration
printNumeration :: S.Handle -> Style -> T.Text -> IO()
printNumeration handle style sentence = do
  numeration <- LEX.setupLexicon sentence
  mapM_ ((T.hPutStrLn handle) . (printLexicalItem style)) numeration

-- | parses sentences in the given corpus and yields a list of SRs in HTML format.
treebankBuilder :: Int -> [T.Text] -> IO()
treebankBuilder beam sentences = do
  S.putStrLn HTML.htmlHeader4MathML
  T.putStrLn HTML.startMathML
  nodes <- mapM (CP.simpleParse beam) sentences
  (\l -> do
         VN.printVerticalMathML l
         VN.srlist2drelTSV l
         ) $ DTS.fromDeBruijnSRlist $ map (CP.sem . head) nodes
  T.putStrLn HTML.endMathML
  S.putStrLn HTML.htmlFooter4MathML

{-
-- | prints every box in the (parsed) CYK chart as a TeX source code.
printChartInTeX :: S.Handle -> Bool -> CP.Chart -> IO()
printChartInTeX handle typeCheck chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          printNodesInTeX handle typeCheck nodes

-- | prints n-nodes (n is a natural number) from given list of CCG nodes (=a parsing result) as a TeX source code.
printNNodesInTeX :: S.Handle -> Bool -> [CCG.Node] -> IO()
printNNodesInTeX handle _ nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.2}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip") nodes
-}

