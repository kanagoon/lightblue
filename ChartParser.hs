{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : ChartParser
Description : A left-corner CKY-parser for lexical grammars
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : alpha
-}
module ChartParser (
  -- * Main parser
  parse,
  -- * Printing functions
  printChart,
  printChartInSimpleText,
  printChartInTeX,
  printNodes,
  posTagger,
  -- * Utilities to filter the parsing results
  topBox,
  bestOnly
  ) where

import Data.List
import Data.Char                       --base
import Data.Fixed                      --base
import qualified System.IO as S        --base
import qualified Data.Text.Lazy as T   --text
import qualified Data.Map as M         --container
import qualified CombinatoryCategorialGrammar as CCG --(Node, unaryRules, binaryRules, trinaryRules, isCONJ, cat, SimpleText)
import qualified JapaneseLexicon as L (Lexicon, lookupLexicon, setupLexicon, emptyCategories)
import qualified TeXmodule as TEX

-- | The type 'Chart' is a type for CYK-charts. 
type Chart = M.Map (Int,Int) [CCG.Node]

{- Come functions for pretty printing Chart/Nodes -}

-- | 'printChart' prints every box in the (parsed) CYK chart as a TeX source code.
printChart :: S.Handle -> Chart -> IO()
printChart handle chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-4em\\scalebox{.3}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip") nodes

-- | `printNodes` prints n-nodes from given list of CCG nodes (=a parsing result) as a TeX source code.
printNodes :: S.Handle -> Int -> [CCG.Node] -> IO()
printNodes handle n nodes = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-2em\\scalebox{.3}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip") $ take n $ nodes

-- | `printChartInTeX` prints CCG nodes (=a parsing result) as a TeX source code.
printChartInTeX :: S.Handle -> [CCG.Node] -> IO()
printChartInTeX handle = mapM_ (\node -> S.hPutStrLn handle $ "\\noindent\\kern-4em\\scalebox{.3}{" ++ T.unpack (TEX.toTeX node) ++ "\\\\}\\par\\medskip")

-- | `printChartInSimpleText` prints CCG nodes (=a parsing result) as a plain text.
printChartInSimpleText :: S.Handle -> [CCG.Node] -> IO()
printChartInSimpleText handle = mapM_ (\node -> S.hPutStr handle $ (T.unpack $ CCG.toText node) ++ "\n") 

-- | `posTagger` prints CCG nodes (=a parsing result) in a \"part-of-speech tagger\" style
posTagger :: S.Handle -> [CCG.Node] -> IO()
posTagger handle nodes = 
  case nodes of
    [] -> S.hPutStrLn handle "No results."
    (n:_) -> mapM_ ((S.hPutStrLn handle) . T.unpack) $ node2PosTags n

node2PosTags :: CCG.Node -> [T.Text]
node2PosTags node@(CCG.Node _ _ _ _ _ _ _) =
  case CCG.daughters node of
    [] -> [T.concat [CCG.pf node, "\t", CCG.toText (CCG.cat node), " \t", CCG.toText (CCG.sem node), "\t", CCG.memo node, "\t[", T.pack (show ((fromRational $ CCG.score node)::Fixed E2)), "]"]]
    dtrs -> [t | dtr <- dtrs, t <- node2PosTags dtr]

-- | `topBox` picks up the nodes in the "top" box in the chart.
topBox :: Chart -> [CCG.Node]
topBox chart = let ((_,k),_) = M.findMax chart in
                 case M.lookup (0,k) chart of 
                   Just nodes -> sort nodes
                   Nothing    -> []

-- | `bestOnly` takes only the nodes with the best score.
bestOnly :: [CCG.Node] -> [CCG.Node]
bestOnly nodes = case nodes of
  [] -> []
  (firstnode:ns) -> firstnode:(takeWhile (\node -> CCG.score(node) >= CCG.score(firstnode)) ns)

{- Main functions -}

-- | Main parsing function
parse :: Int         -- ^ The beam width
         -> T.Text   -- ^ The input text to parse
         -> IO(Chart)
parse beam sentence = do
  --start <- Time.getCurrentTime
  lexicon <- L.setupLexicon sentence
  --stop <- Time.getCurrentTime    
  --S.hPutStrLn S.stderr $ "Setting up numeration: " ++ show (Time.diffUTCTime stop start)
  return $ parseMain beam lexicon sentence

-- | The 'parseMain' function parses a (Japanees) text and generates a CYK-chart
--   The Int is the beam width.
parseMain :: Int -> L.Lexicon -> T.Text -> Chart
parseMain beam lexicon text
  | text == T.empty = M.empty -- foldl returns a runtime error when t is empty
  | otherwise       = let (chart,_,_) = T.foldl' (chartAccumulator beam lexicon) (M.empty, 0, T.empty) (purifyText text)
                      in chart
  where --purifyText :: T.Text -> T.Text
    purifyText = T.filter (\c -> not $ isSpace c || c `elem` ['、','。','，','．','！','？','!','?'])
                 --(\c -> not (isSpace c) && c /= '、' && c /= '。' && c /= '，' && c /= '．' && c /= '！' && c /= '？' && c /= '!' && c /= '?')

-- | The 'lookupChart' function looks up a chart with the key (i,j) 
--   and returns the value of type [Node]
lookupChart :: Int -> Int -> Chart -> [CCG.Node]
lookupChart i j chart = 
  case (M.lookup (i,j) chart) of Just list -> list
                                 Nothing   -> []

type PartialChart = (Chart,Int,T.Text)

-- | The 'chartAccumulator' function
--
chartAccumulator :: Int -> L.Lexicon -> PartialChart -> Char -> PartialChart
chartAccumulator beam lexicon partialchart c = 
  let (chart,i,stack) = partialchart in
  let newstack = (T.cons c stack) in
  let (newchart,_,_,to) = T.foldl' (boxAccumulator beam lexicon) (chart,T.empty,i,i+1) newstack in
  (newchart,to,newstack)

type PartialBox = (Chart,T.Text,Int,Int)

-- | The 'boxAccumulator' function
boxAccumulator :: Int -> L.Lexicon -> PartialBox -> Char -> PartialBox
boxAccumulator beam lexicon partialbox c = 
  let (chart,word,i,j) = partialbox in
  let newword = T.cons c word in 
  let list0 = L.lookupLexicon newword lexicon in
  let list1 = checkUnaryRules list0 in
  let list2 = checkBinaryRules i j chart list1 in
  let list3 = checkCoordinationRule i j chart list2 in
  let list4 = checkParenthesisRule i j chart list3 in
  let list5 = checkEmptyCategories list4 in 
--  let list5 = checkEmptyCategories list4 in 
  let listn = if length list5 <= beam then sort list5 else take beam $ sort list5 in
  ((M.insert (i,j) listn chart), newword, i-1, j)

checkUnaryRules :: [CCG.Node] -> [CCG.Node]
checkUnaryRules prevlist = 
  foldl' (\acc node -> CCG.unaryRules node acc) prevlist prevlist
                      
checkBinaryRules :: Int -> Int -> Chart -> [CCG.Node] -> [CCG.Node]
checkBinaryRules i j chart prevlist = 
  foldl' (\acck k -> foldl' (\accl lnode -> foldl' (\accr rnode -> CCG.binaryRules lnode rnode accr) 
                                                   accl  
                                                   (lookupChart k j chart)) 
                            acck 
                            (lookupChart i k chart)) 
         prevlist
         (take (j-i-1) [i+1..]) -- [k | i<k<j]

checkCoordinationRule :: Int -> Int -> Chart -> [CCG.Node] -> [CCG.Node]
checkCoordinationRule i j chart prevlist =
  foldl' (\acck k -> foldl' (\accc cnode -> foldl' (\accl lnode -> foldl' (\accr rnode -> CCG.coordinationRule lnode cnode rnode accr)
                                                                          accl
                                                                          (lookupChart (k+1) j chart))
                                                   accc
                                                   (lookupChart i k chart))
                            acck
                            (filter (\n -> (CCG.cat n)==CCG.CONJ) (lookupChart k (k+1) chart)))
         prevlist
         (take (j-i-2) [i+1..]) -- [k | i<k<j-1]  i-k k-k+1 k+1-j

checkParenthesisRule :: Int -> Int -> Chart -> [CCG.Node] -> [CCG.Node]
checkParenthesisRule i j chart prevlist 
  | i+3 <= j = foldl' (\accl lnode -> foldl' (\accr rnode -> foldl' (\accc cnode -> CCG.parenthesisRule lnode cnode rnode accc)
                                                                    accr 
                                                                    (lookupChart (i+1) (j-1) chart))
                                             accl
                                             (filter (\n -> (CCG.cat n)==CCG.RPAREN) (lookupChart (j-1) j chart)))
                      prevlist
                      (filter (\n -> (CCG.cat n)==CCG.LPAREN) (lookupChart i (i+1) chart))
  | otherwise = prevlist

checkEmptyCategories :: [CCG.Node] -> [CCG.Node]
checkEmptyCategories prevlist =
  foldr (\p -> (CCG.binaryRules (fst p) (snd p)) . (CCG.binaryRules (snd p) (fst p))) prevlist [(x,y) | x <- prevlist, y <- L.emptyCategories]

