{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : ChartParser
Description : A left-corner CKY-parser for lexical grammars
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta
-}

module ChartParser (
  -- * Main parser
  parse,
  parseMain,
  -- * Printing (in Text) functions
  printChart,
  TEX.printNodesInTeX,
  printChartInSimpleText,
  posTagger,
  -- * Utilities to filter the parsing results
  topBox,
  bestOnly,
  sOnly,
  -- * 
  L.myLexicon,
  L.setupLexicon
  ) where

import Data.List
import Data.Char                       --base
import Data.Fixed                      --base
import qualified Data.Text.Lazy as T   --text
import qualified Data.Map as M         --container
import qualified System.IO as S        --base
import qualified CombinatoryCategorialGrammar as CCG --(Node, unaryRules, binaryRules, trinaryRules, isCONJ, cat, SimpleText)
import qualified JapaneseLexicon as L (Lexicon, lookupLexicon, setupLexicon, emptyCategories, myLexicon)
import qualified TeXmodule as TEX

-- | The type for CYK-charts.
type Chart = M.Map (Int,Int) [CCG.Node]

{- Come functions for pretty printing Chart/Nodes -}

-- | prints every box in the (parsed) CYK chart as a TeX source code.
printChart :: S.Handle -> Chart -> IO()
printChart handle chart = mapM_ printList $ M.toList $ M.filter (/= []) chart
  where printList (key,nodes) = do -- list化したChartを画面表示する。
          S.hPutStr handle $ "\\subsubsection*{" ++ show key ++ ": ノード数 " ++ (show $ length nodes) ++ "}"
          TEX.printNodesInTeX handle nodes

-- | prints CCG nodes (=a parsing result) as a plain text.
printChartInSimpleText :: S.Handle -> [CCG.Node] -> IO()
printChartInSimpleText handle nodes = 
  do
  mapM_ (\node -> S.hPutStr handle $ (T.unpack $ CCG.toText node) ++ "\n") nodes
  S.hPutStrLn handle $ "Number of nodes: " ++ show (length nodes)

-- | prints CCG nodes (=a parsing result) in a \"part-of-speech tagger\" style
posTagger :: S.Handle -> [CCG.Node] -> IO()
posTagger handle nodes = 
  case nodes of
    [] -> S.hPutStrLn handle "No results."
    (n:_) -> mapM_ ((S.hPutStrLn handle) . T.unpack) $ node2PosTags n

node2PosTags :: CCG.Node -> [T.Text]
node2PosTags node@(CCG.Node _ _ _ _ _ _ _ _) =
  case CCG.daughters node of
    [] -> [T.concat [CCG.pf node, "\t", CCG.toText (CCG.cat node), " \t", CCG.toText (CCG.sem node), "\t", CCG.source node, "\t[", T.pack (show ((fromRational $ CCG.score node)::Fixed E2)), "]"]]
    dtrs -> [t | dtr <- dtrs, t <- node2PosTags dtr]

-- | picks up the nodes in the "top" box in the chart.
topBox :: Chart -> [CCG.Node]
topBox chart = let ((_,k),_) = M.findMax chart in
                 case M.lookup (0,k) chart of
                   Just nodes -> sort nodes
                   Nothing    -> []

-- | takes only the nodes with the best score.
bestOnly :: [CCG.Node] -> [CCG.Node]
bestOnly nodes = case nodes of
  [] -> []
  (firstnode:ns) -> firstnode:(takeWhile (\node -> CCG.score(node) >= CCG.score(firstnode)) ns)

-- | `sOnly` 
sOnly :: [CCG.Node] -> [CCG.Node]
sOnly = filter isS
  where isS node = 
          case CCG.cat node of
            CCG.S _ -> True
            _ -> False

{- Main functions -}

-- | Main parsing function, showing the standard use of the `parseMain` function.
parse :: Int           -- ^ The beam width
         -> T.Text     -- ^ A sentence to be parsed
         -> IO(Chart)
parse beam sentence = do
  lexicon <- L.setupLexicon L.myLexicon sentence
  let (chart,_) = parseMain beam lexicon sentence
  return chart

-- | parses a (Japanees) sentence and generates a CYK-chart.
parseMain :: Int          -- ^ The beam width
             -> L.Lexicon -- ^ A lexicon to be used for parsing
             -> T.Text    -- ^ A sentence to be parsed
             -> (Chart, [[CCG.Node]])
parseMain beam lexicon sentence
  | sentence == T.empty = (M.empty,[]) -- foldl returns a runtime error when text is empty
  | otherwise =
      let (chart,_,_,_,nodes) = T.foldl' (chartAccumulator beam lexicon) (M.empty,[0],0,T.empty,[]) sentence in
      (chart,nodes)

-- | triples representing a state during parsing:
-- the parsed result (=chart) of the left of the pivot,
-- the stack of positions of the previous 'separators' (i.e. '、','，',etc)
-- the pivot (=the current parsing position),
-- the revsersed list of chars that has been parsed
-- the list of partial parse results
type PartialChart = (Chart,[Int],Int,T.Text,[[CCG.Node]])

-- | The 'chartAccumulator' function
chartAccumulator :: Int             -- ^ beam width
                    -> L.Lexicon    -- ^ my lexicon
                    -> PartialChart -- ^ accumulated result (Chart, Int, Text)
                    -> Char         -- ^ next char of a unparsed text
                    -> PartialChart
chartAccumulator beam lexicon (chart,seplist,i,stack,parsed) c =
  let newstack = (T.cons c stack);
      (sep:seps) = seplist in
  if c `elem` ['、','，',',','-','―','−','。','．','！','？','!','?'] || isSpace c -- Each seperator is an end of a phase
    then let -- edge = [((x,i+1),(lookupChart x i chart)) | x <- [sep..i-1]]
             -- top = lookupChart sep i chart;
             -- filteredChartList = filter (\p -> fst (fst p) < sep) $ M.toList chart;
             -- newchart1 = M.fromList $ foldl' (\chartList k -> ((k,i+1),(lookupChart k i chart)):chartList) filteredChartList [0..sep];
             --(s1,s2) = T.splitAt (fromIntegral (i+1-sep)) newstack;
             --(newchart2,_,_,_) = T.foldl' (boxAccumulator beam lexicon) (newchart1,(T.reverse s1),sep-1,i+1) s2
             newchart = M.fromList $ foldl' (punctFilter sep i) [] $ M.toList chart
         in (newchart, ((i+1):seps), (i+1), newstack, (take 1 (sort (lookupChart sep (i+1) newchart)):parsed))
    else let (newchart,_,_,_) = T.foldl' (boxAccumulator beam lexicon) (chart,T.empty,i,i+1) newstack;
             newseps | c `elem` ['「','『'] = (i+1:seplist)
                     | c `elem` ['」','』'] = seps
                     | otherwise = seplist 
         in (newchart,newseps,(i+1),newstack,parsed)

punctFilter :: Int -> Int -> [((Int,Int),[CCG.Node])] -> ((Int,Int),[CCG.Node]) -> [((Int,Int),[CCG.Node])]
punctFilter sep i chartList e@((from,to),nodes)
  | to == i = if from <= sep 
                 then ((from,to+1),nodes):chartList
                 else chartList
  | otherwise = if from < sep
                  then e:chartList
                  else chartList

type PartialBox = (Chart,T.Text,Int,Int)

-- | The 'boxAccumulator' function
boxAccumulator :: Int           -- ^ beam width
                  -> L.Lexicon  -- ^ my lexicon
                  -> PartialBox -- ^ accumulated result (Chart, Text, Int, Int)
                  -> Char       -- ^ 
                  -> PartialBox
boxAccumulator beam lexicon (chart,word,i,j) c =
  let newword = T.cons c word;
      list0 = if (T.compareLength newword 20) == LT -- Does not execute lookup for a long word (run "LongestWord" to check it (23))
                then L.lookupLexicon newword lexicon
                else [];
      list1 = checkUnaryRules list0;
      list2 = checkBinaryRules i j chart list1;
--      list3 = checkCoordinationRule i j chart list2;
      list4 = checkParenthesisRule i j chart list2;
      list5 = checkEmptyCategories list4 in
  ((M.insert (i,j) (cutoff beam list5) chart), newword, i-1, j)

-- | take `beam` nodes from the top of `ndoes`.
cutoff :: Int -> [CCG.Node] -> [CCG.Node]
cutoff beam nodes = if length nodes <= beam then nodes else take beam $ sort nodes

-- | looks up a chart with the key (i,j) and returns the value of type [Node]
lookupChart :: Int -> Int -> Chart -> [CCG.Node]
lookupChart i j chart = 
  case (M.lookup (i,j) chart) of Just list -> list
                                 Nothing   -> []
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

{-
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
-}

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
  foldl' (\p ec -> foldl' (\list node -> (CCG.binaryRules node ec) $ (CCG.binaryRules ec node) list) p p) prevlist L.emptyCategories
  --foldr (\p -> (CCG.binaryRules (fst p) (snd p)) . (CCG.binaryRules (snd p) (fst p))) prevlist [(x,y) | x <- prevlist, y <- L.emptyCategories]

