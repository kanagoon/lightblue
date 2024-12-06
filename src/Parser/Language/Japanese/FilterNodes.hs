{-# LANGUAGE DeriveGeneric, DefaultSignatures, RecordWildCards, OverloadedStrings #-}

module Parser.Language.Japanese.FilterNodes(
    blacklist,
    createFilterFrom
   ) where

import qualified Parser.CCG as CCG
import qualified Parser.ChartParser as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified  Interface  as I
import qualified System.IO as S


blacklist :: [T.Text]
blacklist = [
    "泣き叫",
    "思い描"
    ]

filterBlackListParseSetting :: IO LB.ParseSetting
filterBlackListParseSetting = do
    setting <- LB.defaultParseSetting
    return $ setting {
        LB.ifFilterNode = Just $ createFilterFrom blacklist
    }

createFilterFrom :: [T.Text] -> Int -> Int -> [CCG.Node] -> [CCG.Node]
createFilterFrom blacklist i j nodes =
    createFilterFrom' blacklist nodes

createFilterFrom' :: [T.Text] -> [CCG.Node] -> [CCG.Node]
createFilterFrom' _ [] = []
createFilterFrom' blacklist (c:cs) =
    if (CCG.rs c == CCG.LEX) && (TL.toStrict $ CCG.pf c) `elem` blacklist
        then createFilterFrom' blacklist cs
    else c : createFilterFrom' blacklist cs
