{-# LANGUAGE RecordWildCards #-}

{-|
Copyright   : (c) Daisuke Bekki, 2024
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

Type checker for Underspecified Dependent Type Theory (Bekki forthcoming).
-}
module DTS.TypeChecker (
  typeInfer
  , typeCheck
  , nullProver
  --, sequentialTypeCheck
  ) where

import Data.List (lookup)    --base
import Control.Applicative (empty)   --base
import Control.Monad (guard,when,sequence)    --base
import Control.Monad.State (lift)  
import ListT (ListT(..),fromFoldable) --list-t
import qualified System.IO as S           --base
import qualified Data.Text.Lazy as T --text
import qualified Data.Text.Lazy.IO as T --text
import Interface.Text (SimpleText(..))
import Interface.Tree (Tree(..),node)
import qualified DTS.DTTdeBruijn as DTTdB
import qualified DTS.UDTTdeBruijn as UDTTdB
import qualified DTS.UDTTwithName as UDTTwN
import qualified DTS.QueryTypes as QT

-- | A skeltal prover 
nullProver :: QT.ProverBuilder
nullProver _ _ = fromFoldable []

-- | The default type inference for UDTT
typeInfer :: QT.TypeInferer -- | QT.Prover -> Bool -> UDTTdB.TypeInferQuery -> ListT IO QT.TypeCheckResult
typeInfer prover verbose tiq@(UDTTdB.TypeInferQuery sig ctx trm) = do
  let typeCheck' = typeCheck prover verbose
      typeInfer' = typeInfer prover verbose
  when verbose $ lift $ T.hPutStrLn S.stderr $ toText $ UDTTwN.fromDeBruijnTypeInferQuery tiq
  case trm of -- sig:Signature, ctx:Context, trm:Preterm DTT, typ:Preterm DTT
    UDTTdB.Var i -> do
      when (i >= length ctx) $ fail $ T.unpack $ T.concat [T.pack (show i), " is out of bound in the context: ", toText ctx]
      return $ Tree QT.Var (DTTdB.Judgment sig ctx (DTTdB.Var i) (ctx!!i)) []
    UDTTdB.Con t -> do
      case lookup t sig of
        Just termA -> return $ Tree QT.Con (DTTdB.Judgment sig ctx (DTTdB.Con t) termA) []
        Nothing -> fail $ T.unpack $ T.concat ["No constant symbol for ", t]
    UDTTdB.Type -> return $ Tree QT.TypeF (DTTdB.Judgment sig ctx DTTdB.Type DTTdB.Kind) []
    UDTTdB.Pi termA termB -> do
      diagramA <- typeCheck' (UDTTdB.Judgment sig ctx termA DTTdB.Type)  -- | diagramAs :: [Tree]
      let termA' = DTTdB.trm $ node diagramA
      diagramB <- typeCheck' (UDTTdB.Judgment sig (termA':ctx) termB DTTdB.Type)
      let termB' = DTTdB.trm $ node diagramB
      return $ Tree QT.PiF (DTTdB.Judgment sig ctx (DTTdB.Pi termA' termB') (DTTdB.typ $ node diagramB)) [diagramA,diagramB]
    UDTTdB.App termM termN -> do
      diagramM <- typeInfer' $ UDTTdB.TypeInferQuery sig ctx termM
      let termM' = DTTdB.trm $ node diagramM
          (DTTdB.Pi termA termB) = DTTdB.typ $ node diagramM
      diagramN <- typeCheck' $ UDTTdB.Judgment sig ctx termN termA
      let termN' = DTTdB.trm $ node diagramN
          termB' = DTTdB.betaReduce $ DTTdB.subst termB termN' 0
      return $ Tree QT.PiE (DTTdB.Judgment sig ctx (DTTdB.App termM' termN') termB') [diagramM, diagramN]
    UDTTdB.Sigma termA termB -> do
      diagramA <- typeCheck' $ UDTTdB.Judgment sig ctx termA DTTdB.Type
      let termA' = DTTdB.trm $ node diagramA
      diagramB <- typeCheck' $ UDTTdB.Judgment sig (termA':ctx) termB DTTdB.Type
      let termB' = DTTdB.trm $ node diagramB
      return $ Tree QT.SigmaF (DTTdB.Judgment sig ctx (DTTdB.Sigma termA' termB') (DTTdB.typ $ node diagramB)) [diagramA,diagramB]
    UDTTdB.Proj UDTTdB.Fst termM -> do
      diagramM <- typeInfer' $ UDTTdB.TypeInferQuery sig ctx termM
      let termM' = DTTdB.trm $ node diagramM
          (DTTdB.Sigma termA _) = DTTdB.typ $ node diagramM
      return $ Tree QT.SigmaE (DTTdB.Judgment sig ctx (DTTdB.Proj DTTdB.Fst termM') termA) [diagramM]
    UDTTdB.Proj UDTTdB.Snd termM -> do
      diagramM <- typeInfer' $ UDTTdB.TypeInferQuery sig ctx termM
      let termM' = DTTdB.trm $ node diagramM
          (DTTdB.Sigma _ termB) = DTTdB.typ $ node diagramM
          termB' = DTTdB.betaReduce $ DTTdB.subst termB (DTTdB.Proj DTTdB.Fst termM') 0
      return $ Tree QT.SigmaE (DTTdB.Judgment sig ctx (DTTdB.Proj DTTdB.Snd termM') termB') [diagramM]
    UDTTdB.Disj termA termB -> do
      diagramA <- typeCheck' $ UDTTdB.Judgment sig ctx termA DTTdB.Type
      let termA' = DTTdB.trm $ node diagramA
      diagramB <- typeCheck' $ UDTTdB.Judgment sig (termA':ctx) termB DTTdB.Type
      let termB' = DTTdB.trm $ node diagramB
      return $ Tree QT.DisjF (DTTdB.Judgment sig ctx (DTTdB.Sigma termA' termB') (DTTdB.typ $ node diagramB)) [diagramA,diagramB]
    UDTTdB.Unpack termP termL termM termN -> do
      diagramL <- typeInfer' $ UDTTdB.TypeInferQuery sig ctx termL
      let termL' = DTTdB.trm $ node diagramL
          (DTTdB.Disj termA termB) = DTTdB.typ $ node diagramL
      diagramP <- typeCheck' $ UDTTdB.Judgment sig ctx termP (DTTdB.Pi (DTTdB.Disj termA termB) DTTdB.Type)
      let termP' = DTTdB.trm $ node diagramP
          termPL = DTTdB.betaReduce $ DTTdB.App termP' termL'
          termPM = DTTdB.betaReduce $ DTTdB.App termP' (DTTdB.Iota DTTdB.Fst (DTTdB.Var 0))
          termPN = DTTdB.betaReduce $ DTTdB.App termP' (DTTdB.Iota DTTdB.Fst (DTTdB.Var 0))
      diagramM <- typeCheck' $ UDTTdB.Judgment sig ctx termM (DTTdB.Pi termA termPM)
      diagramN <- typeCheck' $ UDTTdB.Judgment sig ctx termM (DTTdB.Pi termB termPN)
      let termM' = DTTdB.trm $ node diagramM
          termN' = DTTdB.trm $ node diagramN
      return $ Tree QT.DisjE (DTTdB.Judgment sig ctx (DTTdB.Unpack termP' termL' termM' termN') termPL) [diagramL,diagramP,diagramM,diagramN]
    UDTTdB.Bot -> return $ Tree QT.EnumF (DTTdB.Judgment sig ctx DTTdB.Bot DTTdB.Type) []
    UDTTdB.Top -> return $ Tree QT.EnumF (DTTdB.Judgment sig ctx DTTdB.Top DTTdB.Type) []
    UDTTdB.Unit -> return $ Tree QT.EnumI (DTTdB.Judgment sig ctx DTTdB.Unit DTTdB.Top) []
    UDTTdB.Entity -> return $ Tree QT.EnumF (DTTdB.Judgment sig ctx DTTdB.Entity DTTdB.Type) []
    UDTTdB.Eq termA termM termN -> do
      diagramA <- typeCheck' $ UDTTdB.Judgment sig ctx termA DTTdB.Type
      let termA' = DTTdB.trm $ node diagramA
      diagramM <- typeCheck' $ UDTTdB.Judgment sig ctx termM termA'
      let termM' = DTTdB.trm $ node diagramM
      diagramN <- typeCheck' $ UDTTdB.Judgment sig ctx termN termA'
      let termN' = DTTdB.trm $ node diagramN
      return $ Tree QT.IqF (DTTdB.Judgment sig ctx (DTTdB.Eq termA' termM' termN') DTTdB.Type) [diagramA,diagramM,diagramN]
    UDTTdB.Refl termA termM -> do
      diagramA <- typeCheck' $ UDTTdB.Judgment sig ctx termA DTTdB.Type
      let termA' = DTTdB.trm $ node diagramA
      diagramM <- typeCheck' $ UDTTdB.Judgment sig ctx termM termA'
      let termM' = DTTdB.trm $ node diagramM
      return $ Tree QT.IqI (DTTdB.Judgment sig ctx (DTTdB.Refl termA' termM') (DTTdB.Eq termA' termM' termM')) [diagramA, diagramM]
    --ToDo: add UDTTdB.Idpeel
    UDTTdB.Nat -> return $ Tree QT.NatF (DTTdB.Judgment sig ctx DTTdB.Nat DTTdB.Type) []
    UDTTdB.Zero -> return $ Tree QT.NatI (DTTdB.Judgment sig ctx DTTdB.Zero DTTdB.Nat) []
    UDTTdB.Succ termN -> do
      diagramN <- typeCheck' $ UDTTdB.Judgment sig ctx termN DTTdB.Nat
      let termN' = DTTdB.trm $ node diagramN
      return $ Tree QT.NatI (DTTdB.Judgment sig ctx (DTTdB.Succ termN') DTTdB.Nat) [diagramN]
    --ToDo: add UDTTdB.Natrec
    UDTTdB.Asp termA -> do  -- | Underspecified term
      diagramA <- typeCheck' $ UDTTdB.Judgment sig ctx termA DTTdB.Type
      let termA' = DTTdB.trm $ node diagramA
          psq = DTTdB.ProofSearchQuery sig ctx termA'
      diagramQ <- prover psq
      --lift $ T.putStrLn $ T.concat ["Proof search launched: ", toText psq]
      return diagramQ
    -- UDTTdB.Asp termA termB -> do  -- | Underspecified type 
    --   diagramA <- typeCheck' $ UDTTdB.Judgment sig ctx termA UDTTdB.Type
    --   let termA' = UDTTdB.trm $ node diagramA
    --       psq = QT.ProofSearchQuery sig ctx termA'
    --   diagramQ <- prover psq
    --   lift $ T.putStrLn $ toText psq
    --   let termM = UDTTdB.toUDTT $ UDTTdB.trm $ node diagramQ
    --       termB' = UDTTdB.betaReduce $ UDTTdB.subst termB termM 0
    --   typeCheck' $ UDTTdB.Judgment sig ctx termB' UDTTdB.Type
    -- | type inference fails
    termM -> fail $ T.unpack $ T.concat [toText termM, " is not an inferable term."]

-- | The default type checker for UDTT
typeCheck :: QT.TypeChecker -- | QT.Prover -> Bool -> UDTTdB.Judgment -> ListT IO TypeCheckResult
typeCheck prover verbose tcq@(UDTTdB.Judgment sig ctx trm typ) = do
  let typeCheck' = typeCheck prover verbose
      typeInfer' = typeInfer prover verbose
  when verbose $ lift $ T.hPutStrLn S.stderr $ toText $ UDTTwN.fromDeBruijnJudgment tcq
  case (trm,typ) of -- sig:Signature, ctx:Context, trm:Preterm DTT, typ:Preterm DTT
    (UDTTdB.Lam termM, DTTdB.Pi termA termB) -> do
      diagramA <- typeCheck' $ UDTTdB.Judgment sig ctx (UDTTdB.toUDTT termA) DTTdB.Type
      let termA' = DTTdB.trm $ node diagramA
      diagramM <- typeCheck' $ UDTTdB.Judgment sig (termA':ctx) termM termB
      let termM' = DTTdB.trm $ node diagramM
          termB' = DTTdB.typ $ node diagramM
      return $ Tree QT.PiI (DTTdB.Judgment sig ctx (DTTdB.Lam termM') (DTTdB.Pi termA' termB')) [diagramA, diagramM]
    (UDTTdB.Pair termM termN, DTTdB.Sigma termA termB) -> do
      diagramM <- typeCheck' $ UDTTdB.Judgment sig ctx termM termA
      let termM' = DTTdB.trm $ node diagramM
          termA' = DTTdB.typ $ node diagramM
          termB' = DTTdB.betaReduce $ DTTdB.subst termB termM' 0
      diagramN <- typeCheck' $ UDTTdB.Judgment sig ctx termN termB'
      let termN' = DTTdB.trm $ node diagramN
      return $ Tree QT.SigmaE (DTTdB.Judgment sig ctx (DTTdB.Pair termM' termN') (DTTdB.Sigma termA' termB')) [diagramM,diagramN]
    (UDTTdB.Iota UDTTdB.Fst termM, DTTdB.Disj termA termB) -> do
      diagramM <- typeCheck' $ UDTTdB.Judgment sig ctx termM termA
      let termM' = DTTdB.trm $ node diagramM
          termA' = DTTdB.typ $ node diagramM
      return $ Tree QT.DisjI (DTTdB.Judgment sig ctx (DTTdB.Iota DTTdB.Fst termM') (DTTdB.Disj termA' termB)) [diagramM]
    (UDTTdB.Iota UDTTdB.Snd termN, DTTdB.Disj termA termB) -> do
      diagramN <- typeCheck' $ UDTTdB.Judgment sig ctx termN termB
      let termN' = DTTdB.trm $ node diagramN
          termB' = DTTdB.typ $ node diagramN
      return $ Tree QT.DisjI (DTTdB.Judgment sig ctx (DTTdB.Iota DTTdB.Snd termN') (DTTdB.Disj termA termB')) [diagramN]
    _ -> do
      diagramM <- typeInfer' $ UDTTdB.TypeInferQuery sig ctx trm
      let termA' = DTTdB.typ $ node diagramM
      guard $ termA' == typ
      return diagramM
