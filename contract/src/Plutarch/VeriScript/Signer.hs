{-# LANGUAGE LambdaCase #-}

module Plutarch.VeriScript.Signer (validator) where

import           Plutarch.LedgerApi.AssocMap (plookup, psingleton)
import           Plutarch.LedgerApi.V3       (PRedeemer (..),
                                              PScriptContext (..),
                                              PScriptInfo (..), PTxInInfo (..),
                                              PTxInfo (..), PTxOutRef (..),
                                              PValue (..), PTxId (..))
import           Plutarch.LedgerApi.Value    (PTokenName (..))
import qualified Plutarch.Monadic            as P
import           Plutarch.Prelude            hiding (psingleton)

tokenNameFromOutRef :: Term s (PTxOutRef :--> PTokenName)
tokenNameFromOutRef = phoistAcyclic $ plam $ \outRef -> P.do
  PTxOutRef { ptxOutRef'id, ptxOutRef'idx } <- pmatch outRef
  let idx = pfromData ptxOutRef'idx
  PTxId txId <- pmatch $ pfromData ptxOutRef'id
  pif (255 #< idx)
    (ptraceInfoError "tokenNameFromOutRef: 255 < idx")
    (pcon (PTokenName $ psha3_256 #$ pconsBS # (pintegerToByte # idx) # txId))

validator :: ClosedTerm (PScriptContext :--> PUnit)
validator = plam $ \scriptContext -> P.do
  PScriptContext { pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo } <- pmatch scriptContext
  let ownCs = pmatch pscriptContext'scriptInfo $ \case
                PMintingScript x -> x
                _                -> ptraceInfoError "not minting"
  PRedeemer redeemer <- pmatch pscriptContext'redeemer
  let idx = pasInt # redeemer
  PTxInfo { ptxInfo'inputs, ptxInfo'mint } <- pmatch pscriptContext'txInfo
  let input = pfromData ptxInfo'inputs #!! idx
  PTxInInfo { ptxInInfo'outRef } <- pmatch (pfromData input)
  let tokenName = tokenNameFromOutRef # ptxInInfo'outRef
  PValue map <- pmatch (pfromData ptxInfo'mint)
  let x = pmatch (plookup # pfromData ownCs # map) $ \case
               PJust x  -> x
               PNothing -> ptraceInfoError "no token"
  pif (x #== psingleton # tokenName # 1)
    (pconstant ())
    (ptraceInfoError "Signer: mint incorrect")
