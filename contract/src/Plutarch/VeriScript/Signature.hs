module Plutarch.VeriScript.Signature (validator) where

import           Data.ByteString             (ByteString)
import           Plutarch.LedgerApi.AssocMap (KeyGuarantees (..), PMap, plookup)
import           Plutarch.LedgerApi.V3       (PAddress (..), PCredential (..),
                                              PCurrencySymbol (PCurrencySymbol),
                                              PRedeemer (..),
                                              PScriptContext (..),
                                              PScriptHash (..),
                                              PScriptInfo (..), PTokenName,
                                              PTxInInfo (..), PTxInfo (..),
                                              PTxOut (..), PValue (..))
import           Plutarch.LedgerApi.Value    (pvalueOf)
import qualified Plutarch.Monadic            as P
import           Plutarch.Prelude
import           Plutarch.Unsafe             (punsafeCoerce)

assertTokenNameMinted :: Term s (PMap 'Sorted PTokenName PInteger :--> PTokenName)
assertTokenNameMinted = phoistAcyclic $ plam $ \m -> P.do
  let xs = pto m
  let x = pheadBuiltin # xs
  pif (1 #== pfromData (psndBuiltin # x) #&& pnullBuiltin # (ptailBuiltin # xs))
    (pfromData (pfstBuiltin # x))
    (ptraceInfoError "assertTokenNameMinted: wrong mint")

validator :: ByteString -> ByteString -> ByteString -> ClosedTerm (PScriptContext :--> PUnit)
validator signerScriptHash aScriptHash caScriptHash = plam $ \scriptContext -> P.do
  PScriptContext { pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo } <- pmatch scriptContext
  PTxInfo { ptxInfo'inputs, ptxInfo'outputs, ptxInfo'mint } <- pmatch pscriptContext'txInfo
  PMintingScript ownCs <- pmatch pscriptContext'scriptInfo
  PRedeemer redeemer <- pmatch pscriptContext'redeemer
  let redeemerList = punsafeCoerce @(PBuiltinList PInteger) redeemer
  let inIdx = pheadBuiltin # redeemerList
  let outIdx = pheadBuiltin # (ptailBuiltin # redeemerList)
  PTxInInfo { ptxInInfo'resolved } <- pmatch $ pfromData $ pfromData ptxInfo'inputs #!! inIdx
  PTxOut { ptxOut'value = inVal } <- pmatch ptxInInfo'resolved
  PTxOut { ptxOut'address = outAddr, ptxOut'value = outVal } <- pmatch $ pfromData $ pfromData ptxInfo'outputs #!! outIdx
  PAddress { paddress'credential } <- pmatch outAddr
  PValue map <- pmatch (pfromData ptxInfo'mint)
  let x = pmatch (plookup # pfromData ownCs # map) $ \case
              PJust x  -> x
              PNothing -> ptraceInfoError "no token minted"
  let tn = assertTokenNameMinted # x
  let scriptCred = pcon . PScriptCredential . pdata . pcon . PScriptHash . pconstant
  let inValCorrect   = ptraceInfoIfFalse "inValIncorrect"   $ 1 #== pvalueOf # pfromData inVal # (pcon . PCurrencySymbol . pconstant) signerScriptHash # tn
  let outAddrCorrect = ptraceInfoIfFalse "outAddrIncorrect" $ paddress'credential #== scriptCred aScriptHash #|| paddress'credential #== scriptCred caScriptHash
  let outValCorrect  = ptraceInfoIfFalse "outValIncorrect"  $ 1 #== pvalueOf # pfromData outVal # pfromData ownCs # tn
  pif (inValCorrect #&& outAddrCorrect #&& outValCorrect)
    (pconstant ())
    (ptraceInfoError "Signature: mint incorrect")
