module Plutarch.VeriScript.CounterAttestation (validator) where

import qualified Generics.SOP               as SOP
import           GHC.Generics               (Generic)

import           Plutarch.LedgerApi.V3      (PAddress (..), PMaybeData,
                                             PScriptContext (..),
                                             PScriptInfo (..), PTxInInfo (..),
                                             PTxInfo (..), PTxOut (..), pfromDJust, PDatum (..), PRedeemer (..), POutputDatum (..))
import qualified Plutarch.Monadic           as P
import           Plutarch.Prelude
import Plutarch.LedgerApi.Value (pltPositive)

data PCounterAttestationDatum (s :: S) = PCounterAttestationDatum
  { pcounterAttestationDatum'original   :: Term s (PAsData PByteString) -- ^ the script hash for the original attestation
  , pcounterAttestationDatum'source     :: Term s (PAsData PByteString)
  , pcounterAttestationDatum'address    :: Term s (PMaybeData PByteString)
  , pcounterAttestationDatum'policyId   :: Term s (PMaybeData PByteString)
  , pcounterAttestationDatum'stakingKey :: Term s (PMaybeData PByteString)
  , pcounterAttestationDatum'scriptHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCounterAttestationDatum)

instance PTryFrom PData (PAsData PCounterAttestationDatum)

validator :: ClosedTerm (PScriptContext :--> PUnit)
validator = plam $ \scriptContext -> P.do
  PScriptContext { pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo } <- pmatch scriptContext
  PTxInfo { ptxInfo'inputs, ptxInfo'outputs } <- pmatch pscriptContext'txInfo
  PPair ownRef maybeDatum <- pmatch $ pmatch pscriptContext'scriptInfo $ \case
                                   PSpendingScript ownRef maybeDatum -> pcon $ PPair ownRef maybeDatum
                                   _                                 -> ptraceInfoError "not spending"
  PRedeemer redeemer <- pmatch pscriptContext'redeemer
  let inIdx = pasInt # redeemer
  PTxInInfo { ptxInInfo'outRef, ptxInInfo'resolved } <- pmatch $ pfromData $ pfromData ptxInfo'inputs #!! inIdx
  PTxOut { ptxOut'address = inAddr, ptxOut'value = inVal } <- pmatch ptxInInfo'resolved
  PAddress { paddress'credential } <- pmatch inAddr
  let ownRefCorrect = ownRef #== ptxInInfo'outRef
  let sameOrDifferentAddress = plam $ \ininfo -> P.do
      PTxInInfo { ptxInInfo'outRef = outRef, ptxInInfo'resolved = resolved } <- pmatch $ pfromData ininfo
      PTxOut { ptxOut'address = addr } <- pmatch resolved
      PAddress { paddress'credential = cred } <- pmatch addr
      outRef #== ownRef #|| pnot # (cred #== paddress'credential)
  let onlyOneInput = pall # sameOrDifferentAddress # pfromData ptxInfo'inputs
  let sameAddress = plam $ \out -> P.do
      PTxOut { ptxOut'address = addr } <- pmatch $ pfromData out
      PAddress { paddress'credential = cred } <- pmatch addr
      cred #== paddress'credential
  let outputsAtAddr = pfilter # sameAddress # pfromData ptxInfo'outputs
  let onlyOneOutput = 1 #== plength # outputsAtAddr
  PTxOut { ptxOut'address = outAddr, ptxOut'datum = outDatum, ptxOut'value = outVal } <- pmatch $ pfromData $ pheadBuiltin # outputsAtAddr
  let outAddrCorrect = outAddr #== inAddr
  PDatum inDatum <- pmatch $ pfromDJust # maybeDatum
  let inputDatum = ptryFrom @(PAsData PCounterAttestationDatum) inDatum fst
  let outDatumCorrect = outDatum #== (pcon . POutputDatum . pcon . PDatum . pforgetData) inputDatum
  let outValueCorrect = pltPositive (pfromData inVal) (pfromData outVal)
  let transactionCorrect = ownRefCorrect #&& onlyOneInput #&& onlyOneOutput #&& outAddrCorrect #&& outDatumCorrect #&& outValueCorrect
  pif transactionCorrect (pconstant ()) (ptraceInfoError "PCounterAttestationDatum: invalid transaction")
