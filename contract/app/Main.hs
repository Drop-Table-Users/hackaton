{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import qualified Cardano.Binary                         as CBOR
import           Data.Aeson                             (KeyValue ((.=)),
                                                         object)
import           Data.Aeson.Encode.Pretty               (encodePretty)
import           Data.Bifunctor                         (first)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Base16                 as Base16
import qualified Data.ByteString.Lazy                   as LBS
import           Data.Default                           (def)
import           Data.String                            (IsString (..))
import           Data.Text                              (Text, pack)
import qualified Data.Text                              as Text
import           Data.Text.Encoding                     (encodeUtf8)
import qualified Data.Text.Encoding                     as Text
import           System.IO

import           Plutarch.Evaluate                      (applyArguments,
                                                         evalScript)
import           Plutarch.Internal.Term                 (Config (..),
                                                         LogLevel (..),
                                                         TracingMode (..),
                                                         compile)
import           Plutarch.LedgerApi.V3                  (scriptHash)
import           Plutarch.Prelude
import           Plutarch.Script                        (Script,
                                                         serialiseScript)
import           PlutusLedgerApi.V3                     (ScriptHash (..), ExBudget, Data)

import qualified Plutarch.VeriScript.Attestation        as Attestation
import qualified Plutarch.VeriScript.CounterAttestation as CounterAttestation
import qualified Plutarch.VeriScript.Signature          as Signature
import qualified Plutarch.VeriScript.Signer             as Signer

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript cmp
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO Script
writePlutusScript cfg title filepath term = do
  putStrLn ("- " <> title <> "...")
  case evalT cfg term of
    Left e -> do print e; error "fail"
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV3" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content
      return script

writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO Script
writePlutusScriptTraceBind = writePlutusScript (Tracing LogInfo DoTracingAndBinds)

writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO Script
writePlutusScriptTrace = writePlutusScript (Tracing LogInfo DoTracing)

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO Script
writePlutusScriptNoTrace = writePlutusScript NoTracing

scriptHashToByteString :: ScriptHash -> ByteString
scriptHashToByteString (ScriptHash bs) = (fromString . show) bs

main :: IO ()
main = do
  putStrLn "Writing Plutarch scripts to files:"
  avalidator  <- writePlutusScriptTrace "Attestation"        "compiled/attestation.json"               Attestation.validator
  cavalidator <- writePlutusScriptTrace "CounterAttestation" "compiled/counterAttestation.json" CounterAttestation.validator
  sivalidator <- writePlutusScriptTrace "Signer"             "compiled/signer.json"                         Signer.validator
  let sh = scriptHashToByteString . scriptHash
  writePlutusScriptTrace "Signature" "compiled/signature.json" (Signature.validator (sh avalidator) (sh cavalidator) (sh sivalidator))
  putStrLn "done ... have a great hackaton!"
