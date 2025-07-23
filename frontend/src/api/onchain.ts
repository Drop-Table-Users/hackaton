/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unused-vars */
import { Blockfrost, Data, Lucid, type UTxO } from "@lucid-evolution/lucid";
import { attestationValidatorAddress, counterAttestationValidatorAddress, signatureCurrencySymbol } from "../types/contracts";
import { DatumSchema, type AttestationRow } from "../types/types";

export async function fetchAttestationUTxOs() {
  try {
  const lucid = await Lucid(
      new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
      ),
      "Preprod",
    );

  const allUTxOs = await lucid.utxosAt(attestationValidatorAddress);
  return allUTxOs
  }
  catch (err: any) {
    return []
  }
}

export async function fetchCounterAttestationUTxOs(): Promise<UTxO[]> {
  try {
    const lucid = await Lucid(
      new Blockfrost(
        'https://cardano-preprod.blockfrost.io/api/v0',
        "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
      ),
      'Preprod'
    )

    const allUTxOs = await lucid.utxosAt(counterAttestationValidatorAddress)
    return allUTxOs
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  } catch (err: any) {
    // If the service returns 404 (no UTxOs), or any other error, just return empty array
  
    return []
    
    // Re-throw unexpected errors
    // throw err
  }
}
export async function fetchAttestationRows(): Promise<AttestationRow[]> {
  // 1) grab all attestations
  const attestUtxos: UTxO[] = await fetchAttestationUTxOs()

  // early exit if none
  if (attestUtxos.length === 0) return []

  // 2) grab all counter‐attestations
  const counterUtxos: UTxO[] = await fetchCounterAttestationUTxOs()

  // 3) for each attestation, find the one counter‐utxo with the same scriptHash
  return attestUtxos.map((attest) => {
    // extract the attestation datum so we know its scriptHash
    const attestDatum = Data.from(attest.datum!, DatumSchema)

    // find the counter‐attestation whose datum.scriptHash matches
    const matchingCounter = counterUtxos.find((ctr) => {
      const ctrDatum = Data.from(ctr.datum!, DatumSchema)
      return ctrDatum.scriptHash === attestDatum.scriptHash
    })

    // 4) build and return the row (counterAttestUtxo may be undefined)
    return extractAttestationRow(attest, matchingCounter)
  })
}

export function extractAttestationRow(
  attestUtxo: UTxO,
  counterAttestUtxo?: UTxO
): AttestationRow {
  // 1) Pull out the on-chain datum
  const attestDatum = Data.from(attestUtxo.datum!, DatumSchema)

  // 2) Determine type by which optional field is present
  const type: AttestationRow['type'] =
    attestDatum.address != null
      ? 'Validator'
      : attestDatum.stakingKey != null
      ? 'Staking'
      : 'Policy'

  // 3) Helper to count all tokens under your signature policy
  const countPolicyTokens = (utxo?: UTxO): number =>
    utxo
      ? Object.entries(utxo.assets)
          // skip lovelace, only multi-asset units under your signature policy
          .filter(
            ([unit]) =>
              unit !== 'lovelace' && unit.startsWith(signatureCurrencySymbol)
          )
          .reduce((sum, [, amount]) => sum + Number(amount), 0)
      : 0

  const upvotes = countPolicyTokens(attestUtxo)
  console.log("HERE")
  console.log(upvotes)
  console.log(attestUtxo)
  console.log(signatureCurrencySymbol)
  const downvotes = countPolicyTokens(counterAttestUtxo)

  // 4) Build and return the row, only including counterAttestUtxo if provided
  return {
    name: attestDatum.title!,
    type,
    hash: attestDatum.scriptHash!,
    github: attestDatum.source!,
    upvotes,
    downvotes,
    attestUtxo,
    counterAttestUtxo
  }
}