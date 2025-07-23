import { fromHex, toHex, type UTxO } from "@lucid-evolution/lucid"
import { sha3_256 } from "@noble/hashes/sha3";

export function filterUTxOsByMinLovelace(
  utxos: UTxO[],
  minLovelace: bigint = 5_000_000n
): UTxO[] {
  return utxos.filter(({ assets }) => {
    // assets.lovelace might be undefined if there’s no lovelace entry
    const lovelaceAmt = assets.lovelace ?? 0n
    return lovelaceAmt >= minLovelace
  })
}

export function filterUTxOsByPolicyId(
  utxos: UTxO[],
  policyId: string
): UTxO[] {
  const pid = policyId.toLowerCase()
  return utxos.filter(({ assets }) =>
    Object.keys(assets).some((unit) => {
      if (unit === 'lovelace') return false
      return unit.toLowerCase().startsWith(pid)
    })
  )
}

export function getUniqueTokenName(utxo: UTxO): string {
  const id = fromHex(utxo.txHash);
  const data = new Uint8Array([utxo.outputIndex, ...id]);
  const hash = sha3_256(data);
  return toHex(hash);
}