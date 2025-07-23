/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unused-vars */
import { type Address, applyDoubleCborEncoding, Blockfrost, Data, fromText, getAddressDetails, Lucid, type MintingPolicy, mintingPolicyToId, type SpendingValidator, toUnit, type Unit, type UTxO, validatorToAddress } from "@lucid-evolution/lucid";
import { filterUTxOsByMinLovelace, filterUTxOsByPolicyId, getUniqueTokenName } from "../utils/wallet";
import { AddressSchema, DatumType, fromAddress, type AddressD, type AttestationRow, type CounerAttestationForm, type ProofForm } from "../types/types";
import { attestationValidator, attestationValidatorAddress, counterAttestationValidator, counterAttestationValidatorAddress, signatureCurrencySymbol, signaturePolicy, signerCurrencySymbol, signerPolicy } from "../types/contracts";
// import { validatorToAddress } from "@lucid-evolution/lucid";

interface SignerMintRequest {
  senderAddress: string;
}

interface AttestTransactionRequest {
  senderAddress: string;
  utxo: UTxO;
}

interface CounterAttestTransactionRequest {
  senderAddress: string;
  row: AttestationRow; 
}

interface SignatureTransactionRequest {
  senderAddress: string;
  form: ProofForm;
}

const CounterAttestationDatumSchema = Data.Object({
  originalAttestation: Data.Bytes(), // script hash of the original attestation
  source: Data.Bytes(),
  address: Data.Nullable(AddressSchema),
  policyId: Data.Nullable(Data.Bytes()),
  stakingKey: Data.Nullable(Data.Bytes()),
  scriptHash: Data.Bytes(),
});

  // { pcounterAttestationDatum'original   :: Term s (PAsData PByteString) -- ^ the script hash for the original attestation
  // , pcounterAttestationDatum'source     :: Term s (PAsData PByteString)
  // , pcounterAttestationDatum'address    :: Term s (PMaybeData PByteString)
  // , pcounterAttestationDatum'policyId   :: Term s (PMaybeData PByteString)
  // , pcounterAttestationDatum'stakingKey :: Term s (PMaybeData PByteString)
  // , pcounterAttestationDatum'scriptHash :: Term s (PAsData PByteString)
  // }

type CounterAttestationDatumType = Data.Static<typeof CounterAttestationDatumSchema>;
const CounterAttestationDatumType = CounterAttestationDatumSchema as unknown as CounterAttestationDatumType;

export async function buildSignerTransaction(req: SignerMintRequest) {

  try {
    const lucid = await Lucid(
      new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
      ),
      "Preprod",
    );

    lucid.selectWallet.fromAddress(req.senderAddress, []);
    const ownAddress = await lucid.wallet().address()
    
    const test = fromAddress("addr_test1wqlcn3pks3xdptxjw9pqrqtcx6ev694sstsruw3phd57ttg0lh0zq")
    console.log(test)


    const allUTxOs = await lucid.utxosAt(ownAddress);
    const correctUTxOs = filterUTxOsByMinLovelace(allUTxOs)
    const utxoToUse = correctUTxOs[0]
    const tokenName = getUniqueTokenName(utxoToUse)
    console.log("TokenName: ")
    console.log(tokenName)

    const ourToken : Unit = toUnit(signerCurrencySymbol, tokenName);

    const tx = await lucid
      .newTx()
      .collectFrom([utxoToUse])
      .mintAssets({[ourToken]: BigInt(1)}, Data.to(BigInt(0)))
      .attach.MintingPolicy(signerPolicy)
      .complete();

    return { tx: tx.toCBOR() };
  } catch (error) {
    throw new Error(
      error instanceof Error ? error.message : "Failed to build transaction",
    );
  }
}

export async function buildSignatureTransaction(req: SignatureTransactionRequest) {
  try {
    const lucid = await Lucid(
      new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
      ),
      "Preprod",
    );
    console.log(req.form)

    lucid.selectWallet.fromAddress(req.senderAddress, []);
    const ownAddress = await lucid.wallet().address()
    
    const allUTxOs = await lucid.utxosAt(ownAddress);
    const correctUTxOs = filterUTxOsByMinLovelace(allUTxOs)
    const utxoToUse = correctUTxOs[0]

    const ourDatum : DatumType = {
      title: fromText(req.form.title),
      source: fromText(req.form.source),
      address: req.form.address !== '' ? fromAddress(req.form.address) : null,
      policyId: req.form.policyId !== '' ? fromText(req.form.policyId) : null,
      stakingKey: req.form.stakeKey !== '' ? fromText(req.form.stakeKey) : null,
      scriptHash: fromText(req.form.scriptHash),
    }

    const tx = await lucid
      .newTx()
      .collectFrom([utxoToUse], Data.void())
      .pay.ToAddressWithData(attestationValidatorAddress, {kind: "inline", value: Data.to<DatumType>(ourDatum, DatumType)}, { lovelace: BigInt(3_000_000)  })
      .complete();

    return { tx: tx.toCBOR() };
  } catch (error) {
    throw new Error(
      error instanceof Error ? error.message : "Failed to build transaction",
    );
  }
}

export async function buildAttestTransaction(req: AttestTransactionRequest) {
  try {
    const lucid = await Lucid(
      new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
      ),
      "Preprod",
    );
    lucid.selectWallet.fromAddress(req.senderAddress, []);

    const ownAddress = await lucid.wallet().address()
    const allUTxOs = await lucid.utxosAt(ownAddress);
    const correctUTxOs = filterUTxOsByMinLovelace(allUTxOs)
    const utxoToUse = correctUTxOs[0]
    const tokenName = getUniqueTokenName(utxoToUse)
  
    console.log("TokenName: ")
    console.log(tokenName)

    const signatureToken : Unit = toUnit(signatureCurrencySymbol, tokenName);

    const scriptUTxO = req.utxo
    const baseAssets = scriptUTxO.assets
    const newSignatureCount = (baseAssets[signatureToken] ?? 0n) + 1n
    const valueToPay = {
      ...baseAssets,
      [signatureToken]: newSignatureCount
    }
    const tx = await lucid
      .newTx()
      .collectFrom([scriptUTxO], Data.void())
      .mintAssets({[signatureToken]: BigInt(1)}, Data.to(BigInt(0)))
      .pay.ToContract(attestationValidatorAddress, {
        kind: "inline",
        value: req.utxo.datum!,
      }, valueToPay)
      .attach.SpendingValidator(attestationValidator)
      .attach.MintingPolicy(signaturePolicy)
      .complete();
    
    return { tx: tx.toCBOR() };
  } catch (error) {
    throw new Error(
      error instanceof Error ? error.message : "Failed to build transaction",
    );
  }
}

export async function buildCounterAttestTransaction(
  req: CounterAttestTransactionRequest
) {
  try {
    // Initialize Lucid
    const lucid = await Lucid(
      new Blockfrost(
        'https://cardano-preprod.blockfrost.io/api/v0',
        "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
      ),
      'Preprod'
    )
    lucid.selectWallet.fromAddress(req.senderAddress, [])

    // Prepare signature token
    const ownAddr = await lucid.wallet().address()
    const utxos = await lucid.utxosAt(ownAddr)
    const eligible = filterUTxOsByMinLovelace(utxos)
    const utxoToUse = eligible[0]
    const tn = getUniqueTokenName(utxoToUse)
    const signatureToken: Unit = toUnit(signatureCurrencySymbol, tn)

    // Determine existing counter UTxO (if any)
    const existing = req.row.counterAttestUtxo
    const datum = req.row.attestUtxo.datum!

    // Assets before minting signature
    const baseAssets = existing ? existing.assets : {}
    const currentCount = baseAssets[signatureToken] ?? 0n
    const newCount = currentCount + 1n
    const valueForContract = {
      ...baseAssets,
      [signatureToken]: newCount,
    }

    let tx;
    // collect existing counter UTxO if updating
    if (existing) {
      tx = await lucid.newTx()
        .collectFrom([utxoToUse], Data.void())
        .collectFrom([existing], Data.void())
        .mintAssets({ [signatureToken]: 1n }, Data.to(0n))
        .attach.SpendingValidator(counterAttestationValidator)
        .attach.MintingPolicy(signaturePolicy)
        .pay.ToContract(
          counterAttestationValidatorAddress,
          { kind: 'inline', value: datum },
          valueForContract
        ).complete()
    }
    else {
      tx = await lucid.newTx()
        .collectFrom([utxoToUse], Data.void())
        .mintAssets({ [signatureToken]: 1n }, Data.to(0n))
        .attach.SpendingValidator(counterAttestationValidator)
        .attach.MintingPolicy(signaturePolicy)
        .pay.ToContract(
          counterAttestationValidatorAddress,
          { kind: 'inline', value: datum },
          valueForContract
        ).complete()
    }
    return { tx: tx.toCBOR() }
  } catch (err: any) {
    throw new Error(err instanceof Error ? err.message : 'Failed to build counter-attest tx')
  }
}



// export async function buildSignerTransaction1(req: SignerMintRequest) {
//   const ourTokenCurrencySymbol = mintingPolicyToId(signerPolicy);

//   try {
//     // Initialize Lucid with Blockfrost
//     const lucid = await Lucid(
//       new Blockfrost(
//         "https://cardano-preprod.blockfrost.io/api/v0",
//         "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
//       ),
//       "Preprod",
//     );

//     // get the address of a script
//     // const scriptAddress = validatorToAddress("Preprod", attestationValidator);

//     // get the utxos at a script address
//     //const referenceUTxO = await lucid.utxosAt(validatorToAddress("Preprod", mintingPolicy))

//     // get the UTxO containing an NFT 
//     //const referenceUtxo = await lucid.utxoByUnit(referenceUTxoToken)

//     // Select the sender's wallet
//     lucid.selectWallet.fromAddress(req.senderAddress, []);
    
//     // // get the address of the wallet that currently connected. 
//     const ownAddress = await lucid.wallet().address()
    
//     // const ourDatum : DatumType = 
//     //  {
//     //     seller: fromAddress(ownAddress),
//     //     assetAmount: BigInt(1),
//     //     assetId: ourTokenCurrencySymbol,
//     //     assetName: fromText("hello3"),
//     //   }

//     // Build a transaction to lock 10 ada at the script with the datum above
//     // const tx = await lucid
//     //   .newTx()
//     //   .pay.ToAddressWithData(scriptAddress, {kind: "inline", value: Data.to<DatumType>(ourDatum)}, { lovelace: BigInt(10_000_000) })
//     //   .complete();
    
//     const allUTxOs = await lucid.utxosAt(ownAddress);
//     const correctUTxOs = filterUTxOsByMinLovelace(allUTxOs)
//     const utxoToUse = correctUTxOs[0]
//     const tokenName = getUniqueTokenName(utxoToUse)
//     console.log("TokenName: ")
//     console.log(tokenName)

//     const ourToken : Unit = toUnit(ourTokenCurrencySymbol, tokenName);
//     // const tx = await lucid
//     //   .newTx()
//     //   .collectFrom([scriptUTxO], Data.void())
//     //   .collectFrom([scriptUTxO2], Data.void())
//     //   .pay.ToAddress(scriptAddress, { [ourToken]: BigInt(0) })
//     //   .complete();

//     // const tx = await lucid
//     //   .newTx()
//     //   .pay.ToAddress(scriptAddress, { lovelace: BigInt(10_000_000) })
//     //   .complete();
    
//     // Build a transaction to mint a token 
//     const tx = await lucid
//       .newTx()
//       .collectFrom([utxoToUse])
//       .mintAssets({[ourToken]: BigInt(1)}, Data.to(BigInt(0)))
//       .attach.MintingPolicy(signerPolicy)
//       .complete();

//     // Build the transaction to mint a token to an output containing a reference script:
//     // const tx = await lucid
//     //   .newTx()
//     //   .pay.ToAddressWithData(validatorToAddress("Preview", mintingPolicy), undefined, {
//     //     [ourToken]: BigInt(1)
//     //   }, mintingPolicy)
//     //   .mintAssets({[ourToken]: BigInt(1)}, Data.to(BigInt(6)))
//     //   .attach.MintingPolicy(mintingPolicy)
//     //   .complete();
    
//     // Build the transaction to mint a token and using a refernce input with the script instead of attaching the script directly
//     // const tx = await lucid
//     //   .newTx()
//     //   .readFrom([referenceUtxo])
//     //   .mintAssets({[ourToken]: BigInt(1)}, Data.to(BigInt(6)))
//     //   .complete();

//     // Return the transaction CBOR
//     return { tx: tx.toCBOR() };
//   } catch (error) {
//     throw new Error(
//       error instanceof Error ? error.message : "Failed to build transaction",
//     );
//   }
// }



// export async function buildSignatureTransaction(req: SignatureTransactionRequest) {
//   try {
//     const lucid = await Lucid(
//       new Blockfrost(
//         "https://cardano-preprod.blockfrost.io/api/v0",
//         "preprodJ4VLm1GXzebp8rfGdOOrovlcgCsBwjW2",
//       ),
//       "Preprod",
//     );
//     const scriptAddress = validatorToAddress("Preprod", attestationValidator);

//     lucid.selectWallet.fromAddress(req.senderAddress, []);
//     const ownAddress = await lucid.wallet().address()
    
//     const allUTxOs = await lucid.utxosAt(ownAddress);
//     const correctUTxOs = filterUTxOsByPolicyId(allUTxOs, signerCurrencySymbol)
//     const utxoToUse = correctUTxOs[0]

//     const ourDatum : DatumType = 
//      {
//         title: fromText(req.form.title),
//         source: fromText(req.form.source),
//         address: req.form.address !== '' ? fromAddress(req.form.address) : null,
//         policyId: req.form.policyId !== '' ? fromText(req.form.policyId) : null,
//         stakingKey: req.form.stakeKey !== '' ? fromText(req.form.stakeKey) : null,
//         scriptHash: fromText(req.form.scriptHash),
//       }

//     const ourToken : Unit = toUnit(signerCurrencySymbol, tokenName);

//     const tx = await lucid
//       .newTx()
//       .collectFrom([utxoToUse], Data.void())
//       .mintAssets({[ourToken]: BigInt(1)}, Data.to(BigInt(0)))
//       .attach.MintingPolicy(signaturePolicy)
//       .pay.ToAddressWithData(scriptAddress, {kind: "inline", value: Data.to<DatumType>(ourDatum)}, { lovelace: BigInt(3_000_000),  })
//       .complete();

//     return { tx: tx.toCBOR() };
//   } catch (error) {
//     throw new Error(
//       error instanceof Error ? error.message : "Failed to build transaction",
//     );
//   }
// }