import { Data, getAddressDetails, type Address, type UTxO } from "@lucid-evolution/lucid"

export interface AttestationRow {
  name: string
  type: 'Validator' | 'Policy' | 'Staking'
  hash: string
  github: string
  upvotes: number
  downvotes: number
  attestUtxo: UTxO
  counterAttestUtxo?: UTxO
}

export interface CounerAttestationForm {
  originalScriptHash: string
  source: string
  address: string
  policyId: string
  stakingKey: string
  scriptHash: string
}

export interface ProofForm {
  title: string
  source: string
  type: 'Validator' | 'Policy' | 'Staking'
  scriptHash: string
  address: string
  policyId: string
  stakeKey: string
}

export const CredentialSchema = Data.Enum([
  Data.Object({
      PublicKeyCredential: Data.Tuple([
          Data.Bytes(),
      ]),
  }),
  Data.Object({
      ScriptCredential: Data.Tuple([
          Data.Bytes(),
      ]),
  }),
]);
export type CredentialD = Data.Static<typeof CredentialSchema>;
export const CredentialD = CredentialSchema as unknown as CredentialD;

export const AddressSchema = Data.Object({
  paymentCredential: CredentialSchema,
  stakeCredential: Data.Nullable(
      Data.Enum([
          Data.Object({ Inline: Data.Tuple([CredentialSchema]) }),
          Data.Object({
              Pointer: Data.Tuple([
                  Data.Object({
                      slotNumber: Data.Integer(),
                      transactionIndex: Data.Integer(),
                      certificateIndex: Data.Integer(),
                  }),
              ]),
          }),
      ]),
  ),
});

export type AddressD = Data.Static<typeof AddressSchema>;
export const AddressD = AddressSchema as unknown as AddressD;

// { pattestationDatum'title      :: Term s (PAsData PByteString)
// , pattestationDatum'source     :: Term s (PAsData PByteString)
// , pattestationDatum'address    :: Term s (PMaybeData PByteString)
// , pattestationDatum'policyId   :: Term s (PMaybeData PByteString)
// , pattestationDatum'stakingKey :: Term s (PMaybeData PByteString)
// , pattestationDatum'scriptHash :: Term s (PByteString)

export const DatumSchema = Data.Object({
  title: Data.Bytes(),
  source: Data.Bytes(),
  address: Data.Nullable(AddressSchema),
  policyId: Data.Nullable(Data.Bytes()),
  stakingKey: Data.Nullable(Data.Bytes()),
  scriptHash: Data.Bytes(),
});

export type DatumType = Data.Static<typeof DatumSchema>;
export const DatumType = DatumSchema as unknown as DatumType;

export function fromAddress(address: Address): AddressD {
  // We do not support pointer addresses!

  const { paymentCredential, stakeCredential } = getAddressDetails(address);

  if (!paymentCredential) throw new Error("Not a valid payment address.");

  return {
    paymentCredential: paymentCredential?.type === "Key"
      ? {
        PublicKeyCredential: [paymentCredential.hash],
      }
      : { ScriptCredential: [paymentCredential.hash] },
    stakeCredential: stakeCredential
      ? {
        Inline: [
          stakeCredential.type === "Key"
            ? {
              PublicKeyCredential: [stakeCredential.hash],
            }
            : { ScriptCredential: [stakeCredential.hash] },
        ],
      }
      : null,
  };
}