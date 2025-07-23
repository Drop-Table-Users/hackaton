import { applyDoubleCborEncoding, mintingPolicyToId, validatorToAddress, type MintingPolicy, type SpendingValidator } from "@lucid-evolution/lucid";

// 2#==2
// addr_test1wzkmnu88mhgpsym9tamjvwslzjxp5mhxvjtha3e3qagztsswur5lh
export const attestationValidator: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding("5554010100253357389201073220233d3d2032001499"), // CBOR format from plutus.json
};
export const attestationValidatorAddress = validatorToAddress("Preprod", attestationValidator);
 
// 1#==1
// addr_test1wzuvagl4wjnzrgjf57fh6tk6kmdfpfnah5z88e5m9lv2q3ss5lxwr
export const counterAttestationValidator: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding("5554010100253357389201073120233d3d2031001499"), // CBOR format from plutus.json
};
export const counterAttestationValidatorAddress = validatorToAddress("Preprod", counterAttestationValidator);

export const signerPolicy: MintingPolicy = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding("46450101002499"), // CBOR format from plutus.json
};
export const signerCurrencySymbol = mintingPolicyToId(signerPolicy);

export const signaturePolicy: MintingPolicy = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding("46450101002499"), // CBOR format from plutus.json
};
export const signatureCurrencySymbol = mintingPolicyToId(signaturePolicy);