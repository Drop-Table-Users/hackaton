# Validators

## Signer minting policy

Purpose: a user, who intends to become an attestation signer creates his own
token for authentication purposes.

## Signature minting policy

Purpose: a signer can mint his own signature tokens but can only "attach" them
to Attestation contracts.

## Attestation contract

Datum example:
```
title: "WingRiders order contract v2"
source: "https://github.com/WingRiders/dex-v2-contracts#master"
address: "addr1z8p79rpkcdz8x9d6tft0x0dx5mwuzac2sa4gm8cvkw5hcn8uppf470jxputfh3qh7jdwmf7f9vszatr7hf9ju3jcytaqrj95ar" -- should not contain any stake keys
policyId: "asset12ffdj8kk2w485sr7a5ekmjjdyecz8ps2cm5zed"
stakingKey: "stake1uyvtg6tdh798q986ypqqpgjx3ds45e0ee2kg8qkkj07ly8gdqvfe0"
scriptHash: "e118b4696dbf8a7014fa204000a2468b615a65f9caac8382d693fdf21d"
```

Reference script: the full compiled script.

Value:
- contains the signature tokens of the signers
- minimum Ada

## Counter-attestation contract

Datum example:
```
originalAttestation: "addr1z8p79rpkcdz8x9d6tft0x0dx5mwuzac2sa4gm8cvkw5hcn8uppf470jxputfh3qh7jdwmf7f9vszatr7hf9ju3jcytaqrj95ar" -- reference the first id that exists
source: "https://github.com/WingRiders/dex-v2-contracts#master"
address: "addr1z8p79rpkcdz8x9d6tft0x0dx5mwuzac2sa4gm8cvkw5hcn8uppf470jxputfh3qh7jdwmf7f9vszatr7hf9ju3jcytaqrj96ar" -- may be different from the original
policyId: "asset12ffdj8kk2w485sr7a5ekmjjdyecz8ps2cm6zed"                                                           -- may be different from the original
stakingKey: "stake1uyvtg6tdh798q986ypqqpgjx3ds45e0ee2kg8qkkj07ly8gdqvfe0"
scriptHash: "e118b4696dbf8a7014fa204000a2468b615a65f9caac8382d693fdf21d"
```

Reference script: the full compiled script.

Value:
- contains the signature tokens of the signers
- minimum Ada
