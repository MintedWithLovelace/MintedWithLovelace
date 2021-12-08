# MintedWithLovelace Global Royalty Regitry

## General Idea

Give creators a decentralized/on-chain solution for setting royalties over asset policies, regardless if any 777 token was minted/exists for the given policy and regardless if the policy is now locked and unable to set a standardized policy royalty token.

## Proposed Solution

Creator mints a token for any given policy they want to set a royalty over ("target policy"), from the same policy wallet used to mint the target policy.

This token has the following metadata structure:

`{
    "721": {
        "___this-777-token-policy-ID___": {
            "777": {
                "self": {
                        "type": "all",
                        "scripts": [
                          {
                            "keyHash": "___owner-keyHash___",
                            "type": "sig"
                          },
                          {
                            "type": "before",
                            "slot": "this-locking-slot-height"
                          }
                        ]
                },
                "___target-Policy-ID___": [
                  {
                    "script": {
                        "type": "all",
                        "scripts": [
                          {
                            "keyHash": "___owner-keyHash___",
                            "type": "sig"
                          },
                          {
                            "type": "before",
                            "slot": "target-policyid-locking-slot-height"
                          }
                        ]
                    },
                    "rate": "percentage (e.g. 0.1)",
                    "addr": "addr_payout_address"
                  }
                ]
            }
        }
    }
}`

The initial policy ID is this "777" named token (referencing it is a royalty setting token), of type 721. The first section "self" contains its own policy script data. After this initial "validation/comparison" self field, is the target policy section.  The target Policy ID also owned by the policy hash, and its individual policy script data, rate, and address (or address array per CIP-0027) is then set in this field for reference by secondary markets.

After minting the 777 token, the creator sends this token to a global "Royalty Registry" smartcontract. This is a simple burn contract which locks the token forever.  In this locking transaction, the creator must hash the target policy ID in the proper format: `cardano-cli transaction hash-script-data --script-data-value '"{4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039}"'`. This "burns"/locks the 777 token with it's target policy ID embedded in the Datum at the Royalty Registry smartcontract for markets to query.

A secondary market can easily query the smartcontract address and compare the datums of the utxos, using a valid hash of the policy ID in question. When it finds a match, it simply compares the following:

1. Does the "self" policy script data within the 777 metadata generate the same policy id as itself?
2. Does the policy script data provided for the target policy within the 777 metadata generate the same policy id as itself?
3. Do the keyHashes match (did the same policy wallet create the target and THIS token)?

If these conditions are true, the secondary market can extract the "rate" and the "addr" or list of "addr"s for royalty enforcement, even over a previously minted and locked policy, in a fully decentralized simple way.

## Live Test / Proof of Concept

I created the following test to demonstrate this. The scenario is we have a creator who has previously minted the token with Policy ID "4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039" named "MyNFT" at address addr_test1vpucvftxkp6h2p6rm7l6jq60630jlzhkewcnwzvtsvhecqqu6t472. Here is their minting tx: https://testnet.cardanoscan.io/transaction/b3725a03e8d0e448f6ce4186fe5c9ef2741d21df6b606be93e246560e6d7869d?tab=tokenmint

This creator wants to set royalties for this now-locked policy and can take the following steps to implement this method:

1. Mint the 777 token according to above specs, to addr_test1vqq4sm3xdgdzreyg6zzyz7cw73ndd2vxm33gt469g4gsqfsvaa2v5
    - minting tx showing metadata: https://testnet.cardanoscan.io/transaction/83410575a91c90fe6c70a6f5c30d30e3cbd60d50ee92114c65b1d2bcf8954e38?tab=metadata

2. Compile the RoyaltyRegistry herein to 'royaltyregistry.plutus' and extract the smartcontract address with `cardano-cli address build --testnet-magic $TESTNET_MAGIC_NUM --payment-script-file royaltyregistry.plutus` = addr_test1wrkdjvvtglxsu6vuvzczrd8wpw66j6gtnmynyzn49vwef6s8hsf7l

3. Lock the 777 token at the Royalty Registry smartcontract with the target policy (4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039) hashed properly (resulting in c4e697246a32d4e1bb75f0e089bbc5dc53783efab4e3278e37ed345fb158dde6) into the Datum with `cardano-cli transaction build --tx-in 83410575a91c90fe6c70a6f5c30d30e3cbd60d50ee92114c65b1d2bcf8954e38#0 --tx-in 83410575a91c90fe6c70a6f5c30d30e3cbd60d50ee92114c65b1d2bcf8954e38#1 --tx-out addr_test1wrkdjvvtglxsu6vuvzczrd8wpw66j6gtnmynyzn49vwef6s8hsf7l+1689618+"1 204598a01d0e5e8a2c61ddc8d9a56aa6d80dad6a8b1fddb8988bab52.777" --tx-out-datum-hash-value '"{4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039}"' --change-address=addr_test1vqq4sm3xdgdzreyg6zzyz7cw73ndd2vxm33gt469g4gsqfsvaa2v5 --testnet-magic $TESTNET_MAGIC_NUM --out-file tx.build --alonzo-era`
    - locking tx: https://testnet.cardanoscan.io/transaction/2c954e752ccc6db2b9df28d54aefd27df0a6dd30d055751cd8656aeea3bfff0f

Secondary markets can then take the following steps to utilize this:

1. A secondary market has token 4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039.MyNFT come up for sale. They attempt to find a minting from the procedure laid out in CIP-0027 and none is found. The next place to look on-chain is the global Royalty Registry smartcontract. 

2. The token's policy id is hashed appropriately like so `cardano-cli transaction hash-script-data --script-data-value '"{4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039}"'` which gives us "c4e697246a32d4e1bb75f0e089bbc5dc53783efab4e3278e37ed345fb158dde6". 

3. The secondary market then takes this value and searches for a matching Datum at the Royalty Registry smartcontract via a simple UTXO query like so `cardano-cli query utxo --testnet-magic $TESTNET_MAGIC_NUM --address addr_test1wrkdjvvtglxsu6vuvzczrd8wpw66j6gtnmynyzn49vwef6s8hsf7l --out-file royalties.json` and has the following json file: 
`{
    "2c954e752ccc6db2b9df28d54aefd27df0a6dd30d055751cd8656aeea3bfff0f#1": {
        "address": "addr_test1wrkdjvvtglxsu6vuvzczrd8wpw66j6gtnmynyzn49vwef6s8hsf7l",
        "datumhash": "c4e697246a32d4e1bb75f0e089bbc5dc53783efab4e3278e37ed345fb158dde6",
        "value": {
            "204598a01d0e5e8a2c61ddc8d9a56aa6d80dad6a8b1fddb8988bab52": {
                "777": 1
            },
            "lovelace": 1689618
        }
    }
}`
This json file can be iterated over using offchain apps or scripts to compare the hashed version of the policy id in question against each UTXOs Datum value to find a match.

4. In our example a match is found as seen in the "datumhash" value above. 

5. With this match, the market extracts the metadata from the 777 token with the matching Datum and sees the following:
`{
    "721": {
        "204598a01d0e5e8a2c61ddc8d9a56aa6d80dad6a8b1fddb8988bab52": {
            "777": {
                "self": {
                        "type": "all",
                        "scripts": [
                          {
                            "keyHash": "6f3c8043504c2bd8d7efa9c196533d4cccc6608ad98174c58873ed4d",
                            "type": "sig"
                          },
                          {
                            "type": "before",
                            "slot": "49550301"
                          }
                        ]
                },
                "4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039": [
                  {
                    "script": {
                        "type": "all",
                        "scripts": [
                          {
                            "keyHash": "6f3c8043504c2bd8d7efa9c196533d4cccc6608ad98174c58873ed4d",
                            "type": "sig"
                          },
                          {
                            "type": "before",
                            "slot": "44554131"
                          }
                        ]
                    },
                    "rate": "0.1",
                    "addr": "addr_test1vputel0yvxu2c5tarv8kr2krehune6c77tz4e8uy2q3t6jcgrr7xp"
                  }
                ]
            }
        }
    }
}`

6. From this data ownership/control over both the 777 token and the target policy are able to be easily validated, having matching keyHashes and with the given slot heights of each (the 777 itself and the target policy id). They are then able to extract the rate and payout address(s) from the field for the target policy ID.

## Notes

Using this method only the true owner of the policy is able to mint this 777 targeting token. By using the same policy wallet used for the target policy id, the policy id of the 777 token is able to be generated for validation of the keyHash. Both this 777 token and the target policy id must share the same keyHash. The policy scripts for both this and the target policy are embedded into the metadata under their given headings ("self" for this 77 token and the actual policy id of the target for it's section). There is no need to reuse the token and the Royalty Registry can be a simple "burn" or "always fail" script. The Datum must contain the hash of the target policy ID, for simple query by a secondary market for enforcement.

This could also be potentially used as an override to any rates set individually during minting of any given policy, as it allows for use with a new/planned or locked policy ID.