# MintedWithLovelace Global Royalty Registry

## General Idea

Give creators a decentralized/on-chain solution for setting royalties over asset policies, regardless if any 777 token was minted/exists for the given policy and regardless if the policy is now locked and unable to set a standardized policy royalty token.

## Proposed Solution

Creator mints a token for any given policy they want to set a royalty over ("target policy"), from the same policy wallet used to mint the target policy.

This token has the following metadata structure:

```
{
    "721": {
        "___this-Royalty-Control-token-policy-ID___": {
            "CRC": {
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
                    "addr": "addr_payout_address",
                    "controlslot": "_slot_height_at_time_of_this_token_minting_"
                  }
                ]
            }
        }
    }
}
```
The first section "self" contains its own policy script data. After this initial "validation/comparison" self field, is the target policy section with its heading being the policy ID of this target (which is also hashed in the datum and is now the markets were able to locate this utxo).  The target Policy ID also owned by the policy hash, and its individual policy script data, rate, and address (or address array per CIP-0027) is then set in this field for reference by secondary markets.

After minting the RoyaltyControl token, the creator sends this token to a global "Royalty Registry" smartcontract. This is a simple burn contract which locks the token forever.  In this locking transaction, the creator must hash the target policy ID in the proper format:
`cardano-cli transaction hash-script-data --script-data-value '"{4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039}"'`

This "burns"/locks the Royalty Control token with it's target policy ID embedded in the Datum at the Royalty Registry smartcontract for markets to query.

A secondary market can easily query the smartcontract address and compare the datums of the utxos, using a valid hash of the policy ID in question. When it finds a match, it simply compares the following:

1. Does the policy script data within the Royalty Control metadata (under tag "self") generate the same policy id as the token itself (the policy ID of the Royalty Control token)?
2. Does the policy script data provided for the target policy (under tag = target policyID) within the Royalty Control metadata generate the same policy id as we expect/itself/it's tag name/the datum unhashed?
3. Do the keyHashes match (did the same policy skey create the target and THIS token)?

If these conditions are true, the secondary market can extract the "rate" and the "addr" or list of "addr"s for royalty enforcement, even over a previously minted and locked policy, in a fully decentralized simple way.

A secondary marketplace validating if this Royalty Control token truly owns the policy in question, is presented with the policy script data for both the Royalty Control token itself and the target policy. The secondary marketplace already has the policy id of the target token (and found this transaction by searching the smartcontract utxos for a datum matching the hashed version of the target policy) and calculates if the script data in the metadata seen above calculates to the same policy id of the target.  They then also see the policy id of this Royalty Control token along with it's script data under "self" in the metadata seen above. They can then verify that this script data calculates the policy id of this token.  The "key" for this working is this requirement: The keyHash for both script datas above, is the SAME keyhash. 

Because the keyhash is matching, it is proven that only this single keyhash was used in both policy id mintings, definitively. Ergo, the royalty and payout addresses set for the target policy, were indeed provably entered by the owner of this shared policy signing key. 

My detaching the Royalty Control token from the precise policy which we are controlling royalty for, we now have the ability modify payment data and lower royalty percentages.  And inherently we can also set royalties over policy IDs which we were previously unable to, due to them being locked by the time a solution was in place at all.  

The argument is that this means someone could set 100% later. Secondary markets have to computationally setup functions to find and apply royalties already. This new approach would need to be coded up and implemented and the markets can do something mitigate this if they want to, like a max limit to be honored for example..so setting 100% would max at 10% or something (it would be up to the marketplace to decide IF they want to mitigate and how of course, which also lends to the competitive elements between the markets in an interesting and healthy way).

What might be a good standard in that regard, and in regards to modifications made in general like to payment addresses, is that the secondary marketplace querying for royalty info over a particular policy id would know the height of the time the Royalty Control token was minted from the metadata "controlslot" and compare this across all the entries made by the creator.  

For example perhaps the creator, we'll call her Alice, mints and submits to the smartcontract her first Royalty Control token which has a policy ID of 456, she sets a 5% royalty over previously minted policy 123. Her payout address is just her own.  She does this at slot height 400.  Along comes a company and wants to invest in Alice. As part of the deal they want a share of royalties on all her policies. She now mints another Royalty Control token with policy ID 789 and modifies her metadata to instead now have a list of payout addresses per the investor.."controlslot" has the slot she does this at, we'll say 450, and this token targets policy 123. 

A secondary market can now see both entries at the smartcontract, with matchin datum which hashes to policy 123. So they can look at the "controlslot" and see which is the most recent, 450, and now apply payment to the new addresses.  They would also check if any changes to the % were made, and if it dropped they would honor the lower percentage...if it increased they might not and honor the lowest of the 2, again depending on how markets decide to hanlde that.

And as a sidenote/example of how validation of ownership is possible, in the above example, the keyhash for all 4 policy scripts being presented across the 2 Royalty Control tokens, were all the exact same policy keyhash, because Alice did all of this using a single policy key. This is how secondary markets can be certain both Royalty Control tokens with different policy IDs and the target token with a 3rd different policy ID, all shared 1 signing key, because they compare the Royalty Control tokens own policy ID with it's presented "self" and the policy they are looking up with the other metadata, and verify that BOTH share 1 Policy KeyHash.

Because we are verifying that the keyHashes of both *this* Royalty Control token and our target policy id over which we wish to set the royalties match, we can then simply calculate the policy id from each of these embedded policy scripts and validate that the "self" one is equal to the actual Royalty Control token's own policy ID at this utxo and that the target one identified by it's expected policy id, truly calculates as such...and again that they share the same keyhash.

Someone trying to "fake" a royalty setting token by simply minting with the metadata as outlined above, would fail this validation process as their minted token would not have a policy ID generated from the "self" script data (keyhash and height in our above example). This would prove they are not the owner of the target policy and do not have the right to set royalty over it.

## Testnet Proof of Concept

*Note: I initially thought to give the token the .name 777 to reference the cip-0027. Some people expressed confusion since that is the top level indicator for that token type. So I propose Royalty Control and the .name could be .RC or .CRC (Cardano Royalty Control). However this is really not important for validating marketplaces since they would know the Royalty Registry smartcontract address and query it's utxos, searching for a datum hash of the token they are looking up's policy ID.*

I created the following test to demonstrate this. The scenario is we have a creator who has previously minted the token with Policy ID "4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039" named "MyNFT" at address addr_test1vpucvftxkp6h2p6rm7l6jq60630jlzhkewcnwzvtsvhecqqu6t472.
Here is the minting tx: https://testnet.cardanoscan.io/transaction/b3725a03e8d0e448f6ce4186fe5c9ef2741d21df6b606be93e246560e6d7869d?tab=tokenmint


**This creator wants to set royalties for this now-locked policy and can take the following steps to implement this method:**

1. Mint the Royalty Control token according to above specs, to addr_test1vqq4sm3xdgdzreyg6zzyz7cw73ndd2vxm33gt469g4gsqfsvaa2v5
    - resultant minting tx w/ metadata: https://testnet.cardanoscan.io/transaction/83410575a91c90fe6c70a6f5c30d30e3cbd60d50ee92114c65b1d2bcf8954e38?tab=metadata

2. Compile the RoyaltyRegistry herein to 'royaltyregistry.plutus' and extract the smartcontract address with:
`cardano-cli address build --testnet-magic $TESTNET_MAGIC_NUM --payment-script-file royaltyregistry.plutus` gives us addr_test1wrkdjvvtglxsu6vuvzczrd8wpw66j6gtnmynyzn49vwef6s8hsf7l as the smartcontract address.

3. Lock the Royalty Control token at the Royalty Registry smartcontract with the target policy (4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039) hashed properly (resulting in c4e697246a32d4e1bb75f0e089bbc5dc53783efab4e3278e37ed345fb158dde6) into the Datum with:
```
cardano-cli transaction build \
--tx-in 83410575a91c90fe6c70a6f5c30d30e3cbd60d50ee92114c65b1d2bcf8954e38#0 \
--tx-in 83410575a91c90fe6c70a6f5c30d30e3cbd60d50ee92114c65b1d2bcf8954e38#1 \
--tx-out addr_test1wrkdjvvtglxsu6vuvzczrd8wpw66j6gtnmynyzn49vwef6s8hsf7l+1689618+"1 204598a01d0e5e8a2c61ddc8d9a56aa6d80dad6a8b1fddb8988bab52.777" \
--tx-out-datum-hash-value '"{4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039}"' \
--change-address=addr_test1vqq4sm3xdgdzreyg6zzyz7cw73ndd2vxm33gt469g4gsqfsvaa2v5 \
--testnet-magic $TESTNET_MAGIC_NUM \
--out-file tx.build --alonzo-era
```
    - the resultant locking tx: https://testnet.cardanoscan.io/transaction/2c954e752ccc6db2b9df28d54aefd27df0a6dd30d055751cd8656aeea3bfff0f


**Secondary markets can then take the following steps to utilize this:**

1. A secondary market has token 4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039.MyNFT come up for sale. They attempt to find a minting from the procedure laid out in CIP-0027 and none is found. The next place to look on-chain is the global Royalty Registry smartcontract. 

2. The token's policy id is hashed appropriately like so:
`cardano-cli transaction hash-script-data --script-data-value '"{4316059167b8bbc46c84d2aec144ed469f83db85ad64f139d3c33039}"'` which gives us "c4e697246a32d4e1bb75f0e089bbc5dc53783efab4e3278e37ed345fb158dde6". 

3. The secondary market then takes this value and searches for a matching Datum at the Royalty Registry smartcontract via a simple UTXO query like so:
`cardano-cli query utxo --testnet-magic $TESTNET_MAGIC_NUM --address addr_test1wrkdjvvtglxsu6vuvzczrd8wpw66j6gtnmynyzn49vwef6s8hsf7l --out-file royalties.json` and has the following json file: 
```
{
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
}
```
This json file can be iterated over using offchain apps or scripts to compare the hashed version of the policy id in question against each UTXOs Datum value to find a match.

4. In our example a match is found as seen in the "datumhash" value above. 

5. With this match, the market extracts the metadata from the Royalty Control token with the matching Datum and sees the following:
```
{
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
                    "addr": "addr_test1vputel0yvxu2c5tarv8kr2krehune6c77tz4e8uy2q3t6jcgrr7xp",
                    "controlslot": 4456NNNN
                  }
                ]
            }
        }
    }
}
```

6. From this data ownership/control over both the Royalty Control token and the target policy are able to be easily validated, having verified matching keyHashes and with the given slot heights of both this Royalty Control and the target policy. The key element to this validation is processing the script data from "self" to generate a policy id, then compare this to this Royalty Control token's own policy ID at the utxo and it must match, proving that the single keyHash shared by both this and the target policy scripts, was responsible for minting this Royalty Control token AND the target policy. If all matches up, ownership has been proven and they are then able to extract the rate and payout address(s) from the field for the target policy ID.

## Notes

Using this method only the true owner of the policy is able to mint this Royalty Control targeting token in a successfully verifiable way. By using the same policy signing key used for the target policy id, the policy id of the Royalty Control token is able to be generated for validation of the keyHash. Both this Royalty Control token and the target policy id must share the same keyHash. The policy scripts for both this and the target policy are embedded into the metadata under their given headings ("self" for this Royalty Control token and the actual policy id of the target for it's section). There is no need to reuse the token and the Royalty Registry can be a simple "burn" or "always fail" script. The Datum must contain the hash of the target policy ID, for simple query by a secondary market for enforcement.

This is potentially going to be a CIP, however for now it's a solution I'll be implementing in the MintedWithLovelace dApp.
