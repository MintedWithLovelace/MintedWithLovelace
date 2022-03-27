# Guide to Minting on Cardano Using MintedWithLovelace dApp Minted
To learn more about our little community project, get help, etc, visit us on [Discord](https://discord.gg/2xEVRTSAeQ)

## Introduction to Minting on the Cardano Blockchain

An NFT, or any native token being minted on Cardano, has the following attributes:
- Quantity
- A Policy ID
- A Token Name
- Metadata

The Policy ID is generated using a script file which contains at a minimum the pubkey hash of a policy signing key. A policy signing key is part of a key pair, signing key and verification key, which are in nature the same as the key pair to generate a wallet/Cardano address. In practice however, a policy key pair is not typically used to generate a wallet/address, and solely used to generate a Policy ID. 

The Policy ID is a hash which represents all the parameters within the policy script used to generate the hash. For most NFTs, the script will contain two settings: The pubkey hash of the policy keys as mentioned above; The locking slot height.

In Cardano there is block height and slot height. A block is made up of many slots. Each slot represents about 1 second of time. A locking slot height is the number of slots at which a policy will "lock", meaning nothing can be minted or burned for that policy ID. For example, at the time of writing this guide the Testnet slot height is 53240583. If I wanted to create a Policy ID which locks in 1 month time from now, I would calculate the number of seconds in a typical month (60*60*24*30), which is about 2,492,000 seconds...or in this case slots. I would then add this to the current slot height, giving me a target slot height for locking of 53240583.

The Asset Name is simply the unique name, on chain, for a token. The token could be an NFT or a "coin", depending on the quantity. To qualify to be categorized as an NFT, a given token with a particular asset name and policy ID combination, would need to have a quantity of only 1. As long as the Policy ID is still not locked, in other words we haven't reached the locking slot height on the blockchain, then the policy key holder is able to mint or burn for that policy...meaning you could mint a duplicate NFT and now it's no longer an NFT but rather just a token or "coin".

On the Cardano blockchain a token, whether NFT or other, is represented as policyID.tokenName. The tokenName portion of this name onchain is represented in Hex. For example, if you had an NFT named MyNFT002 and a policy ID of 123456, the name would be hexed and the final onchain name of this NFT would be: 123456.4d794e4654303032

This "full" name can also be represented as an asset name, for example here is an NFT with asset name "asset1ujuq77zjerm6xz25sryuspre7umgen7wmtf3dp" which represents the policy.tokenname of: e6363c3c57db3330b3947e106e53e77c8a11558a767df29a74762f79.43415264616e6f34535045454432333034

The hex name of 43415264616e6f34535045454432333034 converts to the string name of CARdano4SPEED2304. You can see this particular NFT on the blockchain via pool.pm/asset1ujuq77zjerm6xz25sryuspre7umgen7wmtf3dp.

The last requirement for any token minting is Metadata for the token. You can have essentially no metadata if you wanted, or any custom data you choose for any token, and this is defined in a JSON file which is included in the minting transaction. All that is required in this json file is the token type for a native token, which is 721...the policy ID for the given token being minted...and of course the token name, inside the json as plain text. The hexed version of the token name is only onchain.

The metadata json file is also where any image or other media representing the NFT or token is defined. For example, to mint an NFT with an IPFS image associated with it, the json file would need a field for "image" with the ipfs hash in the value field, for example: "image": "ipfs://someIPFShashForTheImage"

The metadata can also include the image directly if you embed the image as SVG code into the json file. In other words, everything in the json file will be embedded into the blockchain directly, without relying on ipfs or some other non-blockchain resource. To do this, the svg code would need to be converted to base64 and embedded into a "list" in the json, broken into 64byte chunks. This applies to embedding HTML as well.

This is because each string field in a Cardano json file is limited to 64 bytes (64 characters include spaces etc), however for normal text you would not need to convert to base64. This same approach can be used to break up long strings, for example if a description is longer than 64 characters, you would break it into 64byte long strings and place each into a string within a list, which is defined by enclosing these strings inside [] and each line separated by commas. Further along in this guide is included a sample JSON file, where you can see this breaking up into chunks being used for a long string.

## Minting With Minted

Before you setup a campaign in Minted, it's important to prepare your NFTs. This involves preparing a JSON file for each NFT along with its asset (image file or other media file). 

Minted makes this a little easier by providing two options for setting up a new NFT campaign/project. You can either bring "fully prepared" json files, each with it's associated image or other media file already uploaded to IPFS, pinned and hashed into the json file...or by allowing the dApp to do the uploading/pinning and hashing and updating of associated json files. For this guide, I'll walk through the latter, but if your json files are already populated with their associated ipfs media links, you can use the option to simply import your prepared json files.

### Part 1 - Prep

#### Preparing Assets
Following is a sample JSON file which is prepped for importing both the json and performing the IPFS pinning/hashing:

```
{
	"721": {
		"POLICY_ID": {
			"MintedWithLovelace01": {
				"name": "Minted With Lovelace #01",
				"image": "IPFS_HASH",
				"category":"This is a 64 byte long description...limit per json string line.",
				"long string":[
					"Long strings can be broken into 64-byte long segments in a list",
					"such as this, where appropriate, and will be stitched together",
					"by the wallet or site viewing the metadata."
				],
				"attributes":[
					"Attribute One",
					"Another attribute",
					"And another"
				]
			}
		}
	}
}
```

As you can see in the above JSON sample, there are 2 placeholders: POLICY_ID and IPFS_HASH

If you are letting Minted handle the IPFS uploading/pinning/hashing, then you would need these two placeholders in these locations with one exception: you may not be including an image as the media.  For example maybe you want to direct to a different type of file, such as text, audio, video, etc. In this case you can use a different identifier besides "image" which represents the filetype and is recognized/standardized.  For a little more reference on Cardano JSON standards, see https://cips.cardano.org/cips/cip25/

For the above example as a JSON file for use with Minted, you would need to also name this as: MintedWithLovelace01.json. Notice this is the same name seen above inside the file, directly following the POLICY_ID key. This same name is also what your image associated with this NFT/json should be named...for example MintedWithLovelace.png might be the image to go along with this particular NFT. 

Once you have your JSON and image files configured and named accordingly, make sure to separate them into two folders, a json folder containing ONLY your prepared json files and an images folder containing ONLY your associated image files. In the example for this guide, we'll have 20 NFTs prepared in 20 json files and 20 related image png files, in their own distinct folders as follows:

![example_folder_layout](https://github.com/MadeWithLovelace/MintedWithLovelace/blob/main/dapp/img1-guide.png)

#### Planning Your Policy ID Params

Because your Policy ID is a hash of both your policy keys and the slot locking height, you will want to consider the following: 

1. Whoever has access to your policy keys, potentially has access to mint or burn tokens associated with any Policy ID generated using those keys. This means simply that you will want to both secure these keys and back them up so as not to lose them. These keys are different than your wallet keys, as we discussed above, but they still carry intrinsic value because of what they are capable of related to your mintings.

2. Once the slot height is reached onchain, the associated Policy ID for that slot locking height will no longer be able to be used to mint or burn tokens for that policy id.  While you can use the same policy keys to generate new/subsequent Policy IDs, they will be different from each other if their slot locking heights are different. This also means if you have an existing policy key pair and have previously generated a Policy ID with a slot locking height that is still in the future, you can import those keys into Minted and it will allow you to use the dApp to mint into that same Policy ID until it's locked.

So if you are planning to do an NFT drop which will last say, 2 weeks total (including any presales, etc), than you would want to make sure to choose a slot locking height which is far enough into the future to ensure minting is possible until the sales are completed. And if you wanted to have the ability to mint into that same policy ID after the sale, you would want to include that timeframe in your calculation as once the Policy ID is locked, it's locked permanently and cannot be minted into ever again. Refer to the introduction above for how to calculate a slot height.

#### Using Testnet

Before you launch a live campaign, you should first setup on Testnet and run some testing mintings to ensure everything works as you expect. When you setup Minted and choose testnet, it will also configure your mainnet settings to reflect what you put into the testnet settings. This is important to note as if everything tests correctly you will be able to either copy your entire hidden settings folder to the mainnet server or simply choose mainnet when launching the dApp after switching your node to mainnet on the same server. 

For slot height, I recommend using the same slot height for testnet as you will in mainnet. Testnet is always "behind" in terms of slot height count from mainnet, so it's no issue to use the same height on both networks, unless you need to test what happens when the slot height is reached...in which case you may want to use a lower slot height for testnet to run such a test.

Testnet also uses different hashes for your campaign address (the pay-to address) and for the policy ID. However, the wallet and policy keys are actually the same regardless of testnet or mainnet. This means when you import your own policy keys, you only need to import them once, regardless if you are operating in testnet or mainnet, and Minted will use the same keys on either network. This also applies to your json files when setting up, Minted will use them in both networks and you just setup one time.

#### Overview of Minted

Before we walk through the steps of setting up and launching a new campaign, a quick overview of how Minted works and where things are located. 

Minted "watches" a Cardano address which is either generated or imported (depending on your choice during setup) and this address is derived from the wallet key pair. Similar to policy key pairs, there are two files, the ".skey" and the ".vkey" files. This is true for both wallet and policy keys. The file you need to keep secured is the skey file in both cases. The vkey file is not at risk, if someone had this file they could only derive your address or policy ID, things which are public already.  The skey file, on the other hand, is used for signing transactions and should never be shared, ever.

Minted sets up a hidden folder located in your home folder for the user you are logged in as when you run Minted. The folder is located at `/home/YOUR_USER/.MintedWithLovelace/` and within it is the config file and a folder called "campaigns", within it a folder for each campaign you setup in Minted. Each campaign's folder contains a mainnet and testnet folder, where network-related logs are kept, and which both contain a copy of your keys and your json files queued for minting, as well as other config and logging files and folders. To backup your keys after setting up a campaign, navigate to that campaigns folder and simply copy the "keys" folder to a secure location. 

Because of this layout and the ability to import keys, you can segment a project which shares a single Policy ID into several campaigns if you want. For example, you may want to run a presale with a whitelist, which only has certain NFTs available for minting...say NFTs 01 through 10. You would setup this campaign, place the desired JSON files and if also hashing the images, those related images, into folders for importing to that campaign...then for the remaining NFTs you might setup a separate campaign but import the same policy keys and generate a new wallet address for the second campaign, then import jsons 11 through 20 for example into that second campaign.

Minted also takes care of 777 or "Royalty Token" minting. A royalty token is used as an on-chain mechanism for marketplaces to lookup your royalty settings over a given Policy ID.  For marketplaces to honor this token it needs to be minted as the first asset minted to a Policy ID and will contain the rate, e.g. 0.1 = 10%, and the payout address for royalties to be paid on any sale of any token minted to that policy ID. If you have already done this, you can choose not to mint a 777 during the setup of a campaign. If you are utilizing multiple campaigns for a single Policy ID, the first you setup you'd want to choose to mint the 777 token (so long as you want to in general, some people don't want to or don't care to do this)..and any subsequent campaigns sharing that same Policy ID, during setup you just don't choose to create it since you already did in the first campaign of the set.

After a campaign is setup the dApp exits and you are instructed to launch it again, after which you'll see the Dashboard for the campaign you select during dApp startup. The payment address for that particular campaign is displayed at the top of the Dashboard, along with the Policy ID for verification. If this is the first time you've run this campaign, and you chose to mint a 777 token, you can do so by choosing Option 2 to run the campaign.  This must be done in testnet and mainnet separately individually, as it will mint into the network exclusively (testnet or mainnet depending on which is running now).

After you have minted any 777 token and are ready to launch your campaign, you can do so using the Dashboard and option 2 again, or if you prefer to mute output and/or run the campaign in the background, you can do so by adding options to the dApp launch command like so for the network type you are running it on: 

For Testnet: `./Minted --option testnet --campaign YourCampaignName`
For Mainnet: `./Minted --option mainnet --campaign YourCampaignName`

Whether running Minted from the Dashboard or in the background as above, logging is sent to...for testnet: `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet/testnet-run.log`...and for mainnet: `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/mainnet/mainnet-run.log`

Finished payments, for example after minting or refunding, are recorded in the following two log files: `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/tx_log.log` and `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/tx_payments.log`

Minted NFTs are taken from `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/minting/auto/queued/` and after processing and minting successfully are moved to `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/minting/auto/minted/` and minted assets are recorded/logged in `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/minting/auto/logs/minted.log`

The whitelist file, which you'll need to fill with any addresses to whitelist, is found at `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/whitelist.txt`.

Minted allows for both Whitelisting or Blacklisting. For whitelisting, you'll need to edit the whitelist.txt file and add each wallet-address in a separate line as many times as you'd like that wallet to be allowed to send a payment. So if you want to allow a whitelisted wallet to pay 3 times maximum, you'd add them to the whitelist 3 times. You only need to add 1 address from a wallet to whitelist the entire wallet. For example, Yoroi and Daedalus allow you to generate many addresses, all associated with 1 wallet. That 1 wallet will be whitelisted by adding just 1 of those addresses. 

Blacklisting also covers an entire wallet and you don't need to manually edit the blacklist file, just choose how many payments are allowed before a wallet is blacklisted, during setup. Blacklisting will result in any payments sent beyond the limit being automatically refunded.

Once everything is setup and the campaign is run, you can simply share the campaigns payment address with your audience and when they send the appropriate payment the dApp handles everything automatically, including minting, refunding, and ending the campaign when NFTs run out. Refunds occur when someone pays who is either not whitelisted if the whitelist is active, tries to send more payments than allowed per blacklist limits, sends an amount below what is required to mint, or sends a payment and includes other tokens in their payment. 

If a person sends a little too much, for example if your NFTs are 10 ADA each and a person sends 15 ADA, they are minted the 1 NFT and refunded the 5 additional ADA in the same return TX. If your campaign allows multiple NFTs, either statically set or dynamic based on the amount received, and there are not enough remaining NFTs to mint, the user will receive the maximum available and the campaign will end. An ended campaign switches to "refund only" mode and automatically refunds any additional payments received after running out of NFTs.

#### Setting Up a Campaign

1. Naming your campaign
This is just for internal purposes and is used when launching Minted as a background service as outlined previously.

2. Next you are prompted for the path to your cardano-cli, in linux (or if you use our node script) this is usually: /usr/local/bin/cardano-cli

3. For testnet you are prompted for the magic number, at the time of this writing it is 1097911063 but you can verify this in the install script for a Cardano node.

4. Minted relies on Blockfrost for finding the sender's address for payments received. You can setup a free account with Blockfrost which allows up to 50K requests per day, however you can only choose either testnet or mainnet per email you register with, so keep in mind that you'll want 2 IDs for this.

5. Next you're asked what network you are on right now, it doesn't matter too much if you setup on a mainnet or testnet system, just make sure you choose the network of the system you are setting up on.  You can copy your hidden .MintedWithLovelace folder between systems and when you launch Minted it will see that folder and previously setup campaigns.

6. You can choose to use a Whitelist or allow anyone to participate with no whitelist. Whitelisting has 2 options, Always on or One time. Always on means you can add an address and that wallet is whitelisted without limit, meaning it won't be removed after a number of payments. One time allows you to add an address as many times as you want to allow that address to send a payment...add it 1 time, that's 1 payment allowed, add it 4 times, that means 4 payments are allowed before that address is removed and no longer whitelisted.

7. Blacklisting can be used with whitelisting or independently and if you activate it you can enter a number representing the number of payments allowed before any wallet is no longer allowed and is blacklisted.

8. Minted will autogenerate wallet and policy key pairs for you if you choose Generate New Wallet & Policy Keys, or you can import either just policy keys or both wallet and policy keys...either from your own key pairs or from another campaign previously created. See the previous section for the file location of keys for a given campaign.

9. Next you need to enter the locking slot height for mainnet and then testnet. As previously mentioned it's a good idea to enter the actual slot height you'll be using, for both networks, since Testnet runs "behind" it will make no difference, unless you want to use testnet to test what happens when you are very close to the locking height or hit it.

10. For roytalty control, you can either choose not to mint a 777 token, mint one, or already minted one for the policy ID (if you imported it for example from a policy where you already minted the 777 token either with Minted or another function). If you choose to mint your 777 token, you will be prompted for the royalty % you want applied to the entire policy ID and the payout address where royalty should be paid.

11. Next you can choose the order in which your NFTs will mint during the campaign, either in order (ascending or descending) or random.

12. Minted allows you to import your prepared JSONs which have their images already hashed and pinned to IPFS, or you can let Minted do this task for you. If you choose the latter option, you'll need your jsons and images ready, named identically to their pairing, and in separate folders (1 folder containing only all jsons; 1 folder containing only all images). If you have a JSON which associates to an image, their names must match...for example: MyNFT99.json / MyNFT99.png

12-a. If you are also using Minted to upload/pin/hash your image files, you'll be prompted to use either Blockfrost or Pinata, and will need to enter the appropriate keys when prompted. These are obtained within the account for the provider you selected.

12-b. After your JSON files have completed processing, they are now imported to the queued folder previously mentioned.

13. Next, choose either Dynamic or Static minting. Dynamic Minting allows the quantity of NFTs a user receives upon payment to be dynamically based on the amount they sent. Static means, everyone gets a set amount of NFT(s) after sending a specified amount of ADA. 

13-a. If you choose Dynamic, you'll need to enter the amount of ADA you want per-NFT and the max number of NFTs allowed to mint in a single payment. For example, maybe you want to limit to 4 NFTs max per mint and want 10 ADA per NFT. When a user sends 40 ADA they get 4, 30 ADA they get 3, 50 ADA they get 4 plus change.

13-b. Static means you define the precise number of NFTs to mint per payment and the amount expected...for example maybe 1 NFT for 20 ADA...if a user sends 10 ADA they are refunded, if they send 30 ADA they get 1 NFT and change.

14. Lastly, you'll be prompted for both a Funding and Payout address, for both mainnet and testnet. Your Funding address is added to an ignore list, meaning Minted will not process mints or refunds for that address and you can fund your campaign to do things such as manual minting, sending funds from your campaign address where needed (like to prove you own the address for registering at some secondary markets, etc).  The Payout address is used to send you profits from NFT sales. This occurs in realtime at the time of minting/processing incoming payments. 

After this, your campaign is setup and the dApp will exit. You can launch again and will have the option to Manage Existing Campaign, where you can view details for the campaign, access the wallet to manage funds, manually mint, launch the campaign from the Dashboard, retry any failed IPFS uploads if failures occurred, and reconfigure a campaign to change the payout/funding addresses, or other details for the campaign.

When the campaign Dashboard is opened, the auto campaign is not running necessarily (unless you launched it in the background). So payments received to a "paused" campaign like this, are not auto processed unless/until you either run the live campaign from within the Dashboard or from the command line. Because of this, it's important to be careful when managing the wallet if the campaign address has been shared, so as not to disturb any payments waiting to be processed. The wallet is segmented to help you know what payments are from your Funded wallet vs a user payment, etc.

#### Further Help and Support

To explore other features, scenarios, get help and support, etc, please join the [official MintedWithLovelace discord](https://discord.gg/2xEVRTSAeQ)

