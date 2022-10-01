# Guide to Minting on Cardano Using MintedWithLovelace dApp Minted Beta
To learn more about our little community project, get help, etc, visit us on [Discord](https://discord.gg/HzKvRWPqy5)


## Introduction to Minting on the Cardano Blockchain

An NFT, or any native token being minted on Cardano, has the following attributes:
- Quantity
- A Policy ID
- A Token Name
- Metadata

The Policy ID is generated using a script file which contains at a minimum the pubkey hash of a policy signing key. A policy signing key is part of a key pair, signing key and verification key, which are in nature the same as the key pair to generate a wallet/Cardano address. In practice however, a policy key pair is not typically used to generate a wallet/address, and solely used to generate a Policy ID. 

The Policy ID is a hash which represents all the parameters within the policy script used to generate the hash. For most NFTs, the script will contain two settings: The pubkey hash of the policy keys as mentioned above; The locking slot height.

In Cardano there is block height and slot height. A block is made up of many slots. Each slot represents about 1 second of time. A locking slot height is the number of slots at which a policy will "lock", meaning nothing can be minted or burned for that policy ID. For example, at the time of writing this guide the Testnet slot height is 53240583. If I wanted to create a Policy ID which locks in 1 month time from now, I would calculate the number of seconds in a typical month `(60*60*24*30)`, which is about 2,492,000 seconds...or in this case slots. I would then add this to the current slot height, giving me a target slot height for locking of 53240583.

The Asset Name is simply the unique name, on chain, for a token. The token could be an NFT or a "coin", depending on the quantity. To qualify to be categorized as an NFT, a given token with a particular asset name and policy ID combination, would need to have a quantity of only 1, being totally unique. As long as the Policy ID is still not locked, in other words we haven't reached the locking slot height on the blockchain if one was set in the policy script, then the policy key holder is able to mint or burn for that policy...meaning you could mint a duplicate NFT and now it's no longer an NFT but rather just a token or "coin".

On the Cardano blockchain a token, whether NFT or other, is represented as policyID.tokenName. The tokenName portion of this name onchain is represented in Hex. For example, if you had an NFT named MyNFT002 and a policy ID of 123456, the name would be hexed and the final onchain name of this NFT would be: 123456.4d794e4654303032 (note: in some representations the "." dot is not used)

This "full" name can also be represented as an asset name, for example here is an NFT with asset name "asset1ujuq77zjerm6xz25sryuspre7umgen7wmtf3dp" which represents the policy.tokenname of: e6363c3c57db3330b3947e106e53e77c8a11558a767df29a74762f79.43415264616e6f34535045454432333034

The hex name of 43415264616e6f34535045454432333034 converts to the string name of CARdano4SPEED2304. You can see this particular NFT on the blockchain via pool.pm/asset1ujuq77zjerm6xz25sryuspre7umgen7wmtf3dp.

The last requirement for any token minting is Metadata for the token. You can have essentially no metadata if you wanted, or any custom data you choose for any token, and this is defined in a JSON file which is included in the minting transaction. All that is required in this json file is the token type for a native token, which is 721...the policy ID for the given token being minted...and of course the token name, inside the json as plain text. The hexed version of the token name is only onchain.

Your metadata should have a schema to it which you've designed for your NFT project. For example, you might have some custom attributes under an "Attributes" category, with consistent names to make it easy for NFT markets to read the unique attributes of each NFT beyond just the name and image.

The metadata json file is also where any image or other media representing the NFT or token is defined. For example, to mint an NFT with an IPFS image associated with it, the json file would need a field for "image" with the ipfs hash in the value field, for example: "image": "ipfs://someIPFShashForTheImage"

The metadata can also include the image directly if you embed the image as SVG code into the json file. In other words, everything in the json file will be embedded into the blockchain directly, without relying on ipfs or some other non-blockchain resource. To do this, the svg code would need to be converted to base64 and embedded into a "list" in the json, broken into 64byte chunks. This applies to embedding HTML as well.

This is because each string field in a Cardano json file is limited to 64 bytes (64 characters include spaces etc), however for normal text you would not need to convert to base64. This same approach can be used to break up long strings, for example if a description is longer than 64 characters, you would break it into 64byte long strings and place each into a string within a list, which is defined by enclosing these strings inside [] and each line separated by commas. Further along in this guide is included a sample JSON file, where you can see this breaking up into chunks being used for a long string.

## Minting With Minted

Before you setup a campaign in Minted, it's important to prepare your NFTs. This involves preparing a JSON file for each NFT along with its asset (image file or other media file). 

Minted makes this a little easier by providing two options for setting up a new NFT campaign/project. You can either bring "mostly prepared" json files, each with it's associated image or other media file already uploaded to IPFS, pinned and hashed into the json file and the placeholder "POLICY_ID" in the policy ID position...or by allowing the dApp to do the uploading/pinning and hashing and updating of associated json files, your json files having the placeholder "IPFS_HASH" in place of the ipfs link.

For this guide, I'll walk through the latter, but if your json files are already populated with their associated ipfs media links, you can use the option to simply import your prepared json files.

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

Note that you will need an individual JSON file representative of each NFT with the opening "721" as seen in the sample.

Also, as you can see in the above JSON sample, there are 2 placeholders: POLICY_ID and IPFS_HASH

If you are letting Minted handle the IPFS uploading/pinning/hashing, then you would need these two placeholders in these locations with one exception: you may not be including an image as the media.  For example maybe you want to direct to a different type of file, such as text, audio, video, etc. In this case you can use a different identifier besides "image" which represents the filetype and is recognized/standardized.  For a little more reference on Cardano JSON standards, see https://cips.cardano.org/cips/cip25/

Also, if you want to utilize 2 + images in your NFT, you can do so by using the image field seen in the sample above to represent your NFT "thumbnail" image, and then for each additional image you would put them into a files list.

For the above example as a JSON file for use with Minted, you would need to also name this as: MintedWithLovelace01.json. Notice this is the same name seen above inside the file, directly following the POLICY_ID key. This same name is also what your image associated with this NFT/json should be named...for example MintedWithLovelace.png might be the image to go along with this particular NFT.

It is best practice (and in some cases in the dapp is required) to name your files to match the corresponding asset each represents. For example, if your NFTs are named SuperRat with a number that contains leading zeros, like 001, 002, etc., then you'd have a JSON file for SuperRat001 named SuperRat001.json and if pinning the images, let's say png files, with the dapp, the image file for this particular NFT would be called SuperRat001.png. So each NFT should have at minimum a JSON file by that NFTs onchain name.

Once you have your JSON and image files configured and named accordingly, make sure to separate them into two folders, a folder containing ONLY your prepared json files and a folder containing ONLY your associated image files. In the example for this guide, we'll have 20 NFTs prepared in 20 json files and 20 related image png files, in their own distinct folders as follows:

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

Minted sets up a hidden folder located in your home folder for the user you are logged in as when you run Minted. The folder is located at `/home/YOUR_USER/.MintedWithLovelace/` and within it is the dapp DB file and a folder called "campaigns", within it a folder for each campaign you setup in Minted. Each campaign's folder contains a mainnet and testnet folder, where network-related logs and some settings, such as plugins, scripts, whitelists, etc are kept, and which both contain a copy of your keys and your json files queued for minting, as well as other config and logging files and folders. To backup your keys after setting up a campaign, navigate to that campaigns folder and simply copy the "keys" folder to a secure location.

Because of this layout and the ability to import keys, you can segment a project which shares a single Policy ID into several campaigns if you want. For example, you may want to run a presale with a whitelist, which only has certain NFTs available for minting...say NFTs 01 through 10. You would setup this campaign, place the desired JSON files and if also hashing the images, those related images, into folders for importing to that campaign...then for the remaining NFTs you might setup a separate campaign but import the same policy keys and generate a new wallet address for the second campaign, then import jsons 11 through 20 for example into that second campaign.

Minted also takes care of 777 or "Royalty Token" minting. A royalty token is used as an on-chain mechanism for marketplaces to lookup your royalty settings over a given Policy ID.  For marketplaces to honor this token it needs to be minted as the first asset minted to a Policy ID and will contain the rate, e.g. 0.1 = 10%, and the payout address for royalties to be paid on any sale of any token minted to that policy ID. If you have already done this, you can choose not to mint a 777 during the setup of a campaign. If you are utilizing multiple campaigns for a single Policy ID, the first you setup you'd want to choose to mint the 777 token (so long as you want to in general, some people don't want to or don't care to do this)..and any subsequent campaigns sharing that same Policy ID, during setup you just don't choose to create it since you already did in the first campaign of the set.

After a campaign is setup the dApp exits and you are instructed to launch it again, after which you'll see the Dashboard for the campaign you select during dApp startup. The payment address for that particular campaign is displayed at the top of the Dashboard, along with the Policy ID for verification. If this is the first time you've run this campaign, and you chose to mint a 777 token, you can do so by choosing Option 2 to run the campaign.  This must be done in testnet and mainnet separately individually, as it will mint into the network exclusively (testnet or mainnet depending on which is running now).

After you have minted any 777 token and are ready to launch your campaign, you can do so using the Dashboard and option 2 again, or if you prefer to mute output and/or run the campaign in the background, you can do so by adding options to the dApp launch command like so for the network type you are running it on: 

For Testnet: `./Minted --option testnet --campaign YourCampaignName`
For Mainnet: `./Minted --option mainnet --campaign YourCampaignName`

Note: Replace ./Minted with the named version of the dapp you are runnint, e.g. `./MintedBetaRC20 ...`

Whether running Minted from the Dashboard or in the background as above, logging is sent to...for testnet: `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet/testnet-run.log`...and for mainnet: `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/mainnet/mainnet-run.log`

Finished payments, for example after minting or refunding, are recorded in the following two log files: `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/tx_log.log` and `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/tx_payments.log`

Minted NFTs are taken from `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/minting/auto/queued/` and after processing and minting successfully are moved to `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/minting/auto/minted/` and minted assets are recorded/logged in `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/minting/auto/logs/minted.log`

The whitelist file you import, is found at `/home/YOU_USER/.MintedWithLovelace/campaigns/YourCampaignName/testnet(or mainnet)/whitelist.csv`. 

To create a whitelist file for importing into your campaign, follow this format, adding each wallet address on a new line accordingly: `address, nfts_total, nfts_per, tx_total`

Minted allows for both Whitelisting or Blacklisting. You only need to add 1 address from a wallet to whitelist the entire wallet. For example, Yoroi and Daedalus allow you to generate many addresses, all associated with 1 wallet. That 1 wallet will be whitelisted by adding just 1 of those addresses. 

Blacklisting also covers an entire wallet and you don't need to manually edit the blacklist file, during setup just choose how many payments are allowed before a wallet is blacklisted. Blacklisting will result in any payments sent beyond the limit being automatically refunded.

Once everything is setup and the campaign is run, you can simply share the campaigns payment address with your audience and when they send the appropriate payment the dApp handles everything automatically, including minting, refunding, and ending the campaign when NFTs run out. Refunds occur when someone pays who is either not whitelisted if the whitelist is active, tries to send more payments than allowed per blacklist limits, sends an amount below what is required to mint, or sends a payment and includes other tokens in their payment. 

If a person sends a little too much, for example if your NFTs are 10 ADA each and a person sends 15 ADA, they are minted the 1 NFT and refunded the 5 additional ADA in the same return TX. If your campaign allows multiple NFTs, either statically set or dynamic based on the amount received, and there are not enough remaining NFTs to mint, the user will receive the maximum available and the campaign will end. An ended campaign switches to "refund only" mode and automatically refunds any additional payments received after running out of NFTs.

#### Setting Up a Minting Campaign

>> Select Campaign Type
Choose whether to setup a minting campaign or a swap-of-existing token campaign. For this walkthrough we are choosing option 1 - Mint

>> Select Mint Campaign Format
In most cases you will choose a normal "Mint-on-Demand Campaign", which we'll choose in this walkthrough: 1 - Mint-on-Demand Campaign. Minted also has the functionality for an on-chain auction style campaign or "biddables", wherein payments sent are considered "bids" and remain at the wallet until the auction end is reached, and a winning payment is accepted, refunding the others.

>> Name This Campaign
Create an internal-use/admin-use name for this campaign. This is only used in the dapp.

>> MAINNET cardano-cli Filepath and TESTNET cardano-cli Filepath
These fields are most likely the same and if you followed a common guide to setup your node, you should be able to enter `/usr/local/bin/cardano-cli` for both network types

>> Override TESTNET_MAGIC_NUM
Most likely option 0 - No.  If you have a test environment and have this system variable set, you can override it if you've switched to a different subtype (preprod vs preview for example)

>> Blockfrost MAINNET API Key
This is for the Cardano Mainnet only. If you are only setting up a test campaign you can bypass this by entering any text

>> Blockfrost TESTNET API Key
This is the testnet-type specific key (e.g. preview..., preprod..., etc) if you are setting up for a testnet environment...if not, you can bypass this by entering any text here

>> Which System is This
Choose the type of instance you're running now. Each time you launch the dapp manually you'll be prompted, allowing your entire dapp folder to be 'portable' between systems. This option is also accessible when running the dapp headless with switch --option mainnet / --option testnet

>> Choose Testnet Type
When running on testnet you are prompted for the type of testnet here, and each time you launch the dapp on your testnet system.  

>> Whitelisting Options
Choose whether to import a whitelist.csv file or have no whitelist for this campaign. Your whitelist file must be in the format: `address, total_nfts_allowed, nfts_per_payment, total_payments_allowed`. When importing, you'll need to enter the full path to the actual whitelist.csv file itself, for example: `/home/username/Documents/whitelist.csv`

>> Use a Blacklist?
If you did not import a whitelist, you have the option to setup a blacklist. This is simply setting the number of payments allowed before blacklisting a wallet.

>> Select Campaign Style?
This option is preselcted until the Airdrops feature is ready!

>> Choose Wallet/Policy Type
Here you can choose to import an existing policy key, wallet and policy keys, or to generate everything new. 

>> MAINNET locking slot height & TESTNET locking slot height
For these two settings, the slot height you enter will be used for generating a slot-locking policy, which locks at the slot you enter. If you want to generate an 'open' policy, which never locks, enter 290 for the field you're setting here.

>> Choose an option for royalty control
Minted can automatically create your 777 Royalty Token, which mints to the same policy ID and must be minted first to be considered valid by most markets. You can also of course skip this step if you don't wish to have a 777 Royalty Token set or if you already minted this token using another method and have imported that policy id into this campaign. If you do proceed to mint the 777, you'll choose option 1 and first enter the Royalty Rate, which is the % you want to set. For 2% you would simply enter a 2 here; for 20% you'd enter 20, etc. After you enter the % you want, the dapp will verify this with a decimal formatted %, and if it looks right confirm. Lastly, you'll be prompted to enter the payout address for royalties. Only 1 is possible at this time.

>> Choose a Minting Sort Type
Choose the order your NFTs should mint.

>> Import and Enable a Plugin?
For most scenarios you'll choose 0 - No, unless you have a custom built Python plugin you are utilizing

>> Import JSON files *OR* Import JSON files & IPFS Pin Image files?
Here you'll choose whether to import your campaign's prepared files, whether only json or both json and images for pinning. Choosing 1 - Yes is most likely for most scenarios (and what we'll assume for this walkthrough), as if you choose no you'll need to manually place your prepared files later or be utilizing a plugin which handles json files.

>> Prepared JSONs with "POLICY_ID" placeholder *OR* Prepared JSONs w/ "POLICY_ID" and "IPFS_HASH" placeholders
Here choose whether you have JSONs with your image IPFS hashes already populated or need to perform the pinning using Minted. For this walkthrough we'll assume you are pinning also and will cover those steps next.

>> JSON Files First / Folder Path Containing ONLY Your Prepared JSON Files
First you'll be prompted to have your JSON files prepped and in a folder by themselves, no other files or folders in that folder. Then provide the full path to that folder.

>> Full path to the FOLDER containing your images (ONLY your images should be in this folder)
Next you'll be prompted for the images folder, again no other files or folders in that folder and the image files must be named to match the corresponding name of your json files, e.g. MyNFT01.json would have a corresponding image file of MyNFT01.png or .jpg, etc. Again, here enter the full path to that folder.

>> Select Pinning Service
Here you'll choose the IPFS pinning service you wish to use. For this walkthrough I'll cover Blockfrost, but the method is fairly similar with each of these. Note: pinning using the dapp only works with the Image tag in your json files, not any additional files-image tags you've set, if you have additional images to set in your JSON in a files list, you should do this pinning yourself and these fields should already be properly populated per NFT.

>> Blockfrost IPFS API Key
You'll need to setup a new project in your Blockfrost account which is IPFS specific. This is a unique key from your testnet or mainnet blockfrost API project keys. If using a free Blockfrost account, you'll have to choose whether to use it for IPFS or Cardano, as it limits you to 1 key.  Make sure this key is correct or the dapp may crash!  Note: After pinning completes, your imported and updated json files are now in the queued folder as previously mentioned.

>> Blockfrost Pinning 
When you first enter your key, the dapp will test it and generate a resulting JSON sample from your JSONs for you to inspect and approve.  If anything looks incorrect, you can exit here and try again. At this stage the dapp will now take the time to pin your images. This may take a while depending on how many images you have to pin and how large they are, your internet speed, etc.  It's best to not interrupt this process unless you know what you're doing.  After complete you'll have a message with the results of how many files processed successfully.

>> Choose Mint Quantity Type
You can choose to have your campaign auto-mint based on a price per-NFT model, or mint a static qty per mint transaction (not per-price based).  For this walkthrough we'll go with option 1, Dynamic.

>> Maximum NFTs Allowed to Mint in a Single Sale
Here you will choose the max-per-sale, so if you setup a dynamic mint price of say 10 ADA per 1 NFT, and enter 10 here, someone sending 120 ADA would still be capped at 10 NFTs and would be refunded the 20 ADA overage.

>> Amount of Lovelace to Include per Mint TX
This is the amount of ADA the autominter should send back along with the NFTs...this is considered the "minimum amount" as the dapp will adjust this price as required by the network, so entering 1500000 (1.5 ADA) here may end up as 2 ADA if the number of NFTs minted requires the higher ADA amount. Note: Be sure to enter the values in lovelace representation, e.g. 1.5 ADA is entered here as 1500000  (6 decimal places).  If you enter a number which is considered "unsafe" as it's too low, the dapp will warn you and you can try again.

>> Choose Return ADA Processing Format
Most scenarios will use option 1 here, as option 2 would send back your included ADA amount for each NFT being minted.

>> Expected Payment in Lovelace per NFT
Here you set the "price" of each NFT for dynamic autominting. If you enter 40 for example, for every 40 ADA received from an address, 1 NFT will be minted up to the limit.  If 50 ADA were received the 10 extra is refunded along with the mint. Remember to enter this price in lovelace format, so 40 ADA would be entered as 40000000 (6 decimals)

>> Enter your FUNDING WALLET Mainnet Address / Testnet Address
This is the address you'll use to fund your campaign for things like minting your Royalty Token or if you are running a swap, etc.

>> Enter your PAYOUT ADDRESS for Mainnet / Testnet
This is the address where you'll be paid with each successful mint.

>> Choose Transaction Grouping Selection
Most scenarios you will choose option 1 - Enable TX Grouping, as this makes your mint campaign much more efficient in grouping together mints when possible.  Option 2 forces the dapp to mint to 1 buyer at a time and is useful if you need to attach tx metadata or other significant accounting needs that demand segmentation.

>> Enable Scripts?
You can optionally import a script to use with your campaign. Scripts or "action scripts" allow you to create a complex set of rules and filters for your campaign, even checking buyer's wallet holdings for specific assets, etc.

>> Enable Minted as a Service?
This allows you to tack on another fee per-mint. This can also be utilized if you need to split payouts to more than 1 address.

After this, your campaign is setup and the dApp will exit. You can launch again and will have the option to Manage Existing Campaign, where you can view details for the campaign, access the wallet to manage funds, manually mint, launch the campaign from the Dashboard, retry any failed IPFS uploads if failures occurred, and reconfigure a campaign to change the payout/funding addresses, or other details for the campaign.

When the campaign Dashboard is opened, the auto campaign is not running necessarily (unless you launched it in the background). So payments received to a "paused" campaign like this, are not auto processed unless/until you either run the live campaign from within the Dashboard or from the command line. Because of this, it's important to be careful when managing the wallet if the campaign address has been shared, so as not to disturb any payments waiting to be processed. The wallet is segmented to help you know what payments are from your Funded wallet vs a user payment, etc.

Minted has a simple fee structure which is 1 ADA per automint transaction, which goes to pay for development work and support other community efforts going forward. This fee is taken in realtime, there are no upfront fees or charges to use the dApp. Manual minting, royalty mints, and refunds do not incur fees.

#### Further Help and Support

To explore other features, scenarios, get help and support, etc, please join the [official MintedWithLovelace discord](https://discord.gg/HzKvRWPqy5)
