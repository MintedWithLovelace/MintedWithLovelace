#!/usr/bin/python
import json
import cardano as tx
from sys import exit
from os import mkdir
from os.path import isfile, join as osjoin

def inputp(prompt, text):
    import readline
    def hook():
        readline.insert_text(text)
        readline.redisplay()
    readline.set_pre_input_hook(hook)
    result = input(prompt)
    readline.set_pre_input_hook()
    return result

def create_smartcontract(default_settings, custom_settings):
    import subprocess
    from os import chdir, remove as osremove, replace as osreplace
    from os.path import realpath, dirname
    # Default settings
    log = default_settings[5]
    cache = default_settings[6]

    # Custom settings
    api_uri = custom_settings[0]
    api_id = custom_settings[1]
    watch_addr = custom_settings[2]
    fee_token_string = custom_settings[3]
    filePre = custom_settings[4]
    tx_hash_in = custom_settings[5]
    tx_amnt_in = custom_settings[6]
    tx_time = custom_settings[7]
    approot = custom_settings[8]
    sc_path = custom_settings[9]
    src = custom_settings[10]
    pubkeyhash = custom_settings[11]
    price = custom_settings[12]
    
    # Replace the validator options
    template_src = src + 'src/' + 'template_SwapToken.hs'
    output_src = src + 'src/' + 'SwapToken.hs'
    with open(template_src, 'r') as smartcontract :
        scdata = smartcontract.read()
        smartcontract.close()
    scdata = scdata.replace('PUBKEY_HASH010101010101010101010101010101010101010101010', ' '.join(pubkeyhash.split()))
    scdata = scdata.replace('PRICE_00000000000000', ' '.join(price.split()))
    with open(output_src, 'w') as smartcontract:
        smartcontract.write(scdata)
        smartcontract.close()
    
    # Compile the plutus smartcontract
    approot = realpath(dirname(__file__))
    chdir(src)
    print("\nPlease wait while your SmartContract source is being compiled, this may take a few minutes . . .\n\n")
    data_build = subprocess.call(['cabal', 'build'], stdout = subprocess.PIPE)
    print('\nGenerating SmartContract Plutus Script . . .')
    data_run = subprocess.call(['cabal', 'run'], stdout = subprocess.PIPE)
    print("\nCheck the above output for any errors.")

    # Move the plutus file to the working directory
    osremove(output_src)
    osreplace(src + 'swaptoken.plutus', sc_path)
    SC_ADDR = tx.get_smartcontract_addr(default_settings, sc_path)

    print('\n================ Finished! ================\n > This SmartContract Address For Your Records Is (if testnet it will be different when you go mainnet): ' + SC_ADDR + '\n\n')
    exit(0)

def pin(config, file):
    pinned_hash = 'error'
    if config[0] == 0:
        ipfs_url = 'https://api.pinata.cloud/pinning/pinFileToIPFS'
        ipfs_data, ipfs_content_type = _encode_files(file, {})
        ipfs_headers = {"Content-Type": ipfs_content_type, "pinata_api_key": config[1][0],"pinata_secret_api_key": config[1][1]}
        ipfs_ret = rpost(ipfs_url, data = ipfs_data, headers = ipfs_headers)
        if 'status_code' not in ipfs_ret.json():
            pinned_hash = ipfs_ret.json()['IpfsHash']
    if config[0] == 1:
        ipfs_url = 'https://ipfs.blockfrost.io/api/v0/ipfs/'
        add_file = 'add'
        pin_file = 'pin/add/'
        ipfs_headers = {'project_id': config[1][0]}

        # Upload file
        ipfs_up_ret = rpost(ipfs_url + add_file, headers = ipfs_headers, files = file)
        if 'status_code' not in ipfs_up_ret.json():
            pinned_hash = ipfs_up_ret.json()['ipfs_hash']
            # Pin file
            ipfs_ret = rpost(ipfs_url + pin_file + pinned_hash, headers = ipfs_headers)
            if 'status_code' in ipfs_ret.json():
                pinned_hash = 'error'
            else:
                pinned_hash = ipfs_up_ret.json()['ipfs_hash']
    return pinned_hash

def process_ipfs(data):
    from requests.models.RequestEncodingMixin import _encode_files
    from requests import post as rpost
    from PIL import Image
    from os import listdir

    # Get list of files and sort it
    img_name_list = listdir(data[4])
    img_list = []
    total = 0
    success = 0
    errors = False
    for img_name in img_name_list:
        total += 1
        try:
            img_list.append(int(img_name.split('.')[0]))
        except:
            errors = True
            with open(pin_log, 'a') as pinlog:
                pinlog.write(str(total) + ',' + img_name + ',Error With Image Name - Must Be A Number\n')
                pinlog.close()
            continue
    img_list.sort()

    # Pin images and generate json files
    for i in img_list:
        nft_num = str(i)
        filename = data[4] + nft_num + '.' + data[3]

        # Pin image and get hash
        try:
            pinned_hash = pin(data[6], {'file': open(filename, 'rb')})
            err_txt = 'API Error'
        except:
            pinned_hash = 'error'
            err_txt = 'File Error: File Extensions Must Match Collection Type'
        if pinned_hash == 'error':
            errors = True
            with open(pin_log, 'a') as pinlog:
                pinlog.write(nft_num + ',' + pinned_hash + ',' + err_txt + '\n')
                pinlog.close()
            continue
        
        # Save all to JSON file
        with open(data[2], 'r') as jsonsrc:
            jsonData = jsonsrc.read()
            jsonsrc.close()
        n_short = data[0] + nft_num
        n_long = data[1] + ' #' + nft_num
        jsonData = jsonData.replace('000_NAME_000', ' '.join(n_short.split()))
        jsonData = jsonData.replace('000_NICENAME_000', ' '.join(n_long.split()))
        # Get attributes from image if set
        if data[5]:
            try:
                im = Image.open(filename)
            except:
                errors = True
                with open(pin_log, 'a') as pinlog:
                    pinlog.write(nft_num + ',' + pinned_hash + ',Error With Image File\n')
                    pinlog.close()
                continue
            im.load()
            img_meta = json.loads(im.info['Comment'])
            nft_atts = img_meta['attributes']
            if type(nft_atts) is list:
                nft_atts = json.dumps(nft_atts)
            jsonData = jsonData.replace('000_ATTS_000', ' '.join(nft_atts.split()))
        jsonData = jsonData.replace('000_IPFSHASH_000', ' '.join(pinned_hash.split()))
        out_json = data[8] + nft_num + '.template_json'
        with open(out_json, 'w') as jsonout:
            jsonout.write(jsonData)
            jsonout.close()

        # Update pinlog
        with open(pin_log, 'a') as pinlog:
            success += 1
            pinlog.write(nft_num + ',' + pinned_hash + ',Successful\n')
            pinlog.close()
    return str(success) + '/' + str(total) + ' pinned successfully', errors

def start_process_ipfs(log, json_out, input_ttype, input_itype):
    print(' ______________________________________________________________________')
    print('|                                                                      |')
    print('|   Welcome to the IPFS Image Preprocess and JSON Template Generator   |')
    print('|                                                                      |')
    print(' ----------------------------------------------------------------------\n')
    input_sname = input('Collection Short Name Prefix:')
    input_lname = input('Collection Friendly Name Prefix:')
    temp_path = MINTSRC + 'template_' + input_ttype + '.json'
    imgs_path = input('Full path to your ' + input_itype + ' images:')
    get_atts_input = input('Extract Attributes From Image Comment Meta? (Type True or False):')
    get_atts = True
    if get_atts_input == 'false' or get_atts_input == 'False':
        get_atts = False
    pinsvc = input('Pinning Service (0 = Pinata.cloud; 1 = BlockFrost.io:')
    if pinsvc == '0':
        p_key = input('Enter Your API Key:')
        p_skey = input('Enter Your API Secret Key:')
    elif pinsvc == '1':
        p_key = input('Enter Your API ID:')
        p_skey = ''
    else:
        print('Not a valid option! Exiting.')
        exit(0)
    api_config = [int(pinsvc), [p_key, p_skey]]
    print('\n\nProcessing . . . Do Not Interrupt This Process (which may take a while!)')
    return process_ipfs([input_sname, input_lname, temp_path, input_itype, imgs_path, get_atts, api_config, log, json_out])

def setup(logroot, profile_name='', reconfig = False, append = False):
    # Default and shared variables
    PROFILE_TYPE, NETWORK_INPUT, MAGIC_INPUT, CLI_PATH_INPUT, API_ID_INPUT, COLLATERAL_INPUT, WLENABLED_INPUT, WHITELIST_ONCE_INPUT, WATCH_SKEY_PATH_INPUT, WATCH_VKEY_PATH_INPUT, SMARTCONTRACT_PATH_INPUT, TOKEN_POLICY_ID_INPUT, TOKEN_NAME_INPUT, EXPECT_ADA_INPUT, MIN_WATCH_INPUT, PRICE_INPUT, TOKEN_QTY_INPUT, RETURN_ADA_INPUT, DEPOSIT_AMNT_INPUT, RECURRINGSTRING_INPUT, SC_ADA_AMNT_INPUT, WT_ADA_AMNT_INPUT, AUTO_REFUND_INPUT, FEE_CHARGE_INPUT, AUCTIONINPUT, MINT_SKEY, MINT_VKEY, MINT_POLICY_SKEY, MINT_POLICY_VKEY, MINT_NFT_NAME, MINT_NFT_QTY, MINT_TARGET_TIP, MINT_LAST_TIP, BLOCKTIMEINPUT, NFT_ADDR, SWAP_TYPE_INPUT = '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''

    # Minting Defaults
    AUCTIONEND = 0
    NFT_MINTED = False

    # Swap Defaults
    SWAP_TYPE = 1
    SMARTCONTRACT_PATH = ''
    COLLATERAL = 1
    DEPOSIT_AMNT = 0
    RECURRING = False
    SC_ADA_AMNT = 0
    PRICE = 0

    if reconfig:
        settings_file = 'profile.json'
        # Load settings
        load_profile = json.load(open(settings_file, 'r'))
        if len(profile_name) == 0:
            profile_name = list(load_profile.keys())[0]
        PROFILE = load_profile[profile_name]
        PROFILE_TYPE = int(PROFILE['type'])
        CLI_PATH_INPUT = PROFILE['cli_path']
        API_ID_INPUT = PROFILE['api']
        COLLATERAL_INPUT = PROFILE['collateral']
        TOKEN_POLICY_ID_INPUT = PROFILE['tokenid']
        TOKEN_NAME_INPUT = PROFILE['tokenname']
        EXPECT_ADA_INPUT = PROFILE['expectada']
        MIN_WATCH_INPUT = PROFILE['min_watch']
        WLENABLED_INPUT = PROFILE['wlenabled']
        WHITELIST_ONCE_INPUT = PROFILE['wlone']
        RETURN_ADA_INPUT = PROFILE['returnada']

        if PROFILE_TYPE == 0:
            WATCH_SKEY_PATH_INPUT = PROFILE['watchskey']
            WATCH_VKEY_PATH_INPUT = PROFILE['watchvkey']
            SMARTCONTRACT_PATH_INPUT = PROFILE['scpath']
            PRICE_INPUT = PROFILE['price']
            TOKEN_QTY_INPUT = PROFILE['tokenqty']
            DEPOSIT_AMNT_INPUT = PROFILE['deposit_amnt']
            RECURRINGSTRING_INPUT = PROFILE['recurring']
            SC_ADA_AMNT_INPUT = PROFILE['sc_ada_amnt']
            WT_ADA_AMNT_INPUT = PROFILE['wt_ada_amnt']
            AUTO_REFUND_INPUT = PROFILE['auto_refund']
            FEE_CHARGE_INPUT = PROFILE['fee_to_charge']
            SWAP_TYPE_INPUT = PROFILE['swap_type']

        if PROFILE_TYPE == 1:
            NFT_MINTED = PROFILE['nft_minted']
            MINT_SKEY = PROFILE['wallet_skey']
            MINT_VKEY = PROFILE['wallet_vkey']
            MINT_POLICY_SKEY = PROFILE['policy_skey']
            MINT_POLICY_VKEY = PROFILE['policy_vkey']
            NFT_ADDR = PROFILE['nft_addr']
            NFT_DATA = PROFILE['nft_data']
            MINT_NFT_NAME = NFT_DATA[0]
            MINT_NFT_QTY = NFT_DATA[1]
            MINT_NFT_JSON = NFT_DATA[2]
            MINT_TARGET_TIP = NFT_DATA[3]
            MINT_LAST_TIP = NFT_DATA[4]
            MINT_TYPE = NFT_DATA[5]
            MINT_ORDER = NFT_DATA[6]
            COL_TYPE = NFT_DATA[7]
            AUCTIONINPUT = PROFILE['auction']
            BLOCKTIMEINPUT = PROFILE['blocktime']
        UNIQUE_NAME = profile_name
        
    
    print(' ---------------------------------------------------------------')
    print('|                                                               |')
    print('|                                                               |')
    print('|                Welcome to Minted With Lovelace                |')
    print('|                      ~ a Cardano dApp ~                       |')
    print('|                                                               |')
    print(' ---------------------------------------------------------------')
    print('|                                                               |')
    if not reconfig:
        print('|                     <<< Initial Setup >>>                     |')
        print('|                                                               |')
        print('|                 0 - Token Swap Profile                        |')
        print('|                 1 - Mint-On-Demand Profile                    |')
        print(' ---------------------------------------------------------------')
        RESPONSE = input('\nEnter Your Choice:')
        PROFILE_TYPE = int(RESPONSE)
        UNIQUE_NAME = input('\nEnter A Unique Profile Name For This Profile\n(no spaces, e.g. CypherMonk_NFT_Sale)\n >Unique Name:')
    else:
        print('|                   <<< Profile Reconfig >>>                    |')
        print('|                                                               |')
        print(' ---------------------------------------------------------------')
        print('\n        !!! WARNING !!!\nSettings for profile "' + profile_name + '" are about to be overwritten!\n\n')
        reconfig_confirm = input('Do you want to continue? (Type Yes or No):')
        if reconfig_confirm != 'yes' and reconfig_confirm != 'Yes' and reconfig_confirm != 'YES':
            print('\nOkay, exiting...')
            exit(0)
    print('\n---------------------------------------------------------------')
    print('                       *IMPORTANT NOTES*                       ')
    print('                   Read Before You Continue\n')
    print('If this is the first time running the setup, a profile.json file')
    print('will be created within this working directory which will store')
    print('this and any other profile settings. You can explicitely call')
    print('a given profile by appending the `--profile MyProfile` option')
    print('when running Minted With Lovelace dApp, replacing the MyProfile')
    print('name with the name of the profile you want to load.')
    print('\nFor new profiles, this will also generate a whitelist.txt file')
    print('in this profile directory eg: ./profiles/MyProfile/whitelist.txt.')
    print('If you enable whitelisting, please add the Cardano addresses you')
    print('would like whitelisted to this file, each on a new line. Note')
    print('that only 1 address from each whitelisted wallet with a complex')
    print('103-length address need be added to whitelist the entire wallet.\n')
    print('Default for all profiles is mainnet, if you want to run a profile')
    print('on testnet, simply run Minted With Lovelace with no switches and')
    print('you will have an option to load any profile and run on testnet.')
    print('\nBefore you setup a Mint-On-Demand profile, ensure your images')
    print('or other files (such as html if choosing this option) are ready')
    print('and in place in an accessible folder on this system.')
    print('---------------------------------------------------------------')
    input('                 >> press ENTER to continue <<')

    # Setup profile-specific cache and log folders
    log = osjoin(osjoin(logroot, UNIQUE_NAME), '')
    cache = osjoin(osjoin(log, 'cache'), '')
    txlog = osjoin(osjoin(log, 'txs'), '')
    try:
        mkdir(log)
    except OSError:
        pass
    try:
        mkdir(cache)
    except OSError:
        pass
    try:
        mkdir(txlog)
    except OSError:
        pass                

    CLI_PATH = inputp('\nExplicit Path To "cardano-cli"\n(leave empty if cardano-cli is in your system path and\nit is the version you want to use with this profile)\n >Cardano-CLI Path:', CLI_PATH_INPUT)
    API_ID = inputp('\nYour Blockfrost Mainnet API ID\n(should match the network-specific ID for mainnet - you can override with your testnet ID when testing)\n >Blockfrost Mainnet API ID:', API_ID_INPUT)
    WLENABLEDSTRING = inputp('\nUse a whitelist?\n(if false, any payment received to the watched address will be checked for matching amount params)\n >Enter True or False:', str(WLENABLED_INPUT))
    WLONESTRING = 'False'
    if PROFILE_TYPE == 0:
        if WLENABLEDSTRING == 'True' or WLENABLEDSTRING == 'true':
            WLONESTRING = inputp('\nRemove A Sender Address From Whitelist After 1 Payment is Received?\n >Enter True or False:', str(WHITELIST_ONCE_INPUT))

    # Process these inputs
    if len(CLI_PATH) == 0:
        CLI_PATH = 'cardano-cli'
    NETWORK = 'mainnet'
    API_URI_PRE = 'https://cardano-'
    API_URI_POST = '.blockfrost.io/api/v0/'
    WLENABLED = False
    WHITELIST_ONCE = False
    if WLENABLEDSTRING == 'True' or WLENABLEDSTRING == 'true':
        WLENABLED = True
    if WLONESTRING == 'True' or WLONESTRING == 'true':
        WHITELIST_ONCE = True

    if PROFILE_TYPE == 0:
        WATCH_SKEY_PATH = inputp('\nSigning Key File Path Of Watch Address\n(e.g. /home/user/node/wallets/watch.skey)\n >Watch Address .skey File Path:', WATCH_SKEY_PATH_INPUT)
        WATCH_VKEY_PATH = inputp('\nVerification Key File Path Of Watch Address\n(e.g. /home/user/node/wallet/watch.vkey)\n >Watch Address .vkey File Path:', WATCH_VKEY_PATH_INPUT)
        SWAP_TYPE_INPUT = inputp('\nSwap Type - 0 = SmartContract Swap; 1 = Standard Wallet Swap (no smartcontract)', SWAP_TYPE_INPUT)
        SWAP_TYPE = int(SWAP_TYPE_INPUT)
        if SWAP_TYPE_INPUT == '0':
            SMARTCONTRACT_PATH = inputp('\nSmart Contract Plutus File Path\n(path to the ".plutus" file - leave blank if you will be using the built-in simple token swap contract)\n >Smart Contract File Path:', SMARTCONTRACT_PATH_INPUT)
        TOKEN_POLICY_ID = inputp('\nToken Policy ID Of Token To Be Deposited for Swap\n(the long string before the dot)\n >Token Policy ID:', TOKEN_POLICY_ID_INPUT)
        TOKEN_NAME = inputp('\nToken Name Of Token To Be Deposited for Swap\n(comes after the dot after the policy ID)\n >Token Name:', TOKEN_NAME_INPUT)
        print('\n\nNOTE: The following amount responses should all be\n      entered in Lovelace e.g. 1.5 ADA = 1500000\n\n')
        print('\nANOTHER NOTE: When running a swap which allows a min amount and up at a dynamic token price \n(like 100 tokens per 1 ADA), keep in mind that you will be sending back 1.5 ADA with the swap.\nSo for example, if you setup to swap 100 to 1 and allow anything above 2 ADA to swap, you will\nonly be keeping .5 ADA from the swap and so your actual net-gain is 100 - .5 ADA in that case.')
        RETURN_ADA = inputp('\nAmount Of Lovelace To Include With Each Swap Transaction\n(cannot be below protocol limit)\n >Included ADA Amount in Lovelace:', RETURN_ADA_INPUT)
        EXPECT_ADA = inputp('\nAmount Of Lovelace To Watch For\n(this is the amount the dApp is watching the wallet for)\n >Watch-for Amount in Lovelace:', EXPECT_ADA_INPUT)
        MININPUT = '0'
        if not EXPECT_ADA:
            MININPUT = inputp('\nMinimum Amount of Lovelace To Watch For\n(minimum to watch for when watching for "any" amount)\n >Watch-for Min Amount in Lovelace:', str(MIN_WATCH_INPUT))
            TOKEN_QTY = inputp('\nDynamic Token Quantity (Per-ADA) To Be Sent In Each Swap Transaction\n(how many tokens-per-ADA to send with each successful matched transaction swap, e.g. putting 100 means 100 Tokens per 1 ADA sent by a user)\n >Tokens Per 1 ADA:', TOKEN_QTY_INPUT)
        else:
            TOKEN_QTY = inputp('\nStatic Token Quantity To Be Sent In Each Swap Transaction\n(how many tokens to send with each successful matched transaction swap)\n >Token Amount To Swap Per TX:', TOKEN_QTY_INPUT)
        if SWAP_TYPE_INPUT == '0':
            PRICE = inputp('\nPrice If Any To Be Paid To Watch Address\n(this is not the amount being watched for)\n >Price Amount in Lovelace:', PRICE_INPUT)
            COLLATSTRING = inputp('\nAmount Of Lovelace Collateral To Include\n(required for smartcontract tx, usually 2000000)\n >Collateral Amount in Lovelace:', str(COLLATERAL_INPUT))
            print('\n\nAfter this setup and any smart-contract generating, you will need to deposit into the smart contract by running: "python3 main.py --profile YOURPROFILE --option deposit". The following inputs are related to deposits. For auto-replenishing a smart-contract wherein you are sending a large amount to be processed in smaller batches, the token quantity you enter in the following input, will apply to each deposit replenish attempt.\n\n')
            DEPOSIT_AMNT = inputp('\nQuantity of Tokens You Will Deposit\n(you can enter a batch amount, when it runs low the app will try to replenish with the same batch amount)\n >Quantity of ' + TOKEN_NAME +' Tokens to Deposit:', DEPOSIT_AMNT_INPUT)
            RECURRINGSTRING = inputp('\nIs This A Recurring Amount?\n(type True or False)\n >Recurring Deposit? ', str(RECURRINGSTRING_INPUT))
            if RECURRINGSTRING == 'True' or RECURRINGSTRING == 'true':
                RECURRING = True
            SC_ADA_AMNT = inputp('\nAmount Of Lovelace To Be At UTxO On SmartContract\n(cannot be lower than protocol, 2 ADA is recommended for most cases)\n >Amount in Lovelace:', SC_ADA_AMNT_INPUT)
        WT_ADA_AMNT = inputp('\nAmount Of Lovelace To Be At UTxO Of Token Change At Watched Wallet\n(cannot be lower than protocol, 2 ADA is recommended for most cases)\n >Amount in Lovelace:', WT_ADA_AMNT_INPUT)
        AUTO_REFUNDSTRING = inputp('\nAutomatically Refund Payments Too Large?\n(type True or False - this will enable auto-refunds for payments which exceed the tokens ever available for swap by the SmartContract)\n >Refunds Enabled?', str(AUTO_REFUND_INPUT))
        FEE_CHARGEINPUT = inputp('\nFee To Charge For Refunds\n(rather than simply deducting a protocol fee, setting a higher fee discourages abuse and more attentive participation..if left blank default is 500000 lovelace)\n >Fee Charged For Refunds in Lovelace:', str(FEE_CHARGE_INPUT))
        if SWAP_TYPE == 0 and len(SMARTCONTRACT_PATH) == 0:
            SMARTCONTRACT_PATH = log + UNIQUE_NAME + '.plutus'
        if SWAP_TYPE == 0:
            COLLATERAL = int(COLLATSTRING)
        MIN_WATCH = int(MININPUT)
        if not FEE_CHARGEINPUT:
            FEE_CHARGEINPUT = "500000"
        FEE_CHARGE = int(FEE_CHARGEINPUT)
        AUTO_REFUND = False
        if AUTO_REFUNDSTRING == 'True' or AUTO_REFUNDSTRING == 'true':
            AUTO_REFUND = True
        
        # Save to dictionary
        rawSettings = {'type':PROFILE_TYPE,'log':log,'cache':cache,'txlog':txlog,'network':NETWORK,'cli_path':CLI_PATH,'api_pre':API_URI_PRE,'api_post':API_URI_POST,'api':API_ID,'collateral':COLLATERAL,'wlenabled':WLENABLED,'wlone':WHITELIST_ONCE,'watchskey':WATCH_SKEY_PATH,'watchvkey':WATCH_VKEY_PATH,'scpath':SMARTCONTRACT_PATH,'tokenid':TOKEN_POLICY_ID,'tokenname':TOKEN_NAME,'expectada':EXPECT_ADA,'min_watch':MIN_WATCH,'price':PRICE,'tokenqty':TOKEN_QTY,'returnada':RETURN_ADA,'deposit_amnt':DEPOSIT_AMNT,'recurring':RECURRING,'sc_ada_amnt':SC_ADA_AMNT,'wt_ada_amnt':WT_ADA_AMNT, 'auto_refund':AUTO_REFUND, 'fee_to_charge':FEE_CHARGE,'swap_type':SWAP_TYPE}

    if PROFILE_TYPE == 1:
        if reconfig:
            input('\nThis reconfiguration function will OVERWRITE your current json, svg, and html files!\n    >> press ENTER to continue <<')
            print('\nContinuing...\n\n')
        else:
            MINT_LAST_TIP = MINT_NFT_NAME = MINT_NFT_QTY = MINT_TYPE = MINT_ORDER = COL_TYPE = ''
        AUCTIONINPUT = inputp('\nAuction style 0 = Traditional Single NFT, Highest Cumulative Bid Wins in Timeframe; 1 = Guess the Secret Reserve Single NFT; 2 = No Auction/Indefinite Multi NFTs:', AUCTIONINPUT)
        AUCTION = int(AUCTIONINPUT)
        if AUCTION == 2:
            # Set Mint Type to multi/filebased
            MINT_TYPE = AUCTION
            mint_root = osjoin(osjoin(log, 'minting'), '')
            to_mint = osjoin(osjoin(mint_root, 'queued'), '')
            fail_mint = osjoin(osjoin(mint_root, 'failed'), '')
            succeed_mint = osjoin(osjoin(mint_root, 'minted'), '')
            try:
                mkdir(mint_root)
            except OSError:
                pass
            try:
                mkdir(to_mint)
            except OSError:
                pass
            try:
                mkdir(fail_mint)
            except OSError:
                pass
            try:
                mkdir(succeed_mint)
            except OSError:
                pass
        if AUCTION == 1 or AUCTION == 2:
            EXPECT_ADA = input('\nExact Amount Of Lovelace To Watch For\n(this is the amount the dApp is watching the wallet for, leave blank if watching for any amount over a certain amount)\n >Exact Watch-for Amount in Lovelace:')
            MININPUT = '0'
            if not EXPECT_ADA:
                MININPUT = input('\nMinimum Amount of Lovelace To Watch For\n(minimum to watch for when watching for "any" amount)\n >Watch-for Min Amount in Lovelace:')
        if AUCTION == 0:
            RESERVE = input('\nSet a Reserve Price? (Yes or No):')
            MININPUT = '0'
            EXPECT_ADA = ''
            if RESERVE == 'yes' or RESERVE == 'Yes':
                MININPUT = input('\nReserve Amount in Lovelace:')
            BLOCKTIMEINPUT = inputp('\nHours Until Auction Close?\n(default is 24 --Enter the hours number in which to end after the first good bid comes in) >Hours Til Close:', BLOCKTIMEINPUT)
            BLOCKTIME = (int(BLOCKTIMEINPUT) * 60) * 60
        print('\nChoose Wallet/Policy Type')
        print('    0 - Generate New Wallet & Policy Keys')
        print('    1 - Use Existing Wallet & Policy Keys')
        wallet_opt = input('\nEnter Your Choice:')
        wallet_opt = int(wallet_opt)
        if wallet_opt == 0:
            wallet_type = input('\nChoose the Network Type, 0 = Mainnet; 1 = Testnet:')
            wallet_type = int(wallet_type)
            temp_settings = ['','',CLI_PATH,NETWORK,temp_magic,'','','',False]
            temp_prepend = '_mainnet-mwl_'
            if wallet_type == 1:
                temp_magic = input('Enter Testnet Magic Number:')
                temp_settings = ['','',CLI_PATH,'testnet-magic',temp_magic,'','','',True]
                temp_prepend = '_testnet-mwl_'
            print('Attempting to create a wallet and policy for this profile . . .')
            key_root = osjoin(osjoin(log, 'keys'), '')
            try:
                mkdir(key_root)
            except OSError:
                pass
            pre_wallet = 'wallet'
            pre_policy = 'policy'
            key_name = '_' + UNIQUE_NAME
            new_wallet_addr = tx.create_keys(temp_settings, [pre_wallet, key_root + pre_wallet + temp_prepend + key_name])
            new_policy_hash = tx.create_keys(temp_settings, [pre_policy, key_root + pre_policy + temp_prepend + key_name])
            if new_wallet_addr == 'error' or new_policy_hash == 'error':
                print('Errors encountered while trying to generate wallet and policy key files and address/hash! Exiting . . .')
                exit(0)
            print('Success, please notate your new wallet address and policy hash:')
            if temp_prepend == '_testnet-mwl_':
                print('*Note: These keys work on testnet only. If you attempt to run on\nmainnet you will be prompted to create mainnet keys\n')
            print('    Wallet Address: << ' + new_wallet_addr + ' >>')
            print('    Policy Hash:    << ' + new_policy_hash + ' >>')
            print('    Path to Keys:   << ' + key_root + ' >>')
            input('\n                  >> press ENTER to continue <<')
        elif wallet_opt == 1:
            MINT_SKEY = inputp('\nEnter the Minting Wallet Signing Key (.skey) File Path:', MINT_SKEY)
            MINT_VKEY = inputp('\nEnter the Minting Wallet Verification Key (.vkey) File Path:', MINT_VKEY)
            #MINT_ADDR = tx.get_address_pubkeyhash(CLI_PATH, MINT_VKEY)
            #input('\nPress any key to verify that the mint address is correct/what you expect given the skey and vkey files: ' + MINT_ADDR)
            MINT_POLICY_SKEY = inputp('\nEnter the Minting Policy Signing Key (.skey) File Path:', MINT_POLICY_SKEY)
            MINT_POLICY_VKEY = inputp('\nEnter the Minting Policy Verification Key (.vkey) File Path:', MINT_POLICY_VKEY)
            POLICY_HASH = tx.get_address_pubkeyhash(CLI_PATH, MINT_POLICY_VKEY)
            input('\nPress ENTER to Verify the Policy Pubkey Hash Associated With Your Policy Keys\n    >> ' + POLICY_HASH + ' <<')
        else:
            print('Invalid option, exiting...')
            exit(0)
        
        NFT_ADDR = inputp('\nMint NFT at Winner Address? (type True or False)\n(recommend: True - otherwise it will mint at your watch/mint address and must be sent manually after auction end):', NFT_ADDR)
        if NFT_ADDR == 'True' or NFT_ADDR == 'true':
            NFT_ADDR = True
        RETURN_ADA = inputp('\nAmount Of Lovelace To Include With Minted Asset UTxO\n(cannot be below protocol limit)\n >Included ADA Amount in Lovelace:', RETURN_ADA_INPUT)
        
        # feature not yet supported WATCH_ADDR = inputp('\nWatch Address\n(change this ONLY IF watching an alternate address):', MINT_ADDR)
        if AUCTION == 0 or AUCTION == 1:
            MINT_NFT_NAME = inputp('\nNFT or Token Name:', MINT_NFT_NAME)
            MINT_NFT_QTY = inputp('\nQuantity (usually 1 for an NFT):', MINT_NFT_QTY)
        ENTER_TARGET = input('\nUse a Set Target Block Height?\n(Yes or No - Yes = Enter a target slot num; No = Enter a count which will be added to the height at time of minting):')
        if ENTER_TARGET == 'Yes' or ENTER_TARGET == 'yes':
            MINT_TARGET_TIP = inputp('\nEnter the block height slot number matching the profile you are wishing to mint into:', MINT_TARGET_TIP)
        else:
            MINT_LAST_TIP = inputp('\nHow Many Slots To Add Until Locked from Minting Height?\n(1 slot ~ 1 second)\nSlots to Add to Block Height at Minting Time:', MINT_LAST_TIP)
        TOKEN_POLICY_ID_INPUT = inputp('\nEnter the Mainnet Policy ID of a Token To Be Used in Fee Refunding and Alt Pay\n(leave empty to not refund fees or offer alt payment):', TOKEN_POLICY_ID_INPUT)
        if len(TOKEN_POLICY_ID_INPUT) > 0:
            TOKEN_NAME_INPUT = inputp('\nMainnet Token Name for Fee Refunding and Alt Pay\n(comes after the dot after the policy ID)\n >Token Name:', TOKEN_NAME_INPUT)
        else:
            TOKEN_POLICY_ID_INPUT = ''
            TOKEN_NAME_INPUT = ''
        COLLATERAL = 1
        MIN_WATCH = int(MININPUT)

        if AUCTION == 0 or AUCTION == 1:
            # Set Mint Type to single/one-off mint
            MINT_TYPE = 1
            # process inputs and prepare save data
            print('\nAll manually edit fields should be prepared AFTER this function generates your json file')

            # For passing address for visible string:
            SVG_HTML = input('\nEnter path to SVG file for HTML embedding:')
            SVG_IMG = input('\nIF DIFFERENT, enter path to SVG file for Image meta:')
            if not SVG_HTML:
                SVG_HTML = log + MINT_NFT_NAME + '.svg'
                print('\nPlace your SVG file for this NFT at the following location and name accordingly:',SVG_HTML)
                input('\nOnce the SVG image is in place, press any key to continue...')
            if not SVG_IMG:
                SVG_IMG = SVG_HTML
            # Get raw svg data to embed into HTML
            with open(SVG_HTML, 'r') as img:
                img_src = img.read()

            # Setup files
            template_html = MINTSRC + 'template.html'
            html_out = log + MINT_NFT_NAME + '-temp.html'
            template_json = MINTSRC + 'template_svg_html_onchain.json'
            out_json = log + MINT_NFT_NAME + '-temp.json'

            with open(template_html, 'r') as htmlsrc:
                htmlData = htmlsrc.read()
                htmlsrc.close()
            htmlData = htmlData.replace('__00_BGIMG_00__', ' '.join(img_src.split()))
            # Write to new html file in profile folder
            with open(html_out, 'w') as htmlout:
                htmlout.write(htmlData)
                htmlout.close()

            # Base64 HTML and SVG if applicable for image of NFT
            svg_json = json.dumps(tx.encode_to_base64(SVG_IMG, 'svg'))

            # Save all to JSON file
            with open(template_json, 'r') as jsonsrc:
                jsonData = jsonsrc.read()
                jsonsrc.close()
            jsonData = jsonData.replace('000_NAME_000', ' '.join(MINT_NFT_NAME.split()))
            if svg_json:
                jsonData = jsonData.replace('000_SVG_000', ' '.join(svg_json.split()))
            with open(out_json, 'w') as jsonout:
                jsonout.write(jsonData)
                jsonout.close()
            MINT_NFT_JSON = out_json
            print('\nYour NFT JSON file has been generated, before proceeding do any manual edits, leaving 000_POLICY_ID_000 in-tact for the next stages of processing. JSON File Located At: ', out_json)

        if AUCTION == 2:
            print('Choose a Minting Sort Type')
            print('    0 - Random Order')
            print('    1 - Ordered, Ascending')
            print('    2 - Ordered, Descending')
            MINT_ORDER = int(inputp('Enter Your Choice:',MINT_ORDER))
            print('Choose Type of Collection')
            print('    0 - PNGs on IPFS')
            print('    1 - SVG+HTML on Cardano (100% on-chain)')
            COL_TYPE = int(inputp('Enter Your Choice:',COL_TYPE))
            if COL_TYPE == 0:
                input_ttype = 'ipfsIMG'
                input_itype = 'png'
            elif COL_TYPE == 1:
                input_ttype = 'onchainSVGHTML'
                input_itype = 'svg'
            else:
                print('Not a valid option! Exiting')
                exit(0)
            ipfs_log = log + 'ipfs.log'
            json_out = to_mint
            MINT_NFT_JSON = [to_mint, fail_mint, succeed_mint]
            result, proc_err = start_process_ipfs(ipfs_log, json_out, input_ttype, input_itype)
            print('\nCompleted IPFS Pinning and JSON File Generating, Results: ' + result)
            if proc_err:
                print('Error(s) encountered. After setup completes, review the ipfs.log file within your profile folder, to find and correct issues with failed images and run the dapp with opts: `--profile ' + UNIQUE_NAME + ' --option ipfs_retry` to try again with only failed images. These must be corrected before running your campaign to avoid missing NFTs from the collection or other errors while minting.')
                input('Press any key to continue...')
                
        # Setup list for NFT data
        NFT_DATA = [MINT_NFT_NAME, MINT_NFT_QTY, MINT_NFT_JSON, MINT_TARGET_TIP, MINT_LAST_TIP, MINT_TYPE, MINT_ORDER, COL_TYPE]

        # Save to dictionary
        rawSettings = {'type':PROFILE_TYPE,'log':log,'cache':cache,'txlog':txlog,'collateral':COLLATERAL,'network':NETWORK,'cli_path':CLI_PATH,'api_pre':API_URI_PRE,'api_post':API_URI_POST,'api':API_ID,'expectada':EXPECT_ADA,'min_watch':MIN_WATCH,'wlenabled':WLENABLED,'wlone':WHITELIST_ONCE,'nft_addr':NFT_ADDR,'wallet_skey':MINT_SKEY,'wallet_vkey':MINT_VKEY,'policy_skey':MINT_POLICY_SKEY,'policy_vkey':MINT_POLICY_VKEY,'returnada':RETURN_ADA,'nft_data':NFT_DATA,'tokenid':TOKEN_POLICY_ID_INPUT,'tokenname':TOKEN_NAME_INPUT,'nft_minted':NFT_MINTED,'auction':AUCTION,'blocktime':BLOCKTIME,'auctionend':AUCTIONEND}

    # Save/Update whitelist and profile.json files
    settings_file = 'profile.json'
    is_set_file = isfile(settings_file)
    if not is_set_file:
        open(settings_file, 'x')
    if not append:
        if reconfig:
            reconfig_profile = json.load(open(settings_file, 'r'))
            reconfig_profile[UNIQUE_NAME] = rawSettings
            jsonSettings = json.dumps(reconfig_profile)
        else:
            writeSettings = {UNIQUE_NAME:rawSettings}
            jsonSettings = json.dumps(writeSettings)
    else:
        append_profile = json.load(open(settings_file, 'r'))
        append_profile[UNIQUE_NAME] = rawSettings
        jsonSettings = json.dumps(append_profile)
    with open(settings_file, 'w') as s_file:
        s_file.write(jsonSettings)
        s_file.close()

    # Setup and save whitelist file
    whitelist_file = log + 'whitelist.txt'
    is_wl_file = isfile(whitelist_file)
    if not is_wl_file:
        try:
            open(whitelist_file, 'x')
            if not WLENABLED:
                with open(whitelist_file, 'w') as wl_header:
                    wl_header.write('none')
                    wl_header.close()
        except OSError:
            pass
    else:
        if not WLENABLED:
            import shutil
            whitelist_bak = log + 'whitelist.bak'
            shutil.copyfile(whitelist_file, whitelist_bak)
            with open(whitelist_file, 'w') as wl_header:
                wl_header.write('none')
                wl_header.close()

    print('\n\n=========================     Profile Saved      =========================\nIf using more than 1 profile, run with this explicit profile with option\n"--profile ' + UNIQUE_NAME + '" e.g. `python3 main.py --profile ' + UNIQUE_NAME + ' --option process`.\n\nExiting . . . \n')
    exit(0)