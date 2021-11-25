import json
import cardano as tx
from getopt import getopt
from sys import exit, argv
from os import mkdir
from os.path import isfile, realpath, dirname, join as osjoin
from time import sleep, strftime, gmtime

def time_now():
    return strftime("%Y-%m-%d_%H-%M-%S", gmtime())

def log_it(msg, prt = DEBUG, err = False):
    pre = time_now() + ': Log: '
    if err:
        pre = time_now() + ': !ERROR!: '
    logmsg = '\n' + pre + msg
    if prt:
        print(logmsg)
    else:
        with open(RUNLOG_FILE, 'a') as runlog:
            runlog.write(logmsg)
            runlog.close()

def deposit(default_settings, custom_settings):
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
    watch_skey_path = custom_settings[8]
    smartcontract_addr = custom_settings[9]
    smartcontract_path = custom_settings[10]
    token_policy_id = custom_settings[11]
    token_name = custom_settings[12]
    deposit_amt = custom_settings[13]
    sc_ada_amt = custom_settings[14]
    ada_amt = custom_settings[15]
    datum_hash = custom_settings[16]
    check_price = custom_settings[17]
    collateral = custom_settings[18]
    tokens_to_swap = custom_settings[19]
    recipient_addr = custom_settings[20]
    replenish = custom_settings[21]

    submit_settings = [api_uri, api_id, watch_addr, fee_token_string, filePre, tx_hash_in, tx_amnt_in, tx_time]

    # Clear cache
    tx.clean_folder(default_settings)
    tx.proto(default_settings)
    tx.get_utxo(default_settings, watch_addr, 'utxo.json')
    
    # Get wallet utxos
    utxo_in, utxo_col, tokens, flag, _ = tx.get_txin(default_settings, 'utxo.json', collateral)

    # Check for price amount + other needed amounts in watched wallet
    if check_price > 0:
        price = check_price
        fee_buffer = 1000000
        check_price = int(check_price) + int(sc_ada_amt) + int(ada_amt) + int(collateral) + fee_buffer
        if not replenish:
            log_it('Check Price now: ' + str(check_price), True)
        is_price_utxo = tx.get_txin(default_settings, 'utxo.json', collateral, False, '', int(check_price))
        if not is_price_utxo:
            if not replenish:
                log_it('Not enough ADA in your wallet to cover Price and Collateral. Add some funds and try again.', True)
                exit(0)

    if not flag: # TODOdeposit: Test with different tokens at bridge wallet
        filePreCollat = 'collatRefill_' + timenow() + '_'
        if not replenish:
            log_it('No collateral UTxO found! Attempting to create...', True)
        _, until_tip, block = tx.get_tip(default_settings)
        # Setup UTxOs
        tx_out = tx.process_tokens(default_settings, tokens, watch_addr, ['all'], ada_amt) # Process all tokens and change
        tx_out += ['--tx-out', watch_addr + '+' + str(collateral)] # Create collateral UTxO
        if not replenish:
            log_it('TX Out Settings for Creating Collateral: ' + tx_out, True)
        tx_data = ['']
        tx.build_tx(default_settings, watch_addr, until_tip, utxo_in, utxo_col, tx_out, tx_data)
        
        # Sign and submit the transaction
        witnesses = [
            '--signing-key-file',
            watch_skey_path
        ]
        tx.sign_tx(default_settings, [witnesses, filePreCollat])
        if not replenish:
            log_it('Submitting and waiting for new UTxO to appear on blockchain...', True)
        catch_err, tx_hash_collat = tx.submit_tx(default_settings, submit_settings)
        log_it('Transaction submitted, results: ' + tx_hash_collat, DEBUG, catch_err)
        if not replenish:
            log_it('Transaction submitted, results: ' + tx_hash_collat, True, catch_err)
    
    # Build, sign, and send transaction
    if flag is True:
        _, until_tip, block = tx.get_tip(default_settings)
        
        # Calculate token quantity and any change
        tok_bal = 0
        sc_out = int(deposit_amt)
        tok_new = 0
        for token in tokens:
            if token == 'lovelace':
                continue
            for t_qty in tokens[token]:
                tok_bal = tokens[token][t_qty]
                tok_new = tok_bal - sc_out - tokens_to_swap
        if tok_new < 0:
            return 'Error: Token Balance in Watched Wallet Too Low For Replenish+Swap'

        # Setup UTxOs
        tx_out = tx.process_tokens(default_settings, tokens, watch_addr, ['all'], ada_amt, [token_policy_id, token_name]) # Account for all except token to swap
        tx_out += ['--tx-out', watch_addr + '+' + str(collateral)] # UTxO to replenish collateral
        if tok_new > 0:
            tx_out += tx.process_tokens(default_settings, tokens, watch_addr, [tok_new], ada_amt) # Account for deposited-token change (if any)
        if tokens_to_swap > 0:
            tx_out += tx.process_tokens(default_settings, tokens, recipient_addr, [tokens_to_swap], ada_amt, [token_policy_id, token_name], False) # UTxO to Send Token(s) to the Buyer
        tx_out += tx.process_tokens(default_settings, tokens, smartcontract_addr, [sc_out], sc_ada_amt, [token_policy_id, token_name], False) # Send just the token for swap
        tx_out += '--tx-out-datum-hash', datum_hash
        tx_data = []
        if replenish:
            if sc_out > tok_bal:
                sc_out = tok_bal
            tx.get_utxo(default_settings, smartcontract_addr, 'utxo_script.json')
            if isfile(cache+'utxo_script.json') is False:
                log_it('Could not file utxo_script.json', DEBUG, True)
                return False
            _, _, sc_tokens, _, data_list = tx.get_txin(default_settings, 'utxo_script.json', collateral, True, datum_hash)

            sc_out_tk = 0
            for token in sc_tokens:
                # lovelace will be auto accounted for using --change-address
                if token == 'lovelace':
                    continue
                for t_qty in sc_tokens[token]:
                    sc_out_tk = [sc_tokens[token][t_qty]]

            for key in data_list:
                # A single UTXO with a single datum can be spent
                if data_list[key] == datum_hash:
                    utxo_in += ['--tx-in', key]
                    break
            tx_out += ['--tx-out', watch_addr + '+' + str(price)] # UTxO for price
            tx_out += tx.process_tokens(default_settings, sc_tokens, watch_addr, sc_out_tk, sc_ada_amt) # UTxO to Get SC Tokens Out
            tx_data = [
                '--tx-out-datum-hash', datum_hash,
                '--tx-in-datum-value', '"{}"'.format(tx.get_token_identifier(token_policy_id, token_name)),
                '--tx-in-redeemer-value', '""',
                '--tx-in-script-file', smartcontract_path
            ]

        tx.build_tx(default_settings, watch_addr, until_tip, utxo_in, utxo_col, tx_out, tx_data)
        
        # Sign and submit the transaction
        witnesses = [
            '--signing-key-file',
            watch_skey_path
        ]
        tx.sign_tx(default_settings, [witnesses, filePre])
        catch_err, tx_hash = tx.submit_tx(default_settings, submit_settings)
        log_it('Transaction Submit Result:' + tx_hash, DEBUG, catch_err)
    else:
        if not replenish:
            log_it('Collateral UTxO missing or couldn\'t be created! Exiting...', True, True)
            exit(0)
        tx_hash = 'error'#TODO
    return tx_hash

def start_deposit(default_settings, custom_settings):
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
    watch_skey_path = custom_settings[8]
    watch_vkey_path = custom_settings[9]
    watch_key_hash = custom_settings[10]
    smartcontract_path = custom_settings[11]
    token_policy_id = custom_settings[12]
    token_name = custom_settings[13]
    check_price = custom_settings[14]
    collateral = custom_settings[15]
    swap_type = custom_settings[16]
    smartcontract_addr = tx.get_smartcontract_addr(default_settings, smartcontract_path)

    print("\n--- NOTE: Proceed Only If You Are Depositing Your NFT or Tokens Into the NFT/Token Swap Smart Contract ---\n")
    print("\n---       Be sure you have at least 1 UTxO in your wallet with 2 ADA for collateral before running this.   ---\n")
    print("\n-----------------------------\n| Please Verify Your Input! |\n-----------------------------\n")
    print("\nMy Watched Wallet Address >> ",watch_addr)
    print("\nMy Address PubKey Hash (for smartcontract validation) >> ",str(watch_key_hash))
    print("\nMy Watched Addresses skey File Path >> ",watch_skey_path)
    print("\nSmartContract Address >> ",smartcontract_addr)
    print("\nNative Token Policy ID >> ",token_policy_id)
    print("\nNative Token Name >> ",token_name)
    
    
    verify = input("\n\nIs the information above correct AND you have a 2 ADA UTxO for Collateral? (yes or no): ")
    
    if verify == ("yes"):
        print("\n\nContinuing . . . \n")
    elif verify == ("no"):
        print("\n\nQuitting, please run again to try again!\n\n")
        exit(0)
    
    deposit_amt = input("\nHow many " + token_name + " tokens are you depositing?\nDeposit Amount:")
    sc_ada_amt = input("\nHow many lovelace to include with token(s) at SmartContract UTxO? (must be at least protocol minimum for token(s))\nLovelace Amount SmartContract:")
    ada_amt = input("\nHow many lovelace to include with token(s) at watched address UTxO(s)? (must be at least protocol minimum for token(s))\nLovelace Amount Wallet:")

    unique_id = tx.get_token_identifier(token_policy_id, token_name)
    catch_err, datum_hash  = tx.get_hash_value(default_settings, '"{}"'.format(unique_id)).replace('\n', '')
    if catch_err:
        log_it('Tried getting datum hash:' + datum_hash, True, catch_err)
        exit(0)
    filePre = 'depositSC_' + time_now() + '_'
    deposit_settings = [api_uri, api_id, watch_addr, fee_token_string, filePre, tx_hash_in, tx_amnt_in, tx_time, watch_skey_path, smartcontract_addr, smartcontract_path, token_policy_id, token_name, deposit_amt, sc_ada_amt, ada_amt, datum_hash, check_price, collateral, 0, '', False]
    tx_hash = deposit(default_settings, deposit_settings)
    log_it('Deposit is processing . . . ', True)
    tx.log_new_txs(default_settings, [api_uri, api_id, watch_addr, ''])
    sleep(2)
    if tx_hash != 'error':
        log_it('Deposit hash:' + tx_hash, True)
    else:
        log_it('Deposit failed:' + tx_hash, True, True)

def withdraw(default_settings, custom_settings):
    print('\nwithdraw function for DEBUG')
    # Defaults settings
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
    watch_skey_path = custom_settings[8]
    smartcontract_addr = custom_settings[9]
    smartcontract_path = custom_settings[10]
    token_policy_id = custom_settings[11]
    token_name = custom_settings[12]
    datum_hash = custom_settings[13]
    recipient_addr = custom_settings[14]
    return_ada = custom_settings[15]
    price = custom_settings[16]
    collateral = custom_settings[17]
    refund_amnt = custom_settings[18]
    refund_type = custom_settings[19]
    magic_price = custom_settings[20]

    submit_settings = [api_uri, api_id, watch_addr, fee_token_string, filePre, tx_hash_in, tx_amnt_in, tx_time]


    # Begin log file
    runlog_file = log + 'run.log'

    # Clear the cache
    tx.clean_folder(default_settings)
    tx.proto(default_settings)
    tx.get_utxo(default_settings, watch_addr, 'utxo.json')

    # Run get_txin
    utxo_in, utxo_col, tokens, flag, _ = tx.get_txin(default_settings, 'utxo.json', collateral)
    #flag = True
    # Build, Sign, and Send TX
    if flag is True or collateral == 1:
        _, until_tip, block = tx.get_tip(default_settings)
        
        token_amnt = ['all']
        if refund_amnt > 0:
            refund_string = recipient_addr + '+' + str(refund_amnt)
            # Check for refund type: 0 = just a simple refund; 1 = include 40 JUDE & any clue; 2 = include any clue
            ct = 0
            
            if refund_type == 1:
                ct = 40
                # Calculate any clue
                if magic_price == 0:
                    ct = 41
                else:
                    clue_diff = int(refund_amnt) / int(magic_price)
                    if clue_diff >= 0.8:
                        ct = 130
                    if clue_diff <= 0.2:
                        ct = 50
                clue_token = str(ct)
                refund_string += '+' + clue_token + ' ' + token_policy_id + '.' + token_name
            if refund_type == 2:
                # Calculate any clue
                if magic_price == 0:
                    ct = 1
                else:
                    clue_diff = int(refund_amnt) / int(magic_price)
                    if clue_diff >= 0.8:
                        ct = 90
                    if clue_diff <= 0.2:
                        ct = 10
                clue_token = str(ct)
                refund_string += '+' + clue_token + ' ' + token_policy_id + '.' + token_name
            if refund_type == 3:
                ct = 40
                if magic_price == 0:
                    ct = 41
                clue_token = str(ct)
                refund_string += '+' + clue_token + ' ' + token_policy_id + '.' + token_name
            if refund_type == 4:
                if magic_price == 0:
                    ct = 1
                clue_token = str(ct)
                refund_string += '+' + clue_token + ' ' + token_policy_id + '.' + token_name

            if ct > 0:
                token_amnt = ['except', ct]
            tx_out_refund = ['--tx-out', refund_string] # UTxO to Refund
        tx_out = tx.process_tokens(default_settings, tokens, watch_addr, token_amnt) # Change
        if collateral != 1:
            tx_out += ['--tx-out', watch_addr + '+' + str(collateral)] # Replenish collateral
        if tx_out_refund:
            tx_out += tx_out_refund # The final refund out if any
        tx_data = []
        if refund_amnt == 0:
            tx.get_utxo(default_settings, smartcontract_addr, 'utxo_script.json')
            if isfile(cache+'utxo_script.json') is False:
                log_it('Could not file utxo_script.json', DEBUG, True)
                return False
            _, _, sc_tokens, _, data_list = tx.get_txin(default_settings, 'utxo_script.json', collateral, True, datum_hash)

            sc_out = 0
            for token in sc_tokens:
                # lovelace will be auto accounted for using --change-address
                if token == 'lovelace':
                    continue
                for t_qty in sc_tokens[token]:
                    sc_out = [sc_tokens[token][t_qty]]
            for key in data_list:
                # A single UTXO with a single datum can be spent
                if data_list[key] == datum_hash:
                    utxo_in += ['--tx-in', key]
                    break
            tx_out += ['--tx-out', watch_addr + '+' + str(price)] # UTxO for price if set to process price payment
            tx_out += tx.process_tokens(default_settings, sc_tokens, watch_addr, sc_out, return_ada) # UTxO to Get SC Tokens Out
            tx_data = [
                '--tx-out-datum-hash', datum_hash,
                '--tx-in-datum-value', '"{}"'.format(tx.get_token_identifier(token_policy_id, token_name)),
                '--tx-in-redeemer-value', '""',
                '--tx-in-script-file', smartcontract_path
            ]
        tx.build_tx(default_settings, watch_addr, until_tip, utxo_in, utxo_col, tx_out, tx_data)
        
        witnesses = [
            '--signing-key-file',
            watch_skey_path
        ]
        tx.sign_tx(default_settings, [witnesses, filePre])
        catch_err, tx_hash = tx.submit_tx(default_settings, submit_settings)
        log_it('Transaction Submit Result:' + tx_hash, DEBUG, catch_err)
    else:
        with open(runlog_file, 'a') as runlog:
            runlog.write('\nNo collateral UTxO found! Please create a UTxO of 2 ADA (2000000 lovelace) before trying again.\n')
            runlog.close()
        tx_hash = 'error'
    return tx_hash

def smartcontractswap(default_settings, custom_settings):
    # Defaults settings
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
    watch_skey_path = custom_settings[8]
    smartcontract_addr = custom_settings[9]
    smartcontract_path = custom_settings[10]
    token_policy_id = custom_settings[11]
    token_name = custom_settings[12]
    datum_hash = custom_settings[13]
    recipient_addr = custom_settings[14]
    return_ada = custom_settings[15]
    price = custom_settings[16]
    collateral = custom_settings[17]
    token_qty = custom_settings[18]
    swap_type = custom_settings[19]

    submit_settings = [api_uri, api_id, watch_addr, fee_token_string, filePre, tx_hash_in, tx_amnt_in, tx_time]

    # Begin log file
    runlog_file = log + 'run.log'

    # Clear the cache
    tx.clean_folder(default_settings)
    tx.proto(default_settings)
    tx.get_utxo(default_settings, watch_addr, 'utxo.json')
    
    # Run get_txin
    contract_utxo_in, utxo_col, tokens, flag, _ = tx.get_txin(default_settings, 'utxo.json', collateral)
    
    # Build, Sign, and Send TX
    if flag is True or swap_type == 1:
        _, until_tip, block = tx.get_tip(default_settings)
        if swap_type == 0:
            tx.get_utxo(default_settings, smartcontract_addr, 'utxo_script.json')
            if isfile(cache+'utxo_script.json') is False:
                with open(runlog_file, 'a') as runlog:
                    runlog.write('\nERROR: Could not file utxo_script.json\n')
                    runlog.close()
                return False
            _, _, sc_tokens, _, data_list = tx.get_txin(default_settings, 'utxo_script.json', collateral, True, datum_hash)
            for key in data_list:
                # A single UTXO with a single datum can be spent
                if data_list[key] == datum_hash:
                    contract_utxo_in += ['--tx-in', key]
                    break
            # Calculate token quantity and change
            sc_bal = 0
            sc_out = int(token_qty)
            sc_new = 0
            for token in sc_tokens:
                # lovelace will be auto accounted for using --change-address
                if token == 'lovelace':
                    continue
                for t_qty in sc_tokens[token]:
                    sc_bal = sc_tokens[token][t_qty]
                    sc_new = sc_bal - sc_out
            tx_out = tx.process_tokens(default_settings, sc_tokens, recipient_addr, [sc_out], return_ada) # UTxO to Send Token(s) to the Buyer
            tx_out += tx.process_tokens(default_settings, tokens, watch_addr) # Change
            tx_out += ['--tx-out', watch_addr + '+' + str(collateral)] # Replenish collateral
            if sc_new > 0:
                tx_out += tx.process_tokens(default_settings, sc_tokens, smartcontract_addr, [sc_new]) # UTxO to Send Change to Script - MUST BE LAST UTXO FOR DATUM
        else:
            # Calculate token quantity and change
            tk_bal = 0
            tk_out = int(token_qty)
            tk_new = 'all'
            for token in tokens:
                # lovelace will be auto accounted for using --change-address
                if token == 'lovelace':
                    continue
                for t_qty in tokens[token]:
                    tk_bal = tokens[token][t_qty]
                    tk_new = tk_bal - tk_out
            tx_out = tx.process_tokens(default_settings, tokens, recipient_addr, [tk_out], return_ada) # UTxO to Send Token(s) to the Buyer
            tx_out += tx.process_tokens(default_settings, tokens, watch_addr, [tk_new]) # Change
        
        if price:
            tx_out += ['--tx-out', watch_addr + '+' + str(price)] # UTxO for price if set to process price payment
        tx_data = []
        if swap_type == 0:
            tx_data = [
                '--tx-out-datum-hash', datum_hash,
                '--tx-in-datum-value', '"{}"'.format(tx.get_token_identifier(token_policy_id, token_name)),
                '--tx-in-redeemer-value', '""',
                '--tx-in-script-file', smartcontract_path
            ]
        tx.build_tx(default_settings, watch_addr, until_tip, contract_utxo_in, utxo_col, tx_out, tx_data)
        
        witnesses = [
            '--signing-key-file',
            watch_skey_path
        ]
        tx.sign_tx(default_settings, [witnesses, filePre])
        catch_err, tx_hash = tx.submit_tx(default_settings, submit_settings)
        log_it('Transaction Submit Result:' + tx_hash, DEBUG, catch_err)
    else:
        with open(runlog_file, 'a') as runlog:
            runlog.write('\nNo collateral UTxO found! Please create a UTxO of 2 ADA (2000000 lovelace) before trying again.\n')
            runlog.close()
        tx_hash = 'error'
    return tx_hash

def check_tip(default_settings, runlog_file, nft_add, nft_lock, manual = False):
    end_minting = False
    if nft_add < 900 and nft_lock == 0:
        if nft_add > 120:
            with open(runlog_file, 'a') as runlog:
                runlog.write('\nLocking tip is probably too soon, trying anyway...')
                runlog.close()
            if manual:
                print('\nTarget locking height is only 2 ~ 15 min in the future, try to continue anyway?')
                try_continue = input('Yes or No:')
                if try_continue == 'No' or try_continue == 'no':
                    print('\nExiting...')
                    exit(0)
        else:
            end_minting = True
            with open(runlog_file, 'a') as runlog:
                runlog.write('\nLocking tip is too soon, cannot mint')
                runlog.close()
            if manual:
                print('\nLocking tip is probably too soon (below 10 slots)! To ensure minting completes in time, please try again with a tip of 900 (15 min) or higher')
                exit(0)
    if nft_lock > 0:
        until_tip = nft_lock
        latest_tip, _, _ = tx.get_tip(default_settings, 0, until_tip)
        tip_diff = until_tip - latest_tip
        if tip_diff < 600 and tip_diff > 120:
            if manual:
                print('\nTarget locking height is only 2 ~ 10 min in the future, try to continue anyway?')
                try_continue = input('Yes or No:')
                if try_continue == 'No' or try_continue == 'no':
                    print('\nExiting...')
                    exit(0)
            else:
                with open(runlog_file, 'a') as runlog:
                    runlog.write('\nTarget locking height is only 2 ~ 10 min in the future, trying anyway...')
                    runlog.close()
        if tip_diff < 120:
            if manual:
                print('\nTarget locking height is in less than 2 min and will likely fail, try anyway?')
                try_continue = input('Yes or No:')
                if try_continue == 'No' or try_continue == 'no':
                    print('\nExiting...')
                    exit(0)
            else:
                end_minting = True
                with open(runlog_file, 'a') as runlog:
                    runlog.write('\nTarget locking height is less than 2 min in the future and will likely fail, cannot mint.')
                    runlog.close()
    else:
        _, until_tip, _ = tx.get_tip(default_settings, nft_add)

    return end_minting, until_tip

def mint(default_settings, custom_settings):
    # Defaults settings
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
    return_ada = custom_settings[8]
    mint_addr = custom_settings[10]
    wallet_skey_path = custom_settings[11]
    policy_skey_path = custom_settings[12]
    nft_addr = custom_settings[13]
    nft_qty = custom_settings[14][0]
    nft_asset = custom_settings[14][1]
    out_script = custom_settings[14][2]
    nft_json_out = custom_settings[14][3]
    until_tip = custom_settings[14][4]
    nft_id = custom_settings[14][5]
    manual = custom_settings[15]
    submit_settings = [api_uri, api_id, watch_addr, fee_token_string, filePre, tx_hash_in, tx_amnt_in, tx_time]

    # Begin log file
    runlog_file = log + 'run.log'

    # Clear the cache
    tx.clean_folder(default_settings)
    tx.proto(default_settings)
    tx.get_utxo(default_settings, mint_addr, 'utxo.json')
    
    # Run get_txin
    utxo_in, utxo_col, tokens, flag, _ = tx.get_txin(default_settings, 'utxo.json')
    
    # Build Raw Temp
    tx_out_own = tx.process_tokens(default_settings, tokens, mint_addr, ['all'], return_ada, '', True, True, return_ada) # ADA Calc Change
    tx_out_nft = ['--tx-out', nft_addr + '+' + return_ada + '+' + nft_qty + ' ' + nft_asset] # Send NFT to winner

    tx_out_temp = tx_out_own
    tx_out_temp += tx_out_nft
    tx_data = [
        '--mint=' + nft_qty + ' ' + nft_asset,
        '--minting-script-file', out_script,
        '--metadata-json-file', nft_json_out
    ]
    witnesses = [
        '--signing-key-file',
        wallet_skey_path,
        '--signing-key-file',
        policy_skey_path
    ]
    utxo_in_count = len(utxo_in) // 2
    utxo_out_count = len(tx_out_temp)
    witness_count = len(witnesses) // 2
    counts = [utxo_in_count, utxo_out_count, witness_count]

    # Get fee and recalculate UTxOs
    fee = tx.build_raw_tx(default_settings, counts, until_tip, utxo_in, utxo_col, tx_out_temp, tx_data, manual)
    reserve_ada = int(return_ada) + int(fee)
    tx_out = tx.process_tokens(default_settings, tokens, mint_addr, ['all'], return_ada, '', True, True, reserve_ada) # ADA Calc Change
    tx_out += tx_out_nft

    # Build new tx with fees
    tx.build_raw_tx(default_settings, counts, until_tip, utxo_in, utxo_col, tx_out, tx_data, manual, fee)
    
    # Sign and send
    tx.sign_tx(default_settings, [witnesses, filePre])
    catch_err, tx_hash = tx.submit_tx(default_settings, submit_settings)
    log_it('Transaction Submit Result:' + tx_hash, DEBUG, catch_err)
    if manual:
        return tx_hash, nft_id, str(until_tip)
    return tx_hash

def process_mint(default_settings, custom_settings):
    # Configure settings
    cli_path = default_settings[2]
    src = custom_settings[9]
    nft_data = custom_settings[14]
    manual = custom_settings[15]
    mint_policy_vkey = custom_settings[16]
    nft_name = nft_data[0]
    nft_qty = nft_data[1]
    nft_json = nft_data[2]
    nft_lock = nft_data[3]
    nft_add = nft_data[4]
    if len(nft_lock) > 0:
        nft_lock = int(nft_lock)
    if len(nft_add) > 0:
        nft_add = int(nft_add)
    mint_type = nft_data[5]
    mint_order = nft_data[6]
    col_type = nft_data[7]
    manual = False
    nft_num = ''
        
    # Multi-NFT Mint-On-Demand Sale
    if mint_type == 2:
        from os import replace as osreplace, remove as osremove
        img_list = nft_qty
        nft_qty = '1'
        json_queue = nft_json[0]
        json_fail = nft_json[1]
        json_succeed = nft_json[2]
        if mint_order == 0:
            import random
            nft_num = str(random.choice(img_list))
        if mint_order == 1:
            img_list.sort()
            nft_num = str(img_list[0])
        if mint_order == 2:
            img_list.sort(reverse = True)
            nft_num = str(img_list[0])
        custom_settings[4] = nft_num + custom_settings[4]
        nft_json = json_queue + nft_num + '.template_json'

    # Get locking tip and check within range or error out
    end_minting, until_tip = check_tip(default_settings, runlog_file, nft_add, nft_lock, manual)
    if end_minting:
        return 'error'

    # Setup NFT files
    template_script = src + 'template.script'
    out_script = log + nft_name + '.script'
    nft_json_name = nft_name + '.json'
    nft_json_out = log + nft_json_name
    out_hash = log + nft_name + '.hash'
    out_id = log + nft_name + '.id'

    is_scriptfile = isfile(out_script)
    is_hashfile = isfile(out_hash)
    is_idfile = isfile(out_id)

    # Setup policy hash file if not exist
    if is_hashfile:
        policy_hash = out_hash.read().strip()
    else:
        policy_hash_err, policy_hash = tx.get_address_pubkeyhash(cli_path, mint_policy_vkey)
        if policy_hash_err:
            log_it(True, policy_hash, False)
        with open(out_hash, 'w') as hashout:
            hashout.write(policy_hash)
            hashout.close()
        
    # Setup policy file if not exist
    if not is_scriptfile:
        with open(template_script, 'r') as script:
            scriptData = script.read()
            script.close()
        scriptData = scriptData.replace('0fff0', ' '.join(policy_hash.split()))
        scriptData = scriptData.replace('1111', ' '.join(str(until_tip).split()))
        with open(out_script, 'w') as scriptout:
            scriptout.write(scriptData)
            scriptout.close()

    # Setup policy id file if not exist
    if is_idfile:
        nft_id = out_id.read().strip()
    else:
        nft_id = tx.get_token_id(default_settings, out_script)
        with open(out_id, 'w') as outid:
            outid.write(nft_id)
            outid.close()

    # Set Asset Name
    # TODOprocmint: Check standards for naming schema
    nft_asset = nft_id + '.' + nft_name + nft_num
    
    # Modify and save JSON file
    with open(nft_json, 'r') as jsonsrc:
        jsonData = jsonsrc.read()
        jsonsrc.close()
    jsonData = jsonData.replace('000_POLICY_ID_000', ' '.join(nft_id.split()))

    # Mint-On-Demand Collection Type 1 (SVG + HTML On-Chain)
    if mint_type == 1 and col_type = 1:
        html_in = log + nft_name + '-temp.html'
        html_out = log + nft_name + '.html'
        with open(html_in, 'r') as htmlsrc:
            htmlData = htmlsrc.read()
            htmlsrc.close()
        firstpart = nft_addr[0:16]
        lastpart = nft_addr[-16:]
        s_addr = firstpart + ' . . . ' + lastpart
        htmlData = htmlData.replace('__00_ADDRL_00__', ' '.join(nft_addr.split()))
        htmlData = htmlData.replace('__00_ADDR_00__', ' '.join(s_addr.split()))
        with open(html_out, 'w') as htmlout:
            htmlout.write(htmlData)
            htmlout.close()
        html_json = json.dumps(tx.encode_to_base64(html_out, 'html'))
        jsonData = jsonData.replace('000_FILE_000', ' '.join(html_json.split()))

    with open(nft_json_out, 'w') as jsonout:
        jsonout.write(jsonData)
        jsonout.close()
    
    # Clean up source json for Sale/Multi-NFT Type post-json-processing
    if mint_type == 2:
        osremove(nft_json)
    
    # Pass vars for mint
    custom_settings[14] = [nft_qty, nft_asset, out_script, nft_json_out, until_tip, nft_id]

    # Mint transaction
    if manual:
        return mint(default_settings, custom_settings)
    try:
        mint_result = mint(default_settings, custom_settings)
    except:
        mint_result = 'error'

    # Process results and return
    if mint_type == 2:
        if mint_result == 'error':
            osreplace(nft_json_out, json_fail + nft_json_name)
            with open(runlog_file, 'a') as runlog:
                runlog.write('\nError Minting: ' + filePre)
                runlog.write('\nAsset moved to: ' + json_fail)
                runlog.close()
        else:
            osreplace(nft_json_out, json_succeed + nft_json_name)
    return mint_result

def manual_mint(mintroot):
    MINT_TYPE = 0 # Set mint type to Manual (0)
    TESTNET = False
    MINT_NETWORK = 'mainnet'
    MINT_ADDR = 'none'
    MINT_TARGET_TIP = '0'
    MINT_LAST_TIP = '0'
    print('\n    =================================================================')
    print('    ===  MintedWithLovelace Native Cardano Token Minting (alpha)  ===')
    print('    =================================================================')
    print('\n\n    Mint NFTs or Tokens. To mint multiple NFTs into a single\n    policy ID, mint the first NFT and take note of the Policy\n    Locking Slot Number that outputs at the end, for subsequent\n    mintings, use this same Locking Slot Number and they will be\n    minted under the same Policy ID (as long as that slot number\n    is more than 2 minutes into the future)')
    MINT_CLI = input('\nCardano CLI path (or leave blank if in system path):')
    if len(MINT_CLI) == 0:
        MINT_CLI = 'cardano-cli'
    MINT_NETWORK = input('\nEnter the Network Type (mainnet or testnet):')
    if MINT_NETWORK == 'testnet':
        TESTNET = True
        MINT_MAGIC = input('\nTestnet Magic Number:')
        MINT_NETWORK = MINT_NETWORK + '-magic'
    MINT_SKEY = input('\nEnter the Minting Wallet Signing Key (.skey) File Path:')
    MINT_VKEY = input('\nEnter the Minting Wallet Verification Key (.vkey) File Path:')
    MINT_POLICY_SKEY = input('\nEnter the Minting Policy Signing Key (.skey) File Path:')
    MINT_POLICY_VKEY = input('\nEnter the Minting Policy Verification Key (.vkey) File Path:')
    NFT_ADDR = input('\nNFT or Token Recipient Address\n(or leave blank to mint at the same address as your minting wallet skey):')
    MINT_NFT_NAME = input('\nNFT or Token Name:')

    # Setup profile-specific cache and log folders
    LOG = osjoin(osjoin(mintroot, MINT_NFT_NAME), '')
    CACHE = osjoin(osjoin(LOG, 'cache'), '')
    TXLOG = osjoin(osjoin(LOG, 'txs'), '')
    try:
        mkdir(LOG)
    except OSError:
        pass
    try:
        mkdir(CACHE)
    except OSError:
        pass
    try:
        mkdir(TXLOG)
    except OSError:
        pass
            
    # Instantiate log for profile
    runlog_file = LOG + 'run.log'
    is_runlog_file = isfile(runlog_file)
    if not is_runlog_file:
        try:
            open(runlog_file, 'x')
        except OSError:
            pass

    MINT_NFT_QTY = input('\nQuantity (usually 1 for an NFT):')
    PREP_JSON = input('\nPrepare and use the svg-html json template? (yes or no):')
    if PREP_JSON == 'yes' or PREP_JSON == 'Yes':
        print('\nAll manually edit fields should be prepared AFTER this function generates your json file')

        # For passing address for visible string:
        NFT_CUSTOM = input('\nValue for "Minting Master" field (buyers address):')
        SVG_HTML = input('\nEnter path to SVG file for HTML embedding:')
        SVG_IMG = input('\nIF DIFFERENT, enter path to SVG file for Image meta:')
        if not SVG_HTML:
            SVG_HTML = LOG + MINT_NFT_NAME + '.svg'
            print('\nPlace your SVG file for this NFT at the following location and name accordingly:',SVG_HTML)
            input('\nOnce the SVG image is in place, press any key to continue...')
        if not SVG_IMG:
            SVG_IMG = SVG_HTML
        firstpart = NFT_CUSTOM[0:16]
        lastpart = NFT_CUSTOM[-16:]
        s_addr = firstpart + ' . . . ' + lastpart

        # Get raw svg data to embed into HTML
        with open(SVG_HTML, 'r') as img:
            img_src = img.read()

        # Setup files
        template_html = MINTSRC + 'template.html'
        html_out = LOG + MINT_NFT_NAME + '.html'
        template_json = MINTSRC + 'template_svg_html_onchain.json'
        out_json = LOG + MINT_NFT_NAME + '-temp.json'

        with open(template_html, 'r') as htmlsrc:
            htmlData = htmlsrc.read()
            htmlsrc.close()
        htmlData = htmlData.replace('__00_BGIMG_00__', ' '.join(img_src.split()))
        htmlData = htmlData.replace('__00_ADDRL_00__', ' '.join(NFT_CUSTOM.split()))
        htmlData = htmlData.replace('__00_ADDR_00__', ' '.join(s_addr.split()))
        # Write to new html file in profile folder
        with open(html_out, 'w') as htmlout:
            htmlout.write(htmlData)
            htmlout.close()

        # Base64 HTML and SVG if applicable for image of NFT
        html_json = json.dumps(tx.encode_to_base64(html_out, 'html'))
        svg_json = json.dumps(tx.encode_to_base64(SVG_IMG, 'svg'))

        # Save all to JSON file
        with open(template_json, 'r') as jsonsrc:
            jsonData = jsonsrc.read()
            jsonsrc.close()
        jsonData = jsonData.replace('000_NAME_000', ' '.join(MINT_NFT_NAME.split()))
        if html_json:
            jsonData = jsonData.replace('000_FILE_000', ' '.join(html_json.split()))
        if svg_json:
            jsonData = jsonData.replace('000_SVG_000', ' '.join(svg_json.split()))
        with open(out_json, 'w') as jsonout:
            jsonout.write(jsonData)
            jsonout.close()
        MINT_NFT_JSON = out_json
        print('\nYour NFT JSON file has been generated, before proceeding do any manual edits, leaving 000_POLICY_ID_000 in-tact for the next stages of processing. JSON File Located At: ', out_json)
    else:
        MINT_NFT_JSON = input('\nPath to this NFT ready-to-mint JSON file\n(Must have 000_POLICY_ID_000 in the policy section and 000_NAME_000 in the main name section, both inside qoutes):')

        # Update provided json and save to this log location
        out_json = LOG + MINT_NFT_NAME + '-temp.json'
        with open(MINT_NFT_JSON, 'r') as jsonsrc:
            jsonData = jsonsrc.read()
            jsonsrc.close()
        jsonData = jsonData.replace('000_NAME_000', ' '.join(MINT_NFT_NAME.split()))
        with open(out_json, 'w') as jsonout:
            jsonout.write(jsonData)
            jsonout.close()
        MINT_NFT_JSON = out_json
            
    SAME_POLICY = input('\nIs this NFT or Token part of an already existing policy?\n(Yes or No - if this is the first asset minted to a policy answer No):')
    if SAME_POLICY == 'Yes' or SAME_POLICY == 'yes':
        MINT_TARGET_TIP = input('\nEnter the Policy Locking Slot Number matching the policy this NFT or Token will mint into\n(If you lost it you can find it within the ".script" file for a previous asset minted into that policy):')
    else:
        MINT_LAST_TIP = input('\nHow Many Slots To Add Until Locked?\n(1 slot ~ 1 second)\nSlots to Add to Current Block Height:')
    print('\n...Building TX')
    NFT_DATA = [MINT_NFT_NAME, MINT_NFT_QTY, MINT_NFT_JSON, MINT_TARGET_TIP, MINT_LAST_TIP, 0, '', '']
    DEFAULT_SETTINGS = ['manual-mintName', 'notype', MINT_CLI, MINT_NETWORK, MINT_MAGIC, LOG, CACHE, TXLOG, TESTNET]
    MINT_ADDR = tx.get_wallet_addr(DEFAULT_SETTINGS, MINT_VKEY)
    if len(NFT_ADDR) == 0:
        NFT_ADDR = tx.get_wallet_addr(DEFAULT_SETTINGS, MINT_VKEY)
    print('\nMAKE CERTAIN YOU HAVE DONE ANY AND ALL MANUAL CHANGES TO THIS NFT META IN THE JSON FILE BEFORE CONTINUING!')
    input('Press any key when ready, to continue...MUST HAVE OVER 5 ADA AT MINTING ADDR')
    input('Are you ABSOLUTELY CERTAIN your json file is ready? You can copy/paste it into pool.pm/test/metadata to make sure!...if you are sure, press any key')
    minted_hash, policyID, policy_tip = process_mint(DEFAULT_SETTINGS, ['', '', MINT_ADDR, '', 'mint-' + MINT_NFT_NAME, 'mint', '', '', '5000000', MINTSRC, MINT_ADDR, MINT_SKEY, MINT_POLICY_SKEY, NFT_ADDR, NFT_DATA, True, MINT_POLICY_VKEY])
    print('\nCompleted - (TX Hash return is ' + minted_hash + ')')
    print('\nIMPORTANT: Take note of the following Policy ID and Policy Locking Slot Number. If you will be minting more NFTs to this same Policy ID, you will need to enter the following Policy Locking Slot Number when minting another NFT into this policy.')
    print('\n    > Asset Name: ', policyID + '.' + MINT_NFT_NAME)
    print('\n    > Minted Qty: ', MINT_NFT_QTY)
    print('\n    > Policy ID: ', policyID)
    print('\n    > Policy Locking Slot Number: ', policy_tip)
    exit(0)

def tx_processor():
    # Defaults Settings
    PROFILE_NAME = DEFAULT_SETTINGS[0]
    PROFILE_TYPE = DEFAULT_SETTINGS[1]
    CLI_PATH = DEFAULT_SETTINGS[2]
    NETWORK = DEFAULT_SETTINGS[3]
    MAGIC = DEFAULT_SETTINGS[4]
    PROFILELOG = DEFAULT_SETTINGS[5]
    PROFILECACHE = DEFAULT_SETTINGS[6]
    PROFILETXS = DEFAULT_SETTINGS[7]
    TESTNET = DEFAULT_SETTINGS[8]

    # Dynamic Custom Settings
    API_URI = CUSTOM_SETTINGS[0]
    API_ID = CUSTOM_SETTINGS[1]
    FEE_TOKEN_STRING = CUSTOM_SETTINGS[2]
    PROFILE = CUSTOM_SETTINGS[3]
    MINTSRC = CUSTOM_SETTINGS[4]
    DELAY_TIME = CUSTOM_SETTINGS[5]
    WATCH_ADDR = CUSTOM_SETTINGS[6]
    SMARTCONTRACT_ADDR = CUSTOM_SETTINGS[7]
    MINT_ADDR = CUSTOM_SETTINGS[8]
    WATCH_KEY_HASH = CUSTOM_SETTINGS[9]

    # Shared Settings
    API_URI_PRE = PROFILE['api_pre']
    API_URI_POST = PROFILE['api_post']
    WLENABLED = PROFILE['wlenabled']
    WHITELIST_ONCE = PROFILE['wlone']
    EXPECT_ADA = PROFILE['expectada']
    MIN_WATCH = PROFILE['min_watch']
    RETURN_ADA = PROFILE['returnada']
    TOKEN_POLICY_ID = PROFILE['tokenid']
    TOKEN_NAME = PROFILE['tokenname']
    COLLATERAL = PROFILE['collateral']
    UPDATEMINTSETTINGS = False
    AUCTION = 9
    REFUND_PENDING = False
    LOGGER_SETTINGS = [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING]

    # Vars profile 0 - SmartContract Swap
    if PROFILE_TYPE == 0:
        WATCH_SKEY_PATH = PROFILE['watchskey']
        WATCH_VKEY_PATH = PROFILE['watchvkey']
        SMARTCONTRACT_PATH = PROFILE['scpath']
        PRICE = PROFILE['price']
        TOKEN_QTY = PROFILE['tokenqty']
        DEPOSIT_AMNT = PROFILE['deposit_amnt']
        RECURRING = PROFILE['recurring']
        SC_ADA_AMNT = PROFILE['sc_ada_amnt']
        WT_ADA_AMNT = PROFILE['wt_ada_amnt']
        AUTO_REFUND = PROFILE['auto_refund']
        FEE_CHARGE = PROFILE['fee_to_charge']
        SWAP_TYPE = PROFILE['swap_type']
        END_NOW = False
        SPENDABLE = True
        if SWAP_TYPE == 1:
            SPENDABLE = False

        # Calculate the "fingerprint" and finalize other variables
        FINGERPRINT = tx.get_token_identifier(TOKEN_POLICY_ID, TOKEN_NAME)
        DATUM_ERR, DATUM_HASH  = tx.get_hash_value(DEFAULT_SETTINGS, '"{}"'.format(FINGERPRINT)).replace('\n', '')
        if DATUM_ERR:
            with open(runlog_file, 'a') as runlog:
                runlog.write('\nError getting Datum Hash:')
                runlog.write('\n' + DATUM_HASH)
                runlog.close()
    
    # Vars profile 1 - NFT AutoMinting
    if PROFILE_TYPE == 1:
        NFT_MINTED = PROFILE['nft_minted']
        NFT_ADDR = PROFILE['nft_addr']
        MINT_SKEY = PROFILE['wallet_skey'] # Similar to WATCH_SKEY_PATH
        MINT_VKEY = PROFILE['wallet_vkey']
        MINT_POLICY_SKEY = PROFILE['policy_skey'] # Additional not used by others
        MINT_POLICY_VKEY = PROFILE['policy_vkey']
        NFT_DATA = PROFILE['nft_data']
        AUCTION = PROFILE['auction']
        BLOCKTIME = PROFILE['blocktime']
        AUCTIONEND = PROFILE['auctionend']
        END_NOW = NFT_MINTED
        PROC_MINT = False
        MINT_TYPE = 1
        if AUCTION == 2:
            MINT_TYPE = AUCTION

    # Begin main payment checking/recording loop here
    while True:
        """
        Main loop: Check for payment, initiate Smart Contract on success
        """
        I_STATE += 1
        if I_STATE == 450:
            log_it('TX Processor active for ' + str(I_STATE) + ' iterations since last update')
            I_STATE = 0
        sleep(2) # Small Delay Improves CPU usage
        result = 'none'
        whitelist_file = PROFILELOG + 'whitelist.txt'
        is_whitelist_file = isfile(whitelist_file)
        if not is_whitelist_file:
            with open(runlog_file, 'a') as runlog:
                runlog.write('\nMissing expected file: whitelist.txt in your profile folder\n')
                runlog.close()
            print('\nMissing Whitelist (whitelist.txt)! Exiting.\n')
            exit(0)
        whitelist_r = open(whitelist_file, 'r')
        windex = 0
        
        # Foreach line of the whitelist file
        for waddr in whitelist_r:
            # Check if whitelist is empty and end app if it is
            windex += 1
            if not EXPECT_ADA:
                EXPECT_ADA = 0
            RECIPIENT_ADDR = waddr.strip()
            tx.log_new_txs(DEFAULT_SETTINGS, LOGGER_SETTINGS)
            result = tx.check_for_payment(DEFAULT_SETTINGS, [API_URI, API_ID, WATCH_ADDR, EXPECT_ADA, MIN_WATCH, RECIPIENT_ADDR, AUCTION, SMARTCONTRACT_ADDR])
            
            # Assign result to vars
            RESLIST = result.split(',')
            TX_HASH = RESLIST[0].strip()
            RECIPIENT_ADDR = RESLIST[1].strip()
            ADA_RECVD = int(RESLIST[2])
            TK_AMT = int(RESLIST[3])
            TK_NAME = RESLIST[4].strip()
            STAT = int(RESLIST[5])
            TX_TIME = RESLIST[6]
            TALLY = RESLIST[7]

            # Check if auction time is ended
            if PROFILE_TYPE == 1 and AUCTION == 0 and AUCTIONEND > 0 and not NFT_MINTED:
                print('Checking if Auction should End...Profile 1, Auction End > 0, NFT_MINTED = False')
                # Get tip now
                AUCTION_NOW, _, _ = tx.get_tip(DEFAULT_SETTINGS, 0)
                # Get and check diff
                auction_diff = int(AUCTIONEND) - int(AUCTION_NOW)
                if auction_diff <= 0:
                    END_NOW = True

            # Archive internal TXs and continue
            if STAT == 2 and not END_NOW:
                print('Stat 2 returned and not end_now')
                continue
            
            # If a SC Swap
            if PROFILE_TYPE == 0:
                print('Profile type 0')
                # Begin prep for functionalization of this
                SWAP_SETTINGS = [[TX_HASH, ADA_RECVD, TX_TIME],runlog_file,result]
                if STAT == 0:
                    # Archive TX and continue for now, may need to refund in future for catching small/under limit tx and auto refunding
                    tx.archive_tx(DEFAULT_SETTINGS, SWAP_SETTINGS[0])
                    continue
                with open(SWAP_SETTINGS[1], 'a') as runlog:
                    runlog.write('\n===== Matching TX: '+str(SWAP_SETTINGS[2])+' =====\nRunning whitelist for addr/ada-rec: '+RECIPIENT_ADDR+' | '+str(ADA_RECVD))
                    runlog.close()
                TOKENS_TOSWAP = 0
                if MIN_WATCH > 0:
                    ADA_TOSWAP = ADA_RECVD
                    #RET_INT = int(RETURN_ADA)
                    #ADA_TOSWAP = ADA_RECVD - RET_INT
                    TOKENS_TOSWAP = int((int(TOKEN_QTY) * ADA_TOSWAP) / 1000000)

                # Get SC Token Balance and Compare
                tx.get_utxo(DEFAULT_SETTINGS, SMARTCONTRACT_ADDR, 'utxo_script_check.json')
                if isfile(PROFILECACHE+'utxo_script_check.json') is False:
                    with open(SWAP_SETTINGS[1], 'a') as runlog:
                        runlog.write('\nERROR: Could not file utxo_script_check.json\n')
                        runlog.close()
                    # Do not archive, continue (keep trying)
                    continue
                _, _, sc_tkns, _, _ = tx.get_txin(DEFAULT_SETTINGS, 'utxo_script_check.json', COLLATERAL, SPENDABLE, DATUM_HASH)
                print('Got back tok of:')
                print(sc_tkns)
                sc_bal = 0
                for token in sc_tkns:
                    print('Token: '+token)
                    if token != TOKEN_POLICY_ID:
                        continue
                    for t_qty in sc_tkns[token]:
                        sc_bal = int(sc_tkns[token][t_qty])
                with open(SWAP_SETTINGS[1], 'a') as runlog:
                    runlog.write('\nSC Token Balance: '+str(sc_bal))
                    runlog.close()
                if TOKENS_TOSWAP > sc_bal:
                    with open(SWAP_SETTINGS[1], 'a') as runlog:
                        runlog.write('\nSupply of SC is lower than swap!')
                        runlog.close()
                    if TOKENS_TOSWAP > int(DEPOSIT_AMNT):
                        with open(SWAP_SETTINGS[1], 'a') as runlog:
                            runlog.write('\nTokens requested exceeds any future deposit currently set. Refunding user, minus fees')
                            runlog.close()
                        if AUTO_REFUND:
                            REFUND_AMNT = ADA_RECVD - FEE_CHARGE
                            with open(SWAP_SETTINGS[1], 'a') as runlog:
                                runlog.write('\nRefunding (tokens exceed): '+str(REFUND_AMNT))
                                runlog.close()
                            filePre = 'refundTO' + RECIPIENT_ADDR + '_' + time_now() + '_'
                            tx_refund_a_hash = withdraw(DEFAULT_SETTINGS, [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING, filePre, TX_HASH, ADA_RECVD, TX_TIME, WATCH_SKEY_PATH, SMARTCONTRACT_ADDR, SMARTCONTRACT_PATH, TOKEN_POLICY_ID, TOKEN_NAME, DATUM_HASH, RECIPIENT_ADDR, RETURN_ADA, PRICE, COLLATERAL, REFUND_AMNT, 0, 0])

                            if tx_refund_a_hash != 'error':
                                with open(SWAP_SETTINGS[1], 'a') as runlog:
                                    runlog.write('\nRefund hash found, TX completed. Writing to payments.log.')
                                    runlog.close()
                            else:
                                with open(SWAP_SETTINGS[1], 'a') as runlog:
                                    runlog.write('\nTX attempted, error returned by withdraw')
                                    runlog.close()
                                # continue to try again (no archive yet)
                                continue

                            # Record the payment as completed, leave whitelist untouched since not a valid swap tx
                            with open(SWAP_SETTINGS[1], 'a') as runlog:
                                runlog.write('\nRefund hash found, TX completed. Writing to payments.log.')
                                runlog.close()
                            payments_file = PROFILELOG + 'payments.log'
                            with open(payments_file, 'a') as payments_a:
                                payments_a.write(SWAP_SETTINGS[2] + '\n')
                                payments_a.close()

                        # TODOswap: May need to improve this logic for missed if statement cases
                        continue
                
                    # Refresh Low SC Balance
                    if RECURRING:
                        with open(SWAP_SETTINGS[1], 'a') as runlog:
                            runlog.write('\nRecurring deposits enabled, attempting to replenish SC...')
                            runlog.close()
                        CHECK_PRICE = 0
                        if EXPECT_ADA != PRICE:
                            CHECK_PRICE = int(PRICE)
                            with open(SWAP_SETTINGS[1], 'a') as runlog:
                                runlog.write('Price set as: '+str(CHECK_PRICE))
                                runlog.close()
                        filePre = 'replenishSC_' + time_now() + '_'
                        tx_rsc_hash = deposit(DEFAULT_SETTINGS, [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING, filePre, TX_HASH, ADA_RECVD, TX_TIME, WATCH_SKEY_PATH, SMARTCONTRACT_ADDR, SMARTCONTRACT_PATH, TOKEN_POLICY_ID, TOKEN_NAME, DEPOSIT_AMNT, SC_ADA_AMNT, WT_ADA_AMNT, DATUM_HASH, CHECK_PRICE, COLLATERAL, TOKENS_TOSWAP, RECIPIENT_ADDR, True])
                        if tx_rsc_hash != 'error':
                            with open(SWAP_SETTINGS[1], 'a') as runlog:
                                runlog.write('\nRefund hash found, TX completed. Writing to payments.log.')
                                runlog.close()
                        else:
                            with open(SWAP_SETTINGS[1], 'a') as runlog:
                                runlog.write('\nTX attempted, error returned by withdraw')
                                runlog.close()
                            # continue and try again (do not archive yet)
                            continue

                        # Record the payment as completed and remove from whitelist if set to true
                        payments_file = PROFILELOG + 'payments.log'
                        with open(payments_file, 'a') as payments_a:
                            payments_a.write(SWAP_SETTINGS[2] + '\n')
                            payments_a.close()
                    
                        # Remove from whitelist if necessary
                        if WLENABLED and WHITELIST_ONCE:
                            clean_wlws = RECIPIENT_ADDR
                            with open(whitelist_file,'r') as read_file:
                                lines = read_file.readlines()
                            currentLine = 0
                            with open(whitelist_file,'w') as write_file:
                                for line in lines:
                                    wl_addr = line.strip('\n')
                                    # Checks for long addr to clean all/any instances of the wallet from the whitelist
                                    if len(wl_addr) == 103:
                                        wl_addr = wl_addr[52:-6]
                                        clean_wlws = clean_wlws[52:-6]
                                    # Compare and write non-matches
                                    if wl_addr != clean_wlws:
                                        write_file.write(line)
                            read_file.close()
                            write_file.close()
                        continue
                    else:
                        with open(SWAP_SETTINGS[1], 'a') as runlog:
                            runlog.write('\nNot a recurring-deposit profile (will try to refund): addr:'+RECIPIENT_ADDR+' | tokens:'+str(TOKENS_TOSWAP)+' | ada:'+str(ADA_RECVD))
                            runlog.close()
                        if AUTO_REFUND:
                            REFUND_AMNT = TOKENS_TOSWAP - FEE_CHARGE
                            with open(SWAP_SETTINGS[1], 'a') as runlog:
                                runlog.write('\nSending Refund: '+str(REFUND_AMNT))
                                runlog.close()
                            filePre = 'refundTO' + RECIPIENT_ADDR + '_' + time_now() + '_'
                            tx_refund_b_hash = withdraw(DEFAULT_SETTINGS, [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING, filePre, TX_HASH, ADA_RECVD, TX_TIME, WATCH_SKEY_PATH, SMARTCONTRACT_ADDR, SMARTCONTRACT_PATH, TOKEN_POLICY_ID, TOKEN_NAME, DATUM_HASH, RECIPIENT_ADDR, RETURN_ADA, PRICE, COLLATERAL, REFUND_AMNT, 0, 0])

                            if tx_refund_b_hash != 'error':
                                with open(SWAP_SETTINGS[1], 'a') as runlog:
                                    runlog.write('\nRefund hash found, TX completed. Writing to payments.log.')
                                    runlog.close()
                            else:
                                with open(SWAP_SETTINGS[1], 'a') as runlog:
                                    runlog.write('\nTX attempted, error returned by withdraw')
                                    runlog.close()
                                # continue to try again (no archive yet)
                                continue
                        else:
                            # Archive completed TX
                            tx.archive_tx(DEFAULT_SETTINGS, [TX_HASH, ADA_RECVD, TX_TIME])
                        # Record the payment as completed
                        payments_file = PROFILELOG + 'payments.log'
                        with open(payments_file, 'a') as payments_a:
                            payments_a.write(SWAP_SETTINGS[2] + '\n')
                            payments_a.close()
                        continue

                # Run swap or minting on matched tx
                with open(SWAP_SETTINGS[1], 'a') as runlog:
                    runlog.write('\nProcess this TX Swap: '+RECIPIENT_ADDR+' | tokens:'+str(TOKENS_TOSWAP)+' | ada:'+str(ADA_RECVD))
                    runlog.close()
                filePre = 'swapTO' + RECIPIENT_ADDR + '_' + time_now() + '_'
                type_title = 'SmartContract Swap'
                tx_final_hash = smartcontractswap(DEFAULT_SETTINGS, [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING, filePre, TX_HASH, ADA_RECVD, TX_TIME, WATCH_SKEY_PATH, SMARTCONTRACT_ADDR, SMARTCONTRACT_PATH, TOKEN_POLICY_ID, TOKEN_NAME, DATUM_HASH, RECIPIENT_ADDR, RETURN_ADA, PRICE, COLLATERAL, TOKENS_TOSWAP, SWAP_TYPE])

            if PROFILE_TYPE == 1:
                print('Profile Type is 1')
                # Process Traditional Auction End
                if AUCTION == 0 and STAT == 2 and END_NOW:
                    print('Traditional Auction - END_NOW = TRUE')
                    final_file = PROFILELOG + 'final.log'
                    is_final_log = isfile(final_file)
                    if not is_final_log:
                        try:
                            open(final_file, 'x')
                            with open(final_file, 'a') as final_header:
                                final_header.write('FromAddr,' + 'Amount,' + 'Action\n')
                                final_header.close()
                        except OSError:
                            pass
                    UPDATEMINTSETTINGS = REFUND_PENDING = NFT_MINTED = PROFILE['nft_minted'] = True
                            
                    # Find and log the final bid addrs and tallys
                    print('Find the winning bidder, entering process_tally with only Default Settings (meaning addr = none and amount = 0)...')
                    highest_addr, highest_tally = tx.process_tally(DEFAULT_SETTINGS)

                # Precheck if block locking height reached for Sale type - in case need to begin refunds
                if AUCTION == 2 and not END_NOW:
                    end_minting, _ = check_tip(DEFAULT_SETTINGS, runlog_file, NFT_DATA[4], NFT_DATA[3])
                    if end_minting:
                        with open(runlog_file, 'a') as runlog:
                            runlog.write('\nFinal NFT to mint, ending sale and setting to refund-only mode')
                            runlog.close()
                        # Set end to now and update settings
                        AUCTIONEND, _, _ = tx.get_tip(DEFAULT_SETTINGS, 0)
                        AUCTIONEND = int(AUCTIONEND)
                        UPDATEMINTSETTINGS = NFT_MINTED = END_NOW = PROFILE['nft_minted'] = True

                # Process for non-record/nft_single_minted and false end_now (auction not ended)
                # TODOmint: Test the logic of this if statement
                if ((STAT == 0 or NFT_MINTED) and not END_NOW) or (AUCTION == 2 and END_NOW):
                    print('STAT was 0 or NFT_MINTED = true...and, END_NOW = False ... OR Auction = 2 (sale) and end_now is true')
                    with open(runlog_file, 'a') as runlog:
                        runlog.write('\n--- Check For Payments Found Refundable NFT TX ---\n')
                        runlog.close()
                    MAGIC_PRICE = EXPECT_ADA
                    if MAGIC_PRICE == 0:
                        MAGIC_PRICE = MIN_WATCH
                    if NFT_MINTED:
                        MAGIC_PRICE = 0
                    REFUND_AMNT = ADA_RECVD
                    if len(TOKEN_POLICY_ID) > 0:
                        # Calculate Fee Refund or Token Included - if any
                        TOKEN_STRING = tx.get_token_string_id(TOKEN_POLICY_ID + '.' + TOKEN_NAME)
                        if TK_AMT >= 20 and TK_NAME == TOKEN_STRING:
                            with open(runlog_file, 'a') as runlog:
                                runlog.write('\nRecieved Tokens, fee refunding: '+str(TK_AMT))
                                runlog.close()
                            REFUND_AMNT = ADA_RECVD + 200000
                            REFUND_TYPE = 2
                        else:
                            REFUND_AMNT = ADA_RECVD
                            REFUND_TYPE = 1
                        # If auction type is Traditional, refund does not include clues
                        if AUCTION == 0 and REFUND_TYPE == 1:
                            REFUND_TYPE = 3
                        if AUCTION == 0 and REFUND_TYPE == 2:
                            REFUND_TYPE = 4
                    # Process the refund and record payment
                    with open(runlog_file, 'a') as runlog:
                        runlog.write('\nRefunding Bid: '+str(REFUND_AMNT))
                        runlog.close()
                    filePre = 'refundTO' + RECIPIENT_ADDR + '_' + time_now() + '_'
                    print('Here refund either mismatch or after the auction is done (depending on auction type) but exit for debug')
                    exit(0)
                    tx_refund_c_hash = withdraw(DEFAULT_SETTINGS, [API_URI, API_ID, MINT_ADDR, FEE_TOKEN_STRING, filePre, TX_HASH, ADA_RECVD, TX_TIME, MINT_SKEY, '', '', TOKEN_POLICY_ID, TOKEN_NAME, '', RECIPIENT_ADDR, RETURN_ADA, '', COLLATERAL, REFUND_AMNT, REFUND_TYPE, MAGIC_PRICE])

                    # Record the payment as completed, leave whitelist untouched since not a valid swap tx
                    if tx_refund_c_hash != 'error':
                        with open(runlog_file, 'a') as runlog:
                            runlog.write('\nHash found, TX completed. Writing to payments.log...')
                            runlog.close()
                        payments_file = PROFILELOG + 'payments.log'
                        with open(payments_file, 'a') as payments_a:
                            payments_a.write(result + '\n')
                            payments_a.close()
                    else:
                        with open(runlog_file, 'a') as runlog:
                            runlog.write('\nTX attempted, error returned by withdraw')
                            runlog.close()
                        # Do not archive in this case, just continue and try again
                    continue

                # If Auction is Traditional and NOT ended
                if AUCTION == 0 and not END_NOW:
                    print('Traditional Auction type = 0 .. and .. END_NOW = False')
                    # If first good bid, set end slot and update settings
                    if AUCTIONEND == 0:
                        print('AuctionEnd = 0, updating with this bid')
                        _, AUCTIONEND, _ = tx.get_tip(DEFAULT_SETTINGS, BLOCKTIME)
                        AUCTIONEND = int(AUCTIONEND)
                        # Update settings with new endtime set
                        UPDATEMINTSETTINGS = True

                # If Auction is Guess
                if AUCTION == 1:
                    print('This is a Guess-style auction, minting, updating and ending')
                    UPDATEMINTSETTINGS = NFT_MINTED = END_NOW = PROFILE['nft_minted'] = True
                
                # Non-Auction / Multiset NFT Sale
                if AUCTION == 2 and STAT == 2 and not END_NOW:
                    # Get folder contents and current count
                    img_name_list = listdir(json_queue)
                    mint_qcount = 0
                    img_list = []
                    for img_name in img_name_list:
                        mint_qcount += 1
                        try:
                            img_list.append(int(img_name.split('.')[0]))
                        except:
                            with open(runlog_file, 'a') as runlog:
                                runlog.write('\nError with image ' + str(img_name))
                                runlog.close()
                            continue
                    NFT_DATA[1] = img_list
                    print('Sale (non-auction), Good TX, Not End_Now')
                    with open(runlog_file, 'a') as runlog:
                        runlog.write('\nFound Good NFT Sale TX! Process this TX Mint')
                        runlog.close()
                    filePre = '_Mint_' + NFT_DATA[0] + '_to_' + RECIPIENT_ADDR + '_at_' + time_now() + '_'
                    type_title = 'Mint-On-Demand_Multi'
                    if NFT_ADDR:
                        MINT_AT = RECIPIENT_ADDR
                    else:
                        MINT_AT = MINT_ADDR
                    with open(runlog_file, 'a') as runlog:
                        runlog.write('\nMinting to address: ' + MINT_AT + ' | json_file:' + NFT_DATA[2] + ' | policy_file:' + MINT_POLICY_VKEY + ' | name:' + NFT_DATA[0] + ' | lock:' + NFT_DATA[3])
                        runlog.close()

                    if mint_qcount == 1:
                        with open(runlog_file, 'a') as runlog:
                            runlog.write('\nFinal NFT to mint, ending sale and setting to refund-only mode')
                            runlog.close()
                        # Set end to now and update settings
                        AUCTIONEND, _, _ = tx.get_tip(DEFAULT_SETTINGS, 0)
                        AUCTIONEND = int(AUCTIONEND)
                        UPDATEMINTSETTINGS = NFT_MINTED = END_NOW = PROFILE['nft_minted'] = True
                    # Ready to mint
                    PROC_MINT = True

                if UPDATEMINTSETTINGS:
                    print('Updating profile.json!')
                    # Open and Update
                    UpdateSetFile = 'profile.json'
                    update_minting = json.load(open(UpdateSetFile, 'r'))
                    LOADED = update_minting[PROFILE_NAME]
                    updateMinting = {'type':LOADED['type'],'log':LOADED['log'],'cache':LOADED['cache'],'txlog':LOADED['txlog'],'collateral':LOADED['collateral'],'network':LOADED['network'],'cli_path':LOADED['cli_path'],'api_pre':LOADED['api_pre'],'api_post':LOADED['api_post'],'api':LOADED['api'],'expectada':LOADED['expectada'],'min_watch':LOADED['min_watch'],'wlenabled':LOADED['wlenabled'],'wlone':LOADED['wlone'],'nft_addr':LOADED['nft_addr'],'wallet_skey':LOADED['wallet_skey'],'wallet_vkey':LOADED['wallet_vkey'],'policy_skey':LOADED['policy_skey'],'policy_vkey':LOADED['policy_vkey'],'returnada':LOADED['returnada'],'nft_data':LOADED['nft_data'],'tokenid':LOADED['tokenid'],'tokenname':LOADED['tokenname'],'nft_minted':NFT_MINTED,'auction':LOADED['auction'],'blocktime':LOADED['blocktime'],'auctionend':AUCTIONEND}

                    # Save/Update whitelist and profile.json files
                    update_minting[PROFILE_NAME] = updateMinting
                    jsonOUTSettings = json.dumps(update_minting)
                    with open(UpdateSetFile, 'w') as update_s:
                        update_s.write(jsonOUTSettings)
                        update_s.close()
                    UPDATEMINTSETTINGS = False
                
                if (AUCTION == 1 and END_NOW) or (AUCTION == 0 and STAT == 2 and END_NOW):
                    print('END_NOW is True, this is the minting section')
                    with open(runlog_file, 'a') as runlog:
                        runlog.write('\nFound Good NFT TX! Process this TX Mint')
                        runlog.close()
                    filePre = NFT_DATA[1] + '_Mint_' + NFT_DATA[0] + '_to_' + RECIPIENT_ADDR + '_at_' + time_now() + '_'
                    type_title = 'Mint-On-Demand_Single'
                    if NFT_ADDR:
                        MINT_AT = RECIPIENT_ADDR
                    else:
                        MINT_AT = MINT_ADDR
                    with open(runlog_file, 'a') as runlog:
                        runlog.write('\nSet minted flag to True')
                        runlog.write('\nMinting to address: '+MINT_AT+' | json_file:'+NFT_DATA[2]+' | policy_file:'+ MINT_POLICY_VKEY + ' | name:' + NFT_DATA[0] + ' | lock:' + NFT_DATA[3])
                        runlog.close()
                    PROC_MINT = True
                elif AUCTION != 2:
                    with open(runlog_file, 'a') as runlog:
                        runlog.write('\nFound Good Bid TX! Record Bid in Payments and in Pending logs')
                        runlog.close()
                    filePre = 'BidPlacedFor_' + NFT_DATA[1] + '-' + NFT_DATA[0] + '_to_' + RECIPIENT_ADDR + '_at_' + time_now() + '_'
                    type_title = 'BidOn_Single'
                    tx_final_hash = 'bid'
                    # Archive the bid tx received
                    tx.archive_tx(DEFAULT_SETTINGS, [TX_HASH, ADA_RECVD, TX_TIME])
                
                if PROC_MINT:
                    print('Next we would mint, bypass/exit for debug..')
                    exit(0)
                    tx_final_hash = process_mint(DEFAULT_SETTINGS, [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING, filePre, TX_HASH, ADA_RECVD, TX_TIME, RETURN_ADA, MINTSRC, MINT_ADDR, MINT_SKEY, MINT_POLICY_SKEY, MINT_AT, NFT_DATA, False, MINT_POLICY_VKEY])

            # With either type watch for TX and record payment
            if tx_final_hash != 'error' or tx_final_hash == 'bid':
                with open(runlog_file, 'a') as runlog:
                    runlog.write('\n' + type_title + ' TX Hash Found: '+tx_final_hash)
                    runlog.close()

                # Record the payment as completed
                payments_file = PROFILELOG + 'payments.log'
                with open(payments_file, 'a') as payments_a:
                    payments_a.write(result + '\n')
                    payments_a.close()

                if WLENABLED and WHITELIST_ONCE:
                    clean_wlws = RECIPIENT_ADDR
                    with open(whitelist_file,'r') as read_file:
                        lines = read_file.readlines()
                    currentLine = 0
                    with open(whitelist_file,'w') as write_file:
                        for line in lines:
                            if line.strip('\n') != clean_wlws:
                                write_file.write(line)
                    read_file.close()
                    write_file.close()
                # Loop and process all non-winning bid tallys if auction ended
                if END_NOW and REFUND_PENDING:
                    print('END_NOW and REFUND_PENDING - looping')
                    with open(final_file, 'r') as final_r:
                        readfi = 0
                        last_tally = 0
                        tally = amount

                        # Iterate over file lines
                        for final_line in final_r:
                            if readfi == 0:
                                readfi += 1
                                continue
                            readfi += 1
                            fl = final_line.split(',')
                            if fl[2] == 'won':
                                continue
                            TX_HASH, ADA_RECVD, TX_TIME = 'none', 0, 0
                            REFUND_ADDR = fl[0]
                            REFUND_AMT = fl[1]
                            MAGIC_PRICE = 0
                            # TODOmint: Refund_type for these cumulative refunds!
                            print('Refund the following (skip actual refund for debug):')
                            print(REFUND_ADDR + ' | ' + str(REFUND_AMT))
                            # For debug:
                            tx_end_refund_hash = 'debug'
                            #tx_end_refund_hash = withdraw(DEFAULT_SETTINGS, [API_URI, API_ID, MINT_ADDR, FEE_TOKEN_STRING, filePre, TX_HASH, ADA_RECVD, TX_TIME, MINT_SKEY, '', '', TOKEN_POLICY_ID, TOKEN_NAME, '', REFUND_ADDR, RETURN_ADA, '', COLLATERAL, REFUND_AMT, REFUND_TYPE, MAGIC_PRICE])

                            # Record the payment as completed, leave whitelist untouched since not a valid swap tx
                            if tx_end_refund_hash != 'error':
                                with open(runlog_file, 'a') as runlog:
                                    runlog.write('\nRefund completed for:' + REFUND_ADDR + ' in amount of ' + REFUND_AMT)
                                    runlog.close()
                            else:
                                with open(runlog_file, 'a') as runlog:
                                    runlog.write('\nRefund TX attempted, error returned by withdraw for address and amount:' + REFUND_ADDR + ' | ' + REFUND_AMT)
                                    runlog.close()
                            #continue
                        print('done refunding! exit debug')
                        exit(0)
            else:
                with open(runlog_file, 'a') as runlog:
                    runlog.write('\n' + type_title + ' Failed: '+RECIPIENT_ADDR+' | '+str(ADA_RECVD))
                    runlog.close()
                # Do not archive, keep trying or refund in case of error due to cannot-mint
        whitelist_r.close()

        # Check again and sleep program for NN seconds
        tx.log_new_txs(DEFAULT_SETTINGS, LOGGER_SETTINGS)
        sleep(DELAY_TIME)

def profile_chooser(profile_name, load_profile):
    profile_list = list(load_profile.keys())
    pnum = 0
    for pname in profile_list:
        adend = ''
        if profile_name == pname:
            adend = ' (loaded)'
        print(str(pnum) + ' - ' + pname + adend)
        pnum += 1
    pselect = input('Enter Your Profile Choice:')
    pselect = int(pselect)
    load_name = list(load_profile.keys())[pselect]
    return load_name, load_profile[load_name]

if __name__ == "__main__":
    # Defaults
    DELAY_TIME = 30
    TESTNET = False
    DEBUG = False

    # Get user options
    arguments = argv[1:]
    shortopts= "po"
    longopts = ["profile=","option="]

    # Setting behaviour for options
    PROFILE_PASSED = ''
    OPTION_PASSED = ''
    options, args = getopt(arguments, shortopts,longopts)
    for opt, val in options: 
        if opt in ("-p", "--profile"):
            PROFILE_PASSED = str(val)
        elif opt in ("-o", "--option"):
            OPTION_PASSED = str(val)

    # Setup Temp Directory (try to)
    scptroot = realpath(dirname(__file__))
    APPROOT = osjoin(scptroot, '')
    SRC = osjoin(osjoin(scptroot, 'smartcontract-src'), '')
    MINTSRC = osjoin(osjoin(scptroot, 'minting-src'), '')

    logname = 'profiles'
    logpath = osjoin(scptroot, logname)
    LOGROOT = osjoin(logpath, '')

    mintlogname = 'minted'
    mintlogpath = osjoin(scptroot, mintlogname)
    MINTROOT = osjoin(mintlogpath, '')

    try:
        mkdir(mintlogname)
    except OSError:
        pass
    try:
        mkdir(logname)
    except OSError:
        pass

    # Setup Settings Dictionary
    settings_file = 'profile.json'
    is_settings_file = isfile(settings_file)
    if not is_settings_file:
        import config
        config.setup(LOGROOT)
    
    # Get any set profile name and load settings
    PROFILE_NAME = ''
    if len(PROFILE_PASSED) > 0:
        PROFILE_NAME = PROFILE_PASSED
    load_profile = json.load(open(settings_file, 'r'))
    if len(PROFILE_NAME) == 0:
        PROFILE_NAME = list(load_profile.keys())[0]
    PROFILE = load_profile[PROFILE_NAME]

    # Present dApp
    if len(OPTION_PASSED) > 0:
        if OPTION_PASSED == 'logonly':
            running = False
        elif OPTION_PASSED == 'process':
            if '_testnet-mwl_' in PROFILE['wallet_vkey']:
                OPTION_PASSED = ''
            else:
                running = True
        elif OPTION_PASSED == 'debug':
            DEBUG = True
        else:
            print('\nInvalid option, exiting...')
            exit(0)
    if len(OPTION_PASSED) == 0:
        if '_testnet-mwl_' in PROFILE['wallet_vkey']:
            print('\nALERT: You tried starting this profile with Testnet wallet keys.\n       To run your profile in testnet mode, choose option 3 below.')
        print('\n ---------------------------------------------------------------')
        print('|                                                               |')
        print('|                                                               |')
        print('|                Welcome to Minted With Lovelace                |')
        print('|                      ~ a Cardano dApp ~                       |')
        print('|                                                               |')
        print(' ---------------------------------------------------------------')
        print('|                                                               |')
        print('|             <<< What would you like to do? >>>                |')
        print('|                                                               |')
        print('|                 0 - Create a New Profile                      |')
        print('|                 1 - Reconfigure an Existing Profile           |')
        print('|                 2 - Log Transactions Only                     |')
        print('|                 3 - Run Profile on Testnet                    |')
        print('|                 4 - Retry Failed IPFS Image Processing        |')
        print('|                 5 - Mint an NFT/Token Manually                |')
        print('|                 6 - Display Profile Data                      |')
        print('|                                                               |')
        print(' ---------------------------------------------------------------')
        option_select = input('\nEnter Your Choice:')
        option_select = int(option_select)
        if option_select == 0:
            import config
            # Choose profile
            PROFILE_NAME, _ = profile_chooser(PROFILE_NAME, load_profile)
            # Run the setup
            config.setup(LOGROOT, PROFILE_NAME, False, True)
        elif option_select == 1:
            import config
            # Choose profile
            PROFILE_NAME, _ = profile_chooser(PROFILE_NAME, load_profile)
            # Run the reconfig
            config.setup(LOGROOT, PROFILE_NAME, True)
        elif option_select == 2:
            # Choose profile
            PROFILE_NAME, PROFILE = profile_chooser(PROFILE_NAME, load_profile)
            # Run the logger
            running = False
        elif option_select == 3:
            # Choose profile
            PROFILE_NAME, PROFILE = profile_chooser(PROFILE_NAME, load_profile)
            # Get and set overrides
            running = True
            TESTNET = True
            DEBUG = True
            NETWORK = PROFILE['network'] = 'testnet-magic'
            MAGIC = input('Enter Testnet Magic Number:')
            API_ID = PROFILE['api'] = input('Enter Blockfrost API Testnet ID:')
            API_NET = 'testnet'
            token_override = input('Override a Token (enter True or False):')
            if token_override == 'true' or token_override == 'True':
                TOKEN_POLICY_ID = PROFILE['tokenid'] = input('Enter Testnet Token Policy ID:')
                TOKEN_NAME = PROFILE['tokenname'] = input('Enter Testnet Token Name:')
            else:
                TOKEN_POLICY_ID = PROFILE['tokenid']
                TOKEN_NAME = PROFILE['tokenname']
            PROFILE['nft_data'][3] = input('Enter Slot Height Override:')
            delay_input = input('Enter a Delay Override in Seconds (30 is default)')
            DELAY_TIME = int(delay_input)
        elif option_select == 4:
            import config
            # Choose profile
            PROFILE_NAME, PROFILE = profile_chooser(PROFILE_NAME, load_profile)
            # Run the reprocess
            ipfs_log = log + 'ipfs_retry.log'
            json_out = PROFILE['nft_data'][2]
            result, proc_err = config.start_process_ipfs(ipfs_log, json_out)
            print('\nComplete. Results: ' + result)
            if proc_err:
                print('ERROR(s) Encountered. Please review the ipfs_retry.log file.')
            exit(0)
        elif option_select == 5:
            # Run manual minting
            manual_mint(MINTROOT)
        elif option_select == 6:
            # Choose profile
            PROFILE_NAME, PROFILE = profile_chooser(PROFILE_NAME, load_profile)
            display_profile = json.load(open(settings_file, 'r'))[PROFILE_NAME]
            # Display profile
            print('\n\n    Stored Settings for Profile ' + PROFILE_NAME)
            print('\n')
            print(display_profile)
            print('\n')
            exit(0)
        else:
            print('\nInvalid option, exiting...')
            exit(0)
    
    # Load Profile Settings
    if not TESTNET:
        NETWORK = PROFILE['network']
        MAGIC = ''
        API_ID = PROFILE['api']
        API_NET = 'mainnet'
        TOKEN_POLICY_ID = PROFILE['tokenid']
        TOKEN_NAME = PROFILE['tokenname']
    CLI_PATH = PROFILE['cli_path']
    FEE_TOKEN_STRING = ''
    API_URI = PROFILE['api_pre'] + API_NET + PROFILE['api_post']
    PROFILELOG = PROFILE['log']
    PROFILECACHE = PROFILE['cache']
    PROFILETXLOG = PROFILE['txlog']
    PROFILE_TYPE = PROFILE['type']
    EXPECT_ADA = PROFILE['expectada']
    COLLATERAL = PROFILE['collateral']

    # Instantiate Main Log File
    RUNLOG_FILE = PROFILELOG + 'run.log'
    is_main_runlog = isfile(RUNLOG_FILE)
    if not is_main_runlog:
        try:
            open(RUNLOG_FILE, 'x')
        except OSError:
            pass

    # Populate main default settings
    DEFAULT_SETTINGS = [PROFILE_NAME, PROFILE_TYPE, CLI_PATH, NETWORK, MAGIC, PROFILELOG, PROFILECACHE, PROFILETXLOG, TESTNET]

    # If profile type is SC swap
    if PROFILE_TYPE == 0:
        # Vars
        WATCH_SKEY_PATH = PROFILE['watchskey']
        WATCH_VKEY_PATH = PROFILE['watchvkey']
        SMARTCONTRACT_PATH = PROFILE['scpath']
        PRICE = PROFILE['price']
        SWAP_TYPE = PROFILE['swap_type']

        # Get dynamic data
        WATCH_ADDR = tx.get_wallet_addr(DEFAULT_SETTINGS, WATCH_VKEY_PATH)
        WATCH_KEY_HASH = tx.get_address_pubkeyhash(CLI_PATH, WATCH_VKEY_PATH)
        MINT_ADDR = ''
        SC_ADDR = WATCH_ADDR
        
        # Check for smartcontract file and prompt to create if not found
        if SWAP_TYPE == 0:
            sc_file = SMARTCONTRACT_PATH
            is_sc_file = isfile(sc_file)
            SC_SETTINGS = [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING, '', 'none', '', '', APPROOT, SMARTCONTRACT_PATH, SRC, WATCH_KEY_HASH, PRICE]
            if not is_sc_file:
                import config
                config.create_smartcontract(DEFAULT_SETTINGS, SC_SETTINGS)
            SC_ADDR = tx.get_smartcontract_addr(DEFAULT_SETTINGS, SMARTCONTRACT_PATH)
            
            # Get custom options relevant to this profile type
            if len(OPTION_PASSED) > 1 and len(API_ID) > 1 and len(WATCH_ADDR) > 1:
                if OPTION_PASSED == 'create_smartcontract':
                    import config
                    config.create_smartcontract(DEFAULT_SETTINGS, SC_SETTINGS)
                if OPTION_PASSED == 'deposit':
                    CHECK_PRICE = 0
                    if EXPECT_ADA != PRICE:
                        CHECK_PRICE = int(PRICE)
                        log_it('To check if price amount in wallet: ' + str(CHECK_PRICE))
                    DEPOSIT_SETTINGS = [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING, '', 'none', '', '', WATCH_SKEY_PATH, WATCH_VKEY_PATH, WATCH_KEY_HASH, SMARTCONTRACT_PATH, TOKEN_POLICY_ID, TOKEN_NAME, CHECK_PRICE, COLLATERAL, SWAP_TYPE]
                    start_deposit(DEFAULT_SETTINGS, DEPOSIT_SETTINGS)

    # If profile type is auction/minting
    if PROFILE_TYPE == 1:
        if len(TOKEN_POLICY_ID) > 1:
            FEE_TOKEN_STRING = TOKEN_POLICY_ID + '.' + TOKEN_NAME
        WATCH_ADDR = SC_ADDR = MINT_ADDR = tx.get_wallet_addr(DEFAULT_SETTINGS, PROFILE['wallet_vkey'])
        WATCH_KEY_HASH = ''

    # Start get_transactions-only thread
    if running == False:
        I_STATE = 0
        log_it('Begin logging transactions')
        while True:
            I_STATE += 1
            if I_STATE == 450:
                log_it('TX Processor active for ' + str(I_STATE) + ' iterations since last update')
                I_STATE = 0
            logging_result = tx.log_new_txs(DEFAULT_SETTINGS, [API_URI, API_ID, WATCH_ADDR, FEE_TOKEN_STRING])
            sleep(2)

    # Start main process TX thread
    if running == True:
        I_STATE = 0
        log_it('Starting Main Process')
        CUSTOM_SETTINGS = [API_URI, API_ID, FEE_TOKEN_STRING, PROFILE, MINTSRC, DELAY_TIME, WATCH_ADDR, SC_ADDR, MINT_ADDR, WATCH_KEY_HASH]
        tx_processor()