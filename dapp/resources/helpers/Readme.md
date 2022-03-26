## How To Setup a Cardano Node

Based on the excellent guide on [CoinCashew](https://www.coincashew.com/coins/overview-ada/guide-how-to-build-a-haskell-stakepool-node), these scripts will allow you to (almost)automate the process of installing a Cardano full node for either Testnet or Mainnet. This is not the same as a staking pool, which the [CoinCashew](https://www.coincashew.com/coins/overview-ada/guide-how-to-build-a-haskell-stakepool-node) guide goes on to cover from where these scripts leave off.

As part of the MintedWithLovelace project, these scripts can be used to easily setup a Cardano Full Node, ready to run Minted. Learn more about the project and the Minted dApp in the [MintedWithLovelace Discord](https://discord.gg/2xEVRTSAeQ).

1. Install Debian 10+ or Ubuntu 18+ on a computer, VM, or VPS/Baremetal Server with at minimum 8 GB of RAM (16 GB is ideal).
2. Once logged into your server, perform a little security "hardening" by following this guide: https://www.coincashew.com/coins/overview-ada/guide-how-to-build-a-haskell-stakepool-node/how-to-harden-ubuntu-server
3. After your server is hardened, run the following Node Setup script for the network type of this system (mainnet or testnet) using the appropriate script from this repo. 
      (Alternatively you may also use the script as a step by step guide and issue each command by hand)

### PREREQS:
Before you run the script of your choice below, first run the following in your terminal and reboot your device

Replace the word "testnet" with "mainnet" if you are running the mainnet script next.

```
# Replace the word testnet with mainnet in this line if you are doing this on a mainnet device:
echo export NODE_CONFIG=testnet>> $HOME/.bashrc

echo export TMPDIR=$HOME/.temp_mwl>> $HOME/.bashrc
echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc
source $HOME/.bashrc
echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc
echo export NODE_HOME=$HOME/cardano-my-node >> $HOME/.bashrc
source $HOME/.bashrc
echo export CARDANO_CLI=/usr/local/bin/cardano-cli >> $HOME/.bashrc
echo export TESTNET_MAGIC_NUM=1097911063 >> $HOME/.bashrc
echo export CARDANO_NODE_SOCKET_PATH="$NODE_HOME/db/socket" >> $HOME/.bashrc
source $HOME/.bashrc
```
Remember to reboot after these prereq commands before running the script.

### For Testnet:
https://github.com/MadeWithLovelace/MintedWithLovelace/blob/main/dapp/resources/helpers/setupTestNode.sh

### For Mainnet:
https://github.com/MadeWithLovelace/MintedWithLovelace/blob/main/dapp/resources/helpers/setupMainNode.sh

After the script completes it will have attempted to launch cardano-node. Check for any errors (there will be some warnings this is normal) and use ./gLiveView to see the live status of the sync/node.

After syncing has reached 100%, run Minted from any folder in your server, preferably a user, not root, and refer to the pinned message in the #_refinery_ channel before setting up your campaign, for a brief rundown of how to use the dApp. If you are not in the Refinery and would like to test the dApp out, send me a DM and let me know and I'll get you setup.
