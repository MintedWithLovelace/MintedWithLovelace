## How To Setup a Cardano Node

The following will allow you to (almost)automate the process of installing a Cardano full node for either Testnet or Mainnet.

*Note: This will setup a full node. To setup a staking node, you will need to perform additional steps which are outlined in the excellent [CoinCashew](https://www.coincashew.com/coins/overview-ada/guide-how-to-build-a-haskell-stakepool-node) guide, from which our node setup has been derived.*

As part of the MintedWithLovelace project, these scripts can be used to easily setup a Cardano Full Node, ready to run the Minted dApp. Learn more about the project and the Minted dApp in the [Official MintedWithLovelace Discord](https://discord.gg/2xEVRTSAeQ).

### Before You Continue
Please make sure your system meets the requirements and if this is a mainnet node, it's advised to take measures to secure your system.

1. Install Debian 10+ or Ubuntu 18+ on a computer, VM, or VPS/Baremetal Server with at minimum 8 GB of RAM (16 GB is ideal).
2. Once logged into your server, perform a little security "hardening" by following this guide: [Hardening an Ubuntu Server](https://www.coincashew.com/coins/overview-ada/guide-how-to-build-a-haskell-stakepool-node/part-i-installation/hardening-an-ubuntu-server)

After your server is hardened, follow the steps below for the network type (testnet or mainnet) you are setting up.

### Node Setup Steps
It's a good idea to update your system before you proceed. Do this with:
```
sudo apt update
sudo apt upgrade -y
```

#### Step 1
Before you run the script of your choice below, first run: [prepInstall.sh](https://github.com/MadeWithLovelace/MintedWithLovelace/raw/main/dapp/resources/helpers/prepSetup.sh)

#### Step 2
Enter the appropriate command into your terminal for either mainnet or testnet:

For Mainnet
```
echo -e "\r" >> $HOME/.bashrc
echo export NODE_CONFIG=mainnet >> $HOME/.bashrc
source $HOME/.bashrc

```

For Testnet
```
echo -e "\r" >> $HOME/.bashrc
echo export NODE_CONFIG=testnet >> $HOME/.bashrc
source $HOME/.bashrc

```

#### Step 3
Enter the following into your terminal and then reboot

```
echo export TMPDIR=$HOME/.temp_mwl>> $HOME/.bashrc
echo export TESTNET_MAGIC_NUM=1097911063 >> $HOME/.bashrc
echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc
echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc
echo export NODE_HOME=$HOME/cardano-my-node >> $HOME/.bashrc
echo export CARDANO_CLI=/usr/local/bin/cardano-cli >> $HOME/.bashrc
echo export CARDANO_NODE_SOCKET_PATH="$NODE_HOME/db/socket" >> $HOME/.bashrc
source $HOME/.bashrc
sudo reboot now

```
Remember to reboot after these prereq commands before running the script!

#### Step 4
Finally, run [setupNode.sh](https://github.com/MadeWithLovelace/MintedWithLovelace/raw/main/dapp/resources/helpers/setupNode.sh)

This may take a while to complete. After it finishes, check for any errors in the terminal output and report them in our Discord.


After the script completes it will have attempted to launch cardano-node. Use ./gLiveView to see the live status of the sync/node.

After syncing has reached 100%, run Minted from any folder in your server, preferably a user, not root, and refer to the pinned message in the #_refinery_ channel before setting up your campaign, for a brief rundown of how to use the dApp. If you are not in the Refinery and would like to test the dApp out, send me a DM and let me know and I'll get you setup.
