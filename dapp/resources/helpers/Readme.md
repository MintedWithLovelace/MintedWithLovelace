## How To Setup a Cardano Node

1. Install Debian 10+ or Ubuntu 18+ on a computer, VM, or VPS/Baremetal Server with at minimum 8 GB of RAM (16 GB is ideal).
2. Once logged into your server, perform a little security "hardening" by following this guide: https://www.coincashew.com/coins/overview-ada/guide-how-to-build-a-haskell-stakepool-node/how-to-harden-ubuntu-server
3. After your server is hardened, run the following Node Setup script for the network type of this system (mainnet or testnet) using the appropriate script from this repo. 
      (Alternatively you may also use the script as a step by step guide and issue each command by hand)
      
### For Testnet:
https://github.com/MadeWithLovelace/MintedWithLovelace/blob/sandbox/dapp/helper-tools/setupTestnetCardanoNode.sh

### For Mainnet:
https://github.com/MadeWithLovelace/MintedWithLovelace/blob/sandbox/dapp/helper-tools/setupMainnetCardanoNode.sh

After the script completes it will have attempted to launch cardano-node. Check for any errors (there will be some warnings this is normal) and use ./gLiveView to see the live status of the sync/node.

After syncing has reached 100%, run Minted from any folder in your server, preferably a user, not root, and refer to the pinned message in the #_refinery_ channel before setting up your campaign, for a brief rundown of how to use the dApp. If you are not in the Refinery and would like to test the dApp out, send me a DM and let me know and I'll get you setup.
