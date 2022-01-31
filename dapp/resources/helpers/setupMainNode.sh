# Prep environment
mkdir $HOME/.temp_mwl

# Following line is for script only:
TMPDIR=$HOME/.temp_mwl

echo export TMPDIR=$HOME/.temp_mwl>> $HOME/.bashrc
source $HOME/.bashrc

# Install Cabal and GHC
### Install dependencies
sudo apt-get update -y
sudo apt-get upgrade -y
sudo apt-get install git jq bc make automake rsync htop curl build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ wget libncursesw5 libtool autoconf pkg-config libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev build-essential curl libgmp-dev libffi-dev libncurses-dev libtinfo5 -y

### Make a git folder for work
mkdir $HOME/git
cd $HOME/git

### Git and install Sodium (healthy kind!)
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout 66f017f1
./autogen.sh
./configure
make
sudo make install

### Run this little program
### IMPORTANT: Answer NO to installing the haskell-language-server (HLS) and NO to stack; Answer YES to automatically add the required PATH variable
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source /home/user/.ghcup/env

### Install Cabal
cd $HOME
ghcup upgrade
ghcup install cabal 3.4.0.0
ghcup set cabal 3.4.0.0

### Install GHC
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7

### Update the PATH
echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc
echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc
echo export NODE_HOME=$HOME/cardano-my-node >> $HOME/.bashrc
echo export NODE_CONFIG=testnet>> $HOME/.bashrc
echo export TESTNET_MAGIC_NUM=1097911063>> $HOME/.bashrc
source $HOME/.bashrc

# Following 4 lines for when running as a script:
LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
NODE_HOME=$HOME/cardano-my-node
NODE_CONFIG=testnet
TESTNET_MAGIC_NUM=1097911063

### Update Cabal and verify versions
### IMPORTANT: Cabal should be at version 3.4.0.0 and GHC should be at version 8.10.7
cabal update
cabal --version
ghc --version

# Build the latest node and cli binaries
cd $HOME/git
git clone https://github.com/input-output-hk/cardano-node.git
cd cardano-node
git fetch --all --recurse-submodules --tags
git checkout $(curl -s https://api.github.com/repos/input-output-hk/cardano-node/releases/latest | jq -r .tag_name)
cabal configure -O0 -w ghc-8.10.7
echo -e "package cardano-crypto-praos\n flags: -external-libsodium-vrf" > cabal.project.local
sed -i $HOME/.cabal/config -e "s/overwrite-policy:/overwrite-policy: always/g"
rm -rf $HOME/git/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.7
cabal build cardano-cli cardano-node

### This build process will take a while

### Copy cardano-cli and cardano-node binaries to bin directory
sudo cp $(find $HOME/git/cardano-node/dist-newstyle/build -type f -name "cardano-cli") /usr/local/bin/cardano-cli
sudo cp $(find $HOME/git/cardano-node/dist-newstyle/build -type f -name "cardano-node") /usr/local/bin/cardano-node

### Verify versions are up to date/expected versions
cardano-node version
cardano-cli version

# Configure the node with latest config files
mkdir $NODE_HOME
cd $NODE_HOME
wget -N https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/${NODE_CONFIG}-config.json
wget -N https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/${NODE_CONFIG}-byron-genesis.json
wget -N https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/${NODE_CONFIG}-shelley-genesis.json
wget -N https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/${NODE_CONFIG}-alonzo-genesis.json
wget -N https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/${NODE_CONFIG}-topology.json
sed -i ${NODE_CONFIG}-config.json \
    -e "s/TraceBlockFetchDecisions\": false/TraceBlockFetchDecisions\": true/g"

### Update bashrc
echo export CARDANO_NODE_SOCKET_PATH="$NODE_HOME/db/socket" >> $HOME/.bashrc
source $HOME/.bashrc

# Following line for when running as a script:
CARDANO_NODE_SOCKET_PATH="$NODE_HOME/db/socket"

### Optional/Recommended
### Create Startup Script
cat > $NODE_HOME/startCardanoNode.sh << EOF 
#!/bin/bash
DIRECTORY=$NODE_HOME
PORT=6000
HOSTADDR=0.0.0.0
TOPOLOGY=\${DIRECTORY}/${NODE_CONFIG}-topology.json
DB_PATH=\${DIRECTORY}/db
SOCKET_PATH=\${DIRECTORY}/db/socket
CONFIG=\${DIRECTORY}/${NODE_CONFIG}-config.json
/usr/local/bin/cardano-node run +RTS -N -A16m -qg -qb -RTS --topology \${TOPOLOGY} --database-path \${DB_PATH} --socket-path \${SOCKET_PATH} --host-addr \${HOSTADDR} --port \${PORT} --config \${CONFIG}
EOF

chmod +x $NODE_HOME/startCardanoNode.sh

### Optionally use systemd / recommended, can use systemctl start/stop/restart on cardano-node 
cat > $NODE_HOME/cardano-node.service << EOF 
# The Cardano node service (part of systemd)
# file: /etc/systemd/system/cardano-node.service 

[Unit]
Description     = Cardano node service
Wants           = network-online.target
After           = network-online.target 

[Service]
User            = ${USER}
Type            = simple
WorkingDirectory= ${NODE_HOME}
ExecStart       = /bin/bash -c '${NODE_HOME}/startCardanoNode.sh'
KillSignal=SIGINT
RestartKillSignal=SIGINT
TimeoutStopSec=300
LimitNOFILE=32768
Restart=always
RestartSec=5
SyslogIdentifier=cardano-node

[Install]
WantedBy	= multi-user.target
EOF

sudo mv $NODE_HOME/cardano-node.service /etc/systemd/system/cardano-node.service
sudo chmod 644 /etc/systemd/system/cardano-node.service
sudo systemctl daemon-reload
sudo systemctl enable cardano-node

### Start the Node
sudo systemctl start cardano-node

# OPTIONAL TOOL
### The gLiveView tool is an easy way to track your sync on the node and many other things

### Install the tool
cd $NODE_HOME
sudo apt install bc tcptraceroute -y
curl -s -o gLiveView.sh https://raw.githubusercontent.com/cardano-community/guild-operators/master/scripts/cnode-helper-scripts/gLiveView.sh
curl -s -o env https://raw.githubusercontent.com/cardano-community/guild-operators/master/scripts/cnode-helper-scripts/env
chmod 755 gLiveView.sh
sed -i env \
    -e "s/\#CONFIG=\"\${CNODE_HOME}\/files\/config.json\"/CONFIG=\"\${NODE_HOME}\/testnet-config.json\"/g" \
    -e "s/\#SOCKET=\"\${CNODE_HOME}\/sockets\/node0.socket\"/SOCKET=\"\${NODE_HOME}\/db\/socket\"/g"

# End!
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "Finished!"
echo ""
echo ""
echo ""
echo ""
echo ""
echo "Before continuing, source the bashrc with the following command: source ~/.bashrc"
echo ""
echo ""
echo "To monitor the progress of the blockchain sync, use either of the following:"
echo ""
echo "Graphical View - issue the following command: ./gLiveView.sh"
echo "Terminal View - issue the following command: sudo journalctl --unit=cardano-node --follow"
