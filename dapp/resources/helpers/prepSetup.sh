# Prep environment
mkdir $HOME/.temp_mwl

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

### IMPORTANT: Answer NO to installing the haskell-language-server (HLS) and NO to stack; Answer YES to automatically add the required PATH variable
echo "Answer YES to automatically add the required PATH variable; Answer NO to installing the haskell-language-server (HLS) and NO to stack"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
