# Bergen

To use this package you need to install stack (haskell) and git (version controll).

On mac the easy way is to use the package installer homebrew. To install homebrew (https://brew.sh) go to terminal and copy the following line

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

After Homebrew is installed go to terminal and write
brew install haskell-stack git

Now you have everything you need. Go to a the folder where you want our haskell package to be installed. Then type

git clone https://github.com/kfollesdal/bergen

cd bergen

stack setup

stack build

You can now use stack ghci to open the haskell interpreter with our package loaded.

Documentation
* stack haddock --open bergen
