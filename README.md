# .emacs.d

My current Emacs settings.\
A kludge of the thoughts of many smart people then finally mine.

## early-init.el

garbage collection\
gccemacs\
default menu, toolbar, scroll-bar settings\
default frame settings

## init.el

main settings

## init-mini.el

An alternate init.el
For embedded controllers / smaller machines which require less packages\
Use a soft link to it from .emacs in your home directory

````sh
ln -s ~/.emacs.d/init-mini.el ~/.emacs
````

## Emacs install

### macos

- using Emacs 30
- on macos installing emacs-plus@30
- if you already have another Emacs installed brew uninstall it first

````bash
brew install emacs-plus@30 --with-xwidgets --with-no-frame-refocus --with-native-comp --with-poll
````

### system install of some packages

- many packages will install automatically but some will need some help

#### markdown

- [markdown-soma](https://github.com/jasonm23/markdown-soma/tree/e604b9e4a65bbd2057befbfaebfa73d00bd9826a)
- used for live updates for markdown
- requires install of `rustup` which can happen from `brew` in osx or best is direct in others
- curl https://sh.rustup.rs -sSf | sh
- then you will need to cd to `.emacs.d/straight/repos/markdown-soma`
- cargo install --path . # note the . after path
- this should compile `soma` and install it in the `~/.cargo/bin/` directory
- my dotfiles in the `.zshenv` file handles adding this directory to the path
- markdownlint-cli is used for the linting of markdown-mode files
- below is the cli for the above

````bash
brew install rustup
cd ~/.emacs.d/straight/repos/markdown-soma/
cargo install --path .

brew install markdownlint-cli
````
