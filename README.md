# .emacs.d
My current Emacs settings.  A kludge of the thoughts of many smart people then finally mine.

## early-init.el:
garbage collection
gccemacs
some-early settings

## init.el:
main settings

## init-mini.el
an alternate init.el for embedded controllers or smalller machines which require less packages
I tend to use by a soft link to it from .emacs in your home directory
````sh
ln -s ~/.emacs.d/init-mini.el ~/.emacs
````



## system install of some packages
- many packages will install automatically but some will need some help
- [markdown-soma](https://github.com/jasonm23/markdown-soma/tree/e604b9e4a65bbd2057befbfaebfa73d00bd9826a)
  - used for live updates for markdown
  - requires install of `rustup` which can happen from `brew` in osx or direct in others
  - then you will need to cd to `.emacs.d/straight/repost/` directory which has `markdown-soma`
  ````bash
  cd ~/.emacs.d/straight/repos/markdown-soma/
  cargo install --path .
  ````
  - this should compile `soma` and install it in the `~/.cargo/bin/` directory
  - my dotfiles in the `.zshenv` file handles adding this directory to the path
  
  



