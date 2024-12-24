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

NOT complete & currently out-of-date...
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
