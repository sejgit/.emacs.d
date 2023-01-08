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
ln -s ~/.emacs.d/init-mini.el .emacs

