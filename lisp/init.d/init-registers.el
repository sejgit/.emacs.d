;;; init-registers.el --- Set up registers

;;; Commentary:
;; Registers allow you to jump to a file or other location
;; quickly.  Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

;;; ChangeLog:
;; 2017 05 09 init copied from Part of the Emacs Starter Kit
;; 2017 08 30 add s-r to sej-mode-map
;; 2018 03 21 added comments refering to register save and helm
;; 2018 08 02 removed commas from list
;; 2018 10 16 mod registers

;;; Code:

;; this is defined in init+bindings.el (kbd "s-r") 'jump-to-register
;; use C-x r s (or x)  to save to register
;; use helm register C-c h C-x r i   or C-c h x (shortcut added in my init-ido-ivy-helm.el)
(set-register ?b '(file . "~/.emacs.d/init.d/init+bindings.el"))
(set-register ?s '(file . "~/.emacs.d/init.d/init+settings.el"))
(set-register ?d '(file . "~/.emacs.d/init.d/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?m '(file . "~/.emacs.d/init.d/init-misc-pkgs.el"))
(set-register ?r '(file . "~/.emacs.d/init.d/init-registers.el"))
(set-register ?a '(file . "~/.emacs.d/init.d/init-appearance.el"))

(provide 'init-registers)

;;; init-registers.el ends here
