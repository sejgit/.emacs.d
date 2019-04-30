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

;;; Code:

;; this is defined in init+bindings.el (kbd "s-r") 'jump-to-register
;; use C-x r s (or x)  to save to register
;; use helm register C-c h C-x r i   or C-c h x (shortcut added in my init-ido-ivy-helm.el)
(dolist (r `(
	     (?b (file . ,"~/.emacs.d/init.d/init-bindings-settings.el"))
	     (?d (file . ,"~/.emacs.d/init.d/"))
	     (?i (file . ,"~/.emacs.d/init.el"))
	     (?m (file . ,"~/.emacs.d/init.d/init-misc-pkgs.el"))
	     (?r (file . ,"~/.emacs.d/init.d/init-registers.el"))
	     ))
  (set-register (car r) (cadr r)))

(provide 'init-registers)
;;; init-registers.el ends here
