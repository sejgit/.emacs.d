;;; init-lisp.el --- elisp setups

;;; Commentary:
;; settings for elisp

;;; ChangeLog
;; 2017 08 25 init SeJ
;; 2017 08 28 added some paredit, eldoc, & ielm settings from EOS
;; 2017 08 30 clean up some comments & defers
;; 2017 09 20 move autocomplete from init-autocomplete.el & delete file
;;            move paredit defun from init-bindings-settings.el
;; 2017 11 18 swap paredit for smartparens
;; 2017 12 01 some mods for use-package & removal of autocomplete
;; 2018 04 04 adding abo-abo's lispy package for specifically better reading of LISP
;; 2018 09 24 some additions from ahai

;;; Code:

(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

;; Smartparens for editing within lisp
(use-package smartparens-config
  :ensure smartparens
  :hook ((prog-mode . smartparens-global-mode)
         (markdown-mode . smartparens-global-mode))
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

              ("C-<down>" . sp-down-sexp)
              ("C-<up>" . sp-up-sexp)

              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)

              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-d" . delete-sexp)

              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)

              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)

              ("C-M-<space>" . sp-mark-sexp))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t))
;;(sp-use-smartparens-bindings)


;; like rainbow-delimiters in elisp modes
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; we don't want this minor mode to be shown in the minibuffer, however
(use-package eldoc
  :ensure t
  :defer t
  :diminish
  eldoc-mode
  :hook  ;; we use eldoc to show the signature of the function at point in the minibuffer
  ((emacs-lisp-mode . eldoc-mode)
   (ielm-mode-hook . eldoc-mode)
   (lisp-interaction-mode-hook . eldoc-mode))
  :config
  (setq eldoc-idle-delay 0.1))

;; add a nice popup for ielm
(defun ielm-other-window ()
  "Run ielm on other window."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key emacs-lisp-mode-map (kbd "H-i") 'ielm-other-window)
(define-key lisp-interaction-mode-map (kbd "H-i") 'ielm-other-window)

;; use flycheck in elisp
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))


;; turn on elisp-slime-nav if available so C-M-. works to jump to function definitions
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package lispy
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	            ("s-8" . lispy-multiline)
	            ("s-*" . lispy-oneline)))

;; When saving an elisp file, remove its compiled version if
;; there is one, as you'll want to recompile it.
(defun sej/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'sej/remove-elc-on-save)

;; Enable eldoc mode, which provides context based documentation
;; in the minibuffer.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; eros-mode will show you the result of evaluating an elisp command
;; as an overlay in your elisp buffer. Try it out with C-x C-e now!
(use-package eros
  :commands eros-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

;; Use C-M-. to jump to the definition of the symbol under the cursor.
(define-key emacs-lisp-mode-map (kbd "C-M-.") 'find-function-at-point)

(provide 'init-lisp)
;;; init-lisp.el ends here
