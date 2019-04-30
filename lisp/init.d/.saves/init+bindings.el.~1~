;;; init+bindings.el --- Emacs bindings
;;; Commentary:
;; Main file for keybindings.

;;; ChangeLog:
;; 2016 12 16 init SeJ
;; 2016 12 21 add kill-this-buffer
;; 2017 01 06 cleanup by move of packages to init-misc-pkgs.el
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add more pragmatic Emacs tips
;; 2017 01 12 add steve drunken tips
;; 2017 01 30 add sudo-edit function (C-x C-r) to edit file as sudo
;; 2017 03 29 add truncate lines setting
;; 2017 05 09 add copy-line C-c C-k
;;	      add some neat keybindings from emacs-starter-kit
;;	      rename file to init-bindings-settings.el
;; 2017 05 12 adds from purcell/emacs.d
;; 2017 05 21 add delete to trash can
;; 2017 05 25 add imenu binding
;; 2017 05 30 hippie-expand remap M-/ C-M-/
;; 2017 06 05 add sej-map keyboard mapping
;; 2017 08 07 add Hyper & Super keys for osx and win remove sej-map
;; 2017 08 21 add some Lisp stuff from Getting Started with Emacs Lisp
;; 2017 08 29 take another run at sej-map
;; 2017 09 08 add code to unset C- M- digit keys
;; 2017 09 18 add goto-line with temp line numbers
;; 2017 09 19 add transpose keybindings & others from magnar
;; 2017 09 20 make more pure keybindings & move others stuff out
;; 2017 12 21 edits move movement bindings into init-movement
;;                  TODO group buffer manipulatino together (sudo-edit, revert, etc...)

;;; Code:

;; set keys for Apple keyboard, for emacs in OS X
;; make cmd key do Meta
;; make opt key do Super
;; make Control key do Control
;; make Fn key do Hyper

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
;; Left Windows key
;; Right Windows key
;; Menu/App key


(if (eq system-type 'darwin)
    (with-no-warnings
      (progn
	(setq mac-command-modifier 'meta)
	(setq mac-option-modifier 'super)
	(setq mac-control-modifier 'control)
	(setq ns-function-modifier 'hyper)
	;; keybinding to toggle full screen mode
	(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)
	)
      (progn
	(setq w32-pass-lwindow-to-system nil)
	(setq w32-lwindow-modifier 'super)
	(setq w32-pass-rwindow-to-system nil)
	(setq w32-rwindow-modifier 'super)
	(setq w32-pass-apps-to-system nil)
	(setq w32-apps-modifier 'hyper)
	)))

;; Below is taken from stackexchange (Emacs)
;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

;; Instead of below going to use the prefix where appropriate but use Hyper & Super a lot
;; (defvar sej-keymap-prefix (kbd "C-c s")
;;   "'sej-mode' keymap prefix.
;; Overrides the default binding.")

;; (defvar sej-mode-map (make-sparse-keymap)
;;   "Keymap for `sej-mode' whose bindings begin with 'sej-keymap-prefix'.")
;; (fset 'sej-mode-map sej-mode-map)

;; (defvar sej-mode-map (let ((map (make-sparse-keymap)))
;;           (define-key map sej-keymap-prefix 'sej-mode-map)
;;           map)
;;   "Keymap for 'sej-mode'.")

(defvar sej-mode-map (make-sparse-keymap)
  "Keymap for 'sej-mode'.")

;;;###autoload
(define-minor-mode sej-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " sej"
  :keymap sej-mode-map)

;;;###autoload
(define-globalized-minor-mode global-sej-mode sej-mode sej-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((sej-mode . ,sej-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-sej-mode ()
  "Turn off sej-mode."
  (sej-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-sej-mode)

(defmacro bind-to-sej-map (key fn)
  "Bind to KEY (as FN) a function to the `sej-mode-map'.
USAGE: (bind-to-sej-map \"f\" #'full-screen-center)."
  `(define-key sej-mode-map (kbd ,key) ,fn))

;; http://emacs.stackexchange.com/a/12906/115
(defun unbind-from-sej-map (key)
  "Unbind from KEY the function from the 'sej-mode-map'.
USAGE: (unbind-from-modi-map \"key f\")."
  (interactive "kUnset key from sej-mode-map: ")
  (define-key sej-mode-map (kbd (key-description key)) nil)
  (message "%s" (format "Unbound %s key from the %s."
			(propertize (key-description key)
				    'face 'font-lock-function-name-face)
			(propertize "sej-mode-map"
				    'face 'font-lock-function-name-face))))
;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  "Shorthand for interactive lambdas (BODY)."
  `(lambda ()
     (interactive)
     ,@body))

(global-set-key (kbd "H-l") (λ (insert "\u03bb")))
(global-set-key (kbd "C-x 8 l") (λ (insert "\u03bb")))
;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))
(global-set-key (kbd "C-x 8 C") (λ (insert "©")))
(global-set-key (kbd "C-x 8 - >") (λ (insert "→")))
(global-set-key (kbd "C-x 8 8") (λ (insert "∞")))
(global-set-key (kbd "C-x 8 v") (λ (insert "✓")))


;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n)))
  )

;; use hyper (alt on osx) for mode type bindings
(define-key sej-mode-map (kbd "H-a") 'counsel-ag)
(define-key sej-mode-map (kbd "H-o") 'org-mode)
(define-key sej-mode-map (kbd "<f1>") 'org-mode)
(define-key sej-mode-map (kbd "H-s") 'shell)
(define-key sej-mode-map (kbd "<f2>") 'shell)
(define-key sej-mode-map (kbd "H-m") 'menu-bar-mode)
(define-key sej-mode-map (kbd "H-h") 'ns-do-hide-emacs)
(define-key sej-mode-map (kbd "H-H") 'ns-do-hide-others)
(define-key sej-mode-map (kbd "s-r") 'jump-to-register)

;;(global-set-key (kbd "H-e") 'mu4e) ; not used for the moment
;;(global-set-key (kbd "M-`") 'ns-next-frame)

;; use super for action type stuff
;; some lisp stuff from Getting Started with Emacs Lisp
(define-key sej-mode-map (kbd "<M-return>") 'eval-last-sexp)
(define-key sej-mode-map (kbd "<s-return>") 'eval-buffer)
(define-key sej-mode-map (kbd "s-b") 'helm-mini)
(define-key sej-mode-map (kbd "s-i") 'emacs-init-time)
(define-key sej-mode-map (kbd "s-s") 'save-buffer) ;; defined just here for ref
(define-key sej-mode-map (kbd "s-q") 'save-buffers-kill-emacs) ;; defined just here for ref
;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(define-key sej-mode-map (kbd "s-o") 'quick-switch-buffer)

;; general keybindings
(define-key global-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "M-'") 'other-window)
(define-key sej-mode-map (kbd "C-j") 'newline-and-indent)
(define-key sej-mode-map (kbd "C-;") 'comment-dwim)
(define-key sej-mode-map (kbd "M-/") 'hippie-expand)
(define-key sej-mode-map (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key sej-mode-map (kbd "M-'") 'next-multiframe-window)

(define-key sej-mode-map (kbd "C-+") 'text-scale-increase)
(define-key sej-mode-map (kbd "C--") 'text-scale-decrease)
(define-key sej-mode-map (kbd "C-x g") 'magit-status)

(define-key sej-mode-map (kbd "s-0") 'delete-window)
(define-key sej-mode-map (kbd "s-1") 'delete-other-windows)
(define-key sej-mode-map (kbd "s-2") 'split-window-vertically)
(define-key sej-mode-map (kbd "s-3") 'split-window-right)
(define-key sej-mode-map (kbd "s-4") 'dired-other-frame)
(define-key sej-mode-map (kbd "s-5") 'make-frame-command)
(define-key sej-mode-map (kbd "s-6") 'delete-other-frames)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "C-x k") 'kill-this-buffer)
(define-key sej-mode-map (kbd "C-x w") 'delete-frame)

;; Zap to char
(define-key sej-mode-map (kbd "M-z") 'zap-to-char)
(define-key sej-mode-map (kbd "s-z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))
(define-key sej-mode-map (kbd "C-M-d") 'backward-kill-word)

;;scroll window up/down by one line
(define-key sej-mode-map (kbd "s-n") (kbd "C-u 1 C-v"))
(define-key sej-mode-map (kbd "s-p") (kbd "C-u 1 M-v"))
(define-key sej-mode-map (kbd "M-SPC") 'cycle-spacing)

;;added tips from steve drunken blog 10 specific ways to improve productivity
(define-key sej-mode-map (kbd "C-x C-m") 'execute-extended-command)
(define-key sej-mode-map (kbd "C-c C-m") 'execute-extended-command)

;; Align your code in a pretty way.
(define-key sej-mode-map (kbd "C-x \\") 'align-regexp)

;; File & buffer finding
(define-key sej-mode-map (kbd "C-x C-M-f") 'find-file-in-project)
(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)
(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)
(define-key sej-mode-map (kbd "M-`") 'file-cache-minibuffer-complete)
(define-key sej-mode-map (kbd "M-n") 'bs-cycle-next)
(define-key sej-mode-map (kbd "M-p") 'bs-cycle-previous)
(define-key sej-mode-map (kbd "C-c b") 'create-scratch-buffer) ; defined below

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; wind move built in package (default bindins are S-<cursor>)
;;(when (fboundp 'windmove-default-keybindings)
;;  (windmove-default-keybindings)) ;; Shift + direction
(winner-mode t)
(define-key sej-mode-map (kbd "C-c <left>")  'windmove-left)
(define-key sej-mode-map (kbd "C-c <right>") 'windmove-right)
(define-key sej-mode-map (kbd "C-c <up>")    'windmove-up)
(define-key sej-mode-map (kbd "C-c <down>")  'windmove-down)
(define-key sej-mode-map (kbd "s-h")         'windmove-left)
(define-key sej-mode-map (kbd "s-l")         'windmove-right)
(define-key sej-mode-map (kbd "s-k")         'windmove-up)
(define-key sej-mode-map (kbd "s-j")         'windmove-down)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; push and jump to mark functions
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "C-`") 'push-mark-no-activate)
(define-key sej-mode-map (kbd "M-`") 'jump-to-mark)

;; function to edit the curent file as root
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "C-x C-r") 'sudo-edit)

;; number lines with rectangle defined in init-writing.el
(define-key sej-mode-map (kbd "C-x r N") 'number-rectangle)

;; line numbers when using goto-line s-l or M-g M-g or M-g g
;; (defined in init-misc-defuns.el)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; framemove will move frames when at limits of current frame
(use-package framemove
  :ensure t
  :defer 5
  :config
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

;; buffer-move to swap buffers between windows
(use-package buffer-move
  :defer 5
  :ensure t)

(use-package goto-chg
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ;;("C-." . goto-last-change)
	      ;; M-. can conflict with etags tag search. But C-. can get overwritten
	      ;; by flyspell-auto-correct-word. And goto-last-change needs a really fast key.
	      ("M-." . goto-last-change)
	      ;; ensure that even in worst case some goto-last-change is available
	      ("C-M-." . goto-last-change)
	      ;; added reverse below
	      ("C-," . goto-last-change-reverse)))

;; redefine M-< and M-> for some modes
(use-package beginend               ; smart M-< & M->
  :ensure t
  :defer 2
  :config
  (beginend-global-mode)
  )

;; efficient moving through search terms
(use-package avy
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-<return>" . avy-goto-word-1)))

;; crux - smart moving to beginning of line or to beginning of text on line
(use-package crux
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-a" . crux-move-beginning-of-line)
	      ("C-k" . crux-smart-kill-line)
	      ("C-c C-k" . crux-duplicate-current-line-or-region)
	      ("s-d" . crux-duplicate-current-line-or-region)
	      ("C-c n" . crux-cleanup-buffer-or-region)))

;; Moves selected region around
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("M-<down>" . drag-stuff-down)
	      ("M-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

;; save the place in files
(use-package saveplace
  :ensure t
  :defer 5
  :config
  (setq-default save-place t))

(provide 'init+bindings)
;;; init+bindings.el ends here
