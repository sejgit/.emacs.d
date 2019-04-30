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
;;        add some neat keybindings from emacs-starter-kit
;;        rename file to init-bindings-settings.el
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
;; 2018 03 19 some cleanup & mods
;; 2018 04 30 global mark and cua-copy-to-global-mark and cua-cut-to-global-mark
;; 2018 06 22 remove H-o for org mode to make room for hl-todo-occur
;; 2018 08 06 H-a from counsel-ag to helm-ag
;; 2018 08 07 re-institute winner-mode std keybindings
;;            replace avy with ace-jump-mode
;;            M-o for ace-window
;;            M-u for string-inflection
;;            added simpleclip for better clipboard integration
;; 2018 09 24 changed RET behaviour to add newline-and-indent
;; 2018 09 26 change modifier keys for mac used in conjuction with karabiner & mac settings
;; 2018 09 28 back to avy; add anzu for query replace
;; 2018 10 02 PC keyboard modifer set-up
;; 2018 10 04 comment out cua global mark mode as not often used
;;            use Alt for some special characters

;;; Code:

;; set keys for Apple keyboard, for emacs in OS X
;; caps lock is control (through karabiner)
;; make Fn key do Hyper
;; make LControl key do RControl (karabiner) & Super (emacs)
;; make opt/alt key do alt
;; make cmd key do Meta

;; make PC keyboard's
;; CapsLock::LControl through AutoHotkeys
;; scroll lock do hyper (tab to scroll lock using AutoHotkeys)
;; Left control key do super (LControl::Appskey using AutoHotkeys)
;; Left Windows left alone due to win10 taking many keys
;; LAlt::Meta
;; RAlt::Alt modifier (RAlt::NumLock using Autohotkeys) **only works as tap & release
;; Rwin is Alt (not used in current laptop)
;; NOTE: only negative of this set-up is RAlt as numlock -> Alt is awkward push & release

;; check OS type
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")
    (setq
     w32-pass-lwindow-to-system t
     w32-recognize-altgr nil
     W32-enable-caps-lock nil
     w32-pass-rwindow-to-system nil
     w32-rwindow-modifier 'meta
     w32-apps-modifier 'super
     w32-pass-alt-to-system t
     w32-alt-is-meta t
     w32-scroll-lock-modifier 'hyper
     w32-enable-num-lock nil)
    (w32-register-hot-key [A-])
    (define-key function-key-map (kbd "<kp-numlock>") 'event-apply-alt-modifier)

    (setenv "PATH"
            (mapconcat
             #'identity exec-path path-separator))
    (add-to-list 'exec-path "c:/msys64/mingw64/bin")
    ))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")
    ;; set up the keyboard for right is osx normal
    ;; left side is Hyper Super Alt Meta
    (setq ns-right-command-modifier 'none)
    (setq ns-right-option-modifier 'none)
    (setq ns-function-modifier 'hyper)
    (setq ns-control-modifier 'control)
    (setq ns-right-control-modifier 'super)
    (setq ns-option-modifier 'alt)
    (setq ns-command-modifier 'meta)
    ;; keybinding to toggle full screen mode
    (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)
    ))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux")
    ;; load-dir init.d
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

(global-set-key (kbd "RET") 'newline-and-indent)

;; Neat bindings for C-x 8 ; put some Alt bindins there for fun as well
(global-set-key (kbd "C-x 8 l") (λ (insert "\u03bb")))
(global-set-key (kbd "A-L") (λ (insert "\u03bb")))
(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))
(global-set-key (kbd "A-T") (λ (insert "™")))
(global-set-key (kbd "C-x 8 C") (λ (insert "©")))
(global-set-key (kbd "A-C") (λ (insert "©")))
(global-set-key (kbd "C-x 8 >") (λ (insert "→")))
(global-set-key (kbd "A->") (λ (insert "→")))
(global-set-key (kbd "C-x 8 8") (λ (insert "∞")))
(global-set-key (kbd "A-8") (λ (insert "∞")))
(global-set-key (kbd "C-x 8 v") (λ (insert "✓")))
(global-set-key (kbd "A-V") (λ (insert "✓")))

;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n)))
  )

;; use hyper (fn on osx) for mode type bindings
(define-key sej-mode-map (kbd "H-a") 'helm-ag)
(define-key sej-mode-map (kbd "<f1>") 'org-mode)
(define-key sej-mode-map (kbd "H-s") 'shell)
(define-key sej-mode-map (kbd "<f2>") 'shell)
(define-key sej-mode-map (kbd "H-m") 'menu-bar-mode)
(define-key sej-mode-map (kbd "H-h") 'ns-do-hide-emacs)
(define-key sej-mode-map (kbd "H-H") 'ns-do-hide-others)
(define-key sej-mode-map (kbd "H-e") 'eshell)
(define-key sej-mode-map (kbd "H-f") 'helm-flycheck) ;;defined here for ref
(define-key sej-mode-map (kbd "C-c g") 'google-this) ;; defined here for ref
(define-key sej-mode-map (kbd "H-g") 'google-this) ;; defined here for ref
(define-key sej-mode-map (kbd "C-x G") 'gist-list) ;; defined here for ref
(define-key sej-mode-map (kbd "H-G") 'gist-list) ;; defined here for ref
(define-key sej-mode-map (kbd "C-x M") 'git-messenger:popup-message) ;; defined here for ref
(define-key sej-mode-map (kbd "H-m") 'git-messenger:popup-message) ;; defined here for ref
(define-key sej-mode-map (kbd "C-h SPC") 'helm-all-mark-rings) ;; defined here for ref
(define-key sej-mode-map (kbd "H-SPC") 'helm-all-mark-rings) ;; defined here for ref

;; use super for action type stuff
;; some lisp stuff from Getting Started with Emacs Lisp
(define-key sej-mode-map (kbd "<s-return>") 'eval-last-sexp)
(define-key sej-mode-map (kbd "<H-return>") 'eval-buffer)
(define-key sej-mode-map (kbd "<A-return>") 'eval-buffer) ;; H-ret issues with mac
(define-key sej-mode-map (kbd "s-r") 'jump-to-register)
(define-key sej-mode-map (kbd "s-b") 'helm-mini) ;; defined here for ref
(define-key sej-mode-map (kbd "s-i") 'emacs-init-time)
(define-key sej-mode-map (kbd "s-s") 'save-buffer) ;; defined here for ref
(define-key sej-mode-map (kbd "s-q") 'save-buffers-kill-emacs) ;; defined here for ref
(define-key sej-mode-map (kbd "s-[") 'flycheck-previous-error) ;; defined here for ref
(define-key sej-mode-map (kbd "s-]") 'flycheck-next-error) ;; defined here for ref
(define-key sej-mode-map (kbd "s-f") 'flycheck-list-errors) ;; defined here for ref
(define-key sej-mode-map (kbd "s-/") 'define-word-at-point) ;; defined here for ref
(define-key sej-mode-map (kbd "s-|") 'powerthesaurus-lookup-word-dwim) ;; defined here for ref
(define-key sej-mode-map (kbd "s-w") 'delete-frame)

(define-key sej-mode-map (kbd "s-0") 'delete-window)
(define-key sej-mode-map (kbd "s-1") 'delete-other-windows)
(define-key sej-mode-map (kbd "s-2") 'split-window-vertically)
(define-key sej-mode-map (kbd "s-3") 'split-window-right)
(define-key sej-mode-map (kbd "s-4") 'dired-other-frame)
(define-key sej-mode-map (kbd "s-5") 'make-frame-command)
(define-key sej-mode-map (kbd "s-6") 'delete-other-frames)
(define-key sej-mode-map (kbd "s-7") (lambda () (interactive)
                                       (save-excursion
                                         (other-window 1)
                                         (quit-window))))

;; wind move built in package (default bindins are S-<cursor>)
;;  (windmove-default-keybindings)) ;; Shift + direction
;; winner-mode is to undo & redo windows with C-c left and C-c right
(when (fboundp 'winner-mode)
  (winner-mode t))
(define-key sej-mode-map (kbd "s-h") 'windmove-left)
(define-key sej-mode-map (kbd "s-l") 'windmove-right)
(define-key sej-mode-map (kbd "s-k") 'windmove-up)
(define-key sej-mode-map (kbd "s-j") 'windmove-down)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;init-frame-cmds bindings here for convenience
(define-key sej-mode-map (kbd "C-c s <left>") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "C-c s <S-left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "C-c s <right>") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "C-c s <S-right>") 'sej/frame-resize-r2)

(define-key sej-mode-map (kbd "s-<left>") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "s-S-<left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "s-<right>") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "s-S-<right>") 'sej/frame-resize-r2)


;; File & buffer finding
(define-key sej-mode-map (kbd "C-x C-M-f") 'find-file-in-project)
(define-key sej-mode-map (kbd "C-x C-M-f") 'find-file-in-project)
(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)
(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)
(define-key sej-mode-map (kbd "M-`") 'file-cache-minibuffer-complete)
(define-key sej-mode-map (kbd "s-n") 'bs-cycle-next)
(define-key sej-mode-map (kbd "s-p") 'bs-cycle-previous)
(setq-default bs-default-configuration "all-intern-last")
(define-key sej-mode-map (kbd "C-c b") 'sej/create-scratch-buffer) ; defined below
(define-key sej-mode-map (kbd "C-c s s") 'sej/create-scratch-buffer) ; defined below
(define-key sej-mode-map (kbd "C-c <tab>") 'sej/indent-buffer) ; defined below

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(define-key sej-mode-map (kbd "s-o") 'quick-switch-buffer)

;; general keybindings
(define-key global-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "C-h C-h") nil)

(define-key sej-mode-map (kbd "M-'") 'next-multiframe-window)
(define-key sej-mode-map (kbd "C-j") 'newline-and-indent)
(define-key sej-mode-map (kbd "C-;") 'comment-dwim-2) ; defined in init-misc-packages
(define-key sej-mode-map (kbd "M-/") 'hippie-expand)
(define-key sej-mode-map (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key sej-mode-map (kbd "C-s") 'swiper-isearch)

(define-key sej-mode-map (kbd "C-+") 'text-scale-increase)
(define-key sej-mode-map (kbd "C--") 'text-scale-decrease)
(define-key sej-mode-map (kbd "C-x g") 'magit-status)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "C-x k") 'kill-this-buffer)
(define-key sej-mode-map (kbd "C-x w") 'delete-frame)

;; Zap to char
(define-key sej-mode-map (kbd "M-z") 'zap-to-char)
(define-key sej-mode-map (kbd "s-z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char))) ;
(define-key sej-mode-map (kbd "C-M-d") 'backward-kill-word)

;;scroll window up/down by one line
(define-key sej-mode-map (kbd "A-n") (lambda () (interactive) (scroll-up 1)))
(define-key sej-mode-map (kbd "A-p") (lambda () (interactive) (scroll-down 1)))
(define-key sej-mode-map (kbd "A-SPC") 'cycle-spacing)

;;added tips from steve drunken blog 10 specific ways to improve productivity
(define-key sej-mode-map (kbd "C-x C-m") 'execute-extended-command)
(define-key sej-mode-map (kbd "C-c C-m") 'execute-extended-command)

;; Align your code in a pretty way.
(define-key sej-mode-map (kbd "C-x \\") 'align-regexp)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; push and jump to mark functions
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "C-`") 'sej/push-mark-no-activate)
(define-key sej-mode-map (kbd "M-`") 'sej/jump-to-mark)

;; function to edit the curent file as root
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "C-c C-s") 'sej/sudo-edit)

;; number lines with rectangle defined in init-writing.el
(define-key sej-mode-map (kbd "C-x r N") 'number-rectangle)

;; line numbers when using goto-line M-g M-g or M-g g
;; (defined in init-misc-defuns.el)
(global-set-key [remap goto-line] 'sej/goto-line-with-feedback)

;;
;;; commented out for now as not used much
;;
;; cualess-global-mark and copy or cut to this global mark
;; usefull to set and then copy multiple items to the same place
;; found in emacsen and https://www.reddit.com/r/emacs/comments/8ekz0u/how_to_pastethencopy/
;; (setq cua-enable-cua-keys nil)
;; (global-set-key (kbd "C-S-SPC") (defun cualess-global-mark ()
;;                                   (interactive)
;;                                   (cua-mode 1)
;;                                   (call-interactively 'cua-toggle-global-mark)))
;; (defadvice cua--deactivate-global-mark (after cua--deactivate-global-mark-and-cua-mode activate)
;;   (cua-mode 0))
;; (setq cua-global-mark-keep-visible nil)
;; (global-set-key (kbd "M-W") 'cua-copy-to-global-mark)
;; (global-set-key (kbd "C-S-w") 'cua-cut-to-global-mark)


;; Display incremental search stats in the modeline.
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1)
  ;; Anzu provides a version of `query-replace' and friends which give visual
  ;; feedback when composing regexps. Let's replace the regular versions.
  :bind(("C-%" . anzu-query-replace-at-cursor)
        ("M-%" . anzu-query-replace)
        ("C-M-%" . anzu-query-replace-regexp))
  :diminish anzu-mode)

;; framemove will move frames when at limits of current frame
(use-package framemove
  :load-path "lisp/framemove"
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
              ("C-<return>" . avy-goto-word-1)
              ("s-." . avy-goto-word-0)))

;; efficient moving around screen
;; (use-package ace-jump-mode
;;   :ensure t
;;   :bind (:map sej-mode-map
;;               ("C-c SPC" . ace-jump-word-mode)
;;               ("C-u C-c SPC" . ace-jump-char-mode)
;;               ("C-u C-u C-c SPC" . ace-jump-line-mode)
;;               ("C-<return>" . ace-jump-char-mode)
;;               ("C-S-<return>" . ace-jump-mode-pop-mark))
;;   :config
;;   (ace-jump-mode-enable-mark-sync))

;; for selecting a window to switch to
(use-package ace-window
  :ensure t
  :bind (:map sej-mode-map
              ("M-o" . ace-window)
              ("C-x M-o" . ace-swap-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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

;; underscore -> upcase -> camelcase conversion
(use-package string-inflection
  :ensure t
  :bind (:map sej-mode-map
              ("M-u" . my-string-inflection-all-cycle)))

;; simplified access to the system clipboard in Emacs
(use-package simpleclip
  :ensure t
  :hook (after-init . simpleclip-mode)
  :bind (:map sej-mode-map
              ("s-x" . simpleclip-cut)
              ("s-c" . simpleclip-copy)
              ("s-v" . simpleclip-paste)
              ("C-S-v" . scroll-down-command)
              ("H-v" . scroll-down-command)
              ("M-v" . scroll-down-command)
              )) ;; this last one will help integration with Flycut

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
