(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(when sys/win32p
  (setenv "PATH"
          (mapconcat
           #'identity exec-path path-separator))
  (add-to-list 'exec-path "c:/msys64/mingw64/bin"))

(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(setq-default locate-command "which")

(use-package server
  :ensure nil
  :hook (sej/after-init . server-mode)
  )

(use-package saveplace
  :ensure nil
  :hook (sej/after-init . save-place-mode)
  )

(use-package recentf
  :ensure nil
  :hook (sej/after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'")))

(use-package savehist
  :ensure nil
  :hook (sej/after-init . savehist-mode)
  :config
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300))

(cond
 (sys/win32p ; Microsoft Windows
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
    ))

 (sys/macp ; OSX
  (progn
    (message "Mac OSX")
    (if (boundp 'mac-carbon-version-string) ;; using mac-port?
        ( progn
          ;; for emacs-mac-port
          (setq mac-right-command-modifier 'none)
          (setq mac-right-option-modifier 'none)
          (setq mac-function-modifier 'hyper)
          (setq mac-control-modifier 'control)
          (setq mac-right-control-modifier 'super)
          (setq mac-option-modifier 'alt)
          (setq mac-command-modifier 'meta))
      ( progn
        ;; for regular Emacs port
        (setq ns-right-command-modifier 'none)
        (setq ns-right-option-modifier 'none)
        (setq ns-function-modifier 'hyper)
        (setq ns-control-modifier 'control)
        (setq ns-right-control-modifier 'super)
        (setq ns-option-modifier 'alt)
        (setq ns-command-modifier 'meta)
        ))))

 (sys/linuxp ; linux
  (progn
    (message "Linux")
    ;; load-dir init.d
    )))

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
(define-key sej-mode-map (kbd "H-a") 'counsel-ag)
(define-key sej-mode-map (kbd "<f1>") 'org-mode)
(define-key sej-mode-map (kbd "H-s") 'shell)
(define-key sej-mode-map (kbd "<f2>") 'shell)
(define-key sej-mode-map (kbd "H-m") 'menu-bar-mode)
(if (boundp 'mac-carbon-version-string) ; mac-ports or ns emacs?
    (progn
      (define-key sej-mode-map (kbd "H-h") (lambda () (interactive) (mac-send-action 'hide)))
      (define-key sej-mode-map (kbd "H-H") (lambda () (interactive) (mac-send-action 'hide-other))))
  (progn
    (define-key sej-mode-map (kbd "H-h") 'ns-do-hide-emacs)
    (define-key sej-mode-map (kbd "H-H") 'ns-do-hide-others))
  )
(define-key sej-mode-map (kbd "H-e") 'eshell)
(define-key sej-mode-map (kbd "H-f") 'flycheck-list-errors) ;;defined here for ref
(define-key sej-mode-map (kbd "C-c g") 'google-this) ;; defined here for ref
(define-key sej-mode-map (kbd "H-g") 'google-this) ;; defined here for ref
(define-key sej-mode-map (kbd "C-x G") 'gist-list) ;; defined here for ref
(define-key sej-mode-map (kbd "H-G") 'gist-list) ;; defined here for ref
(define-key sej-mode-map (kbd "C-x M") 'git-messenger:popup-message) ;; defined here for ref
(define-key sej-mode-map (kbd "H-m") 'git-messenger:popup-message) ;; defined here for ref

(define-key sej-mode-map (kbd "C-h SPC") 'helm-all-mark-rings) ;; defined here for ref
(define-key sej-mode-map (kbd "H-SPC") 'helm-all-mark-rings) ;; defined here for ref

;; some lisp stuff from Getting Started with Emacs Lisp
(define-key sej-mode-map (kbd "<s-return>") 'eval-last-sexp)
(define-key sej-mode-map (kbd "<H-return>") 'eval-buffer)
(define-key sej-mode-map (kbd "<A-return>") 'eval-region)

;; use super for action type stuff
(define-key sej-mode-map (kbd "s-r") 'jump-to-register)
(define-key sej-mode-map (kbd "s-b") 'ivy-switch-buffer) ;; defined here only
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
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)

;;init-frame-cmds bindings here for convenience
(define-key sej-mode-map (kbd "C-c s <up>") 'sej/frame-resize-full)
(define-key sej-mode-map (kbd "C-c s <left>") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "C-c s <S-left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "C-c s <right>") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "C-c s <S-right>") 'sej/frame-resize-r2)

(define-key sej-mode-map (kbd "s-<up>") 'sej/frame-resize-full)
(define-key sej-mode-map (kbd "s-<left>") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "s-S-<left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "s-<right>") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "s-S-<right>") 'sej/frame-resize-r2)


;; File & buffer finding
(define-key sej-mode-map (kbd "C-x M-f") 'counsel-projectile-find-file)
(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)
(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)
(define-key sej-mode-map (kbd "M-`") 'file-cache-minibuffer-complete)
(define-key sej-mode-map (kbd "s-n") 'bs-cycle-next) ; buffer cycle next
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
(global-set-key [remap goto-line] 'goto-line-preview)
