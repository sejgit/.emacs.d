;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; turn on syntax highlightng for all buffers
(global-font-lock-mode t)

;; raise the maximum number of logs in the *Messages* buffer
(setq message-log-max 16384)

;; wait a bit longer than the default 0.5s before assuming Emacs is idle
(setq idle-update-delay 2)

;; make gnutls a bit safer
(setq gnutls-min-prime-bits 4096)

;; remove irritating 'got redefined' messages
(setq ad-redefinition-action 'accept)

;; figure out current hostname
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; allow exit without asking to kill processes
(setq confirm-kill-processes nil)

(defconst sej-homepage
  "https://github.com/sejgit/.emacs.d"
  "The Github page of SeJ Emacs.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(eval-when-compile
  (require 'init-const))

(defgroup sej nil
  "SeJ Emacs customizations."
  :group 'convenience)

(defcustom sej-full-name "Stephen Jenkins"
  "Set user full name."
  :type 'string)

(defcustom sej-mail-address "stephenearljenkins@gmail.com"
  "Set user email address."
  :type 'string)

(defcustom sej-proxy "localhost:80"
  "Set network proxy."
  :type 'string)

(defcustom sej-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Classic theme" classic)
          (const :tag "Doom theme" doom)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "Daylight theme" daylight)
          symbol))

(defcustom sej-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :type 'boolean)

(defcustom sej-lsp 'lsp-mode
  "Set language server."
  :type '(choice
          (const :tag "LSP Mode" 'lsp-mode)
          (const :tag "eglot" 'eglot)
          nil))

(defcustom sej-benchmark nil
  "Enable the init benchmark or not."
  :type 'boolean)

(defcustom sej-org-directory "~/gdrive/todo"
  "Set org directory"
  :type 'string)

(defcustom sej-project-org-capture-text "Project"
  "Text for the Label for the Org Capture Project journal"
  :type 'string)

(defcustom sej-project-org-capture-file "~/exampleproject/journal.org"
  "Filename for the Org Capture Project Journal"
  :type 'string)

(defcustom sej-latex-directory "~/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64/"
  "Directory for Latex"
  :type 'string)

;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
;; place to hold specific & secret stuff ~/.ssh is best
(add-hook 'after-init-hook
          (progn
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))
            (let ((file
                   (expand-file-name "custom-post.el" "~/.ssh/")))
              (if (file-exists-p file)
                  (load file)))
            ))

;; add my custom hook
(defvar sej/after-init-hook nil
  "Hook called after emacs-init and some time.")

(defvar sej/idle-timer 5
  "Var to set time in seconds for idle timer.")
(when sys/macp
  (setq sej/idle-timer 1))

(defun sej/run-my-after-init-hook ()
  "Function to define when to run my startup hooks"
  (interactive)
  (message "set-up my hooks")
  (run-with-idle-timer sej/idle-timer nil
                       (lambda ()
                         (message "start running my hooks")
                         (run-hooks 'sej/after-init-hook)
                         (message "done running my hooks")
                         )))

(add-hook 'after-init-hook 'sej/run-my-after-init-hook)
;; (remove-hook 'after-init-hook 'sej/run-my-after-init-hook)
(add-hook 'emacs-startup-hook 'sej/frame-resize-full)

(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq load-prefer-newer t)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

(use-package benchmark-init
  :demand t
  :config
  (benchmark-init/activate)
  ;; To disable collection of benchmark data after init is done.
  ;;(add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

;; check OS type
(when
    sys/win32p
  (progn
    (message "Microsoft Windows")
    ;;see if we can get some speed improvements
    (use-package auto-compile
      :demand t
      :config
      (progn
        (auto-compile-on-load-mode)
        (auto-compile-on-save-mode)))

    ;; set exec-path for latex installation
    (setq exec-path (append (list sej-latex-directory "/mingw64/bin/") exec-path))

    ;; load AutoHotkey mode
    (load-library "xahk-mode")))

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

;; The EMACS environment variable being set to the binary path of emacs.
(setenv "EMACS"
        (file-truename (expand-file-name invocation-name invocation-directory)))

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
        )))))

(cond
 (sys/win32p ; Microsoft Windows
  (progn
    (message "Microsoft Windows")
    (setq w32-pass-lwindow-to-system t
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
    )))

(cond
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

;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n)))
  )

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

(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

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

(define-key sej-mode-map (kbd "H-a") 'counsel-ag)
(define-key sej-mode-map (kbd "<f1>") 'org-mode)
(define-key sej-mode-map (kbd "H-s") 'shell)
(define-key sej-mode-map (kbd "<f2>") 'shell)
(define-key sej-mode-map (kbd "H-m") 'menu-bar-mode)

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


(if (boundp 'mac-carbon-version-string) ; mac-ports or ns emacs?
    (progn
      (define-key sej-mode-map (kbd "H-h") (lambda () (interactive) (mac-send-action 'hide)))
      (define-key sej-mode-map (kbd "H-H") (lambda () (interactive) (mac-send-action 'hide-other))))
  (progn
    (define-key sej-mode-map (kbd "H-h") 'ns-do-hide-emacs)
    (define-key sej-mode-map (kbd "H-H") 'ns-do-hide-others))
  )

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

(define-key sej-mode-map (kbd "<s-return>") 'eval-last-sexp)
(define-key sej-mode-map (kbd "<H-return>") 'eval-buffer)
(define-key sej-mode-map (kbd "<A-return>") 'eval-region)
