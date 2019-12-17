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

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

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

(when (not emacs/>=26p)
  (error "This requires Emacs 26 and above")
  )

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; No splash screen
(setq inhibit-startup-message t)

;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 1024 100))

;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Set deferred timer to reset them
(run-with-idle-timer 5 nil
                     (lambda ()
                       (setq gc-cons-threshold gc-cons-threshold-original)
                       (setq file-name-handler-alist file-name-handler-alist-original)))

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

(size-indication-mode 1)
(blink-cursor-mode -1)
(setq track-eol t) ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

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

(when sys/win32p
  (setenv "PATH"
          (mapconcat
           #'identity exec-path path-separator))
  ;; set exec-path for latex installation
  (setq exec-path (append (list sej-latex-directory
                                "c:/msys64/mingw64/bin"
                                "/mingw64/bin/") exec-path))
  ;; load AutoHotkey mode
  (load-library "xahk-mode"))

(when (or sys/mac-x-p sys/linux-x-p)
  (setq exec-path (append exec-path '("/usr/local/bin")))
  ;; (use-package exec-path-from-shell
  ;;   ;; :demand t
  ;;   :init
  ;;   (exec-path-from-shell-initialize))
  )

(setq-default locate-command "which")

;; The EMACS environment variable being set to the binary path of emacs.
(setenv "EMACS"
        (file-truename (expand-file-name
                        invocation-name invocation-directory)))

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

(use-package server
  :ensure nil
  :hook (sej/after-init . server-mode)
  )

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

(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

(defmacro λ (&rest body)
  "Shorthand for interactive lambdas (BODY)."
  `(lambda ()
     (interactive)
     ,@body))

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

(define-key global-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "C-h C-h") nil)

(define-key sej-mode-map (kbd "C-j") 'newline-and-indent)
(define-key sej-mode-map (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "RET") 'newline-and-indent)

;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))

(define-key sej-mode-map (kbd "C-M-d") 'backward-kill-word)
(define-key sej-mode-map (kbd "A-SPC") 'cycle-spacing)

;; Align your code in a pretty way.
(define-key sej-mode-map (kbd "C-x \\") 'align-regexp)

(define-key sej-mode-map (kbd "H-m") 'menu-bar-mode)
(define-key sej-mode-map (kbd "H-i") 'emacs-init-time)

(defun sej/save-macro (name)
  "Save a macro.  Take a NAME as argument and save the last defined macro under this name at the end of your init file."
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (switch-to-buffer nil))

(defun sej/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (interactive)
  (s-trim (shell-command-to-string command)))

(defun sej/exec-with-rc (command &rest args)
  "Run a shell command and return a list containing two values: its return
code and its whitespace trimmed output."
  (interactive)
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun sej/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (interactive)
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun sej/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (interactive)
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun sej/exec-if-exec (command args)
  "If `command' satisfies `sej/is-exec', run it with `args' and return its
output as per `sej/exec'. Otherwise, return nil."
  (interactive)
  (when (sej/is-exec command) (sej/exec (s-concat command " " args))))

(use-package list-environment
  :commands list-environment)

(use-package esup
  :init
  (autoload 'esup "esup" "Emacs Start Up Profiler." nil))

(use-package try)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :commands which-key-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-h <ret>" . which-key-show-major-mode)
              ("C-h C-k" . which-key-show-top-level))
  :config
  (which-key-setup-minibuffer))

(use-package helpful
  :after counsel
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c C-d" . helpful-at-point)
              ("C-h c" . helpful-command)
              ("C-h C" . helpful-command)
              ("C-h k" . helpful-key)
              ("C-h M" . helpful-macro))
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  )

(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

(defun sej/update-config ()
  "Update git tracked Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating Emacs configurations...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished. Restart Emacs to complete the process."))
      (message "\"%s\" doesn't exist." dir))))

(defun sej/update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished."))
      (message "\"%s\" doesn't exist." dir))))

;; TODO: need to bring together ~/org/ and deft ~/gdrive/todo/
(defun sej/update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished."))
      (message "\"%s\" doesn't exist." dir))))

(defun sej/update-all()
  "Update dotfiles, org files, Emacs confgiurations and packages, ."
  (interactive)
  (sej/update-config)
  (sej/update-dotfiles)
  (sej/update-org))

(defun sej/recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

;; Recompile site-lisp directory
(defun sej/recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun sej/proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" sej-proxy)
    (message "No proxy")))

(defun sej/proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,sej-proxy)
                             ("https" . ,sej-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (setq url-http-proxy-basic-auth-storage sej-url-http-proxy-basic-auth-storage)
  (sej/proxy-http-show))

(defun sej/proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (setq url-http-proxy-basic-auth-storage nil)
  (sej/proxy-http-show))

(defun sej/proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (sej/proxy-http-disable)
    (sej/proxy-http-enable)))

(defvar socks-noproxy)
(defvar socks-server)
(defun sej/proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 1086 5))
  (message "Enable socks proxy."))

(defun sej/proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (message "Disable socks proxy."))

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun standardize-theme (theme)
  "Standardize THEME."
  (pcase theme
    ('default 'doom-Iosvkem)
    ('classic 'doom-molokai)
    ('doom 'doom-peacock)
    ('dark 'doom-Iosvkem)
    ('light 'doom-one-light)
    ('daylight 'doom-tomorrow-day)
    (_ theme)))

(defun sej/load-theme (theme)
  "Set color THEME."
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             '(default classic peacock dark light daylight)))))
  (let ((theme (standardize-theme theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun is-doom-theme-p (theme)
  "Check whether the THEME is a doom theme. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name (standardize-theme theme))))

(if (is-doom-theme-p sej-theme)
    (progn
      (use-package doom-themes
        :init (sej/load-theme sej-theme)
        :config
        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)
        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config))

      ;; Make certain buffers grossly incandescent
      (use-package solaire-mode
        :functions persp-load-state-from-file
        :hook (((after-change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
               (minibuffer-setup . solaire-mode-in-minibuffer)
               (after-load-theme . solaire-mode-swap-bg))
        :config
        (solaire-mode-swap-bg)
        (advice-add #'persp-load-state-from-file
                    :after #'solaire-mode-restore-persp-mode-buffers)))
  (progn
    (ignore-errors
      (sej/load-theme sej-theme))))

(when sys/macp
  (set-default-font "SF Mono-14"))

(define-key sej-mode-map (kbd "s-4") 'dired-other-frame)
(define-key sej-mode-map (kbd "s-5") 'make-frame-command)
(define-key sej-mode-map (kbd "s-6") 'delete-other-frames)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "s-w") 'delete-frame)
(define-key sej-mode-map (kbd "C-x w") 'delete-frame)

(setq frame-title-format '("SeJ Emacs - %b"))
(setq icon-title-format frame-title-format)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

(when sys/mac-x-p
  (use-package ns-auto-titlebar
    :config

    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-hook 'after-load-theme-hook
              (lambda ()
                (let ((bg (frame-parameter nil 'background-mode)))
                  (set-frame-parameter nil 'ns-appearance bg)
                  (setcdr (assq 'ns-appearance default-frame-alist) bg))))
    (ns-auto-titlebar-mode))

  (if (boundp 'mac-carbon-version-string) ; mac-ports or ns emacs?
      (progn
        (define-key sej-mode-map (kbd "s-h") (lambda () (interactive) (mac-send-action 'hide)))
        )
    (progn
      (define-key sej-mode-map (kbd "s-h") 'ns-do-hide-emacs)
      )
    )
  )

(define-key sej-mode-map (kbd "C-c s <up>") 'sej/frame-resize-full)
(define-key sej-mode-map (kbd "H-C-j") 'sej/frame-resize-full)

(defun sej/frame-resize-full ()
  "Set frame full height and 1/2 wide, position at screen left."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (- (display-pixel-width) (if sys/macp (eval 13) (eval 25)))
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

(define-key sej-mode-map (kbd "C-c s <left>") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "H-C-h") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "<A-M-left>") 'sej/frame-resize-l)

(defun sej/frame-resize-l ()
  "Set frame full height and 1/2 wide, position at screen left."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

(define-key sej-mode-map (kbd "C-c s <S-left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "H-C-S-h") 'sej/frame-resize-l2)

(defun sej/frame-resize-l2 ()
  "Set frame full height and 1/2 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

(define-key sej-mode-map (kbd "C-c s <right>") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "H-C-l") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "<A-M-right>") 'sej/frame-resize-r)
 ;; last one is for non MACOS or just in case BTT is not installed on MACOS
(defun sej/frame-resize-r ()
  "Set frame full height and 1/2 wide, position at screen right."
  (interactive)
  (set-frame-position (selected-frame) (- (truncate (/ (display-pixel-width) 2)) 0) 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

(define-key sej-mode-map (kbd "C-c s <S-right>") 'sej/frame-resize-r2)
(define-key sej-mode-map (kbd "H-C-S-l") 'sej/frame-resize-r2)

(defun sej/frame-resize-r2 ()
  "Set frame full height and 1/2 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

(when sys/mac-x-p
  (setq ns-use-native-fullscreen nil))
(bind-keys ("H-C-k" . toggle-frame-fullscreen)
           ("C-c s F" . toggle-frame-fullscreen))

(define-key sej-mode-map (kbd "s-s") 'save-buffer)
(define-key sej-mode-map (kbd "s-q") 'save-buffers-kill-emacs)

(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)

(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)
(define-key sej-mode-map (kbd "s-r") 'revert-buffer)


;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "C-x k") 'kill-this-buffer)

;; toggle two most recent buffers
(define-key sej-mode-map (kbd "s-o") 'quick-switch-buffer)

(define-key sej-mode-map (kbd "s-n") 'bs-cycle-next) ; buffer cycle next
(define-key sej-mode-map (kbd "s-p") 'bs-cycle-previous)

(setq-default bs-default-configuration "all-intern-last")

(defun sej/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun sej/minibuffer-exit-hook ()
  (setq gc-cons-threshold gc-cons-threshold-original))

(add-hook 'minibuffer-setup-hook #'sej/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'sej/minibuffer-exit-hook)

(defun sej/sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; function to edit the curent file as root
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "C-c C-s") 'sej/sudo-edit)

(defun sej/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun sej/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun sej/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun sej/browse-homepage ()
  "Browse the Github page of SeJ Emacs."
  (interactive)
  (browse-url sejgit-homepage))

(define-key sej-mode-map (kbd "C-c s h") 'sej/browse-homepage)

(defun sej/quit-and-kill-auxiliary-windows ()
  "Kill buffer and its window on quitting"
  (local-set-key (kbd "q") 'kill-buffer-and-window))
(add-hook 'special-mode 'sej/quit-and-kill-auxiliary-windows)
(add-hook 'compilation-mode-hook 'sej/quit-and-kill-auxiliary-windows)

(use-package autorevert
  :ensure nil
  :diminish
  :hook (sej/after-init . global-auto-revert-mode))

(setq initial-scratch-message "")
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the *scratch* buffer, but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(defun sej/create-scratch-buffer nil
  "Create a new scratch buffer to work in (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))
(defalias 'create-scratch-buffer 'sej/create-scratch-buffer)
(define-key sej-mode-map (kbd "C-c b") 'sej/create-scratch-buffer)
(define-key sej-mode-map (kbd "C-c s s") 'sej/create-scratch-buffer)

(use-package persistent-scratch
  :preface
  (defun my-save-buffer ()
    "Save scratch and other buffer."
    (interactive)
    (let ((scratch-name "*scratch*"))
      (if (string-equal (buffer-name) scratch-name)
          (progn
            (message "Saving %s..." scratch-name)
            (persistent-scratch-save)
            (message "Wrote %s" scratch-name))
        (save-buffer))))
  :hook (after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
              ("C-x C-s" . my-save-buffer)))

(define-key sej-mode-map (kbd "s-0") 'delete-window)
(define-key sej-mode-map (kbd "s-1") 'delete-other-windows)
(define-key sej-mode-map (kbd "s-2") 'split-window-vertically)
(define-key sej-mode-map (kbd "s-3") 'split-window-right)

(define-key sej-mode-map (kbd "s-7") (lambda () (interactive)
                                       (save-excursion
                                         (other-window 1)
                                         (quit-window))))
(define-key sej-mode-map (kbd "M-'") 'next-multiframe-window)

;;scroll window up/down by one line
(define-key sej-mode-map (kbd "A-n") (lambda () (interactive) (scroll-up 1)))
(define-key sej-mode-map (kbd "A-p") (lambda () (interactive) (scroll-down 1)))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

(use-package ace-window
  :bind (([remap other-window] . ace-window)
         ("M-o" . ace-window))
  :custom-face
  (aw-leading-char-face ((t (:inherit error :bold t :height 1.1))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :hook (after-init . ace-window-display-mode)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :bind (:map sej-mode-map
              ("C-c <left>" . winner-undo)
              ("C-c <right>" . winner-redo))
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package golden-ratio
  :hook (sej/after-init . golden-ratio-mode)
  :defines sej-mode-map
  :diminish golden-ratio-mode
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window)
  (setq golden-ratio-auto-scale t))

(use-package shackle
  :commands shackle-display-buffer
  :hook (sej/after-init . shackle-mode)
  :config
  ;; Enforce rules for popups
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)

  (eval-and-compile
    (defun shackle-last-popup-buffer ()
      "View last popup buffer."
      (interactive)
      (ignore-errors
        (display-buffer shackle-last-buffer)))
    (bind-key "C-h z" #'shackle-last-popup-buffer)

    ;; Add keyword: `autoclose'
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (delete-window window)

              (pop shackle--popup-window-list))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; rules
  (setq shackle-default-size 0.4)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-rule nil)
  (setq shackle-rules
        '(("*Help*" :select t :size 0.3 :align 'below :autoclose t)
          ("*compilation*" :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*ert*" :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          ("*Warnings*" :size 0.3 :align 'below :autoclose t)
          ("*Messages*" :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t)
          ("*Paradox Report*" :size 0.3 :align 'below :autoclose t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)

          (ag-mode :select t :align 'below)
          (grep-mode :select t :align 'below)
          (ivy-occur-grep-mode :select t :align 'below)
          (pt-mode :select t :align 'below)
          (rg-mode :select t :align 'below)

          (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
          (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.4 :align 'below :autoclose t)
          (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'below)
          (tabulated-list-mode :align 'below))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  (after-save . doom-modeline-update-buffer-file-name)
  (after-save . doom-modeline-update-buffer-file-state-icon)
  :init
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-github t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-persp-name nil))

(defun mode-line-height ()
  "Get current height of mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))

(use-package all-the-icons
  :if (display-graphic-p)
  :custom-face
  ;; Reset colors since they are too dark in `doom-themes'
  (all-the-icons-silver ((((background dark)) :foreground "#716E68")
                         (((background light)) :foreground "#716E68")))
  (all-the-icons-lsilver ((((background dark)) :foreground "#B9B6AA")
                          (((background light)) :foreground "#7F7869")))
  (all-the-icons-dsilver ((((background dark)) :foreground "#838484")
                          (((background light)) :foreground "#838484")))
  :init
  (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t))
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode  all-the-icons-octicon "markdown" :face all-the-icons-blue)))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package goto-line-preview
  :hook ((goto-line-preview-before-hook . (lambda() (display-line-numbers-mode 1)))
         (goto-line-preview-after-hook . (lambda() (display-line-numbers-mode -1))))
  :bind ([remap goto-line] . goto-line-preview))

;; yes and no settings
(defalias 'yes-or-no-p 'y-or-n-p)

;; do/don't indicate empty or end of a buffer
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries t)
(setq-default show-trailing-whitespace nil)
(setq-default mode-require-final-newline nil)
(setq-default require-final-newline nil)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
(setq scroll-margin 3)

;; each line of text gets one line on the screen
(setq-default truncate-lines 1)
(setq font-lock-maximum-decoration t
      truncate-partial-width-windows 1)

;; ignore case when searching
(setq-default case-fold-search 1)

;; add a new line when going to the next line
(setq next-line-add-newlines t)

;;(transient-mark-mode t)
(setq select-enable-clipboard t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; Make compilation buffers scroll to follow the output, but stop scrolling
;; at the first error.
(setq compilation-scroll-output 'first-error)

;; echo keystrokes ; no dialog boxes ; visable bell ; highlight parens
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Add proper word wrapping
(global-visual-line-mode t)

(setq-default backup-directory-alist
              '(("." . ".saves")))    ; don't litter my fs tree

(setq vc-make-backup-files t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . ".saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; remove kill buffer with live process prompt
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq-default kill-read-only-ok t)

;; hide mouse while typing
(setq make-pointer-invisible t)

;; color codes
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; uniquify settings
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(if sys/macp (setq trash-directory "~/.Trash"))
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(add-hook 'before-save-hook 'time-stamp)   ; update time-stamps in files

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

(setq-default major-mode 'text-mode)

;; Sentences do not need double spaces to end. Period.
(setq sentence-end-double-space nil)

(use-package iedit
  :defines desktop-minor-mode-table
  :bind ((:map sej-mode-map
               ("A-;" . iedit-mode)
               ("C-x r RET" . iedit-rectangle-mode))
         (:map isearch-mode-map ("A-;" . iedit-mode-from-isearch))
         (:map esc-map ("A-;" . iedit-execute-last-modification))
         (:map help-map ("A-;" . iedit-mode-toggle-on-function)))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

(use-package multiple-cursors
  :bind ((:map sej-mode-map
               ("C-S-c C-S-c"   . mc/edit-lines)
               ("C->"           . mc/mark-next-like-this)
               ("C-<"           . mc/mark-previous-like-this)
               ("C-c C-<"       . mc/mark-all-like-this)
               ("C-M->"         . mc/skip-to-next-like-this)
               ("C-M-<"         . mc/skip-to-previous-like-this)
               ("s-<mouse-1>"   . mc/add-cursor-on-click)
               ("C-S-<mouse-1>" . mc/add-cursor-on-click))
         (:map mc/keymap
               ("C-|" . mc/vertical-align-with-space))))

(use-package imenu
  :ensure nil
  :bind (:map sej-mode-map
              ("C-." . imenu))
  :hook
  (org-mode . imenu-add-menubar-index)
  (prog-mode . imenu-add-menubar-index) )

(use-package ivy
  :diminish
  :hook (sej/after-init . ivy-mode)
  :bind ( ("s-b" . ivy-switch-buffer)
          ("C-c C-r" . ivy-resume)
          ("C-c v p" . ivy-push-view)
          ("C-c v o" . ivy-pop-view)
          ("C-c v ." . ivy-switch-view)
          :map ivy-minibuffer-map
          ("M-j" . ivy-yank-word))
  :config (ivy-mode)
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)      ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  ;; (setq ivy-format-function 'ivy-format-function-arrow)
  (setq ivy-initial-inputs-alist nil))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)
         :map swiper-map
         ("M-q" . swiper-query-replace)) )

(use-package counsel
  :after ivy
  :diminish
  :defines (projectile-completion-system magit-completing-read-function)
  :bind-keymap ("H-c" . counsel-mode-map)
  :bind (
         ([remap execute-extended-command] . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         :map counsel-mode-map
         ([remap dired] . counsel-dired)
         ("C-x C-r" . counsel-recentf)
         ("C-c s j" . counsel-mark-ring)
         ("H-SPC" . counsel-mark-ring)
         ("C-c L" . counsel-load-library)
         ("C-c P" . counsel-package)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("H-a"   . counsel-ag)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)
         ("C-c c L" . counsel-load-library)
         ("C-c c P" . counsel-package)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c g" . counsel-grep)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c z" . counsel-fzf)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         )
  :hook ((ivy-mode . counsel-mode))
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (swiper-all . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-pt . ivy--regex-plus)
          (counsel-ack . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((cmd (cond ((executable-find "rg")
                    "rg -S --no-heading --line-number --color never '%s' %s")
                   ((executable-find "ag")
                    "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
                   (t counsel-grep-base-command))))
    (setq counsel-grep-base-command cmd))

  ;; Pre-fill search keywords
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  (defvar my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines
      keep-lines
      ivy-read
      swiper
      swiper-all
      swiper-isearch
      counsel-grep-or-swiper
      counsel-grep
      counsel-ack
      counsel-ag
      counsel-rg
      counsel-pt))

  (defun my-ivy-fly-back-to-present ()
    (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((memq this-command '(self-insert-command
                                ivy-yank-word))
           (delete-region (point)
                          (point-max)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion
            (insert (propertize future 'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

(use-package counsel-projectile
  :after counsel
  :hook (sej/after-init . counsel-projectile-mode)
  :config
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (counsel-projectile-mode 1))

(use-package google-this
  :diminish google-this-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("s-g" . google-this)
              ("C-c g" . google-this))
  :config
  (google-this-mode 1))

(when (executable-find "ag")
  (use-package ag
    :after counsel
    :commands ag
    :bind (:map sej-mode-map
                ("s-a" . counsel-projectile-ag)
                ("H-a" . counsel-ag))
    :config
    (setq ag-executable (executable-find "ag")))
  (setq-default ag-highlight-search t))

(use-package ivy-yasnippet
  :after ivy yasnippet
  :commands ivy-yasnippet--preview
  :bind ("C-c C-y" . ivy-yasnippet)
  :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

(use-package ivy-xref
  :after ivy
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package flyspell-correct-ivy
  :after flyspell ivy
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)
              ("C-M-;" . flyspell-correct-wrapper)
              ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(cond
 (sys/linux-x-p
  (bind-key "C-c s a" #'counsel-linux-app counsel-mode-map))
 (sys/macp
  (use-package counsel-osx-app
    :after counsel
    :bind ("C-c s a" . counsel-osx-app))))

(use-package counsel-tramp
  :after counsel
  :bind ("C-c s v" . counsel-tramp))

(use-package ivy-rich
  :after ivy all-the-icons
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-family
              all-the-icons-match-to-alist
              all-the-icons-faicon
              all-the-icons-octicon
              all-the-icons-dir-is-submodule)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if (and buffer-file-name
                            (all-the-icons-auto-mode-match?))
                       (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                     (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (concat ivy--directory candidate))
             (file (file-name-nondirectory path))
             (icon (cond
                    ((file-directory-p path)
                     (cond
                      ((and (fboundp 'tramp-tramp-file-p)
                            (tramp-tramp-file-p default-directory))
                       (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                      ((file-symlink-p path)
                       (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                      ((all-the-icons-dir-is-submodule path)
                       (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                      ((file-exists-p (format "%s/.git" path))
                       (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                      (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                           (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                    ((string-match "^/.*:$" path)
                     (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                    ((not (string-empty-p file))
                     (all-the-icons-icon-for-file file :v-adjust -0.05)))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-function-icon (_candidate)
    "Display function icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

  (defun ivy-rich-variable-icon (_candidate)
    "Display variable icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (defun ivy-rich-face-icon (_candidate)
    "Display face icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2)))

  (defun ivy-rich-keybinding-icon (_candidate)
    "Display keybindings icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

  (when (display-graphic-p)
    (defun ivy-rich-bookmark-type-plus (candidate)
      (let ((filename (ivy-rich-bookmark-filename candidate)))
        (cond ((null filename)
               (all-the-icons-material "block" :v-adjust -0.2 :face 'warning)) ; fixed #38
              ((file-remote-p filename)
               (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
              ((not (file-exists-p filename))
               (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
              ((file-directory-p filename)
               (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
              (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
    (advice-add #'ivy-rich-bookmark-type :override #'ivy-rich-bookmark-type-plus))
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          persp-switch-to-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-M-x
          (:columns
           ((ivy-rich-function-icon)
            (counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((ivy-rich-function-icon)
            (counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))
           :delimiter "\t")
          counsel-describe-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))
           :delimiter "\t")
          counsel-describe-face
          (:columns
           ((ivy-rich-face-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-descbinds
          (:columns
           ((ivy-rich-keybinding-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info))
           :delimiter "\t")
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer))
           :delimiter "\t")
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-dir-transformer))
           :delimiter "\t")
          )))

(use-package re-builder
  :ensure nil
  :config (setq reb-re-syntax 'string))

(setq-default tab-width 2
              indent-tabs-mode nil
              fill-column 80)
;; Line and Column
(setq column-number-mode t)
(setq line-number-mode t)

;; Javascript
(setq-default js2-basic-offset 2)

;; JSON
(setq-default js-indent-level 2)

;; Coffeescript
(setq coffee-tab-width 2)

;; Typescript
(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)

;; Python
(setq-default py-indent-offset 2)

;; XML
(setq-default nxml-child-indent 2)

;; C
(setq-default c-basic-offset 2)

;; HTML etc with web-mode
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

(use-package dtrt-indent
  :hook (sej/after-init . dtrt-indent-mode)
  :diminish)

(use-package aggressive-indent
  :diminish
  :hook (after-init . global-aggressive-indent-mode)
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode))
    (push mode aggressive-indent-excluded-modes))
  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

(defun sej/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(define-key sej-mode-map (kbd "C-c <tab>") 'sej/indent-buffer)

(use-package smart-tab
  :diminish ""
  :defines
  smart-tab-using-hippie-expand
  :init
  (setq smart-tab-using-hippie-expand t)
  :config
  (global-smart-tab-mode 1)
  (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'shell-mode))

(use-package undo-tree
  :diminish
  :hook (sej/after-init . global-undo-tree-mode)
  :bind (:map sej-mode-map
              ("C-z" . undo-tree-undo)
              ("C-r" . undo-tree-redo)
              ("C-x u" . undo-tree-visualize)
              ("C-x r u" . undo-tree-save-state-to-register)
              ("C-x r U" . undo-tree-save-state-from-register))
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil
              undo-tree-history-directory-alist
              `(("." . ,(locate-user-emacs-file "undo-tree-hist/"))))  )

(use-package saveplace
  :ensure nil
  :hook (sej/after-init . save-place-mode))

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

(use-package crux
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c o" . crux-open-with)
              ("C-k" . crux-smart-kill-line)
              ("C-S-RET" . crux-smart-open-line-above)
              ([(shift return)] . crux-smart-open-line)
              ("C-c n" . crux-cleanup-buffer-or-region)
              ("C-c u" . crux-view-url)
              ("s-d" . crux-duplicate-current-line-or-region)
              ("C-c C-k" . crux-duplicate-current-line-or-region)
              ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
              ([remap kill-whole-line] . crux-kill-whole-line)
              ("C-<backspace>" . crux-kill-line-backwards))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (crux-reopen-as-root-mode))

(use-package mwim
  :bind (:map sej-mode-map
              ("C-a" . mwim-beginning)
              ("C-e" . mwim-end))) ; better than crux

(use-package avy
  :bind (:map sej-mode-map
              ("C-'" . avy-goto-char)
              ("H-'" . avy-goto-char-2)
              ("M-g l" . avy-goto-line)
              ("H-l" . avy-goto-line)
              ("M-g w" . avy-goto-word-1)
              ("H-w" . avy-goto-word-1))
  :config (setq avy-background t))

(use-package goto-chg
  :defines sej-mode-map
  :bind ("C-," . goto-last-change))

(use-package beginend               ; smart M-< & M->
  :hook (sej/after-init . beginend-global-mode))

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode))
  :config
  ;; this makes forward-word & backward-word understand snake & camel case
  (setq c-subword-mode t)
  (global-subword-mode t))

(use-package string-inflection
  :bind (:map sej-mode-map
              ("M-u" . string-inflection-all-cycle)))

(defun sej/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; push and jump to mark functions
(define-key sej-mode-map (kbd "C-S-<SPC>") 'sej/push-mark-no-activate)

(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

(use-package easy-kill-extras
  :bind (([remap kill-ring-save] . easy-kill) ; M-w
         ([remap mark-sexp] . easy-mark-sexp) ; C-M-@
         ([remap mark-word] . easy-mark-word) ; M-@
         ([remap zap-to-char] . easy-mark-to-char)

         ;; Integrate `expand-region'
         :map easy-kill-base-map
         ("o" . easy-kill-er-expand)
         ("i" . easy-kill-er-unexpand))
  :config
  (setq easy-kill-alist '((?w word           " ")
                          (?s sexp           "\n")
                          (?l list           "\n")
                          (?f filename       "\n")
                          (?d defun          "\n\n")
                          (?D defun-name     " ")
                          (?e line           "\n")
                          (?b buffer-file-name)

                          (?^ backward-line-edge "")
                          (?$ forward-line-edge "")
                          (?h buffer "")
                          (?< buffer-before-point "")
                          (?> buffer-after-point "")
                          (?f string-to-char-forward "")
                          (?F string-up-to-char-forward "")
                          (?t string-to-char-backward "")
                          (?T string-up-to-char-backward "")))    )

(when sys/macp
  (defun sej/copy-from-osx ()
    "For copying from osx."
    (shell-command-to-string "pbpaste"))

  (defun sej/paste-to-osx (text &optional push)
    "For copying to osx TEXT with optional PUSH."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'sej/paste-to-osx)
  (setq interprogram-paste-function 'sej/copy-from-osx))

(use-package avy-zap
  :bind (:map sej-mode-map
              ("M-z" . avy-zap-to-char-dwim)
              ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package delsel
  :ensure nil
  :config (setq-default delete-selection-mode nil))

(use-package rect
  :ensure nil)

(use-package drag-stuff
  :diminish
  :bind (:map sej-mode-map
              ("M-<down>" . drag-stuff-down)
              ("H-n" . drag-stuff-down)
              ("M-<up>" . drag-stuff-up)
              ("H-p" . drag-stuff-up))
  ;; :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode))

(use-package smart-region
  :bind ([remap set-mark-command] . smart-region) ; C-SPC
  :config (smart-region-on))

(use-package expand-region
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("s-=" . er/expand-region)
              ("s--" . er/contract-region)))

(use-package smart-hungry-delete
  :diminish
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :hook (sej/after-init . smart-hungry-delete-add-default-hooks))

(when sys/macp
  (defun sej/retrieve-url ()
    "Retrieve the URL of the current Safari page as a string."
    (org-trim (shell-command-to-string
               "osascript -e 'tell application \"Safari\" to return URL of document 1'")))
  (defun sej/insert-url ()
    "Insert URL of current browser page into Emacs buffer."
    (interactive)
    (insert (sej/retrieve-url)))

  (define-key sej-mode-map (kbd "C-H-u") 'sej/insert-url))

(use-package ace-link
  :bind (:map sej-mode-map
              ("H-u" . ace-link-addr)
              ("C-c s u" . ace-link-addr)) )

(use-package restclient)

(use-package hl-line
  :ensure nil
  :hook (sej/after-init . global-hl-line-mode))

(use-package symbol-overlay
  :diminish
  :defines iedit-mode
  :commands (symbol-overlay-get-symbol
             symbol-overlay-assoc
             symbol-overlay-get-list
             symbol-overlay-jump-call)
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode-end . symbol-overlay-mode)))

(use-package dimmer
  :defer 5
  :config
  (setq dimmer-fraction 0.20)
  (dimmer-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(when (display-graphic-p)
  (use-package highlight-indent-guides
    :diminish
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive 'top)))

(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

(use-package hl-todo
  :custom-face (hl-todo ((t (:box t :inherit))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t o" . hl-todo-occur)
              ("H-o" . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("H-p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("H-n" . hl-todo-next))
  :hook ((sej/after-init . global-hl-todo-mode)
         (prog-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))
  :config
  ;; defcustom hl-todo-keyword-faces
  ;;   '(("HOLD" . "#d0bf8f")
  ;;     ("TODO" . "#cc9393")
  ;;     ("NEXT" . "#dca3a3")
  ;;     ("THEM" . "#dc8cc3")
  ;;     ("PROG" . "#7cb8bb")
  ;;     ("OKAY" . "#7cb8bb")
  ;;     ("DONT" . "#5f7f5f")
  ;;     ("FAIL" . "#8c5353")
  ;;     ("DONE" . "#afd8af")
  ;;     ("NOTE"   . "#d0bf8f")
  ;;     ("KLUDGE" . "#d0bf8f")
  ;;     ("HACK"   . "#d0bf8f")
  ;;     ("TEMP"   . "#d0bf8f")
  ;;     ("FIXME"  . "#cc9393")
  ;;     ("XXX+"   . "#cc9393"))
  (push 'org-mode hl-todo-include-modes)
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(use-package diff-hl
  :defines (diff-hl-margin-symbols-alist desktop-minor-mode-table)
  :commands diff-hl-magit-post-refresh
  :custom-face
  (diff-hl-change ((t (:background "#46D9FF"))))
  (diff-hl-delete ((t (:background "#ff6c6b"))))
  (diff-hl-insert ((t (:background "#98be65"))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)
  (setq diff-hl-draw-borders nil)
  (if sys/mac-x-p (set-fringe-mode '(4 . 8)))

  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist
          '((insert . " ") (delete . " ") (change . " ")
            (unknown . " ") (ignored . " ")))
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package volatile-highlights
  :diminish
  :hook (sej/after-init . volatile-highlights-mode))

(use-package whitespace
  :ensure nil
  :diminish
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

(use-package pulse
  :ensure nil
  :preface
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point) 'next-error))

  (defun my-pulse-momentary (&rest _)
    "Pulse the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my-pulse-momentary-line)))

  (defun my-recenter-and-pulse(&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary))

  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my-recenter-and-pulse-line))
  :init
  (dolist (cmd '(recenter-top-bottom
                 other-window ace-window windmove-do-window-select
                 pager-page-down pager-page-up
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my-pulse-momentary-line))
  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'my-recenter-and-pulse)))

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30)))
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t))

(use-package mic-paren
  :hook (prog-mode . paren-activate)
  :config
  (setq paren-highlight-offscreen t))

(use-package eglot
  :hook ((python-mode c-mode go-mode bash-mode sh-mode javascript-mode java-mode)  . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c h" . eglot-help-at-point)
              ("C-c x" . xref-find-definitions))
  :config
  (setq help-at-pt-display-when-idle t))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . global-prettify-symbols-mode)
         (prog-mode . (lambda ()
                        (setq prettify-symbols-alist
                              '(("lambda" . ?λ)
                                ("->" . ?→)
                                ("->>" . ?↠)
                                ("=>" . ?⇒)
                                ("map" . ?↦)
                                ("/=" . ?≠)
                                ("!=" . ?≠)
                                ("==" . ?≡)
                                ("<=" . ?≤)
                                (">=" . ?≥)
                                ("=<<" . (?= (Br . Bl) ?≪))
                                (">>=" . (?≫ (Br . Bl) ?=))
                                ("<=<" . ?↢)
                                (">=>" . ?↣)
                                ("&&" . ?∧)
                                ("||" . ?∨)
                                ("not" . ?¬))))))
  :init (setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package format-all
  :bind (:map sej-mode-map
              ("C-c s f" . format-all-buffer)
              ("A-f" . format-all-buffer)))

(use-package tramp
  :commands
  tramp-default-method
  tramp-default-user
  tramp-default-host
  :config
  (if sys/macp
      (setq
       tramp-default-method "ssh"
       password-cache-expiry nil)
    (setq
     tramp-default-method "ssh"
     tramp-default-user "pi"
     tramp-default-host "home"
     password-cache-expiry nil)
    )
  (setq tramp-use-ssh-controlmaster-options nil)

  (defadvice tramp-handle-write-region
      (after tramp-write-beep-advice activate)
    "Make tramp beep after writing a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-do-copy-or-rename-file
      (after tramp-copy-beep-advice activate)
    "Make tramp beep after copying a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-insert-file-contents
      (after tramp-insert-beep-advice activate)
    "Make tramp beep after inserting a file."
    (interactive)
    (beep))
  )

(use-package pass
  :commands pass)

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :diminish indent-guide-mode)

(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ; M-;

(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-diff-options "-w")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-vertically)
  (setq ediff-shell (getenv "$SHELL")))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-layout-mode t)
  (electric-indent-mode t)
  ;; Ignore electric indentation for python and yaml
  (defun electric-indent-ignore-mode (char)
    "Ignore electric indentation for 'python-mode'.  CHAR is input character."
    (if (or (equal major-mode 'python-mode)
            (equal major-mode 'yaml-mode))
        'no-indent
      nil))
  (add-hook 'electric-indent-functions 'electric-indent-ignore-mode))

(use-package compile
  :ensure nil
  :preface
  ;; ANSI Coloring
  ;; @see https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  (defun my-colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

(use-package dumb-jump
  :after hydra
  :hook (sej/after-init . dumb-jump-mode)
  :defines sej-mode-map
  :functions dumb-jump-hydra/body
  :bind (:map sej-mode-map
              ("M-g o" . dumb-jump-go-other-window)
              ("M-g j" . dumb-jump-go)
              ("M-g i" . dumb-jump-go-prompt)
              ("M-g x" . dumb-jump-go-prefer-external)
              ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-prefer-searcher 'ag)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))

  (add-to-list 'dumb-jump-language-file-exts  '(:language "elisp" :ext ".org[ emacs-lisp ]*]" :agtype "elisp" :rgtype "elisp"))

  (defun dumb-jump-get-language-from-mode ()
    "Extract the language from the 'major-mode' name.  Currently just everything before '-mode'."
    (let* ((lookup '(sh "shell" cperl "perl" matlab "matlab" emacs-lisp "elisp"))
           (m (dumb-jump-get-mode-base-name))
           (result (plist-get lookup (intern m))))
      result))

  (defhydra hydra-dumb-jump (:color blue :hint none)
    "
^Jump^                            ^Other^
^^────────────────────────────────^^───────────────
_j_: Go                           _i_: Prompt
_o_: Go other window              _l_: Quick look
_e_: Go external                  _b_: Back
_x_: Go external other window
"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Go other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")
    ("q" nil "quit"))
  (bind-key "M-g h" #'hydra-dumb-jump/body dumb-jump-mode-map))

(use-package flymake
  :ensure t
  :defines sej-mode-map
  :hook (post-command . flymake-error-at-point)
  :bind (:map sej-mode-map
              ("H-[" . flymake-goto-prev-error)
              ("H-]" . flymake-goto-next-error))
  :init
  (defun flymake-error-at-point ()
    "Show the flymake error in the minibuffer when point is on an invalid line."
    (when (get-char-property (point) 'flymake-overlay)
      (let ((help (get-char-property (point) 'help-echo)))
        (if help (message "%s" help))))))

(use-package flycheck
  ;; ;:diminish flycheck-mode
  :defines sej-mode-map
  :hook (prog-mode . global-flycheck-mode)
  :bind
  (:map sej-mode-map
        ("s-[" . flycheck-previous-error)
        ("s-]" . flycheck-next-error)
        ("C-c f" . flycheck-list-errors)
        ("s-f" . flycheck-list-errors)        )
  :init
  (global-flycheck-mode 1)
  :config
  (defadvice flycheck-next-error (before wh/flycheck-next-error-push-mark activate)
    (push-mark))
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save
                                              mode-enabled
                                              idle-change
                                              idle-buffer-switch))
  (custom-set-faces
   '(flycheck-error ((((class color)) (:underline "Red"))))
   '(flycheck-warning ((((class color)) (:underline "Orange")))))

  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-flake8-maximum-line-length 79)
  (setq flycheck-highlighting-mode 'lines)
  (progn    (set-face-attribute 'flycheck-warning nil
                                :inherit 'warning
                                :underline nil)
            (set-face-attribute 'flycheck-error nil
                                :inherit 'error
                                :underline nil)))

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-pos-tip-display-errors-tty-function #'flycheck-popup-tip-show-popup))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package emr
  ;; Just hit H-r to access your refactoring tools in any supported mode.
  :bind (:map sej-mode-map
              ("C-c s r" . emr-show-refactor-menu)
              ("H-r" . emr-show-refactor-menu) )
  :hook (prog-mode . emr-initialize))

(use-package projectile
  :diminish
  :bind ("H-f" . projectile-find-file)
  :bind-keymap (  ("s-P" . projectile-command-map)
                  ("C-c p" . projectile-command-map))
  :hook (sej/after-iinit . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ivy)
  :config
  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".tern-port")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS")
  (add-to-list 'projectile-globally-ignored-files "GSYMS")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (setq projectile-project-search-path '("~/Projects/" "~/" "~/Documents/" "~/gdrive/"))

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (executable-find "rg")
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  )

(use-package magit
  :bind (("C-x g" . magit-status)
         ("<f12>" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  (if (fboundp 'transient-append-suffix)
      ;; Add switch: --tags
      (transient-append-suffix 'magit-fetch
        "-p" '("-t" "Fetch all tags" ("-t" "--tags")))))

(use-package forge
  :after magit
  :demand)

(use-package magit-todos
  :hook (sej/after-init . magit-todos-mode))

(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

(use-package git-messenger
  :bind (
         :map sej-mode-map
         (
          ("C-x v p" . git-messenger:popup-message)
          ("C-x M" . git-messenger:popup-message)
          ("H-m" . git-messenger:popup-message)
          )
         :map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

(use-package smerge-mode
  :ensure nil
  :after hydra
  :diminish
  :commands (smerge-mode
             smerge-auto-leave
             smerge-next
             smerge-prev
             smerge-keep-base
             smerge-keep-upper
             smerge-keep-lower
             smerge-keep-all
             smerge-keep-current
             smerge-keep-current
             smerge-diff-base-upper
             smerge-diff-upper-lower
             smerge-diff-base-lower
             smerge-refine
             smerge-ediff
             smerge-combine-with-next
             smerge-resolve
             smerge-kill-current)
  :init
  (defhydra hydra-smerge
    (:color red :hint none :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^──────────-^^───────────────────^^─────────────────────^^──────────────────
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine               _ZZ_: Save and bury
^^           _RET_: current       _E_diff                _q_: cancel
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (hydra-smerge/body))))))

(use-package browse-at-remote
  :bind (:map sej-mode-map
              (("C-c s b" . browse-at-remote)
               ("C-x v B" . browse-at-remote))
              :map vc-prefix-map
              ("B" . browse-at-remote)))

(use-package gist
  :defines sej-mode-map
  :bind  (:map sej-mode-map
               ("C-x G" . gist-list)
               ("H-G" . gist-list)))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(defun sej/git-blame-line ()
  "Runs `git blame` on the current line and
   adds the commit id to the kill ring"
  (interactive)
  (let* ((line-number (save-excursion
                        (goto-char (point-at-bol))
                        (+ 1 (count-lines 1 (point)))))
         (line-arg (format "%d,%d" line-number line-number))
         (commit-buf (generate-new-buffer "*git-blame-line-commit*")))
    (call-process "git" nil commit-buf nil
                  "blame" (buffer-file-name) "-L" line-arg)
    (let* ((commit-id (with-current-buffer commit-buf
                        (buffer-substring 1 9)))
           (log-buf (generate-new-buffer "*git-blame-line-log*")))
      (kill-new commit-id)
      (call-process "git" nil log-buf nil
                    "log" "-1" "--pretty=%h   %an   %s" commit-id)
      (with-current-buffer log-buf
        (message "Line %d: %s" line-number (buffer-string)))
      (kill-buffer log-buf))
    (kill-buffer commit-buf)))

(define-key sej-mode-map (kbd "H-b") 'sej/git-blame-line)

(use-package hippie-expand
  :ensure nil
  :bind (:map sej-mode-map
              ("M-/" . hippie-expand))
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-line-all-buffers
          try-complete-lisp-symbol-partially
          try-compelete-lisp-symbol)))

(use-package company
  :diminish company-mode
  :defines
  sej-mode-map
  company-dabbrev-ignore-case
  company-dabbrev-downcase
  company-dabbrev-code-modes
  company-dabbrev-code-ignore-case
  :commands company-abort
  :bind (
         ("<backtab>" . company-yasnippet)
         :map sej-mode-map
         (("C-<tab>" . company-complete)
          ("M-<tab>" . company-complete))
         :map company-active-map
         (("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-d" . company-show-doc-buffer)
          ("C-l" . company-show-location)
          ("<tab>" . company-complete))
         :map company-search-map
         (("C-p" . company-select-previous)
          ("C-n" . company-select-next)))
  :hook (sej/after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(when emacs/>=26p
  (use-package company-box
    :after company
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (setq company-box-backends-colors nil)
    (setq company-box-show-single-candidate t)
    (setq company-box-max-candidates 50)

    ;; Support `company-common'
    (defun my-company-box--make-line (candidate)
      (-let* (((candidate annotation len-c len-a backend) candidate)
              (color (company-box--get-color backend))
              ((c-color a-color i-color s-color) (company-box--resolve-colors color))
              (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
              (candidate-string (concat (propertize company-common 'face 'company-tooltip-common)
                                        (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
              (align-string (when annotation
                              (concat " " (and company-tooltip-align-annotations
                                               (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
              (space company-box--space)
              (icon-p company-box-enable-icon)
              (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
              (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                              (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                            (company-box--apply-color icon-string i-color)
                            (company-box--apply-color candidate-string c-color)
                            align-string
                            (company-box--apply-color annotation-string a-color)))
              (len (length line)))
        (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                         'company-box--color s-color)
                             line)
        line))
    (advice-add #'company-box--make-line :override #'my-company-box--make-line)

    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

    (with-eval-after-load 'all-the-icons
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
              (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
              (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
              (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
              (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
              (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
              (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
              (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
              (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
              (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
              (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
              (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2)))))))

(use-package company-quickhelp
  :after company
  :hook ((global-company-mode company-mode) . company-quickhelp-mode)
  :bind (:map company-active-map
              ("H-h" . company-quickhelp-manual-begin))
  :config (setq company-quickhelp-delay 1))

(use-package company-statistics
  :after company
  :hook (sej/after-init . company-statistics-mode))

(use-package company-try-hard
  :commands company-try-hard
  :bind (("H-/" . company-try-hard)
         :map company-active-map
         ("H-/" . company-try-hard)))

(use-package company-shell
  :after company
  :init
  (defun sej/company-shell-hook ()
    (add-to-list 'company-backends '(company-shell
                                     company-shell-env
                                     company-fish-shell)) )
  :hook (sej/after-init . sej/company-shell-hook) )

(use-package company-jedi
  :after company
  :init
  (defun sej/company-jedi-hook ()
    (add-to-list 'company-backends 'company-jedi)  )
  :hook (python-mode . sej/company-jedi-hook) )

(use-package company-arduino
  :after company
  :hook ( (arduino-mode . irony-mode)
          (irony-mode . company-arduino-turn-on) )
  :config
  ;; Configuration for company-c-headers.el
  ;; The `company-arduino-append-include-dirs' function appends
  ;; Arduino's include directories to the default directories
  ;; if `default-directory' is inside `company-arduino-home'. Otherwise
  ;; just returns the default directories.
  ;; Please change the default include directories accordingly.
  (defun my-company-c-headers-get-system-path ()
    "Return the system include path for the current buffer."
    (let ((default '("/usr/include/" "/usr/local/include/")))
      (company-arduino-append-include-dirs default t)))
  (setq company-c-headers-path-system 'my-company-c-headers-get-system-path)

  ;; If you are already using company-irony and company-c-headers, you might have same setting. That case, you can omit below setting.
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-c-headers))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((sej/after-init . yas-reload-all)
         ((prog-mode org-mode) . yas-minor-mode))
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("<A-tab>" . yas-expand)
              :map yas-keymap
              ("<tab>" . nil)
              ("TAB" . nil)
              ("M-n" . yas-next-field-or-maybe-expand)
              ("M-p" . yas-prev-field))
  :config (use-package yasnippet-snippets))

(use-package hydra)

(define-key sej-mode-map (kbd "<s-return>") 'eval-last-sexp)
(define-key sej-mode-map (kbd "<H-return>") 'eval-buffer)
(define-key sej-mode-map (kbd "<A-return>") 'eval-region)

(define-key emacs-lisp-mode-map (kbd "C-c D") 'toggle-debug-on-error)

;; use flycheck in elisp
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode))

(use-package eldoc
  :diminish eldoc-mode
  :hook
  ((emacs-lisp-mode . eldoc-mode)
   (ielm-mode . eldoc-mode)
   (lisp-interaction-mode . eldoc-mode)
   (eval-expression-minibuffer-setup . eldoc-mode))
  :config
  (setq eldoc-idle-delay 0.1) )

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :config
  (global-unset-key (kbd "C-c C-d d"))
  (global-unset-key (kbd "C-c C-d C-d")))

(use-package eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode))

(defun sej/ielm-other-window ()
  "Run ielm on other window."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key sej-mode-map (kbd "s-i") 'sej/ielm-other-window)

(defun sej/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'sej/remove-elc-on-save)

(use-package python
  :ensure nil
  :defines gud-pdb-command-name pdb-path flycheck-disabled-checkers
  :interpreter "python"
  :bind (:map python-mode-map
              ("<backtab>" . python-back-indent)
              ("<f9>" . py-insert-debug))
  :hook ((python-mode . flycheck-mode)
         (python-mode . (lambda ()
                          (add-to-list 'flycheck-disabled-checkers 'python-pylint)))
         )
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")

  (define-skeleton python-insert-docstring
    "Insert a Python docstring."
    "This string is ignored!"
    "\"\"\"" - "\n\n    \"\"\"")

  (define-key python-mode-map (kbd "s-\\") 'python-insert-docstring)

  (setq fill-column 79)
  (setq-default flycheck-flake8rc "~/.config/flake8rc")
  (setq python-check-command "flake8")
  (setq tab-width 2)

  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python"))))
  )

(use-package live-py-mode)

(use-package yapfify
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))

(use-package ein
  :diminish ein:notebook-mode
  :defines ein:completion-backend
  :init (setq ein:completion-backend 'ein:use-company-backend))

(use-package pip-requirements)

(use-package pyvenv
  :hook (pyvenv-post-activate . pyvenv-restart-python))

(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

(use-package json-mode)

(use-package js2-mode
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  (with-eval-after-load 'flycheck
    (if (or (executable-find "eslint_d")
            (executable-find "eslint")
            (executable-find "jshint"))
        (setq js2-mode-show-strict-warnings nil))
    (if (executable-find "eslint_d")
        ;; https://github.com/mantoni/eslint_d.js
        ;; npm -i -g eslint_d
        (setq flycheck-javascript-eslint-executable "eslint_d"))))

(use-package js2-refactor
  :after js2-mode
  :diminish js2-refactor-mode
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package mocha
  :config (use-package mocha-snippets))

(use-package skewer-mode
  :diminish skewer-mode skewer-css skewer-html
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode)))

(use-package web-beautify
  :init
  (with-eval-after-load 'js-mode
    (bind-key "C-c b" #'web-beautify-js js-mode-map))
  (with-eval-after-load 'js2-mode
    (bind-key "C-c b" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c b" #'web-beautify-js json-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c b" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c b" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c b" #'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

(use-package haml-mode)

(use-package php-mode
  :mode (("\\.module$" . php-mode)
         ("\\.inc$" . php-mode)
         ("\\.install$" . php-mode)
         ("\\.engine$" . php-mode)
         ("\\.\\(?:php\\|phtml\\)\\'" . php-mode)))

(use-package yaml-mode
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook ((c-mode-common . flycheck-mode)
         (c-mode-common . (lambda ()
                            (c-set-style "bsd")
                            (setq tab-width 4)
                            (setq c-basic-offset 4))))
  :init
  ;; Set the default formatting styles for various C based modes
  (setq c-default-style
        '((awk-mode . "awk")
          (other . "java")))
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :hook (c++-mode modern-c++-font-lock-mode)))

(use-package csharp-mode)

(use-package arduino-mode
  :mode "\\.ino$"
  :config
  (setq arduino-mode-home "/Users/stephenjenkins/Projects/sej/Arduino")
  (setq arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino"))

(use-package swift-mode
  :config
  (use-package flycheck-swift
    :after flycheck
    :commands flycheck-swift-setup
    :init (flycheck-swift-setup)))

(use-package rust-mode
  :config (setq rust-format-on-save t))

(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

(use-package ztree
  :commands ztree)

(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon)
  :commands ibuffer-find-file
  :bind (:map sej-mode-map
              ("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

  ;; Display buffer icons on GUI
  (when (display-graphic-p)
    ;; To be correctly aligned, the size of the name field must be equal to that
    ;; of the icon column below, plus 1 (for the tab I inserted)
    (define-ibuffer-column icon (:name "   ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    (let ((tab-width 1))
      (setq ibuffer-formats '((mark modified read-only locked
                                    ;; Here you may adjust by replacing :right with :center or :left
                                    ;; According to taste, if you want the icon further from the name
                                    " " (icon 1 -1 :left :elide) "\t" (name 18 18 :left :elide)
                                    " " (size 9 -1 :right)
                                    " " (mode 16 16 :left :elide) " " filename-and-process)
                              (mark " " (name 16 -1) " " filename)))))

  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.25)
               " ")
            "Project: "))))

(define-key sej-mode-map (kbd "s-r") 'jump-to-register)
                                        ; (kbd "C-x r j") is build in global
(set-register ?b '(file . "~/.ssh/custom-post.el"))
(set-register ?s '(file . "~/.emacs.d/lisp/init-bindings.el"))
(set-register ?a '(file . "~/.emacs.d/lisp/init-appearance.el"))
(set-register ?m '(file . "~/.emacs.d/lisp/init-misc-pkgs.el"))
(set-register ?r '(file . "~/.emacs.d/lisp/init-registers.el"))
(set-register ?d '(file . "~/.emacs.d/lisp/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))

(when sej-dashboard
  (use-package dashboard
    :after hydra
    :diminish (dashboard-mode page-break-lines-mode)
    :defines (persp-save-dir persp-special-last-buffer sej-mode-map)
    :functions (all-the-icons-faicon
                all-the-icons-material
                open-custom-file
                persp-get-buffer-or-null
                persp-load-state-from-file
                persp-switch-to-buffer
                winner-undo
                widget-forward)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :bind (("<f6>" . open-dashboard)
           (:map sej-mode-map
                 ("C-c s d" . open-dashboard))
           (:map dashboard-mode-map
                 ("H" . browse-homepage)
                 ("R" . restore-session)
                 ("L" . persp-load-state-from-file)
                 ("S" . open-custom-file)
                 ("U" . sej-update)
                 ("q" . quit-dashboard)
                 ("C-p" . widget-backward)
                 ("C-n" . widget-forward)
                 ("<up>" . widget-backward)
                 ("<down>" . widget-forward)
                 ("<tab>" . widget-forward)
                 ("C-i" . widget-forward)
                 ("<backtab>" . widget-backward)
                 ))
    :hook (dashboard-mode . (lambda ()
                              (setq-local frame-title-format "")
                              (setq-local tab-width 1)))
    :init (dashboard-setup-startup-hook)
    :config
    ;; (setq dashboard-banner-logo-title "SeJ EMACS")
    (setq dashboard-startup-banner nil)
    (setq dashboard-center-content nil)
    (setq dashboard-show-shortcuts t)
    (setq dashboard-items '((recents  . 15)
                            (bookmarks . 15)
                            (projects . 10)
                            (registers . 10)))

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1)))
          (setq dashboard-recover-layout-p t))
      (delete-other-windows)

      ;; Refresh dashboard buffer
      (if (get-buffer dashboard-buffer-name)
          (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (goto-char (point-min))
      (dashboard-goto-recent-files))

    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
           (message "Error: Unable to restore last session -- %s" err)))
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (funcall (local-key-binding "p")))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (funcall (local-key-binding "m")))

    (defun dashboard-goto-registers ()
      "Go to registers."
      (interactive)
      (funcall (local-key-binding "e")))

    ;; Add heading icons
    (defun dashboard-insert-heading-icon (heading &optional _shortcut)
      (when (display-graphic-p)
        ;; Load `all-the-icons' if it's unavailable
        (unless (featurep 'all-the-icons)
          (require 'all-the-icons nil t))

        (insert (cond
                 ((string-equal heading "Recent Files:")
                  (all-the-icons-octicon "file-text" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Bookmarks:")
                  (all-the-icons-octicon "bookmark" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Registers:")
                  (all-the-icons-octicon "clippy" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Projects:")
                  (all-the-icons-octicon "file-directory" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))))
        (insert " ")))
    (advice-add #'dashboard-insert-heading :before #'dashboard-insert-heading-icon)

    ;; Add file icons
    ;; MUST redefine the sections because of the macro `dashboard-insert-section-list'
    (defmacro dashboard-insert-section-list (section-name list action &rest rest)
      "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST to widget creation."
      `(when (car ,list)
         (mapc (lambda (el)
                 (let ((widget nil))
                   (insert "\n    ")
                   (when (display-graphic-p)
                     (insert (if-let ((path (car (last (split-string ,@rest " - ")))))
                                 (if (file-directory-p path)
                                     (cond
                                      ((and (fboundp 'tramp-tramp-file-p)
                                            (tramp-tramp-file-p default-directory))
                                       (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                                      ((file-symlink-p path)
                                       (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                                      ((all-the-icons-dir-is-submodule path)
                                       (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                                      ((file-exists-p (format "%s/.git" path))
                                       (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                                      (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                                           (apply (car matcher) (list (cadr matcher) :v-adjust 0.01)))))
                                   (all-the-icons-icon-for-file (file-name-nondirectory path)))
                               (all-the-icons-octicon "clippy" :height 1.0 :v-adjust 0.01)
                               ))
                     (insert "\t"))
                   (setq widget
                         (widget-create 'push-button
                                        :action ,action
                                        :mouse-face 'highlight
                                        :button-prefix ""
                                        :button-suffix ""
                                        :format "%[%t%]"
                                        ,@rest))))
               ,list)))

    (defmacro dashboard-insert-shortcut (shortcut-char
                                         search-label
                                         &optional no-next-line)
      "Insert a shortcut SHORTCUT-CHAR for a given SEARCH-LABEL.
Optionally, provide NO-NEXT-LINE to move the cursor forward a line."
      `(progn
         (eval-when-compile (defvar dashboard-mode-map))
         (let ((sym (make-symbol (format "Jump to \"%s\"" ,search-label))))
           (fset sym (lambda ()
                       (interactive)
                       (unless (search-forward ,search-label (point-max) t)
                         (search-backward ,search-label (point-min) t))
                       ,@(unless no-next-line
                           '((forward-line 1)))
                       (back-to-indentation)
                       (if (display-graphic-p) (widget-forward 1))))
           (eval-after-load 'dashboard
             (define-key dashboard-mode-map ,shortcut-char sym)))))

    ;; Recentf
    (defun dashboard-insert-recents (list-size)
      "Add the list of LIST-SIZE items from recently edited files."
      (recentf-mode)
      (dashboard-insert-section
       "Recent Files:"
       recentf-list
       list-size
       "r"
       `(lambda (&rest ignore) (find-file-existing ,el))
       (abbreviate-file-name el)))

    ;; Bookmarks
    (defun dashboard-insert-bookmarks (list-size)
      "Add the list of LIST-SIZE items of bookmarks."
      (require 'bookmark)
      (dashboard-insert-section
       "Bookmarks:"
       (dashboard-subseq (bookmark-all-names)
                         0 list-size)
       list-size
       "m"
       `(lambda (&rest ignore) (bookmark-jump ,el))
       (let ((file (bookmark-get-filename el)))
         (if file
             (format "%s - %s" el (abbreviate-file-name file))
           el))))

    ;; Projectile
    (defun dashboard-insert-projects (list-size)
      "Add the list of LIST-SIZE items of projects."
      (require 'projectile)
      (projectile-load-known-projects)
      (dashboard-insert-section
       "Projects:"
       (dashboard-subseq (projectile-relevant-known-projects)
                         0 list-size)
       list-size
       "p"
       `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
       (abbreviate-file-name el)))

    ;;
    ;; Registers
    ;;
    (defun dashboard-insert-registers (list-size)
      "Add the list of LIST-SIZE items of registers."
      (require 'register)
      (dashboard-insert-section
       "Registers:"
       register-alist
       list-size
       "e"
       (lambda (&rest ignore) (jump-to-register (car el)))
       (format "%c - %s" (car el) (register-describe-oneline (car el)))))

    (defun dashboard-insert-buttons ()
      "Insert buttions after the banner."
      (interactive)
      (with-current-buffer (get-buffer dashboard-buffer-name)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (search-forward dashboard-banner-logo-title nil t)

          (insert "")
          (widget-create 'url-link
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-octicon "mark-github"
                                                           :height 1.1
                                                           :v-adjust 0.0
                                                           :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Homepage" 'face 'font-lock-keyword-face))
                         :help-echo "Browse homepage"
                         :mouse-face 'highlight
                         sej-homepage)
          (insert " ")
          (widget-create 'push-button
                         :help-echo "Restore previous session"
                         :action (lambda (&rest _) (restore-session))
                         :mouse-face 'highlight
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-material "restore"
                                                            :height 1.35
                                                            :v-adjust -0.24
                                                            :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Session" 'face 'font-lock-keyword-face)))
          (insert " ")
          (widget-create 'file-link
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-octicon "tools"
                                                           :height 1.1
                                                           :v-adjust 0.0
                                                           :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Settings" 'face 'font-lock-keyword-face))
                         :help-echo "Open custom file"
                         :mouse-face 'highlight
                         custom-file)
          (insert " ")
          (widget-create 'push-button
                         :help-echo "Update SeJ Emacs"
                         :action (lambda (&rest _) (sej-update))
                         :mouse-face 'highlight
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-material "update"
                                                            :height 1.35
                                                            :v-adjust -0.24
                                                            :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Update" 'face 'font-lock-keyword-face)))
          (insert " ")
          (widget-create 'push-button
                         :help-echo "Help (?/h)"
                         :action (lambda (&rest _) (hydra-dashboard/body))
                         :mouse-face 'highlight
                         :tag (concat
                               (if (display-graphic-p)
                                   (all-the-icons-faicon "question"
                                                         :height 1.2
                                                         :v-adjust -0.1
                                                         :face 'font-lock-string-face)
                                 (propertize "?" 'face 'font-lock-string-face))))
          (insert "\n")

          (insert (concat
                   (propertize (format "%d packages loaded in %s"
                                       (length package-activated-list) (emacs-init-time))
                               'face 'font-lock-comment-face)))
          (insert "\n")
          )))
    (add-hook 'dashboard-mode-hook #'dashboard-insert-buttons)

    (defun dashboard-insert-footer ()
      "Insert footer of dashboard."
      (interactive)
      (with-current-buffer (get-buffer dashboard-buffer-name)
        (let ((inhibit-read-only t))
          (goto-char (point-max))

          (insert "\n\n")
          (insert " ")
          (insert (propertize
                   (format "SeJ, %s" (format-time-string "%Y"))
                   'face font-lock-doc-face))
          (insert "\n"))))
    (add-hook 'dashboard-mode-hook #'dashboard-insert-footer)

    (defhydra hydra-dashboard (:color red :hint none)
      "
^Head^               ^Section^            ^Item^                  ^Dashboard^
^^───────────────────^^───────────────────^^──────────────────────^^───────────────
_U_pdate             _r_: Recent Files    _RET_: Open             _<f2>_: Open
_H_omePage           _m_: Bookmarks       _<tab>_/_C-i_: Next       _Q_: Quit
_R_ecover session    _p_: Projects        _<backtab>_: Previous
_L_ist sessions      _e_: Registers       _C-n_: Next line
_S_ettings                                _C-p_: Previous Line
"
      ("<tab>" widget-forward)
      ("C-i" widget-forward)
      ("<backtab>" widget-backward)
      ("RET" widget-button-press :exit t)
      ("g" dashboard-refresh-buffer :exit t)
      ("r" dashboard-goto-recent-files)
      ("p" dashboard-goto-projects)
      ("m" dashboard-goto-bookmarks)
      ("e" dashboard-goto-registers)
      ("H" browse-homepage :exit t)
      ("R" restore-session :exit t)
      ("L" persp-load-state-from-file :exit t)
      ("S" open-custom-file :exit t)
      ("U" sej-update :exit t)
      ("C-n" next-line)
      ("C-p" previous-line)
      ("<f2>" open-dashboard :exit t)
      ("Q" quit-dashboard :exit t)
      ("q" nil "quit")
      ("C-g" nil "quit"))
    (bind-keys :map dashboard-mode-map
               ("h" . hydra-dashboard/body)
               ("?" . hydra-dashboard/body))))

(use-package page-break-lines
  :config
  (setq global-page-break-lines-mode t)
  )

(use-package autoinsert
  :hook (find-file . auto-insert)
  :defines
  auto-insert-query
  auto-insert-directory
  :init
  (setq auto-insert-directory "~/.emacs.d/templates/")
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  :config
  (define-auto-insert ".*\\.py[3]?$" "template.py")
  (define-auto-insert ".*\\.el" "template.el")
  )

(defun sej/create-non-existent-directory ()
  "Ask to make directory for file if it does not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p? (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'sej/create-non-existent-directory)

(unless sys/win32p
  (use-package sudo-edit))

(use-package vlf-setup
  :ensure vlf
  :commands (vlf vlf-occur-load vlf-ediff-files))

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    ;; Use GNU ls as `gls' from `coreutils' if available.
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (when (executable-find "gls")
      (setq insert-directory-program (executable-find "gls")
            dired-use-ls-dired t) ))

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first"))

(use-package all-the-icons-dired
  :diminish
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (defun my-all-the-icons-dired--display ()
    "Display the icons of files without colors in a dired buffer."

    ;; Fix: not display icons after dired commands (e.g insert-subdir, create-directory)
    ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/11
    (all-the-icons-dired--reset)

    (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
      (setq-local all-the-icons-dired-displayed t)
      (let ((inhibit-read-only t)
            (remote-p (and (fboundp 'tramp-tramp-file-p)
                           (tramp-tramp-file-p default-directory))))
        (save-excursion
          (setq-local tab-width 1)
          (goto-char (point-min))
          (while (not (eobp))
            (when (dired-move-to-filename nil)
              (insert "\t")
              (let ((file (dired-get-filename 'verbatim t)))
                (unless (member file '("." ".."))
                  (let ((filename (dired-get-filename nil t)))
                    (if (file-directory-p filename)
                        (let ((icon
                               (cond
                                (remote-p
                                 (all-the-icons-octicon "file-directory" :height 1.0 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))
                                ((file-symlink-p filename)
                                 (all-the-icons-octicon "file-symlink-directory" :height 1.0 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))
                                ((all-the-icons-dir-is-submodule filename)
                                 (all-the-icons-octicon "file-submodule" :height 1.0 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))
                                ((file-exists-p (format "%s/.git" filename))
                                 (all-the-icons-octicon "repo" :height 1.1 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust ))
                                (t (let ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                                     (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust)))))))
                          (insert icon))
                      (insert (all-the-icons-icon-for-file file :v-adjust -0.05))))
                  (insert "\t"))))
            (forward-line 1))))))
  (advice-add #'all-the-icons-dired--display :override #'my-all-the-icons-dired--display))

(use-package dired-aux :ensure nil)

(use-package dired-x
  :ensure nil
  :demand
  :config
  (let ((cmd (cond
              (sys/mac-x-p "open")
              (sys/linux-x-p "xdg-open")
              (sys/win32p "start")
              (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(use-package quick-preview
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c q" . quick-preview-at-point)
              :map dired-mode-map
              ("Q" . quick-preview-at-point)))

(use-package browse-at-remote
  :bind ("C-c s b" . browse-at-remote))

(use-package diredfl
  :init (diredfl-global-mode 1))

(use-package deft
  :ensure t
  :defines sej-mode-map deft-text-mode
  :bind (:map sej-mode-map
              ("<f7>" . deft)
              ("C-c d" . deft))
  :config
  (setq deft-directory sej-org-directory)
  (setq deft-use-filename-as-title t
        deft-default-extension "org"
        deft-text-mode (quote (org-mode))
        deft-org-mode-title-prefix t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval 0
        deft-recursive t
        deft-extensions (quote ("org" "text" "md" "markdown" "txt"))
        deft-org-mode-title-prefix t))

(use-package writegood-mode
  :hook (markdown-mode . writegood-mode)
  )

(use-package olivetti
  :diminish
  :hook (text-mode-hook . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

(use-package markdown-mode
  :defines flycheck-markdown-markdownlint-cli-config
  :preface
  ;; Install: pip install grip
  (defun markdown-preview-grip ()
    "Render and preview with `grip'."
    (interactive)
    (let ((program "grip")
          (port "6419")
          (buffer "*gfm-to-html*"))

      ;; If process exists, kill it.
      (markdown-preview-kill-grip buffer)

      ;; Start a new `grip' process.
      (start-process program buffer program (buffer-file-name) port)
      (sleep-for 1) ; wait for process start
      (browse-url (format "http://localhost:%s/%s.%s"
                          port
                          (file-name-base)
                          (file-name-extension
                           (buffer-file-name))))))

  (defun markdown-preview-kill-grip (&optional buffer)
    "Kill `grip' process."
    (interactive)
    (let ((process (get-buffer-process (or buffer "*gfm-to-html*"))))
      (when process
        (kill-process process)
        (message "Process %s killed" process))))

  ;; Install: npm i -g markdownlint-cli
  (defun set-flycheck-markdownlint ()
    "Set the `mardkownlint' config file for the current buffer."
    (let* ((md-lint ".markdownlint.json")
           (md-file buffer-file-name)
           (md-lint-dir (and md-file
                             (locate-dominating-file md-file md-lint))))
      (setq-local flycheck-markdown-markdownlint-cli-config
                  (concat md-lint-dir md-lint))))
  :bind (:map markdown-mode-command-map
              ("g" .  markdown-preview-grip)
              ("k" .  markdown-preview-kill-grip))
  :hook ((markdown-mode . flyspell-mode)
         (markdown-mode . auto-fill-mode)
         (markdown-mode . set-flycheck-markdownlint)
         (markdown-mode . visual-line-mode)
         (markdown-mode . writegood-mode))
  :functions writegood-mode
  :commands (markdown-mode gfm-mode)
  :mode   (("README\\.md\\'" . gfm-mode)
           ("github\\.com.*\\.txt\\'" . gfm-mode)
           ("\\.md\\'"          . markdown-mode)
           ("\\.markdown\\'"    . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-header-scaling t)
  (setq markdown-command "pandoc --smart -f markdown -t html")

  (when sys/macp
    (let ((typora "/Applications/Typora.app/Contents/MacOS/Typora"))
      (if (file-exists-p typora)
          (setq markdown-open-command typora))))

  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (setq markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
box-sizing: border-box;
max-width: 740px;
width: 100%;
margin: 40px auto;
padding: 0 10px;
}
</style>
<script src='http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
document.body.classList.add('markdown-body');
document.querySelectorAll('pre[lang] > code').forEach((code) => {
code.classList.add(code.parentElement.lang);
hljs.highlightBlock(code);
});
});
</script>
")

(use-package markdown-toc))

(use-package textile-mode
  :mode "\\.textile\\'")

(use-package adoc-mode)

(use-package abbrev
  :ensure nil
  :hook ((sej/after-init org-mode) . abbrev-mode)
  :diminish abbrev-mode
  :config
  (setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...
  (define-abbrev-table
    'global-abbrev-table
    '(("<sej" "stephenearljenkins" nil 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<orgh" "" 'sej/org-header 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<orgl" "" 'sej/org-wrap-elisp 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<orgs" "" 'sej/org-wrap-source 0))))

(defun sej/number-rectangle (start end format-string from)
  "Delete text in the region-rectangle, then number it from (START to END with FORMAT-STRING FROM)."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: "
                      (if (looking-back "^ *" nil nil) "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (loop with column = (current-column)
          while (and (<= (point) end) (not (eobp)))
          for i from from   do
          (move-to-column column t)
          (insert (format format-string i))
          (forward-line 1)))
  (goto-char start))

(define-key sej-mode-map (kbd "C-c s n") 'sej/number-rectangle)
(define-key sej-mode-map (kbd "C-x r N") 'sej/number-rectangle)

(use-package flyspell
  :functions
  flyspell-correct-word
  flyspell-goto-next-error
  :defines
  sej-mode-map
  :bind
  (:map sej-mode-map
        ("<f8>" . ispell-word)
        ("s-," . ispell-word)
        ("C-<f8>" . flyspell-mode)
        ("M-<f8>" . flyspell-check-next-highlighted-word)
        ("S-<f8>" . ispell-region)
        ("s-." . ispell-region)
        )
  :hook (((text-mode outline-mode org-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word) ;;for mac
  (define-key flyspell-mouse-map [mouse-3] #'undefined)

  (setq ispell-personal-dictionary "~/sej.ispell")

  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (setq ispell-dictionary-alist '(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_GB-ise") nil utf-8)
                                  ("canadian" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_CA") nil utf-8)
                                  ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))

  (setq ispell-dictionary "canadian")

  (defun flyspell-check-next-highlighed-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-list-command "list"))

(use-package powerthesaurus
  :bind (:map sej-mode-map
              ("C-c s t" . powerthesaurus-lookup-word-dwim)
              ("s-|" . powerthesaurus-lookup-word-dwim)))

(use-package define-word
  :unless sys/macp
  :bind (:map sej-mode-map
              ("C-c s w" . define-word-at-point)
              ("H-d" . define-word-at-point)
              ("H-D" . define-word)
              ("s-\\" . define-word-at-point)))

(use-package osx-dictionary
  :if sys/macp
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c s w" . osx-dictionary-search-word-at-point)
              ("s-\\" . osx-dictionary-search-word-at-point)
              ("H-d" . osx-dictionary-search-word-at-point)
              ("H-D" . osx-dictionary-search-pointer)
              ("C-c s i" . osx-dictionary-search-input)
              ))

(use-package view
  :defines (View-scrool-half-page-forward View-scrool-half-page-backward)
  :bind (:map view-mode-map (("C-v" . 'View-scroll-half-page-forward)
                             ("M-v" . 'View-scroll-half-page-backward)

                             ;; less like
                             ("N" . View-search-last-regexp-backward)
                             ("?" . View-search-regexp-backward?)
                             ("g" . View-goto-line)
                             ("G" . View-goto-line-last)
                             ;; vi/w3m like
                             ("h" . backward-char)
                             ("j" . next-line)
                             ("k" . previous-line)
                             ("l" . forward-char)))
  :config
  (defun View-goto-line-last (&optional line)
    "goto last line"
    (interactive "P")
    (forward-line (line-number-at-pos (point-max)))))

(when (executable-find "ps2pdf")
  (use-package ps-print
    :ensure nil
    :commands (ps-print-preprint ps-print-with-faces)
    :init
    (defun sej/pdf-print-buffer-with-faces (&optional filename)
      "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
      (interactive (list (if current-prefix-arg
                             (ps-print-preprint 4)
                           (concat (file-name-sans-extension (buffer-file-name))
                                   ".ps"))))
      (ps-print-with-faces (point-min) (point-max) filename)
      (shell-command (concat "ps2pdf " filename))
      (delete-file filename)
      (message "Deleted %s" filename)
      (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf")))

    ))

(when (display-graphic-p)
  (use-package pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward))
    :config
    (setq pdf-view-midnight-colors '("#ededed" . "#21242b"))
    (setq pdf-annot-activate-created-annotations t)

    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when sys/macp
      (setenv "PKG_CONFIG_PATH"
              "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig"))
    (pdf-tools-install t nil t t)

    ;; Recover last viewed position
    (use-package pdf-view-restore
      :hook (pdf-view-mode . pdf-view-restore-mode)
      :init (setq pdf-view-restore-filename
                  (locate-user-emacs-file ".pdf-view-restore")))))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
  :hook (nov-mode . my-nov-setup))

(use-package org
  :ensure org-plus-contrib
  :defines
  sej-mode-map
  org-capture-bookmark
  org-capture-templates
  org-agenda-window-setup
  org-agenda-span
  org-agenda-skip-scheduled-if-deadline-is-shown
  org-agenda-todo-ignore-deadlines
  org-agenda-todo-ignore-scheduled
  org-agenda-sorting-strategy
  org-agenda-skip-deadline-prewarning-if-scheduled
  :functions
  sej/org-capture-get-src-block-string
  which-function
  :mode ("\\.org$" . org-mode)
  :hook ((org-mode . flyspell-mode)
         (org-mode . writegood-mode)
         (org-mode . visual-line-mode))
  :bind (:map sej-mode-map
              ("<f1>" . org-mode)
              ("C-c l" . org-store-link)
              ("C-c c" . org-capture)
              ("C-c a" . org-agenda)
              :map org-mode-map
              ("C-M-\\" . org-indent-region)
              ("S-<left>" . org-shiftleft)
              ("S-<right>" . org-shiftright)
              )
  :config
  (setq org-directory sej-org-directory)
  (defconst org-file-inbox (concat org-directory "/inbox.org"))
  (defconst org-file-someday (concat org-directory "/someday.org"))
  (defconst org-file-gtd (concat org-directory "/gtd.org"))
  (defconst org-file-journal (concat org-directory "/journal.org"))
  (defconst org-file-notes (concat org-directory "/notes.org"))
  (defconst org-file-code (concat org-directory "/snippets.org"))
  (setq org-replace-disputed-keys t
        org-hide-emphasis-markers t
        org-default-notes-file org-file-notes
        org-capture-bookmark t
        org-refile-use-outline-path 'file
        org-log-done 'note
        org-log-done t
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
                            (sequence "DELIGATE(D)" "CHECK(C)" "|" "VERIFIED(V)")
                            (sequence "|" "CANCELED(x)"))
        org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("WAITING" . (:foreground "blue" :weight bold))
                                 ("DONE" . (:foreground "green" :weight bold))
                                 ("DELIGATE" . (:foreground "blue" :weight bold))
                                 ("VERIFIED" . (:foreground "green" :weight bold))
                                 ("CANCELED" . (:foreground "grey" :weight bold)))
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-startup-folded nil
        )
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (let* ((variable-tuple
          (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  (define-skeleton org-skeleton
    "Header info for a emacs-org file."
    "Title: "
    "#+TITLE:" str " \n"
    "#+DATE:" '(org-date-from-calendar) " \n"
    "#+AUTHOR: " '(sej-full-name) "\n"
    "#+email: " '(sej-mail-address) "\n"
    "#+INFOJS_OPT: \n"
    "#+BABEL: :session *C* :cache yes :results output graphics :exports both :tangle yes \n"
    "-----\n\n")
  (global-set-key [C-S-f4] 'org-skeleton)

  (setq sej-project-org-capture-list (list
                                      "p" sej-project-org-capture-text 'entry (list 'file+olp+datetree sej-project-org-capture-file "Journal" ) "* %i%?\n %U"))

  (setq org-capture-templates (append
                               '(
                                 ("i" "Inbox" entry (file+headline org-file-inbox  "Inbox") "* %i%?\n %U")
                                 ("j" "Journal" entry (file+olp+datetree org-file-journal "Journal")  "* %i%?\n %U")
                                 ("n" "Notes" entry (file+headline org-file-notes  "Notes") "* %i%?\n %U")
                                 ("s" "Someday" entry (file+headline org-file-someday  "Someday") "* %i%?\n %U")
                                 ;;("t" "Todo" entry (file+headline org-file-gtd  "Todo") "* TODO %i%?")
                                 ("c" "code snippet" entry (file+headline org-file-code "code snippets")
                                  "* %?\n%(my/org-capture-code-snippet \"%F\")")
                                 )
                               (list sej-project-org-capture-list)))

  ;; org-mode agenda options
  (setq org-agenda-files (list org-file-inbox org-file-journal org-file-notes org-file-someday org-file-gtd)
        org-refile-targets '((org-file-gtd :maxlevel . 3)
                             (org-file-someday :maxlevel . 1))
        org-agenda-window-setup (quote current-window) ;open agenda in current window
        org-deadline-warning-days 7     ;warn me of any deadlines in next 7 days
        org-agenda-span (quote fortnight) ;show me tasks scheduled or due in next fortnight
        org-agenda-skip-scheduled-if-deadline-is-shown t ;don't show tasks as scheduled if they are already shown as a deadline
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        org-agenda-sorting-strategy ;sort tasks in order of when they are due and then by priority
        (quote
         ((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))))

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (ein . t)
                               (perl . t)
                               (python . t)
                               (ein . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (arduino . t)
                               (shell . t)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list) )

(use-package ob-go
  :init (cl-pushnew '(go . t) load-language-list))

(use-package ob-rust
  :init (cl-pushnew '(rust . t) load-language-list))

(use-package ob-ipython)

(use-package org-rich-yank
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config (org-bullets-mode 1))

(use-package org-fancy-priorities
  :diminish
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (unless (char-displayable-p ?❗)
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package poporg
  :ensure t
  :bind (:map sej-mode-map
              ("C-c s o" . poporg-dwim)))

(define-skeleton sej/org-header
  "Insert a standard header for org-mode files"
  "Title: "
  "#+TITLE: " str \n
  "#+AUTHOR: " (user-full-name) \n
  "#+EMAIL: " user-mail-address \n
  "#+SETUPFILE: ~/eos/setupfiles/default.setup

  | *Author* | {{{author}}} ({{{email}}})    |
  | *Date*   | {{{time(%Y-%m-%d %H:%M:%S)}}} |

  * Introduction  " \n)

(define-skeleton sej/org-wrap-elisp
  "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code"
  nil
  > "#+BEGIN_SRC emacs-lisp" \n
  > _ \n
  > "#+END_SRC" \n)

(define-skeleton sej/org-wrap-source
  "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type"
  "Language: "
  > "#+BEGIN_SRC " str \n
  > _ \n
  > "#+END_SRC" \n)

(use-package eshell
  :ensure nil
  :defines (compilation-last-buffer
            eshell-prompt-function)
  :commands (eshell/alias
             eshell-send-input
             eshell-flatten-list
             eshell-interactive-output-p
             eshell-parse-command
             eshell-command
             eshell)
  :defines (sej-mode-map
            eshell-mode-map)
  :hook  (
          (eshell-mode . (lambda ()
                           (eshell/alias "f" "find-file $1")
                           (eshell/alias "ff" "find-file $1")
                           (eshell/alias "e" "find-file $1")
                           (eshell/alias "ee" "find-file-other-window $1")
                           (eshell/alias "emacs" "find-file $1")
                           (eshell/alias "fo" "find-file-other-window $1")
                           (eshell/alias "d" "dired $1")
                           (eshell/alias "ll" "ls  -AlohG --color=always $1")
                           (eshell/alias "la" "ls -al $1")
                           (eshell/alias "gd" "magit-diff-unstaged")
                           (eshell/alias "gds" "magit-diff-staged")
                           (bind-keys :map eshell-mode-map
                                      ("M-P" . eshell-previous-prompt)
                                      ("M-N" . eshell-next-prompt)
                                      ("M-R" . eshell-previous-matching-input)
                                      ("C-l" . eshell/clear)
                                      )
                           (sej/eshell-prompt)
                           )))

  :bind (
         :map sej-mode-map
         ("H-e" . eshell)
         ("C-c e" . eshell)
         ("C-c s e" . eshell) )

  :config
  (require 'esh-opt)
  (require 'em-cmpl)
  (require 'em-smart)
  (require 'em-term)
  (require 'em-prompt)


  (setenv "PAGER" "cat")

  ;; Visual commands
  (setq eshell-visual-commands (append '("screen" "htop" "ncftp" "elm" "el" "nano" "ssh" "nethack" "dstat" "tail")))
  (setq eshell-visual-subcommands (append '("git" ("log" "diff" "show"))))


  (setq eshell-glob-case-insensitive nil
        eshell-error-if-no-glob nil
        eshell-scroll-to-bottom-on-input nil
        eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-cmpl-cycle-completions nil
        ;; auto truncate after 12k lines
        eshell-buffer-maximum-lines 12000
        ;; history size
        eshell-history-size 500
        ;; buffer shorthand -> echo foo > #'buffer
        eshell-buffer-shorthand t
        ;; my prompt is easy enough to see
        eshell-highlight-prompt nil
        ;; treat 'echo' like shell echo
        eshell-plain-echo-behavior t
        ;; add -lh to the `ls' flags
        ;; eshell-ls-initial-args "-lh"
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)

  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1))

(use-package eshell-prompt-extras
  :after esh-opt
  :defines eshell-highlight-prompt
  :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda
        eshell-prompt-function 'epe-theme-dakrone
        ;; See eshell-prompt-function below
        ;; eshell-prompt-regexp "^[^#$\n]* [#$] "
        epe-git-dirty-char " Ϟ"
        )
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  ;; epe-git-dirty-char "*"
  )

(defun eshell/truncate-eshell-buffers ()
  "Truncates all eshell buffers"
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
        (eshell-truncate-buffer)))))

;; After being idle for 50 seconds, truncate all the eshell-buffers if
;; needed. If this needs to be canceled,
;; you can run `(cancel-timer sej/eshell-truncate-timer)'
(setq sej/eshell-truncate-timer
      (run-with-idle-timer 50 t #'eshell/truncate-eshell-buffers))

(use-package esh-autosuggest
  :defines ivy-display-functions-alist
  :preface
  (defun setup-eshell-ivy-completion ()
    (setq-local ivy-display-functions-alist
                (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                      ivy-display-functions-alist)))
  :bind (:map eshell-mode-map
              ([remap eshell-pcomplete] . completion-at-point))
  :hook ((eshell-mode . esh-autosuggest-mode)
         (eshell-mode . setup-eshell-ivy-completion)))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

(defun eshell/emacs (&rest args)
  "Open a file (ARGS) in Emacs.  Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defalias 'eshell/e 'eshell/emacs)

(defun eshell/ec (&rest args)
  "Compile a file (ARGS) in Emacs.  Use `compile' to do background make."
  (if (eshell-interactive-output-p)
      (let ((compilation-process-setup-function
             (list 'lambda nil
                   (list 'setq 'process-environment
                         (list 'quote (eshell-copy-environment))))))
        (compile (eshell-flatten-and-stringify args))
        (pop-to-buffer compilation-last-buffer))
    (throw 'eshell-replace-command
           (let ((l (eshell-stringify-list (eshell-flatten-list args))))
             (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)

(defun eshell-view-file (file)
  "View FILE.  A version of `view-file' which properly rets the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((buffer (find-file-noselect file)))
    (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file (ARGS).  \"less +42 foo\" will go to line 42 in the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell-view-file file)
          (forward-line line))
      (eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

(defun eshell/d (&rest args)
  (dired (pop args) "."))

(defun eshell/magit ()
  "Function to open magit-status for the current directory."
  (interactive)
  (magit-status default-directory)
  nil)

(use-package shell
  :ensure nil
  :hook ((shell-mode . n-shell-mode-hook)
         (shell-mode . ansi-color-for-comint-mode-on)

         (comint-output-filter-functions . comint-strip-ctrl-m)
         (comint-output-filter-functions . comint-truncate-buffer))
  :bind  (:map sej-mode-map
               ("H-s" . shell)
               ("C-c s S" . shell))
  :config
  (defun n-shell-simple-send (proc command)
    "Various PROC COMMANDs pre-processing before sending to shell."
    (cond
     ;; Checking for clear command and execute it.
     ((string-match "^[ \t]*clear[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer))
     ;; Checking for man command and execute it.
     ((string-match "^[ \t]*man[ \t]*" command)
      (comint-send-string proc "\n")
      (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
      (setq command (replace-regexp-in-string "[ \t]+$" "" command))
      ;;(message (format "command %s command" command))
      (funcall 'man command))
     ;; Send other commands to the default handler.
     (t (comint-simple-send proc command))))

  (defun n-shell-mode-hook ()
    "Shell mode customizations."
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input)
    (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
    (setq comint-input-sender 'n-shell-simple-send))

  (setq system-uses-terminfo nil)       ; don't use system term info

  (setq comint-scroll-to-bottom-on-input t ;; always insert at the bottom
        ;; always add output at the bottom
        comint-scroll-to-bottom-on-output nil
        ;; scroll to show max possible output
        comint-scroll-show-maximum-output t
        ;; no duplicates in command history
        comint-input-ignoredups t
        ;; insert space/slash after file completion
        comint-completion-addsuffix t
        ;; if this is t, it breaks shell-command
        comint-prompt-read-only nil))

(use-package shell-pop
  :bind ("C-c s p" . shell-pop)
  :init (let ((val
               (if sys/win32p
                   '("eshell" "*eshell*" (lambda () (eshell)))
                 '("ansi-term" "*ansi-term*"
                   (lambda () (ansi-term shell-pop-term-shell))))))
          (setq shell-pop-shell-type val)))

(defun sej/shell-kill-buffer-sentinel (process event)
  "Function to kill shell buffer upon (PROCESS EVENT)."
  (when (memq (process-status process) '(exit signal))
    (kill-buffer)))

(defun sej/kill-process-buffer-on-exit ()
  "Function to kill buffer on exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'sej/shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'sej/kill-process-buffer-on-exit))

(use-package with-editor
  :hook ((shell-mode . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))

(use-package keychain-environment
  :hook (sej/after-init . keychain-refresh-environment))
