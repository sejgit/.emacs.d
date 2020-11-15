;;; init.el --- SeJ Emacs configurations. -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d
;; Version: 1.0
;; Keywords: .emacs.d sejgit

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;;; Commentary:
;;
;; SeJ Emacs configurations.
;;
;; My attempt at an ORG tangled init file.
;; then back again to an outline / outshine / pretty-outlines file.


;;; Changelog
;;
;; - <2019-04-28> Merge from old .emacs.d
;; - <2019-10-20> remove helm stuff; remove most messages
;; - <2019-10-22> start to tangle in init-org.org
;; - <2019-11-27> remove untangled files
;; - <2019-10-29 Tue> Init SeJ
;; - <2019-11-27 Wed> tangled done now working on efficiency
;; - <2019-12-05 Thu> testing with clean install
;; - <2019-12-23 Mon> add links to all packages
;; - <2020-01-04 Sat> linux changes
;; - <2020-01-31 Fri> orgmode pretty changes
;; - <2020-02-21 Fri> package -> straight
;; - <2020-05-17 Sun> try outline/outshine/pretty-outlines
;; - <2020-07-26 Sun> clean-up init files final move from org tangled
;; - <2020-09-21 Mon> small mods
;; - <2020-09-22 Tue> move to helm

;;; Code:
(message "Emacs start")

;;; initialize environment
;;;;; system constants
;; - section for global constants

;; only turned on when needed
;;(setq debug-on-error t)
;;(setq debug-on-event t)
;;(setq debug-on-message "Problems while trying to load feature ‘org-man’")


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
  ( >= emacs-major-version 26 )
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")


;;;;; customization variables set
;; - set-up Emacs customizations choices which are then modified by custom.el

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

(defcustom sej-dashboard t
  "Use dashboard at startup or not. If Non-nil, use dashboard, otherwise will restore previous session."
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

(defcustom sej-org-directory "~/Documents/orgtodo"
  "Set org directory."
  :type 'string)

(defcustom sej-project-org-capture-text "Project"
  "Text for the Label for the Org Capture Project journal."
  :type 'string)

(defcustom sej-project-org-capture-file "~/Documents/orgtodo/journal.org"
  "Filename for the Org Capture Project Journal."
  :type 'string)

(defcustom sej-latex-directory "/Library/TeX/texbin"
  "Directory for Latex."
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


;;;;; general settings
(when (not emacs/>=26p)
  (error "This requires Emacs 26 and above")
  )

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

(setq-default locate-command "which")

;; The EMACS environment variable set to the binary path of emacs.
(setenv "EMACS"
        (file-truename (expand-file-name
                        invocation-name invocation-directory)))

;; set startup directory
(setq default-directory "~/")


;;;;; Straight package manager set-up
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;;;; Use-Package set-up
;; - https://github.com/jwiegley/use-package
;; - https://github.com/emacsmirror/diminish
;; - https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; - https://github.com/jwiegley/use-package#use-package-ensure-system-package

(straight-use-package 'use-package)

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
(use-package bind-key
  :bind ("H-d" . describe-personal-keybindings))

;; Auto installing OS system packages
;; ensure-system-package keyword to ensure system binaries exist alongside your package
(use-package use-package-ensure-system-package)
(use-package helm-system-packages)


;;;;; system settings
;; - Set environment variables based on current system
;;;;;; OSX & Linux
;; - [[https://github.com/purcell/exec-path-from-shell][exec-path-from-shell]]
;; - set-up exec-path and hook for server-start
(when (or sys/macp sys/linuxp)
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell t)
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize))
  (add-hook 'emacs-startup-hook 'sej/server-mode)

  (require 'warnings)
  (customize-save-variable 'warning-suppress-types (quote ((server))))

  (defun sej/server-mode ()
    "Start server-mode without errors"
    (interactive)
    (with-demoted-errors
        (message "Server exists -- not starting new one.")
      (server-mode)
      )
    )
  )


;;;;;; Windows
;; - [[https://github.com/xahlee/xahk-mode.el][xahlee xahk-mode]]
;; (when sys/win32p
;;   (setenv "PATH"
;;           (mapconcat
;;            #'identity exec-path path-separator))
;;   ;; set exec-path for latex installation
;;   (setq exec-path (append (list sej-latex-directory
;;                                 "c:/msys64/mingw64/bin"
;;                                 "/mingw64/bin/") exec-path))
;;   ;; load AutoHotkey mode
;;   (use-package xahk-mode
;;     :straight (xahk-mode.el :type git :host github :repo "xahlee/xahk-mode.el") ))


;;; general keybindings
;;;; modifiers
;;;;; OSX Apple keyboard
;; - caps lock is control (through karabiner)
;; Fn key do Hyper
;; LControl key do RControl (karabiner) which is Super (emacs)
;; left opt/alt key do emacs Alt modifier
;; right opt/alt key do regular alt key
;; left and right command(apple) key do Meta
;; spacebar acts as super key with other key
;; karabiner.json backup files in dotfiles under .config directory
;; - https://github.com/pqrs-org/Karabiner-Elements
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


;;;;; Windows keyboard
;; - CapsLock::LControl through AutoHotkeys
;; scroll lock do hyper (tab to scroll lock using AutoHotkeys)
;; Left control key do super (LControl::Appskey using AutoHotkeys)
;; Left Windows left alone due to win10 taking many keys
;; LAlt::Meta
;; RAlt::Alt modifier (RAlt::NumLock using Autohotkeys) **only works as tap & release
;; Rwin is Alt (not used in current laptop)
;; NOTE: only negative of this set-up is RAlt as numlock -> Alt is awkward push & release
;; - https://www.autohotkey.com/
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


;;;;; Linux keyboard
;; - nothing set at this moment
(cond
 (sys/linuxp ; linux
  (progn
    (message "Linux")
    ;; load-dir init.d
    )))


;;;; keybindings global
;;;;;  shorthand for interactive lambdas
(defmacro λ (&rest body)
  "Shorthand for interactive lambdas (BODY)."
  `(lambda ()
     (interactive)
     ,@body))


;;;;;  transpose lines/words/sexps/params global
;; - Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)


;;;;;  special character definitions
;; - Neat bindings for C-x 8 ; put some Alt bindins there for fun as well
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


;;;; keybindings sej-mode-map
;;;;; sej-mode-map set-up
;; - Below taken from stackexchange (Emacs)
;; Main use is to have my key bindings have the highest priority
;; - https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el
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


;;;;; general sej-mode-map bindings
(define-key global-map (kbd "C-c .") 'org-time-stamp)
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


;;; general functions / packages
;;;;; sej/save-macro
;; - save last macro to init file
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


;;;;; sej/exec
;; - executable functions from ohai and modified for my uses
;; - not key defined
(defun sej/exec (command)
  "Run a shell COMMAND and return its output as a string, whitespace trimmed."
  (interactive)
  (s-trim (shell-command-to-string command)))

(defun sej/exec-with-rc (command &rest args)
  "Run a shell COMMAND ARGS and return a list containing two values: its return code and its whitespace trimmed output."
  (interactive)
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun sej/is-exec (command)
  "Return non-nil if COMMAND is an executable on the system search path."
  (interactive)
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun sej/resolve-exec (command)
  "If COMMAND is an executable on the system search path.
Return its absolute path.  Otherwise, return nil."
  (interactive)
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun sej/exec-if-exec (command args)
  "If COMMAND satisfies `sej/is-exec', run it with ARGS and return its output as per `sej/exec'. Otherwise, return nil."
  (interactive)
  (when (sej/is-exec command) (sej/exec (s-concat command " " args))))


;;;;; list-environment
;; - environment variables tabulated
;; - process environment editor
;; - https://github.com/dgtized/list-environment.el
(use-package list-environment
  :commands list-environment)


;;;;; esup
;; - Emacs startup profiler
;; - https://github.com/jschaf/esup
(use-package esup
  :init
  (autoload 'esup "esup" "Emacs Start Up Profiler." nil))


;;;; help
;;;;; which-key
;; - minibuffer keybinding prompts
;; - https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key")
  :diminish which-key-mode
  :hook (emacs-startup . which-key-mode)
  :commands which-key-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-h RET" . which-key-show-major-mode)
              ("C-h C-k" . which-key-show-top-level))
  :config
  (which-key-setup-minibuffer))


;;;;; helpful
;; - helpful is an improved help-fns & help-fns+
;; - https://github.com/Wilfred/helpful
(use-package helpful
  :straight (helpful :type git :host github :repo "Wilfred/helpful")
  :bind ( ("C-h C-d" . helpful-at-point)
          ("C-h c" . helpful-command)
          ("C-h C" . helpful-command)
          ("C-h k" . helpful-key)
          ("C-h M" . helpful-macro))  )


;;;;; helm-descbinds
;; - Discover key bindings narrowing with helm
;; - [[https://github.com/emacs-helm/helm-descbinds][helm-descbinds]]
(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))


;;;;; helm-describe-modes
;; - Discover key bindings and their meaning for the current Emacs major mode
;; - [[https://github.com/emacs-helm/helm-describe-modes][helm-describe-modes]]
(use-package helm-describe-modes
  :bind (([remap describe-mode] . helm-describe-modes)))


;;;; update
;;;;; sej/update-config
;; - helper function to pull latest config from git tracked dir
;; - not bound
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


;;;;; sej/update-dotfiles
;; - helper function to pull latest dotfiles config from git tracked dir
;; - not bound
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


;;;;; sej/update-all
;; - helper function to pull latest files from git tracked dir
;; - not bound
(defun sej/update-all()
  "Update dotfiles, org files, Emacs confgiurations and packages, ."
  (interactive)
  (sej/update-config)
  (sej/update-dotfiles))


;;;; network proxy
;;;;; sej/proxy-http-show
;;- what are the current proxy settings
;;- based on the Emacs settings variables
;;- not bound
(defun sej/proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" sej-proxy)
    (message "No proxy")))


;;;;; sej/proxy-http-enable
;; - enable proxy settings
;; - based on Emacs custom settings
;; - not bound
(defun sej/proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,sej-proxy)
                             ("https" . ,sej-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (setq url-http-proxy-basic-auth-storage sej-url-http-proxy-basic-auth-storage)
  (sej/proxy-http-show))


;;;;; sej/proxy-http-disable
;; - disable proxy settings
;; - based on Emacs custom settings
;; - not bound
(defun sej/proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (setq url-http-proxy-basic-auth-storage nil)
  (sej/proxy-http-show))


;;;;; sej/proxy-http-toggle
;; - toggle proxy settings
;; - based on Emacs custom settings
;; - not bound
(defun sej/proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (sej/proxy-http-disable)
    (sej/proxy-http-enable)))


;;;;; sej/proxy-socks-enable
;; - enable socks proxy settings
;; - based on Emacs custom settings
;; - not bound
(defvar socks-noproxy)
(defvar socks-server)
(defun sej/proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 1086 5))
  (message "Enable socks proxy."))


;;;;; sej/proxy-socks-disable
;; - disable socks proxy settings
;; - based on Emacs custom settings
;; - not bound
(defun sej/proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (message "Disable socks proxy."))


;;; user interface
;;;; themes
;;;;; suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)


;;;;; modus themes
(use-package modus-themes
  :straight (modus-themes :type git :host github :repo "protesilaos/modus-themes")
  :hook (after-init . (lambda() (load-theme 'modus-vivendi)))
  :config
  (dolist (theme '("operandi" "vivendi"))
    (contrib/format-sexp
     (defun prot/modus-%1$s ()
       (setq modus-%1$s-theme-bold-constructs nil
             modus-%1$s-theme-slanted-constructs t
             modus-%1$s-theme-faint-syntax nil
             modus-%1$s-theme-mode-line '3d ; {nil,'3d,'moody}
             modus-%1$s-theme-fringes 'intense ; {nil,'subtle,'intense}
             modus-%1$s-theme-faint-syntax t
             modus-%1$s-theme-intense-hl-line nil
             modus-%1$s-theme-intense-paren-match t
             modus-%1$s-theme-links 'faint-neutral-underline ; {nil,'faint,'neutral-underline,'faint-neutral-underline,'no-underline}
             modus-%1$s-theme-no-mixed-fonts nil
             modus-%1$s-theme-prompts 'intense ; {nil,'subtle,'intense}
             modus-%1$s-theme-completions nil ; {nil,'moderate,'opinionated}
             modus-%1$s-theme-diffs 'fg-only ; {nil,'desaturated,'fg-only}
             modus-%1$s-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
             modus-%1$s-theme-headings  ; Read the manual for this one
             '((1 . t)
               (2 . no-bold)
               (t . rainbow-no-bold))
             modus-%1$s-theme-variable-pitch-headings t
             modus-%1$s-theme-scale-headings t
             modus-%1$s-theme-scale-1 1.1
             modus-%1$s-theme-scale-2 1.15
             modus-%1$s-theme-scale-3 1.21
             modus-%1$s-theme-scale-4 1.27
             modus-%1$s-theme-scale-5 1.33)
       (load-theme 'modus-%1$s t) )
     theme))
 )


;;;;; font
(when sys/macp
  (set-face-attribute 'default nil :font "SF Mono-13")
  (set-fontset-font t 'unicode "Apple Symbols" nil 'prepend)
  )


;;;; frames
;;;;; general frames key-bindings
(define-key sej-mode-map (kbd "s-4") 'dired-other-frame)
(define-key sej-mode-map (kbd "s-5") 'make-frame-command)
(define-key sej-mode-map (kbd "s-6") 'delete-other-frames)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "s-w") 'delete-frame)
(define-key sej-mode-map (kbd "C-x w") 'delete-frame)


;;;;; general frame settings
(setq frame-title-format '("SeJ Emacs - %b"))
(setq icon-title-format frame-title-format)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Resize frame to left half after startup
(if (display-graphic-p)
    (add-hook 'emacs-startup-hook 'sej/frame-resize-l) )


;;;;; mac specific frame settings
;; - matching dark/light modes and for hiding
;; - https://github.com/purcell/ns-auto-titlebar
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


;;;;; sej/frame-resize-full
;; - generic way to move and resize frame to full width of screen
;; - set frame full height and full wide and position at screen left
(define-key sej-mode-map (kbd "C-c s <up>") 'sej/frame-resize-full)
(define-key sej-mode-map (kbd "H-C-j") 'sej/frame-resize-full)

(defun sej/frame-resize-full ()
  "Set frame full height and 1/2 wide, position at screen left."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (- (display-pixel-width) (if sys/macp (eval 13) (eval 25)))
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )


;;;;; sej/frame-resize-l
;; - generic way to move and resize frame to left half of screen
;; - set frame full height and 1/2 wide and position at screen left
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


;;;;; sej/frame-resize-l2
;; - generic way to move and resize frame to left half of screen in extended monitor
;; - set frame full height and 1/2 wide and position at screen left
;; of screen in extended monitor display
;; - assumes monitors are same resolution
(define-key sej-mode-map (kbd "C-c s <S-left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "H-C-S-h") 'sej/frame-resize-l2)

(defun sej/frame-resize-l2 ()
  "Set frame full height and 1/2 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )


;;;;; sej/frame-resize-r
;; - generic way to move and resize frame to right half of screen
;; - set frame full height and 1/2 wide and position at screen right
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


;;;;; sej/frame-resize-r2
;; - generic way to move and resize frame to right half of screen in extended monitor
;; - set frame full height and 1/2 wide and position at screen right
;; of left hand screen in extended monitor display
;; - assumes monitors are same resolution
(define-key sej-mode-map (kbd "C-c s <S-right>") 'sej/frame-resize-r2)
(define-key sej-mode-map (kbd "H-C-S-l") 'sej/frame-resize-r2)

(defun sej/frame-resize-r2 ()
  "Set frame full height and 1/2 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )


;;;;; fullscreen
;; - To address blank screen issue with child-frame in fullscreen
(when sys/mac-x-p
  (setq ns-use-native-fullscreen nil))
(bind-keys ("H-C-f" . toggle-frame-fullscreen)
           ("C-c s F" . toggle-frame-fullscreen))


;;;; buffers
;;;;; buffer key-bindngs
(define-key sej-mode-map (kbd "s-s") 'save-buffer)
(define-key sej-mode-map (kbd "s-q") 'save-buffers-kill-emacs)

(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)

(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)
(define-key sej-mode-map (kbd "s-r") 'revert-buffer)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "C-x k") 'kill-this-buffer)


;;;;; bs.el cycle buffer settings
(define-key sej-mode-map (kbd "s-n") 'bs-cycle-next) ; buffer cycle next
(define-key sej-mode-map (kbd "s-p") 'bs-cycle-previous)

(setq-default bs-default-configuration "all-intern-last")


;;;;; sej/dos2unix
;; - convert the current buffer to UNIX file format
;; - not bound
(defun sej/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))


;;;;; sej/unix2dos
;; - convert the current buffer to DOS file format
;; - not bound
(defun sej/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


;;;;; sej/save-buffer-as-utf8
;; - revert a buffer with coding-system and save as utf-8
(defun sej/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))


;;;;; sej/browse-homepage
;; - Browse my github homepage
;; - bound to C-c s h
(defun sej/browse-homepage ()
  "Browse the Github page of SeJ Emacs."
  (interactive)
  (browse-url sejgit-homepage))

(define-key sej-mode-map (kbd "C-c s h") 'sej/browse-homepage)


;;;;; sej/quit-and-kill-auxiliary-windows
(defun sej/quit-and-kill-auxiliary-windows ()
  "Kill buffer and its window on quitting."
  (local-set-key (kbd "q") 'kill-buffer-and-window))
(add-hook 'special-mode 'sej/quit-and-kill-auxiliary-windows)
(add-hook 'compilation-mode-hook 'sej/quit-and-kill-auxiliary-windows)


;;;;; scratch buffer set-up
;; - initial message
;; - bury don't kill scratch
(setq initial-scratch-message "")
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the *scratch* buffer, but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))


;;;;; sej/create-scratch-buffer
;; - as name suggests
;; - defined as C-c b
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
(define-key sej-mode-map (kbd "C-c S") 'sej/create-scratch-buffer)
(define-key sej-mode-map (kbd "C-c s S") 'sej/create-scratch-buffer)


;;;;; persistent-scratch
;; - keep the scratch buffer from session to session
;; - https://github.com/Fanael/persistent-scratch
(use-package persistent-scratch
  :straight (persistent-scratch :type git :host github :repo "Fanael/persistent-scratch")
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
  :hook (emacs-startup . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
              ("C-x C-s" . my-save-buffer)))


;;;; windows
;;;;; window key-bindings
;; super versions of C-x window bindings
(define-key sej-mode-map (kbd "s-0") 'delete-window)
(define-key sej-mode-map (kbd "s-1") 'delete-other-windows)
(define-key sej-mode-map (kbd "s-2") 'split-window-vertically)
(define-key sej-mode-map (kbd "s-3") 'split-window-right)

(define-key sej-mode-map (kbd "s-7") (lambda () (interactive)
                                       (save-excursion
                                         (other-window 1)
                                         (quit-window))))

;; wind move to multifram window
(define-key sej-mode-map (kbd "M-'") 'next-multiframe-window)

;; movement complementary to windmove / windswap
(define-key sej-mode-map (kbd "H-h") 'left-char)
(define-key sej-mode-map (kbd "H-j") 'up-char)
(define-key sej-mode-map (kbd "H-k") 'down-char)
(define-key sej-mode-map (kbd "H-l") 'right-char)

;;scroll window up/down by one line
(define-key sej-mode-map (kbd "A-n") (lambda () (interactive) (scroll-up 1)))
(define-key sej-mode-map (kbd "A-p") (lambda () (interactive) (scroll-down 1)))


;;;;; windmove
;; built-in window movement
;; down
(define-key sej-mode-map (kbd "A-<down>") 'windmove-down)
(define-key sej-mode-map (kbd "A-k") 'windmove-down)
(define-key sej-mode-map (kbd "S-A-<down>") 'windmove-delete-down)
(define-key sej-mode-map (kbd "A-K") 'windmove-delete-down)
;; up
(define-key sej-mode-map (kbd "A-<up>") 'windmove-up)
(define-key sej-mode-map (kbd "A-j") 'windmove-up)
(define-key sej-mode-map (kbd "S-A-<up>") 'windmove-delete-up)
(define-key sej-mode-map (kbd "A-J") 'windmove-delete-up)
;; left
(define-key sej-mode-map (kbd "A-<left>") 'windmove-left)
(define-key sej-mode-map (kbd "A-h") 'windmove-left)
(define-key sej-mode-map (kbd "S-A-<left>") 'windmove-delete-left)
(define-key sej-mode-map (kbd "A-H") 'windmove-delete-left)
;; right
(define-key sej-mode-map (kbd "A-<right>") 'windmove-right)
(define-key sej-mode-map (kbd "A-l") 'windmove-right)
(define-key sej-mode-map (kbd "S-A-<right>") 'windmove-delete-right)
(define-key sej-mode-map (kbd "A-L") 'windmove-delete-right)


;;;;; windswap
;; complement to windmove to swap windows
;; [[https://github.com/purcell/windswap][purcell/windswap]]
(use-package windswap
  :straight (windswap :type git :host github :repo "purcell/windswap")
  :bind ((:map sej-mode-map
               ("A-s-h" . windswap-left)
               ("A-s-l" . windswap-right)
               ("A-s-j" . windswap-up)
               ("A-s-k" . windswap-down)))  )


;;;;; mouse & smooth scroll
;; - Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)


;;;;; ace-window
;; - quickly selecting a window to switch to
;; - C-u prefex to move window
;; - C-u C-u prefex to delete window
;; - https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight (ace-window :type git :host github :repo "abo-abo/ace-window")
  :bind (([remap other-window] . ace-window)
         ("M-o" . ace-window))
  :custom-face
  (aw-leading-char-face ((t (:inherit error :bold t :height 1.1))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;;;;; winner
;; - Restore old window configurations
;; - [[https://www.emacswiki.org/emacs/WinnerMode][winner-mode]]
(use-package winner
  :straight (winner :type built-in)
  :commands (winner-undo winner-redo)
  :bind ( ("C-c w <left>" . winner-undo)
          ("C-c w <right>" . winner-redo))
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


;;;;; golden-ratio
;; - keep windows balanced with in-focus window larger
;; - https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :hook (emacs-startup . golden-ratio-mode)
  :diminish golden-ratio-mode
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window)
  (setq golden-ratio-auto-scale t))


;;;; mode-line
;;;;; doom-modeline
;; - A fancy and fast mode-line inspired by minimalism design
;; - https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :hook (emacs-startup . doom-modeline-mode)
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


;;;;; hide-mode-line
;; - A minor mode that hides (or masks) the mode-line in your current buffer
;; - https://github.com/hlissner/emacs-hide-mode-line
(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))


;;;;; all-the-icons
;; - NOTE: Must run `M-x all-the-icons-install-fonts' manually on Windows
;; - https://github.com/domtronn/all-the-icons.el
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


;;;; line numbers
;;;;; display-line-numbers
;; - only in prog modes
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/display-line-numbers.el
(use-package display-line-numbers
  :straight (display-lne-numbers :type built-in)
  :hook (prog-mode . display-line-numbers-mode))


;;;;; goto-line-preview
;; - Preview line when executing goto-line command.
;; - M-g g
;; - https://github.com/jcs-elpa/goto-line-preview
(use-package goto-line-preview
  :hook ((goto-line-preview-before-hook . (lambda() (display-line-numbers-mode 1)))
         (goto-line-preview-after-hook . (lambda() (display-line-numbers-mode -1))))
  :bind ([remap goto-line] . goto-line-preview))


;;; text manipulation
;;;; text manipulation settings
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

;; automatically save place in files so return to same place in next session
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/saveplace.el
(save-place-mode 1)

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
  "When popping the mark, continue popping until we move the cursor."
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


;;;; multi-edit
;;;;; multiple cursors
;; - Multiple cursors
;; - https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (( ("C-S-c C-S-c"   . mc/edit-lines)
           ("C->"           . mc/mark-next-like-this)
           ("C-<"           . mc/mark-previous-like-this)
           ("C-c C-<"       . mc/mark-all-like-this)
           ("C-M->"         . mc/skip-to-next-like-this)
           ("C-M-<"         . mc/skip-to-previous-like-this)
           ("s-<mouse-1>"   . mc/add-cursor-on-click))
         (:map mc/keymap
               ("C-|" . mc/vertical-align-with-space))))


;;;; search
;;;;; helm & imenu (using helm-semantic-or-imenu)
;; - Framework for mode-specific buffer indexes
;; - https://www.emacswiki.org/emacs/ImenuMode
;; - https://emacs-helm.github.io/helm/
(use-package helm
  :diminish helm-mode
  :hook ((emacs-startup . helm-mode)
         (org-mode . imenu-add-menubar-index)
         (prog-mode . imenu-add-menubar-index))
  :init (setq org-imenu-depth 6)
  :bind (("M-y" . helm-show-kill-ring)
         ("M-i" . helm-swoop)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-h g" . helm-google-suggest)
         ("C-c ," . helm-calcul-expression)
         ("C-c g" . helm-gid)
         ("C-c i" . helm-imenu)
         ("C-c I" . helm-imenu-in-all-buffers)
         ("C-h C-s" . helm-occur)
         ("C-h f" . helm-find)
         ("C-h i" . helm-info-at-point)
         ("C-h r" . helm-info-emacs)
         ("C-h s" . helm-swoop)
         ("C-x C-d" .   helm-browse-project)
	 ("M-g a" .     helm-do-grep-ag)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-c C-f" . helm-recentf)
         ("C-c SPC" . helm-all-mark-rings)
         ([remap jump-to-register] . helm-register)
         ([remap list-buffers]     . helm-buffers-list)
         ([remap dabbrev-expand]   . helm-dabbrev)
         ([remap find-tag]         . helm-etags-select)
         ([remap xref-find-definitions] . helm-etags-select)
         (:map helm-map
               ("<tab" . helm-execute-persistent-action) ; rebind tab to run persistent action
               ("C-i" . helm-execute-persistent-action) ; make TAB work in terminal
               ("C-z" . helm-select-action) ; list actions using C-z
               )
         (:map sej-mode-map
               ("C-." . helm-semantic-or-imenu) ))
  :config
  (require 'helm-config)
  (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-candidate-number-limit 100
        helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things quickly.
        helm-yas-display-key-on-candidate t
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t
        helm-split-window-in-side-p  t ; open helm buffer inside current window
        helm-move-to-line-cycle-in-source  t ; move to end or beginning of source.
        helm-ff-search-library-in-sexp  t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount  8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf  t
        helm-echo-input-in-header-line  t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)

  (helm-autoresize-mode 1))


;;;;; helm-swoop
;; - i-search enhancement
;; - [[https://github.com/emacsorphanage/helm-swoop][helm-swoop]]
(use-package helm-swoop
  :after helm
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         (:map isearch-mode-map
               ("M-i" . helm-swoop-from-isearch))
         (:map helm-swoop-map
               ("M-i" . helm-multi-swoop-all-from-helm-swoop)
               ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop))  )
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)

  ;; ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)

  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t)

  ;; If you prefer fuzzy matching
  (setq helm-swoop-use-fuzzy-match t)
)


;;;;; ag
;; - searching with the silver searcher
;; - https://github.com/Wilfred/ag.el
(when (executable-find "ag")
  (use-package ag
    :commands ag
    :config
    (setq ag-executable (executable-find "ag")))
  (setq-default ag-highlight-search t)


;;;;; helm-ag
  ;; - silver searcher helm narrowing
  ;; - [[https://github.com/emacsorphanage/helm-ag][helm-ag]]
  (use-package helm-ag
    :bind(:map sej-mode-map
               ("s-a" . helm-projectile-ag)
               ("H-a" . helm-ag) ))  )


;;;;; re-builder
;; - set built in regex helper to string format
;; - https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package re-builder
  :straight (re-builder :type built-in)
  :config (setq reb-re-syntax 'string))


;;;; indentation
;;;;; indentation settings
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


;;;;; dtrt-indent
;; - automatically set the right indent for other people's files
;; - https://github.com/jscheid/dtrt-indent
(use-package dtrt-indent
  :hook (emacs-startup . dtrt-indent-mode)
  :diminish)


;;;;; sej/indent-buffer
;; - bound to C-c <tab>
(defun sej/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(define-key sej-mode-map (kbd "C-c s <tab>") 'sej/indent-buffer)


;;;; history packages
;;;;; undo-fu
;; - Simple, stable linear undo with redo for Emacs.
;; - https://gitlab.com/ideasman42/emacs-undo-fu
(use-package undo-fu
  :diminish
  :bind ( ("C-z" . undo-fu-only-undo)
          ("C-S-z" . undo-fu-only-redo))
  :config (setq undo-fu-allow-undo-in-region t))


;;;;; undo-fu-session
;; - Save & recover undo steps between Emacs sessions.
;; - https://gitlab.com/ideasman42/emacs-undo-fu-session
(use-package undo-fu-session
  :after undo-fu
  :config (global-undo-fu-session-mode))


;;;;; recentf
;; - recent file history list settings
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el
(use-package recentf
  :straight (recentf :type built-in)
  :hook (emacs-startup . recentf-mode)
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


;;;;; savehist
;; - recent buffer history settings
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el
(use-package savehist
  :straight (savehist :type built-in)
  :hook (emacs-startup . savehist-mode)
  :config
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300))


;;;; movement
;;;;; crux
;; - a Colection of Rediculously Useful eXtensions
;; - smart moving to beginning of line or to beginning of text on line
;; - https://github.com/bbatsov/crux
(use-package crux
  :bind ( ("C-c o" . crux-open-with)
          ("C-k" . crux-smart-kill-line)
          ("C-S-RET" . crux-smart-open-line-above)
          ([(shift return)] . crux-smart-open-line)
          ("C-c n" . crux-cleanup-buffer-or-region)
          ("C-c u" . crux-view-url)
          ("s-k" . crux-duplicate-current-line-or-region)
          ("C-c C-k" . crux-duplicate-current-line-or-region)
          ("C-c M-k" . crux-duplicate-and-comment-current-line-or-region)
          ([remap kill-whole-line] . crux-kill-whole-line)
          ("C-<backspace>" . crux-kill-line-backwards))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (crux-reopen-as-root-mode))


;;;;; mwim
;; - better than crux for C-e mwim-end
;; - will cycle between end of code and end-of-code plus comments
;; - https://github.com/alezost/mwim.el
(use-package mwim
  :bind ( ("C-a" . mwim-beginning)
          ("C-e" . mwim-end))) ; better than crux


;;;;; avy
;; - Jump to things in Emacs tree-style
;; - https://github.com/abo-abo/avy
(use-package avy
  :bind ( ("C-'" . avy-goto-char)
          ("H-'" . avy-goto-char-2)
          ("M-g l" . avy-goto-line)
          ("H-l" . avy-goto-line)
          ("M-g w" . avy-goto-word-1)
          ("H-w" . avy-goto-word-1))
  :config (setq avy-background t))


;;;;; goto-chg
;; - goto the last changes made in buffer
;; - https://github.com/emacs-evil/goto-chg
(use-package goto-chg
  :bind ( ("H-." . goto-last-change)
          ("H-," . goto-last-change-reverse)) )


;;;;; beginend
;; - smart moves redefining M-< and M-> for some modes
;; - https://github.com/DamienCassou/beginend
(use-package beginend               ; smart M-< & M->
  :hook (emacs-startup . beginend-global-mode))


;;;;; subword
;; - Handling capitalized subwords in a nomenclature
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/subword.el
(use-package subword
  :straight (subword :type built-in)
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode))
  :config
  ;; this makes forward-word & backward-word understand snake & camel case
  (setq c-subword-mode t)
  (global-subword-mode t))


;;;;; string inflection
;; - underscore -> UPCASE -> Camelcase conversion
;; - https://github.com/akicho8/string-inflection
(use-package string-inflection
  :bind ( ("M-u" . string-inflection-all-cycle)))


;;;;; sej/push-mark-no-activate
;; - defined in sej-mode-map as C-S-<SPC>
(defun sej/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; push and jump to mark functions
(define-key sej-mode-map (kbd "C-S-<SPC>") 'sej/push-mark-no-activate)


;;;; regions
;;;;; easy-kill-extras
;; - This package contains extra functions for easy-kill/easy-mark.
;; - Kill & Mark things easily
;; - https://github.com/leoliu/easy-kill
;; - https://github.com/knu/easy-kill-extras.el
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


;;;;; sej/copy-from-osx, sej/copy-to-osx
;; - https://gist.github.com/the-kenny/267162
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


;;;;; avy-zap
;; - Kill text between the point and the character CHAR
;; - https://github.com/cute-jumper/avy-zap
(use-package avy-zap
  :bind ( ("M-z" . avy-zap-to-char-dwim)
          ("M-Z" . avy-zap-up-to-char-dwim)))


;;;;; delsel
;; - Do not delete selection if you insert
;; - https://github.com/typester/emacs/blob/master/lisp/delsel.el
(use-package delsel
  :straight (delsel :type built-in)
  :config (setq-default delete-selection-mode nil))


;;;;; rect
;; - Rectangle
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/rect.el
(use-package rect
  :straight (rect :type built-in))


;;;;; drag-stuff
;; - Drag stuff (lines, words, region, etc...) around
;; - https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :diminish
  :bind ( ("M-<down>" . drag-stuff-down)
          ("H-n" . drag-stuff-down)
          ("M-<up>" . drag-stuff-up)
          ("H-p" . drag-stuff-up))
  :config
  (drag-stuff-global-mode)
  (drag-stuff-define-keys)
  (add-to-list 'drag-stuff-except-modes 'org-mode))


;;;;; smart-region
;; - Smartly select region, rectangle, multi cursors
;; - remaping set-mark-command to smart-region
;; - https://github.com/uk-ar/smart-region
(use-package smart-region
  :bind ([remap set-mark-command] . smart-region) ; C-SPC
  :config (smart-region-on))


;;;;; expand-region
;; - expand selection region larger & smaller
;; - https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind ( ("s-=" . er/expand-region)
          ("s--" . er/contract-region)))


;;;;; smart-hungry-delete
;; - Hungry deletion
;; - https://github.com/hrehfeld/emacs-smart-hungry-delete
(use-package smart-hungry-delete
  :diminish
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :config (smart-hungry-delete-add-default-hooks))


;;;;; whole-line or region
;; - operate on current line if region undefined
;; - [[https://github.com/purcell/whole-line-or-region][purcell/whole-line-or-region]]
(use-package whole-line-or-region
  :hook
  (emacs-startup . whole-line-or-region-global-mode))


;;;; url actions
;;;;; sej/url-insert
;; - from jcs (Irreal) blog to copy url from safari and paste at point
;; - https://irreal.org/blog/?p=2895
(when sys/macp
  (defun sej/url-insert (link)
    "Retrieve URL from current Safari page and prompt for description.
      Insert an Org link at point."
    (interactive "sLink Description: ")
    (let ((result (shell-command-to-string
                   "osascript -e 'tell application \"Safari\" to return URL of document 1'")))
      (insert (format "[[%s][%s]]" (org-trim result) link))))

  (define-key sej-mode-map (kbd "C-H-u") 'sej/url-insert))


;;;;; ace-link
;; - Quickly follow links
;; - https://github.com/abo-abo/ace-link
(use-package ace-link
  :bind (("H-u" . ace-link-addr)
         ("C-c s u" . ace-link-addr)
         :map org-mode-map
         ("H-u" . ace-link-org))
  :config (ace-link-setup-default))


;;;;; org-link-minor-mode
;; enables org-mode style fontification and activation of bracket links
;; [[https://github.com/seanohalpin/org-link-minor-mode][org-link-minor-mode github]]
(use-package org-link-minor-mode
  :straight (org-link-minor-mode :type git :host github :repo "seanohalpin/org-link-minor-mode")
  :hook prog-mode)


;;;;; restclient
;; - Allows query of a restclient with the results left into the buffer
;; - use GET or POST plus http:10.0.1.1/rest/
;; then use C-c to execute (check examples directory for more)
;; - https://github.com/pashky/restclient.el
(use-package restclient)


;;;; highlighting faces fonts
;;;;; hl-line
;; - Highlight the current line
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/hl-line.el
(use-package hl-line
  :straight (hl-line :type built-in)
  :hook (emacs-startup . global-hl-line-mode))


;;;;; symbol-overlay
;; - Highlight symbols and move between them
;; - https://github.com/wolray/symbol-overlay
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


;;;;; dimmer
;; - minor mode that indicates currently active buffer by dimming the faces in others
;; - https://github.com/gonewest818/dimmer.el
(use-package dimmer
  :config
  (setq dimmer-fraction 0.20)
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t))


;;;;; highlight-numbers
;; - hightlight-numbers in a special way
;; - https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))


;;;;; highlight-indent-guides
;; - Highlight indentations
;; - https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :if window-system
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack))


;;;;; rainbow-mode
;; - Colorize color names in buffers
;; - https://github.com/tcrayford/emacs/blob/master/vendor/rainbow-mode.el
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))


;;;;; hl-todo
;; - Highlight TODO and similar keywords in comments and strings
;; - https://github.com/tarsius/hl-todo
(use-package hl-todo
  :custom-face (hl-todo ((t (:box t :inherit))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t o" . hl-todo-occur)
              ("H-t" . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("H-P" . hl-todo-previous)
              ("H-N" . hl-todo-next) )
  :hook ((emacs-startup . global-hl-todo-mode)
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


;;;;; diff-hl
;; - Highlight uncommitted changes
;; - https://github.com/dgutov/diff-hl
(use-package diff-hl
  :defines (diff-hl-margin-symbols-alist desktop-minor-mode-table)
  :commands diff-hl-magit-post-refresh
  :custom-face
  (diff-hl-change ((t (:background "#46D9FF"))))
  (diff-hl-delete ((t (:background "#ff6c6b"))))
  (diff-hl-insert ((t (:background "#98be65"))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((emacs-startup . global-diff-hl-mode)
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


;;;;; volatile-highlights
;; - Highlight some buffer region operations
;; - https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :diminish
  :hook (emacs-startup . volatile-highlights-mode))


;;;;; whitespace
;; - Visualize TAB, (HARD) SPACE, NEWLINE
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/whitespace.el
(use-package whitespace
  :straight (whitespace :type built-in)
  :diminish
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face trailing space-before-tab empty space-after-tab))
  (setq whitespace-line-column fill-column) ;; limit line length
  )


;;;;; pulse
;; - Pulse current line
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/cedet/pulse.el
(use-package pulse
  :straight (pulse :type built-in)
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


;;;;; rainbow-delimiters
;; - rainbow-delimiters-mode - multicoloured brackets
;; - https://github.com/Fanael/rainbow-delimiters
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


;;;;; mic-paren
;; - show parens even off screen
;; - https://github.com/emacsattic/mic-paren/blob/d0410c7d805c9aaf51a1bcefaaef092bed5824c4/mic-paren.el
(use-package mic-paren
  :hook (prog-mode . paren-activate)
  :config
  (setq paren-highlight-offscreen t))


;;;;; outline outshine pretty-outlines
;; - program modes outline much like org-mode
;; - [[https://www.emacswiki.org/emacs/OutlineMinorMode][outline-minor-mode wiki]]
;; - [[https://github.com/alphapapa/outshine][outshine]]
;; - [[https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/display/local/pretty-outlines/pretty-outlines.el][pretty-outlines]]
(use-package outshine
  :bind ("M-S-<return>" . outshine-insert-heading)
  :hook ((prog-mode          . outline-minor-mode)
         (outline-minor-mode . outshine-mode))
  :config
  (setq my-black "#1b1b1e")

  (custom-theme-set-faces
   'user
   `(outline-1 ((t (:height 1.8 :foreground "#c8d8e3"
                            :background ,my-black :weight bold))))
   `(outline-2 ((t (:height 1.5 :foreground "#268bd2"
                            :background ,my-black :weight bold))))
   `(outline-3 ((t (:height 1.2 :foreground "#2aa198"
                            :background ,my-black :weight bold))))
   `(outline-4 ((t (:height 1.05 :foreground "#818e96"
                            :background ,my-black :weight bold)))))
  )


(use-package pretty-outlines
  :straight (.spacemacs.d :type git :host github
                          :repo "ekaschalk/.spacemacs.d"
                          :files ("layers/display/local/pretty-outlines/*.el"))
  :after (outshine)
  :hook ((outline-mode . pretty-outlines-set-display-table)
         (outline-minor-mode . pretty-outlines-set-display-table)
         (emacs-lisp-mode . pretty-outlines-add-bullets)
         (python-mode . pretty-outlines-add-bullets))
  :init
  (if (fboundp 'package-installed-p)
      t
    (defun package-installed-p (dummy) t))
  :config
  (setq pretty-outlines-ellipsis "~")
  (setq pretty-outlines-bullets-bullet-list
        '(#x2022 ))  )


;;; programming
;;;;; lsp
;; - client for Language Server Protocol servers
;; - [[https://github.com/emacs-lsp/lsp-mode][lsp-mode]]
;; bash-mode: npm i -g bash-language-server
;; c++-mode, objc-mode, cudo-mode: ccls uses lsp-mode & see ccls below
;; csharp-mode: supports automatic installation M-x lsp-csharp-update-server
;; cmake: pip3 install cmake-language-server
;; css-mode: npm install -g vscode-css-languageserver-bin
;; go-mode: gopls
;; html-mode: npm install -g vscode-html-languageserver-bin
;; java-mode: self install eclipse JDT language server
;; javascript-mode: npm i -g javascript-typescript-langserver
;; json-mode: npm install -g vscode-html-languageserver-bin
;; perl-mode: cpan Perl::LanguageServer
;; python: pip install ‘python-language-server[all]’
(use-package lsp-mode
  :if (eq sej-lsp 'lsp-mode)
  :hook (((
           bash-mode
           c++-mode
           c-mode
           objc-mode
           cuda-mode
           cmake-mode
           fortran-mode
           go-mode
           html-mode
           haskell-mode
           java-mode
           json-mode
           perl-mode
           python-mode
           ruby-mode
           rust-mode
           sql-mode
           yaml-mode
           ) . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "s-l"))


;;;;; lsp-ui
;; - contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses
;; - [[https://github.com/emacs-lsp/lsp-ui][lsp-ui]]
;; - M-. M-?
(use-package lsp-ui
  :if (eq sej-lsp 'lsp-mode)
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; M-.
         ([remap xref-find-references] . lsp-ui-peek-find-references)) ; M-?
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-ignore-duplicate t))


;;;;; helm-lsp
;; - alternative of the build-in lsp-mode xref-appropos which provides as you type completion
;; - [[https://github.com/emacs-lsp/helm-lsp][helm-lsp]]
;; - TODO keep here or move to helm area?  add a link?
(use-package helm-lsp
  :if (eq sej-lsp 'lsp-mode)
  :after lsp-mode
  :commands helm-lsp-workspace-symbol)


;;;;; dap-mode
;; - (use-package dap-LANGUAGE) to load the dap adapter for your language
;; - [[https://github.com/emacs-lsp/dap-mode][dap-mode]]
(use-package dap-mode
  :if (eq sej-lsp 'lsp-mode)
  :after lsp)


;;;;; eglot
;; - simple client for Language Server Protocol servers
;; - https://github.com/joaotavora/eglot
(use-package eglot
  :if (eq sej-lsp 'eglot)
  :hook ((python-mode c-mode go-mode bash-mode sh-mode javascript-mode java-mode)  . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c h" . eglot-help-at-point)
              ("C-c x" . xref-find-definitions))
  :config
  (setq help-at-pt-display-when-idle t))


;;;;; prog-mode
;; - generalized program mode
;; - Prettify Symbols
;; - e.g. display “lambda” as “λ”
(setq-default prettify-symbols-alist '(("lambda" . ?λ)
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
                                       ("not" . ?¬)
                                       ("=>" . ?⇨)
                                       ("#+BEGIN_SRC" . ?†)
                                       ("#+END_SRC" . ?†)
                                       ("#+begin_src" . ?†)
                                       ("#+end_src" . ?†)
                                       ))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode)


;;;;; format-all
;; - auto-format source code in many languages using the same command for all languages
;; - You will need to install external programs to do the formatting
;; - https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :bind (:map sej-mode-map
              ("C-c s f" . format-all-buffer)
              ("A-f" . format-all-buffer)))


;;;;; tramp
;; - remote editing
;; - https://www.gnu.org/software/tramp/
(use-package tramp
  :straight (tramp :type built-in)
  :defer 5
  :config
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setq tramp-default-method "ssh"
        tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        tramp-adb-program "adb"
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        ;; don't generate backups for remote files opened as root (security hazzard)
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo"))))))))

  (use-package tramp-sh
    :straight (tramp-sh :type built-in)
    :config
    (add-to-list 'tramp-remote-path "/usr/local/sbin")
    (add-to-list 'tramp-remote-path "/opt/java/current/bin")
    (add-to-list 'tramp-remote-path "/opt/gradle/current/bin")
    (add-to-list 'tramp-remote-path "~/bin")))


;;;;; pass
;; - major-mode to manage your password-store (pass) keychain
;; - https://github.com/NicolasPetton/pass
(use-package pass
  :commands pass)


;;;;; indent-guide
;; - show vertical lines to guide indentation
;; - https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :diminish indent-guide-mode)


;;;;; comment-dwim-2
;; - replacement for the Emacs built-in command comment-dwim
;; - https://github.com/remyferre/comment-dwim-2
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ; M-;


;;;;; ediff
;; - A saner diff
;; - https://www.gnu.org/software/emacs/manual/html_mono/ediff.html
(use-package ediff
  :straight (ediff :type built-in)
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-diff-options "-w")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-vertically)
  (setq ediff-shell (getenv "$SHELL")))


;;;;; elec-pair
;; - Automatic parenthesis pairing
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/elec-pair.el
(use-package elec-pair
  :straight (elec-pair :type built-in)
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
  ;; (add-hook 'electric-indent-functions 'electric-indent-ignore-mode)
  )


;;;;; compile
;; - Compilation Mode
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html
(use-package compile
  :straight (compile :type built-in)
  :preface
  ;; ANSI Coloring
  ;; @see https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  (defun my-colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))


;;;;; dumb-jump
;; - Jump to definition via `ag'/`rg'/`grep'
;; - https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :after hydra
  :hook ((emacs-startup . dumb-jump-mode)
         (xref-backend-functions . dumb-jump-xref-activate))
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("M-g o" . dumb-jump-go-other-window)
              ("M-g j" . dumb-jump-go)
              ("M-g i" . dumb-jump-go-prompt)
              ("M-g x" . dumb-jump-go-prefer-external)
              ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-prefer-searcher 'ag))

;;;;; flymake
;; - built-in emacs syntax checker
;; - https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top
(use-package flymake
  :straight (flymake :type built-in)
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


;;;;; flycheck
;; - added in emacs syntax checker
;; - https://www.flycheck.org/en/latest/
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


;;;;; flycheck-popup-tip
;; - Flycheck extension minor-mode for displaying errors from Flycheck using popup.el
;; - https://github.com/flycheck/flycheck-popup-tip
(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-pos-tip-display-errors-tty-function #'flycheck-popup-tip-show-popup))


;;;;; flycheck-color-mode-line
;; - minor-mode for Flycheck which colors the mode line according to
;; the Flycheck state of the current buffer
;; - https://github.com/flycheck/flycheck-color-mode-line
(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))


;;;;; emr
;; - a framework for providing language-specific refactoring in Emacs.
;; It includes refactoring commands for a variety of languages
;; Just hit M-RET to access your refactoring tools in any supported mode.
;; - https://github.com/emacsmirror/emr
(use-package emr
  ;; Just hit H-r to access your refactoring tools in any supported mode.
  :bind (:map sej-mode-map
              ("C-c s r" . emr-show-refactor-menu)
              ("H-r" . emr-show-refactor-menu) )
  :hook (prog-mode . emr-initialize))


;;;;; projectile
;; - Manage and navigate projects
;; - https://github.com/bbatsov/projectile
(use-package projectile
  :diminish
  :bind ("H-f" . projectile-find-file)
  :bind-keymap (  ("s-P" . projectile-command-map)
                  ("C-c p" . projectile-command-map))
  :hook (emacs-startup . projectile-global-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  (setq projectile-git-submodule-command nil)
  :config
  (setq projectile-enable-caching t)
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


(use-package helm-projectile
  :after (helm)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))


;;;; vcs
;;;;; magit
;; - interface to the version control system Git
;; - https://magit.vc/
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


;;;;; forge
;; - Access Git forges from Magit
;; To start using Forge in a certain repository visit the Magit status buffer
;; for that repository and type f y (forge-pull). Alternatively you can use M-x
;; forge-add-repostiory, which makes it possible to add a forge repository without
;; pulling all topics and even without having to clone the respective Git repository.
;; - https://github.com/magit/forge
(use-package forge
  :after magit
  :demand)


;;;;; magit-todos
;; - Show tasks from commit files
;; - https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :commands(magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 100)
  (custom-set-variables
   '(magit-todos-keywords (list "TODO(SeJ)"))
   '(magit-todos-ignore-file-suffixes '("todo"))
   '(magit-todos-exclude-globs '("*.map" "*.html"))))


;;;;; git-timemachine
;; - Walk through git revisions of a file
;; - https://github.com/emacsmirror/git-timemachine
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))


;;;;; smerge-mode
;; - Resolve diff3 conflicts
;; - http://web.mit.edu/Emacs/source/emacs/lisp/smerge-mode.el
(use-package smerge-mode
  :straight (smerge-mode :type built-in)
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


;;;;; browse-at-remote
;; - Open github/gitlab/bitbucket page
;; - https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :bind (:map sej-mode-map
              (("C-c s B" . browse-at-remote)
               ("C-x v B" . browse-at-remote))
              :map vc-prefix-map
              ("B" . browse-at-remote)))


;;;;; gist
;; - gist client
;; - Functions:
;; gist-list - Lists your gists in a new buffer. Use arrow keys
;; to browse, RET to open one in the other buffer.

;; gist-region - Copies Gist URL into the kill ring.
;; With a prefix argument, makes a private gist.

;; gist-region-private - Explicitly create a private gist.

;; gist-buffer - Copies Gist URL into the kill ring.
;; With a prefix argument, makes a private gist.

;; gist-buffer-private - Explicitly create a private gist.

;; gist-region-or-buffer - Post either the current region, or if mark
;; is not set, the current buffer as a new paste at gist.github.com .
;; Copies the URL into the kill ring.
;; With a prefix argument, makes a private paste.

;; gist-region-or-buffer-private - Explicitly create a gist from the
;; region or buffer.
;; - https://github.com/defunkt/gist.el
(use-package gist
  :defines sej-mode-map
  :bind  (:map sej-mode-map
               ("C-c s G" . gist-list)
               ("H-G" . gist-list)))


;;;;; git config modes
;; - Emacs major modes for various Git configuration files.
;; - gitattributes-mode , gitconfig-mode , gitignore-mode
;; - https://github.com/magit/git-modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)


;;;;; sej/git-blame-line
;; - Runs `git blame` on the current line and adds the commit id to the kill ring
(defun sej/git-blame-line ()
  "Run `git blame` on the current line and add the commit id to the kill ring."
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

(define-key sej-mode-map (kbd "C-c s b") 'sej/git-blame-line)
(define-key sej-mode-map (kbd "H-b") 'sej/git-blame-line)


;;;; completion
;;;;; hippie-expand
;; - built-in package to expand at point in various ways
;; - https://www.emacswiki.org/emacs/HippieExpand

(use-package hippie-expand
  :straight (hippie-expand :type built-in)
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


;;;;; company
;; - Modular in-buffer completion framework for Emacs
;; - http://company-mode.github.io/
(use-package company
  :diminish company-mode
  :defines
  company-dabbrev-ignore-case
  company-dabbrev-downcase
  company-dabbrev-code-modes
  company-dabbrev-code-ignore-case
  :commands company-abort
  :bind (
         (("C-<tab>" . company-complete)
          ("<backtab>" . company-yasnippet)
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
  :hook (emacs-startup . global-company-mode)
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


;;;;; company-box
;; - a company front-end with Icons
;; - https://github.com/sebastiencs/company-box
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)

  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))

(with-eval-after-load 'all-the-icons
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-fileicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
              (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
              (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
              (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
              (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
              (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
              (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
              (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
              (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
              (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
              (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
              (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
              (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))


;;;;; company-quickhelp
;; - documentation popups that appear when idling on a completion candidate.
;; - https://github.com/company-mode/company-quickhelp
(use-package company-quickhelp
  :after company
  :hook ((global-company-mode company-mode) . company-quickhelp-mode)
  :bind (:map company-active-map
              ("H-h" . company-quickhelp-manual-begin))
  :config (setq company-quickhelp-delay 1))


;;;;; company-statistics
;; - Set up statistics and rank company completions based on frequency
;; - https://github.com/company-mode/company-statistics
(use-package company-statistics
  :after company
  :hook (emacs-startup . company-statistics-mode))


;;;;; company-try-hard
;; - A company-complete alternative that tries much harder to find completions.
;; If none of the current completions look good, call the command again to try
;; the next backend.
;; - https://github.com/Wilfred/company-try-hard
(use-package company-try-hard
  :commands company-try-hard
  :bind (("H-/" . company-try-hard)
         :map company-active-map
         ("H-/" . company-try-hard)))


;;;;; company-shell
;; - Company mode completion backends for your shell scripting.
;; - you also need bash-language-server installed and on your PATH
;; npm install -g bash-language-server
;; - https://github.com/Alexander-Miller/company-shell
(use-package company-shell
  :after company
  :init
  (defun sej/company-shell-hook ()
    (add-to-list 'company-backends '(company-shell
                                     company-shell-env
                                     company-fish-shell)) )
  :hook (emacs-startup . sej/company-shell-hook) )


;;;;; yasnippet
;; - short-cut completions
;; - https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ( (emacs-startup . yas-reload-all)
          ((prog-mode org-mode go-mode) . yas-minor-mode))
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


;;;;; hydra
;; - Make bindings that stick around
;; - https://github.com/abo-abo/hydra
(use-package hydra)


;;;; lisp
;;;;; lisp settings
;; - some lisp stuff from Getting Started with Emacs Lisp
(define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "H-<return>") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "A-<return>") 'eval-region)

(define-key emacs-lisp-mode-map (kbd "C-c D") 'toggle-debug-on-error)
(global-set-key (kbd "C-c s E") 'toggle-debug-on-error)

;; use flycheck in elisp
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))

(defun sej/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise if PREFIX the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sej/eval-last-sexp-or-region)


;;;;; lispy /* NOT USED */
;; - This package reimagines Paredit
;; - https://github.com/abo-abo/lispy
;; - a popular method to navigate and edit LISP code in Emacs.
;; - The killer-feature are the short bindings:

;; command                    binding  binding  command
;; paredit-forward             C-M-f      j     lispy-down
;; paredit-backward            C-M-b      k     lispy-up
;; paredit-backward-up         C-M-u      h     lispy-left
;; paredit-forward-up          C-M-n      l     lispy-right
;; paredit-raise-sexp          M-r        r     lispy-raise
;; paredit-convolute-sexp      M-?        C     lispy-convolute
;; paredit-forward-slurp-sexp  C-)        >     lispy-slurp
;; paredit-forward-barf-sexp   C-}        <     lispy-barf
;; paredit-backward-slurp-sexp C-(        >     lispy-slurp
;; paredit-backward-barf-sexp  C-{        <     lispy-barf

;; (use-package lispy
;;   :hook (emacs-lisp-mode . lispy-mode))


;;;;; eldoc
;; - we don't want this minor mode to be shown in the minibuffer, however
;; we use eldoc to show the signature of the function at point in the minibuffer
;; - https://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :diminish eldoc-mode
  :hook
  ((emacs-lisp-mode . eldoc-mode)
   (ielm-mode . eldoc-mode)
   (lisp-interaction-mode . eldoc-mode)
   (eval-expression-minibuffer-setup . eldoc-mode))
  :config
  (setq eldoc-idle-delay 0.1) )


;;;;; elisp-slime-nav
;; - turn on elisp-slime-nav
;; - M-. works to jump to function definitions
;; - M-, to jump back
;; - https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
  :config
  (global-unset-key (kbd "C-c C-d d"))
  (global-unset-key (kbd "C-c C-d C-d")))


;;;;; eros
;; - eros-mode will show you the result of evaluating an elisp command
;; as an overlay in your elisp buffer. Try it out with C-x C-e or s-<return>
;; - https://github.com/xiongtx/eros
(use-package eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode))


;;;;; sej/ielm-other-window
;; - add a nice popup for ielm
;; - https://www.emacswiki.org/emacs/InferiorEmacsLispMode
(defun sej/ielm-other-window ()
  "Run ielm on other window."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key sej-mode-map (kbd "s-i") 'sej/ielm-other-window)


;;;;; sej/remove-elc-on-save
;; - When saving an elisp file, remove its compiled version if
;; there is one, as you'll want to recompile it.

(defun sej/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'sej/remove-elc-on-save)


;;;; python
;;;;; python
;; - Install:
;; pip3 install -U setuptools
;; pip3 install python-language-server[all] --isolated
;; [all] should give you: jedi, rope, pyflakes, pycodestyle, pydocstyle,
;; autopep8, YAPF
;; - http://wikemacs.org/wiki/Python
(use-package python
  :straight (python :type built-in)
  :bind (:map python-mode-map
              ("s-\\" . python-insert-docstring) )
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")

  (setq
        lsp-pyls-plugins-jedi-completion-enabled t
        lsp-pyls-plugins-pylint-enabled t
        lsp-pyls-plugins-pydocstyle-enabled t
        lsp-pyls-plugins-rope-completion-enabled t
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-mypy-enabled t
        lsp-pyls-plugins-black-enabled t
        )

  (define-skeleton python-insert-docstring
    "Insert a Python docstring."
    "This string is ignored!"
    "\"\"\"" - "\n\n    \"\"\"")  )


;;;;; live-py-mode
;; - Live Coding in Python
;; - Open any Python file, and activate live-py-mode with M-x live-py-mode.
;; - You should see an extra window on the right that shows the results of
;; running your code.
;; - https://github.com/donkirkby/live-py-plugin
(use-package live-py-mode)


;;;;; ein
;; - Emacs IPython Notebook
;; #BEGIN_SRC ein-python :session localhost :results raw drawer
;; import numpy, math, matplotlib.pyplot as plt
;; %matplotlib inline
;; x = numpy.linspace(0, 2*math.pi)
;; plt.plot(x, numpy.sin(x))
;; #+END_SRC

;; - Use M-x ein:connect-to-notebook to submit code from an arbitrary
;; buffer to a running jupyter kernel
;; - M-x ein:run launches a jupyter process from emacs
;; - M-x ein:login to a running jupyter server
;; - https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :diminish ein:notebook-mode
  :defines ein:completion-backend
  :init (setq ein:completion-backend 'ein:use-company-backend))


;;;;; pip-requirements
;; - major mode for editing pip requirement files
;; - https://github.com/Wilfred/pip-requirements.el
(use-package pip-requirements)


;;;;; pyvenv
;; - simple global minor mode which will replicate the changes done
;; by virtualenv activation inside Emacs
;; - https://github.com/jorgenschaefer/pyvenv
;; You can use (add-dir-local-variable) to set pyvenv-workon for a particular project.
(use-package pyvenv
  :hook (pyvenv-post-activate . pyvenv-restart-python)
  :config
  (setq pyvenv-workon ".python-environments/default") ; default venv
  (pyvenv-tracking-mode 1) ; automatically use pyvenv-workon via dir-locals
  )


;;;; web modes
;;;;; web-mode
;; - Major mode for editing web templates
;; - http://web-mode.org/
;; - https://github.com/fxbois/web-mode
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


;;;;; css-eldoc
;; - eldoc-mode plugin for CSS
;; - https://github.com/zenozeng/css-eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))


;;;;; json-mode
;; - Major mode for editing JSON files.
;; - Extends the builtin js-mode to add better syntax highlighting for JSON
;; and some nice editing keybindings.
;; - https://github.com/joshwnj/json-mode
(use-package json-mode)


;;;;; js2-mode
;; - Improved JavaScript editing mode
;; - https://github.com/mooz/js2-mode
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


;;;;; js2-refactor
;; - JavaScript refactoring library for Emacs
;; - https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :after js2-mode
  :diminish js2-refactor-mode
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))


;;;;; mocha
;; - Run Mocha or Jasmine tests
;; - https://github.com/scottaj/mocha.el
(use-package mocha
  :config (use-package mocha-snippets))


;;;;; skewer-mode
;; - Live browser JavaScript, CSS, and HTML interaction
;; - M-x run-skewer to attach a browser to Emacs
;; C-x C-e: Evaluate the form before the point and display the result in the
;; minibuffer. If given a prefix argument, insert the result into the
;; current buffer.
;; C-M-x: Evaluate the top-level form around the point.
;; C-c C-k: Load the current buffer.
;; C-c C-z: Select the REPL buffer
;; - https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :diminish skewer-mode skewer-css skewer-html
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode)))


;;;;; web-beautify
;; - Format HTML, CSS and JavaScript/JSON by js-beautify
;; - Insta;; npm -g install js-beautify
;; - https://github.com/yasuyk/web-beautify
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


;;;;; haml-mode
;; - major mode for the haml mark-up language
;; - https://github.com/nex3/haml-mode
(use-package haml-mode)


;;;;; php-mode
;; - major mode for editing PHP code
;; - https://github.com/emacs-php/php-mode
(use-package php-mode
  :mode (("\\.module$" . php-mode)
         ("\\.inc$" . php-mode)
         ("\\.install$" . php-mode)
         ("\\.engine$" . php-mode)
         ("\\.\\(?:php\\|phtml\\)\\'" . php-mode)))


;;;;; yaml-mode
;; - YAML major mode support
;; - https://www.emacswiki.org/emacs/YamlMode
(use-package yaml-mode
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))


;;;;; nxml-mode
;; - major mode for editing XML
;; - https://www.gnu.org/software/emacs/manual/html_node/nxml-mode/Introduction.html
(use-package nxml-mode
  :straight (nxml-mode :type built-in)
  :mode (("\\.xaml$" . xml-mode)))


;;;; c program modes
;;;;; c-mode
;; - C/C++ Mode
;; - https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(use-package cc-mode
  :straight (cc-mode :type built-in)
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
          (other . "java"))))


;;;;; ccls
;; - c++-mode, objc-mode, cuda-mode: lsp server
;; - used for both lsp & eglot so no :if statement
;; - [[https://github.com/MaskRay/ccls/wiki/lsp-mode][emacs-ccls]]
(use-package ccls
  :init (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
  :config
  '(ccls-initialization-options (quote (compilationDatabaseDirectory :build))))


;;;;; modern-cpp-font-lock
;; - Syntax highlighting support for "Modern C++" - until C++20 and Technical Specification
;; - [[https://github.com/ludwigpacifici/modern-cpp-font-lock][modern-cpp-font-lock]]
(use-package modern-cpp-font-lock
  :straight (modern-cpp-font-lock
             :type git
             :host github
             :repo "ludwigpacifici/modern-cpp-font-lock")
  :hook (c++-mode . modern-c++-font-lock-mode))


;;;;; csharp-mode
;; - mode for editing C# in emacs. It’s based on cc-mode
;; - https://github.com/josteink/csharp-mode
(use-package csharp-mode
  :straight (csharp-mode
             :type git
             :host github
             :repo "josteink/csharp-mode"))


;;;;; arduino-cli-mode
;; - minor mode for using the excellent new arduino command line interface
;; - [[https://github.com/motform/arduino-cli-mode][arduino-cli-mode]]
(use-package arduino-cli-mode
  :straight (arduino-cli-mode
             :type git
             :host github
             :repo "motform/arduino-cli-mode")
  :hook (c++-mode . arduino-cli-mode)
  :ensure-system-package arduino-cli
  :config
  (setq arduino-cli-warnings 'all
        arduino-cli-verify t
        arduino-cli-default-fqbn "esp8266:esp8266:d1"
        arduino-cli-default-port "/dev/cu.wchusbserial1430"))


;;;;; company-c-headers
;; - This library enables the completion of C/C++ header file names using Company mode for Emacs
;; - [[https://github.com/randomphrase/company-c-headers][company-c-headers]]
(use-package company-c-headers
  :after company
  :straight (company-c-headers
             :type git
             :host github
             :repo "randomphrase/company-c-headers")
  :init
  (add-to-list 'company-backends 'company-c-headers)
  (defun my-company-c-headers-get-system-path ()
  "Return the system include path for the current buffer."
  (let ((default '("/usr/include/" "/usr/local/include/")))))
(setq company-c-headers-path-system 'my-company-c-headers-get-system-path))


;;;;; platformio-mode
;; - minor mode which allows quick building and uploading of PlatformIO projects
;;   with a few short key sequences.
;; - Code completion can be provided by installing any package compatible with .clang_complete files,
;;   such as irony-mode.
;; - To keep the index up to date, run platformio-init-update-workspace (C-c i i)
;;   after installing any libraries.
;; - [[https://github.com/ZachMassia/platformio-mode][PlatformIO Mode]]
(use-package platformio-mode
  :straight (platformio-mode
             :type git
             :host github
             :repo "ZachMassia/PlatformIO-Mode")
  :hook ((c++-mode c-mode objc-mode cuda-mode) . platformio-conditionally-enable))


;;;;; swift-mode
;; - support for Apple's Swift programming language
;; - https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :hook (swift-mode . (lambda() (lsp)))
  :config
  (require 'lsp-sourcekit)
  (use-package flycheck-swift
    :after flycheck
    :commands flycheck-swift-setup
    :init (flycheck-swift-setup)))


;;;;; lsp-sourcekit for swift-mode
;; - a client for sourcekit-lsp a swift/c/c++/objective-c language server created by apple
;; - support for Apple's Swift programming language
;; - [[https://github.com/emacs-lsp/lsp-sourcekit][lsp-sourcekit]]
(use-package lsp-sourcekit
  :after lsp-mode
  :commands swift-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode-beta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))


;;;;; rust-mode
;; - rust language package
;; - https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :config (setq rust-format-on-save t))


;;;;; go lang
;; - two different ways to configure
;; - [[https://arenzana.org/2019/12/emacs-go-mode-revisited/][golang with eglot/lsp]]  [[https://github.com/golang/tools/blob/master/gopls/README.md][glpls documentation]]
;; - need to install golang and go get golang/x/tools/gopls
;; - [[https://sandyuraz.com/articles/go-emacs/][go-mode native completion]]
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  :bind (("C-c s c" . compile)
         ;; ("M-." . godef-jump)
         )
  :config
  (setq compilation-window-height 14)
  (defun sej/compilation-hook ()
    (when (not (get-buffer-window "*compilation*"))
      (save-selected-window
        (save-excursion
          (let* ((w (split-window-vertically))
                 (h (window-height w)))
            (select-window w)
            (switch-to-buffer "*compilation*")
            (shrink-window (- h compilation-window-height)))))))
  (add-hook 'compilation-mode-hook 'sej/compilation-hook)

  (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  (setq compilation-scroll-output t))

;;;; other program modes
;;;;; csv-mode
;; - major mode for csv
;; - https://www.emacswiki.org/emacs/csv-mode.el
(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))


;;; files
;;;;; ibuffer
;; - operate on buffers much in the same manner as Dired.
;; - https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :straight (ibuffer :type built-in)
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


;;;;; registers
;; - Registers allow you to jump to a file or other location quickly.
;; Use C-x r j or s-r followed by the letter of the register
;; (i for init.el, r for this file) to jump to it.
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html
(define-key sej-mode-map (kbd "H-j") 'jump-to-register)
;; (kbd "C-x r j") is built-in global
(set-register ?c '(file . "~/.ssh/custom-post.el"))
(set-register ?d '(file . "~/.dotfiles/"))
(set-register ?e '(file . "~/.emacs.d/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))


;;;;; dashboard
;; - all-in-one start-up screen with current files / projects
;; - https://github.com/emacs-dashboard/emacs-dashboard
  (use-package dashboard
    :if (eq sej-dashboard t)
    :straight (emacs-dashboard
               :type git
               :host github
               :repo "emacs-dashboard/emacs-dashboard")
    :diminish (dashboard-mode page-break-lines-mode)
    :commands sej/open-dashboard
    :hook (emacs-startup . sej/open-dashboard)
    :bind (("<f6>" . sej/open-dashboard)
           (:map sej-mode-map
                 ("C-c s d" . sej/open-dashboard)))
    :config
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    (setq dashboard-startup-banner (locate-user-emacs-file "emacs.png"))
    (setq dashboard-set-init-info t)
    (setq dashboard-items '((recents  . 15)
                            (bookmarks . 15)
                            (projects . 10)
                            (registers . 10)))
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (dashboard-setup-startup-hook)
    (dashboard-insert-startupify-lists)

    (defun sej/open-dashboard ()
      "Move to the dashboard package buffer."
      (interactive)
      (switch-to-buffer "*dashboard*") ) )


;;;;; page-break-lines
;; - display ^L page breaks as tidy horizontal lines
;; - https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :config
  (setq global-page-break-lines-mode t)
  )


;;;;; autoinsert
;; - mode that comes with Emacs that automagically inserts text into new buffers
;; based on file extension or the major mode
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/autoinsert.el
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
  (define-auto-insert ".*\\.el" "template.el") )


;;;;; sej/create-non-existent-directory
;; - Offer to create parent directories if they do not exist
;; automatically run after save
;; - http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun sej/create-non-existent-directory ()
  "Ask to make directory for file if it does not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p? (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'sej/create-non-existent-directory)


;;;;; sudo-edit
;; - Open files as sudo
;; - https://github.com/nflath/sudo-edit
(unless sys/win32p
  (use-package sudo-edit))


;;;;; vlf-setup
;; - vlf lets you handle very large files for viewing
;; - VLF operations are grouped under the C-c C-v prefix by default
;; - https://github.com/m00natic/vlfi
(use-package vlf-setup
  :straight (vlf :host github
                 :repo "m00natic/vlfi")
  :commands (vlf vlf-occur-load vlf-ediff-files))


;;; dired
;;;;; dired
;; - Directory operations
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html#Dired
(use-package dired
  :straight (dired :type built-in)
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


;;;;; all-the-icons-dired
;; - Shows icons in dired buffer
;; - https://github.com/jtbm37/all-the-icons-dired
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


;;;;; dired-aux
;; - auxiliary functionality of dired
;; - https://github.com/jwiegley/emacs-release/blob/master/lisp/dired-aux.el
(use-package dired-aux
  :straight (dired-aux :type built-in))


;;;;; dired-x
;; - Extra Dired functionality
;; - https://www.gnu.org/software/emacs/manual/html_node/dired-x/
(use-package dired-x
  :straight (autorevert :type built-in)
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


;;;;; quick-preview
;; - Quick-preview provides a nice preview of the thing at point for files.
;; - https://github.com/myuhe/quick-preview.el
(use-package quick-preview
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c s q" . quick-preview-at-point)
              ("C-c q" . quick-preview-at-point)
              :map dired-mode-map
              ("Q" . quick-preview-at-point)))


;;;;; browse-at-remote
;; - browse file at remote source
;; - https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :bind ("C-c s b" . browse-at-remote))


;;;;; diredfl
;; - Extra font-lock rules for a more Colourful dired
;; - https://github.com/purcell/diredfl
(use-package diredfl
  :init (diredfl-global-mode 1))


;;; writing & reading
;;;;; deft
;; - Deft is an Emacs mode for quickly browsing, filtering,
;; and editing directories of plain text notes
;; - https://jblevins.org/projects/deft/
(use-package deft
  :defines sej-mode-map deft-text-mode
  :bind (:map sej-mode-map
              ("<f7>" . deft)
              ("C-c s D" . deft)
              ("C-c D" . deft))
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


;;;;; writegood-mode
;; - minor mode to aid in finding common writing problems
;; - https://github.com/bnbeckwith/writegood-mode
(use-package writegood-mode
  :bind (("C-c C-g w" . writegood-mode)
         ("C-c C-g g" . writegood-grade-level)
         ("C-c C-g e" . writegood-reading-ease))
  :hook (markdown-mode . writegood-mode) )


;;;;; markdown-mode
;; - markdown-mode used a lot on Github
;; - https://jblevins.org/projects/markdown-mode/
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


;;;;; markdown-toc
  ;; - Table of contents
  ;; - Inside a markdown file, the first time,
  ;; place yourself where you want to insert the TOC:
  ;; M-x markdown-toc-generate-toc
  ;; - https://github.com/ardumont/markdown-toc
  (use-package markdown-toc))


;;;;; textile-mode
;; - textile markup editing major mode
;; - https://github.com/juba/textile-mode
(use-package textile-mode
  :mode "\\.textile\\'")


;;;;; adoc-mode
;; - adoc-mode is an Emacs major mode for editing AsciiDoc files.
;; It emphasizes on the idea that the document is highlighted
;; so it pretty much looks like the final output.
;; - https://github.com/sensorflo/adoc-mode/wiki
(use-package adoc-mode
  :mode "\\.txt\\'")


;;;;; abbrev
;; - for inserting abbreviations
;; - https://www.emacswiki.org/emacs/AbbrevMode
(use-package abbrev
  :straight (abbrev :type built-in)
  :hook ((emacs-startup org-mode) . abbrev-mode)
  :diminish abbrev-mode
  :config
  (setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...
  (define-abbrev-table
    'global-abbrev-table
    '(("ssej" "stephenearljenkins" nil 0 )))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("orgh" "" sej/org-header 0)
      ("orgl" "" sej/org-wrap-elisp 0)
      ("orgs" "" sej/org-wrap-source 0))))


;;;;; sej/number-rectangle
;; - Let's say you have a list like:
;; First Item
;; Second Item
;; Third Item
;; Fourth Item
;; And you want to number it to look like:
;; 1. First Item
;; 2. Second Item
;; 3. Third Item
;; 4. Fourth Item
;; This function allows you to hit ***C-x r N ***and specify the pattern
;; and starting offset to number lines in rectangular-selection mode:
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

(define-key sej-mode-map (kbd "C-c s N") 'sej/number-rectangle)
(define-key sej-mode-map (kbd "C-x r N") 'sej/number-rectangle)


;;;;; flyspell
;; - main spelling package
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
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


;;;;; powerthesaurus
;; - simple plugin to integrate Emacs with powerthesaurus.org
;; - https://github.com/SavchenkoValeriy/emacs-powerthesaurus
(use-package powerthesaurus
  :bind (:map sej-mode-map
              ("C-c s t" . powerthesaurus-lookup-word-dwim)
              ("s-|" . powerthesaurus-lookup-word-dwim)))


;;;;; define-word
;; - Word Definition search for non-osx
;; - https://github.com/abo-abo/define-word
(use-package define-word
  :unless sys/macp
  :bind (:map sej-mode-map
              ("C-c s s" . define-word-at-point)
              ("s-\\" . define-word-at-point)))


;;;;; osx-dictionary
;; - define-word OR
;; - pilot osx-dictionary only for osx
;; use osx dictionary when possible
;; - https://github.com/xuchunyang/osx-dictionary.el
(use-package osx-dictionary
  :if sys/macp
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c s s" . osx-dictionary-search-word-at-point)
              ("s-\\" . osx-dictionary-search-word-at-point)
              ("C-c s i" . osx-dictionary-search-input)
              ))


;;;;; helm-wordnet
;; - helm interface to WordNet install
;; - [[https://github.com/raghavgautam/helm-wordnet][helm-wordnet]]
;; - [[https://wordnet.princeton.edu/][WordNet]]
(use-package helm-wordnet
  :after helm
  :bind ("C-c s w" . helm-wordnet-suggest))


;;;;; sej/pdf-print-buffer-with-faces (ps-print)
;; - print file in the current buffer as pdf
;; - https://www.emacswiki.org/emacs/PsPrint
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


;;;;; pdf-tools
;; - PDF reader
;; - https://github.com/politza/pdf-tools
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


;;;;; nov
;; - Epub reader
;; - https://github.com/wasamasa/nov.el
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
  :hook (nov-mode . my-nov-setup))


;;;; org
;;;;; org
;; - org mode for keeping notes, maintaining lists, planning
;; - https://orgmode.org/
(use-package org
  :defines  sej-mode-map
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
  :hook ( (org-mode . flyspell-mode)
          (org-mode . writegood-mode)
          (org-mode . visual-line-mode))
  :bind (:map sej-mode-map
              ("C-c l" . org-store-link)
              ("C-c c" . org-capture)
              ("C-c a" . org-agenda)
              :map org-mode-map
              ("C-M-\\" . org-indent-region)
              ("S-<left>" . org-shiftleft)
              ("S-<right>" . org-shiftright)
              )
  :config
  (setq org-ellipsis "⤵")
  (use-package org-plus-contrib)
  (require 'org-protocol)
  (require 'ol-man)
  (setq org-directory sej-org-directory)
  (defconst org-file-inbox (concat org-directory "/inbox.org"))
  (defconst org-file-someday (concat org-directory "/someday.org"))
  (defconst org-file-gtd (concat org-directory "/gtd.org"))
  (defconst org-file-journal (concat org-directory "/journal.org"))
  (defconst org-file-notes (concat org-directory "/notes.org"))
  (defconst org-file-code (concat org-directory "/snippets.org"))
  (setq org-replace-disputed-keys t
        org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t
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
        org-highlight-latex-and-related '(latex)
        )

  (let* ((variable-tuple
          (if (display-graphic-p)
              (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                    ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                    ((x-list-fonts "Verdana")         '(:font "Verdana"))
                    ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                    (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))
            nil
            )
          )
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

  (setq sej-project-org-capture-list (list
                                      "p" sej-project-org-capture-text 'entry (list 'file+olp+datetree sej-project-org-capture-file "Journal" )
                                      "* %U\n %l\n %i%?\n"))
  ;; Bookmarks in Safari
  ;;
  ;; Org Capture Journal  javascript:location.href='org-protocol://capture?template=x'
  ;;                               +'&url='+encodeURIComponent(window.location.href)
  ;;                               +'&title='+encodeURIComponent(document.title)
  ;;                               +'&body='+encodeURIComponent(window.getSelection()) ;
  ;; Org Capture Link  javascript:window.location.href='org-protocol://store-link?'
  ;;                               +'url='+encodeURIComponent(location.href)
  ;;                               +'&title='+encodeURIComponent(document.title) ;

  (setq org-capture-templates (append
                               '(
                                 ("j" "Journal" entry (file+olp+datetree  org-file-journal "Journal")
                                  "* %U\n %l\n %i%?\n")
                                 ("x" "WebJournal" entry (file+olp+datetree  org-file-journal "Journal")
                                  "* %U\n %:annotation\n i=%i%?\n")
                                 ("n" "Notes" entry (file+headline org-file-notes  "Notes")
                                  "* %U\n %i%?\n")
                                 ("s" "Someday" entry (file+headline org-file-someday  "Someday")
                                  "* %?\n %i\n %a")
                                 ("t" "Todo" entry (file+headline org-file-gtd  "Todo")
                                  "* TODO %?\n %i\n %a")
                                 ("c" "code snippet" entry (file+headline org-file-code "code snippets")
                                  "* %?\n%(my/org-capture-code-snippet \"%F\")")
                                 )
                               (list sej-project-org-capture-list)))


  ;; org-mode agenda options
  (setq org-agenda-files (list org-file-inbox org-file-journal org-file-notes org-file-someday org-file-gtd)
        org-refile-targets '((org-file-gtd :maxlevel . 3)
                             (org-file-someday :maxlevel . 1))
        org-agenda-window-setup (quote current-window) ;open agenda in current window
        org-deadline-warning-days 7 ;warn me of any deadlines in next 7 days
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
                               (shell . t)
                               ))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))




;;;;; ob-go
;; - org-bable functions for go evaluations
;; - https://github.com/pope/ob-go
(use-package ob-go)


;;;;; ob-rust
;; - org-babel functions for rust evaluation
;; - https://github.com/zweifisch/ob-rust
(use-package ob-rust)


;;;;; ob-ipython
;; - library that allows Org mode to evaluate code blocks using a Jupyter kernel
;; (Python by default)
;; - https://github.com/gregsexton/ob-ipython
(use-package ob-ipython)


;;;;; org-rich-yank
;; - Rich text clipboard when yanking code into org buffer
;; consider demand t as lazy loading may not work
;; - https://github.com/unhammer/org-rich-yank
(use-package org-rich-yank
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))


;;;;; org-superstar-mode
;; - Show org-mode bullets as UTF-8 characters ( rewritten org-bullets )
;; - [[https://github.com/integral-dw/org-superstar-mode][github org-superstar-mode]]
;; - https://github.com/sabof/org-bullets

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-prettify-item-bullets t)
  ;; :custom
  ;; (org-superstar-headline--bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  )


;;;;; org-fancy-priorities
;; - displays org priorities as custom strings
;; - https://github.com/harrybournis/org-fancy-priorities
(use-package org-fancy-priorities
  :diminish
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (unless (char-displayable-p ?❗)
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))


;;;;; toc-org
;; - Table of contents updated at save to header with TOC tag
;; - https://github.com/snosov1/toc-org
(use-package toc-org
  :hook (org-mode . toc-org-mode))


;;;;; poporg
;; - While editing a buffer containing a program, you may edit a comment block
;; or a string (often a doc-string) in Org mode
;; - https://github.com/pinard/poporg
(use-package poporg
  :ensure t
  :bind (:map sej-mode-map
              ("C-c s o" . poporg-dwim)))


;;;;; org-num
;; - outline numbering as overlays on Org mode headlines
;; in the org repo but not part of official orgmode
;; - https://github.com/bzg/org-mode/blob/master/lisp/org-num.el
(use-package org-num
  :straight(org-num :local-repo "org/lisp/")
  :hook (org-mode . org-num-mode))


;;;;; org-pretty-tags
;; - Display text or image surrogates for Org mode tags.
;; - https://gitlab.com/marcowahl/org-pretty-tags
(use-package org-pretty-tags
  :hook (org-mode . org-pretty-tags-global-mode)
  :config
  (setq org-pretty-tags-surrogate-strings
        (quote
         (("TOPIC" . "☆")
          ("PROJECT" . "💡")
          ("SERVICE" . "✍")
          ("Blog" . "✍")
          ("music" . "♬")
          ("security" . "🔥"))))
  (org-pretty-tags-global-mode))


;;;;; org-skeleton
;; - skeleton template for new org file
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
;;;;; sej/org-wrap-elisp skeleton
;; - skeletons are a kind of yasnippet but they don't mess with keybindings
;; - skeleton to wrap elisp babel source

(define-skeleton sej/org-wrap-elisp
  "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code"
  nil
  > "#+BEGIN_SRC emacs-lisp" \n
  > _ \n
  > "#+END_SRC" \n)


;;;;; sej/org-wrap-source skeleton
;; - skeletons are a kind of yasnippet but they don't mess with keybindings
;; - skeleton to wrap generic babel source
(define-skeleton sej/org-wrap-source
  "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type"
  "Language: "
  > "#+BEGIN_SRC " str \n
  > _ \n
  > "#+END_SRC" \n)


;;; eshell & shell
;;;; eshell
;;;;; eshell
;; - Emacs command shell ; much better than shell
;; - https://www.gnu.org/software/emacs/manual/html_mono/eshell.html
;; - https://www.masteringemacs.org/article/complete-guide-mastering-eshell
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
                           (eshell/alias "ll" "ls  -al $1")
                           (eshell/alias "la" "ls -a $1")
                           (eshell/alias "l" "ls -a $1")
                           (eshell/alias "gd" "magit-diff-unstaged")
                           (eshell/alias "gds" "magit-diff-staged")
                           (bind-keys :map eshell-mode-map
                                      ("M-P" . eshell-previous-prompt)
                                      ("M-N" . eshell-next-prompt)
                                      ("M-R" . eshell-previous-matching-input)
                                      ("C-l" . eshell/clear)
                                      )
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
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)

  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1))


;;;;; eshell-prompt-extras
;; - Display extra information for prompt
;; - See: https://github.com/kaihaosw/eshell-prompt-extras
(use-package eshell-prompt-extras
  :after esh-opt
  :defines eshell-highlight-prompt
  :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda
        eshell-prompt-function 'epe-theme-dakrone
        epe-git-dirty-char " Ϟ"
        )
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  ;; epe-git-dirty-char "*"
  )


;;;;; eshell/truncate-eshell-buffers
;; - truncates all eshell buffers after t time (5s)
(defun eshell/truncate-eshell-buffers ()
  "Truncates all eshell buffers."
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


;;;;; eshell/clear
;; - clear the eshell buffer / screen
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))


;;;;; eshell/emacs
;; - edit a file in eshell without re-rerunning Emacs
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


;;;;; eshell/ec
;; - Compile a file (ARGS) in Emacs.  Use `compile' to do background make.
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


;;;;; eshell-view-file
;; - A version of `view-file' which properly rets the eshell prompt.
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


;;;;; eshell/less
;; - Invoke `view-file' on a file.  \"less +42 foo\" will go to line 42 in the buffer
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


;;;;; eshell/cds
;; - change directory to the project's root
(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))


;;;;; eshell/d
;; - shortcut for dired in eshell
(defun eshell/d (&rest args)
  "Shortcut of d for dired in eshell with ARGS."
  (dired (pop args) "."))


;;;;; eshell/magit
;; - function to open magit-status for the current directory
(defun eshell/magit ()
  "Function to open magit-status for the current directory."
  (interactive)
  (magit-status default-directory)
  nil)


;;;; shell
;;;;; shell
;; - basic emacs shell ; eshell is much better
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html
(use-package shell
  :ensure nil
  :hook ((shell-mode . n-shell-mode-hook)
         (shell-mode . ansi-color-for-comint-mode-on)

         (comint-output-filter-functions . comint-strip-ctrl-m)
         (comint-output-filter-functions . comint-truncate-buffer))
  :bind  (:map sej-mode-map
               ("H-S" . shell)
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


;;;;; shell-pop
;; - pop-up shell
;; - https://github.com/kyagi/shell-pop-el
(use-package shell-pop
  :bind ("C-c s p" . shell-pop)
  :init (let ((val
               (if sys/win32p
                   '("eshell" "*eshell*" (lambda () (eshell)))
                 '("ansi-term" "*ansi-term*"
                   (lambda () (ansi-term shell-pop-term-shell))))))
          (setq shell-pop-shell-type val)))


;;;;; sej/shell-kill-buffers
;; - kill shell buffer upon exit
(defun sej/shell-kill-buffer-sentinel (process event)
  "Function to kill shell buffer upon (PROCESS EVENT)."
  (when (memq (process-status process) '(exit signal))
    (kill-buffer)))


;;;;; sej/kill-process-buffer-on-exit
;; - make sure processes get killed on Emacs-exit
(defun sej/kill-process-buffer-on-exit ()
  "Function to kill buffer on exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'sej/shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'sej/kill-process-buffer-on-exit))


;;;;; with-editor
;; - things that invoke $EDITOR will use the current Emacs
;; - https://github.com/magit/with-editor
(use-package with-editor
  :hook ((shell-mode . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))


;;;;; keychain-environment
;; - set up any SSH or GPG keychains that the Keychain tool has set up for us
;; - https://github.com/tarsius/keychain-environment
(use-package keychain-environment
  :hook (emacs-startup . keychain-refresh-environment))

;;; init.el --- end
(message "init.el ends here")
(provide 'init)
;;; init.el ends here
