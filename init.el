;;; init.el --- SeJ Emacs configurations. -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins
;; URL: https://github.com/sejgit/.emacs.d
;; Version: 0.1.0
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
;; a long journey to here...
;;
;; using Emacs 30
;; on macos installing emacs-plus@30
;; brew install emacs-plus@30 --with-xwidgets --with-no-frame-refocus --with-native-comp --with-poll

;;; Changelog
;; not every commit ; just the big stuff
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
;; - <2020-09-22 Tue> move to helm
;; - <2020-11-21 Sat> move custom.el fonts to init.el
;; - <2021-01-04 Mon> gccemacs changes & simplifications
;; - <2021-01-08 Fri> some clean-up
;; - <2021-04-26 Mon> move from Helm to Selectrum
;; - <2021-10-14 Thu> move from projectile to built-in project.el
;; - <2021-11-11 Thu> add frame centre function & fix no littering
;; - <2022-12-20 Tue> remove lsp -yes again- as Eglot & Tree-sitter are in for Emacs29
;; - <2023-01-30 Mon> vertigo orderless consult corfu
;; - <2023-02-12 Sun> :disabled flycheck packages for now in favour of flymake
;; - <2023-03-06 Mon> mode to built-in use-package
;; - <2023-11-24 Fri> updates for Emacs30
;; - <2024-02-15 Thu> free-bsd (berkley-unix) adds for EISY

;;; Code:
(message "Emacs start")

;;; initialize environment
;;;;; debug
;; only turned on when needed
(setq debug-on-error t)
;;(setq debug-on-event t)

;;;;; system custom constants
;; - section for global constants
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/freebsdp
  (eq system-type 'berkeley-unix)
  "Are we running on a FreeBSD system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running on a graphic Mac system?")

(defconst sys/mac-x86-p
  (and (string= (substring system-configuration 0 6) "x86_64") sys/macp)
  "Are we running X86 Mac system?")

(defconst sys/mac-AA64-p
  (and (string= (substring system-configuration 0 7) "aarch64") sys/macp)
  "Are we running Apple Silicon Mac system?")

;; specific vars for different systems
(cond (sys/mac-x86-p
       (setq sej/menu-height -25)) ;; x86 Intel mac
      (sys/mac-AA64-p
       (setq sej/menu-height -37)) ;; Apple silicon mac
      (t
       (setq sej/menu-height -30))) ;; default for other

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/freebsd-x-p
  (and (display-graphic-p) sys/freebsdp)
  "Are we running under graphic FreeBSD system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=30p
  ( >= emacs-major-version 30 )
  "Emacs is 30 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

;;;;; should i even be here
(when (not emacs/>=29p)
  (error "This requires Emacs 29 and above")  )

;;;;;  Warnings
;; set-up server & suppress warnings
;; - [[https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/warnings.el][warnings.el]]
(require 'warnings)
;; remove warnings for cl depreciated and server already running
(setq warning-suppress-types (quote ((cl server iedit))))
(setq warning-suppress-log-types (quote ((cl) )))
(setq byte-compile-warnings '(cl-functions))

;;;;; Straight package manager set-up
(setq-default straight-repository-branch "develop"
              straight-fix-org t
                                        ; straight-fix-flycheck t ;; not currently using flycheck
              straight-use-package-by-default t ;; so dont have to use :straight all the time
              straight-check-for-modifications '(check-on-save find-when-checking))
(setq vc-follow-symlinks t)
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
;; Should set before loading `use-package'
(setq-default use-package-always-defer t
              use-package-compute-statistics t
              use-package-expand-minimally t
              use-package-enable-imenu-support t)
;; part of Emacs built-in as of Emacs29
;;(if nil        ;; emacs/>=29p or nil to **short circuit until bind-key updates => Emacs@30**
;; (progn
;;   (eval-when-compile
;;     (require 'use-package))
;;   (require 'bind-key)
;;   (use-package system-packages)
;;   (require 'use-package-ensure-system-package))
;;  (progn
(straight-use-package 'use-package)
(use-package bind-key
  :ensure t
  :demand t)
(use-package use-package-ensure-system-package
  :ensure t)
;;))

;;;;; OSX System specific environment setting
(when sys/macp
  (message "Mac OSX")
;;;;;; OSX Apple keyboard
  ;; - caps lock is control (through karabiner)
  ;; Fn key do Hyper
  ;; LControl key do RControl (karabiner) which is Super (emacs)
  ;; left opt/alt key do emacs Alt modifier
  ;; right opt/alt key do regular alt key
  ;; left and right command(apple) key do Meta
  ;; spacebar acts as super key with other key
  ;; karabiner.json backup files in dotfiles under .config directory
  ;; - https://github.com/pqrs-org/Karabiner-Elements
  (if (boundp 'mac-carbon-version-string) ;; using mac-port?
      ( progn
        ;; for emacs-mac-port -- default
        (setq mac-right-command-modifier 'none) ;right command is left alone to mac
        (setq mac-right-option-modifier 'none) ;Stays as alt key (like å∫ç∂)
        (setq mac-function-modifier 'hyper) ;hyper is function & held tab key (Karabiner)
        (setq mac-control-modifier 'control) ;Karabiner swapped & caps_lock
        (setq mac-right-control-modifier 'super) ; actually left control
        (setq mac-option-modifier 'alt) ; left option is A-alt key
        (setq mac-command-modifier 'meta)) ;left command is meta
    ( progn
      ;; for regular Emacs port -- in-case other is installed
      (setq ns-right-command-modifier 'none)
      (setq ns-right-option-modifier 'none)
      (setq ns-function-modifier 'hyper)
      (setq ns-control-modifier 'control)
      (setq ns-right-control-modifier 'super)
      (setq ns-option-modifier 'alt)
      (setq ns-command-modifier 'meta)
      ))
  (when sys/mac-x-p
    (use-package exec-path-from-shell)
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

;;;;; Linux System specific environment setting
(when sys/linuxp
  (message "Linux")
;;;;;; Linux keyboard
  ;; - nothing set at this moment
  ;; load-dir init.d
  (setq exec-path (append exec-path '("/usr/local/bin")))  )

;;;;; FreeBSD System specific environment setting
(when sys/freebsdp
  (message "FreeBSD")
;;;;;; FreeBSD keyboard
  ;; - nothing set at this moment
  ;; load-dir init.d
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq insert-directory-program "/usr/local/bin/gls"))

;;;;; Microsoft Windows specific environment settings
;; set execution paths
(when sys/win32p
  (message "Microsoft Windows")
;;;;;; Windows keyboard
  ;; - CapsLock::LControl through AutoHotkeys
  ;; scroll lock do hyper (tab to scroll lock using AutoHotkeys)
  ;; Left control key do super (LControl::Appskey using AutoHotkeys)
  ;; Left Windows left alone due to win10 taking many keys
  ;; LAlt::Meta
  ;; RAlt::Alt modifier (RAlt::NumLock using Autohotkeys) **only works as tap & release
  ;; Rwin is Alt (not used in current laptop)
  ;; NOTE: only negative of this set-up is RAlt as numlock -> Alt is awkward push & release
  ;; - https://www.autohotkey.com/
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
  (setenv "PATH"
          (mapconcat
           #'identity exec-path path-separator))

  ;; set exec-path for latex installation
  (setq exec-path (append (list sej-latex-directory
                                "c:/msys64/mingw64/bin"
                                "/mingw64/bin/") exec-path))

;;;;;; AutoHotkey Mode xahk-mode
  ;; - load AutoHotkey mode only used for Microsoft Windows
  ;; - [[https://github.com/xahlee/xahk-mode.el][xahlee xahk-mode]]
  (use-package xahk-mode
    :if sys/win32p
    :demand t
    :straight (xahk-mode.el :type git
                            :host github
                            :repo "xahlee/xahk-mode.el") )  )

;;;;; Blackout
;; Similar to packages like minions, diminish, or delight.
;; You can alter how your minor and major modes show up in the mode-line.
;; [[https://github.com/raxod502/blackout][Blackout]]
(use-package blackout
  :demand t
  :straight (:host github :repo "raxod502/blackout"))

;;;;; Alert
;; Alert is a Growl-workalike for Emacs
;; uses a common notification interface and multiple, selectable "styles"
;; [[https://github.com/jwiegley/alert][Alert]]
(use-package alert
  :config
  (if sys/macp (setq alert-default-style #'osx-notifier))  )

;;;;; Server set-up
;; set-up Emacs server
(use-package emacs
  :when (or sys/macp sys/linuxp sys/freebsdp)
  :straight (:type built-in)
  :hook (emacs-startup . sej/server-mode)
  :config
  (defun sej/server-mode ()
    "Start server-mode without errors"
    (interactive)
    (with-demoted-errors
        "%S -- Server exists -- not starting new one."
      (load "server")
      (unless (server-running-p) (server-start)) ) ) )

;;;;; GCMH
;; The Garbage Collector Magic Hack
;; [[https://github.com/emacsmirror/gcmh][github gcmh]]
(use-package gcmh
  :blackout t
  :hook (after-init . gcmh-mode)
  :init
  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)
  (add-hook 'suspend-hook #'gcmh-idle-garbage-collect)
  (setq gcmh-idle-delay 10))

;;;;; customization variables set
;; set-up Emacs customizations choices which are then modified by custom.el
(defgroup sej nil
  "SeJ Emacs customizations."
  :group 'convenience)

(defcustom sej-homepage "https://github.com/sejgit/.emacs.d"
  "The Github page of Emacs Owner."
  :type 'string)

(defcustom sej-full-name "Stephen Jenkins"
  "Set user full name."
  :type 'string)

(defcustom sej-mail-address "random@gmail.com"
  "Set user email address."
  :type 'string)

(defcustom sej-dashboard t
  "If Non-nil, use dashboard at start-up, otherwise will restore previous session."
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

(defcustom sej-irc-nick "nick"
  "Nickname for ERC/IRC."
  :type 'string)

(defcustom sej-irc-pass "pass"
  "Password for ERC/IRC."
  :type 'string)

(defcustom sej-chatgpt-user "user"
  "User name for chatgpt default user."
  :type 'string)

(defcustom sej-chatgpt-key "key"
  "Key for chatgpt should be secret."
  :type 'string)

;;;;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file :noerror))

;;;;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
;; place to hold specific & secret stuff ~/.ssh is best
(progn
  (let ((file
         (expand-file-name "custom-post.el" user-emacs-directory)))
    (if (file-exists-p file)
        (load file :noerror)))
  (let ((file
         (expand-file-name "custom-post.el" "~/.ssh/")))
    (if (file-exists-p file)
        (load file :noerror))) )

;;;;; Compdef
;; in buffer completion to set local completion backends
;; [[https://gitlab.com/jjzmajic/compdef][Compdef]]
(use-package compdef
  :demand t
  :straight (:host gitlab :repo "jjzmajic/compdef"))

;;;;; dash
;; A modern list API for Emacs. No 'cl required.
;; [[https://github.com/magnars/dash.el][dash.el]]
(use-package dash
  :demand t)

;;;;; f
;; modern API for working with files and directories in Emacs.
;; [[https://github.com/rejeep/f.el][f.el]]
(use-package f
  :demand t)

;;;;; s
;; The long lost Emacs string manipulation library.
;; [[https://github.com/magnars/s.el][s.el]]
(use-package s
  :demand t)

;;;;; cl-lib
;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/cl-lib.el][cl-lib.el]]
;; (use-package cl-lib
;;   :demand t
;;   :straight (:type built-in))

;;;;; Org-Plus-Contrib
;; We need to intercept the built-in org-version that ships with Emacs we have to do this early.
(straight-use-package '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;;;;; Emacs internal settings
;; - a use-package friendly place to put settings
;;   no real extra value to putting as setq but feels clean
(use-package emacs
  :straight (:type built-in)
  :custom
;;;;;; general
  (default-directory (f-expand "$HOME") "Set startup directory.")
  (locate-command "which")
  (message-log-max 16384 "Raise the maximum number of logs in the *Messages* buffer.")
  (ad-redefinition-action 'accept "Remove irritating 'got redefined' messages.")
  (hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" ""
                                      (with-output-to-string (call-process "hostname"
                                                                           nil
                                                                           standard-output)))
            "Figure out current hostname.")
  (confirm-kill-processes nil "Allow exit without asking to kill processes.")
  (visible-bell t "Flash for bell.")
  (inhibit-compacting-font-caches t "Don’t compact font caches during GC.")
  (case-fold-search 1 "Ignore case when searching.")
  (grep-use-headings t "use headings rather than per line")
  (echo-keystrokes 0.1 "How quick to display multi-keystrokes.")
  (next-line-add-newlines t "Add a new line when going to the next line.")

;;;;;; whitespace and end-of-buffer settings
  (indicate-empty-lines t)
  (indicate-buffer-boundaries t)
  (show-trailing-whitespace nil)
  (mode-require-final-newline nil)
  (require-final-newline nil)

;;;;;; tabs, indentation and the TAB key
  (tab-always-indent 'complete)
  (tab-first-completion 'word-or-paren-or-punct)
  (tab-width 4)
  (indent-tabs-mode nil)
  
;;;;;; long line settings
  (truncate-lines 1)
  (font-lock-maximum-decoration t)
  (truncate-partial-width-windows 1)

;;;;;; backups
  (backup-directory-alist '(("." . ".saves")) "Don't litter my fs tree.")
  (vc-make-backup-files t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)

;;;;;; mouse
  (make-pointer-invisible t "Hide mouse while typing.")
  (mouse-drag-mode-line-buffer t "dragging on the buffer name to other programs")
  (mouse-drag-and-drop-region-cross-program t "allows dragging text in the region from Emacs to other program")

;;;;;; kill & clipboard settings
  (kill-buffer-query-functions
   (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions)
   "Remove kill buffer with live process prompt.")
  (select-enable-clipboard t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;;; adaptive fill settings
  (adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
  (adaptive-fill-first-line-regexp "^* *$")

;;;;;; UTF-8 please
  (locale-coding-system 'utf-8) ; pretty
  (set-terminal-coding-system 'utf-8) ; pretty
  (set-keyboard-coding-system 'utf-8) ; pretty
  (set-selection-coding-system 'utf-8) ; please
  (prefer-coding-system 'utf-8) ; with sugar on top

  :init
;;;;;; completion
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq sentence-end-double-space nil) ; Sentences do not need double spaces to end. Period.
  (global-visual-line-mode t) ; Add proper word wrapping
  (global-font-lock-mode t) ; turn on syntax highlighting for all buffers

;;;;;; color codes
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;;;;; Deleting files go to OS's trash folder
  (setq delete-by-moving-to-trash t)
  (if sys/macp (setq trash-directory "~/.Trash"))

;;;;;; update time-stamps in files
  (add-hook 'before-save-hook 'time-stamp)

;;;;;; yes and no settings
  (defalias 'yes-or-no-p 'y-or-n-p)

;;;;;; Don't use system tooltips
  (setq use-system-tooltips nil)

;;;;;; windows
  (window-divider-mode)

;;;;;; Automatically visit symlink sources
  (setq find-file-visit-truename t)
  (setq vc-follow-symlinks t)

;;;;;; Smooth scrolling in Emacs29
  (if (version< emacs-version "29.0")
      (pixel-scroll-mode)
    (pixel-scroll-precision-mode 1)
    (setq pixel-scroll-precision-large-scroll-height 35.0))

;;;;;; shorthand for interactive lambdas
  (defmacro λ (&rest body)
    "Shorthand for interactive lambdas (BODY)."
    `(lambda ()
       (interactive)
       ,@body))

;;;;;; pop-to-mark-command
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

;;;;;; transpose lines/words/sexps/params global
  (unbind-key "M-t") ;; which used to be transpose-words
  (unbind-key "C-q")
  (unbind-key "M-z")
  (unbind-key "C-h C-h")

  :bind* (:prefix-map transpose-map
                      :prefix "M-t"
                      :prefix-docstring "transpose map"
                      ("l" . transpose-lines)
                      ("w" . transpose-words)
                      ("s" . transpose-sexps)
                      ("p" . transpose-params))

;;;;;; special character definitions λ
  :bind* (:prefix-map special-char-map
                      :prefix "C-x 8"
                      :prefix-docstring "special char map"
                      ("l" ("λ" . (lambda () (interactive) (insert "\u03bb"))))
                      ("t" ("™" . (lambda () (interactive) (insert "™"))))
                      ("c" ("©" . (lambda () (interactive) (insert "©"))))
                      (">" ("→" . (lambda () (interactive) (insert "→"))))
                      ("8" ("∞" . (lambda () (interactive) (insert "∞"))))
                      ("v" ("✓" . (lambda () (interactive) (insert "✓")))))

;;;;;; sej-C-q bindings
  :bind* (:prefix-map sej-C-q-map
                      :prefix "C-q"
                      :prefix-docstring "SeJ Personal C-q key bindings"
                      ("v" . emacs-version)
                      ("\\" . align-regexp) ;Align your code in a pretty way.
                      ("." . org-time-stamp)
                      ("D" . describe-personal-keybindings)
                      :prefix-map term-map
                      :prefix "C-q S"
                      :prefix-docstring "Term bindings")

  :bind* (:map override-global-map
               ("s-." . pop-to-mark-command)
	           ("M-j" . join-line)
               ("C-x j" . duplicate-dwim)))

;;;;; Simple
;; built-in simple settings
;;
(use-package simple
  :blackout ((visual-line-mode . "")
             (auto-fill-mode . ""))
  :straight (:type built-in)
  :init
  (setq blink-matching-paren 'jump-offscreen
        column-number-mode t
        delete-trailing-lines t
        eval-expression-print-length nil
        eval-expression-print-level nil
        idle-update-delay 1
        kill-do-not-save-duplicates t
        kill-ring-max 300
        kill-ring-deindent-mode t
        track-eol t
        line-move-visual nil
        line-number-mode t
        save-interprogram-paste-before-kill t
        kill-read-only-ok t
        shift-select-mode nil
        set-mark-command-repeat-pop t
        backward-delete-char-untabify-method nil))

;;;;; minibuffer
;; minibuffer settings
;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/minibuffer.el][minibuffer.el]]
(use-package minibuffer
  :no-require t
  :straight (:type built-in)
  :preface
  ;; Set garbage collection threshold
  (defun sej/minibuffer-setup-hook ()
    (setq gc-cons-threshold extended-gc-cons-threshold))

  (defun sej/minibuffer-exit-hook ()
    (setq gc-cons-threshold default-gc-cons-threshold))

  (defun sej/always-exit-minibuffer-first ()
    (if-let ((minibuffer (active-minibuffer-window)))
        (with-current-buffer (window-buffer minibuffer)
          (minibuffer-keyboard-quit))
      (funcall keyboard-quit)))

  :init
  (add-hook 'minibuffer-setup-hook #'sej/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'sej/minibuffer-exit-hook)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (advice-add #'sej/always-exit-minibuffer-first :around 'keyboard-quit)

  :config
  (setq completion-cycle-threshold 3
        completion-flex-nospace nil
        completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (project-file (styles . (basic substring partial-completion orderless)))
          (imenu (styles . (basic substring orderless)))
          (kill-ring (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (eglot (styles . (basic substring orderless)))
          (buffer (styles initials basic))
          (info-menu (styles basic)))
        completions-format 'vertical
        read-answer-short t
        completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        resize-mini-windows t))

;;;;; uniquify
;; built-in package to make buffer names unique but identifiable
(use-package uniquify
  :straight (:type built-in)
  :init
  (setq  uniquify-ignore-buffers-re "^\\*"
         uniquify-buffer-name-style 'post-forward-angle-brackets
         uniquify-strip-common-suffix t
         uniquify-after-kill-buffer-p t
         uniquify-separator "/"))

;;;;; no-littering feature
;; set the default paths for configuration files & persistent data
;; [[https://github.com/emacscollective/no-littering][no-littering]]
(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "~/.local/share/emacs/")
        no-littering-var-directory (expand-file-name "~/.cache/emacs/"))
  (defalias 'nl-var-expand #'no-littering-expand-var-file-name)
  (defalias 'nl-etc-expand #'no-littering-expand-etc-file-name)

  (setq auto-save-file-name-transforms `((".*"
                                          ,(no-littering-expand-var-file-name "auto-save/") t))))


;;; general functions / packages
;;;;; sej/save-macro
;; - save last macro to init file
(defun sej/save-macro (name)
  "Save a macro with NAME as argument; save the macro at the end of your init file."
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
  "Run a shell COMMAND ARGS; return code and its whitespace trimmed output."
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
  "If COMMAND `sej/is-exec' run it with ARGS, return per `sej/exec' or nil."
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
;; (use-package esup
;;   :ensure t)

;;;;; Advice
;; accept versus warn from the Advice system.
;; [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html][Advising Emacs Lisp Functions]]
(use-package advice
  :straight (:type built-in)
  :init
  (setq-default ad-redefinition-action 'accept))

;;;;; calc
;; built-in calculator
;; [[https://www.gnu.org/software/emacs/manual/html_mono/calc.html][GNU Emacs Calculator]]
(use-package calc
  :straight (:type built-in)
  :commands (quick-calc calc)
  :config
  (setq math-additional-units
        '((GiB "1024 * MiB" "Giga Byte")
          (MiB "1024 * KiB" "Mega Byte")
          (KiB "1024 * B" "Kilo Byte")
          (B nil "Byte")
          (Gib "1024 * Mib" "Giga Bit")
          (Mib "1024 * Kib" "Mega Bit")
          (Kib "1024 * b" "Kilo Bit")
          (b "B / 8" "Bit"))))

;;;;; checkdoc
;; built-in checker of buffer for style issues
;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/checkdoc.el][checkdoc.el]]
(use-package checkdoc
  :straight (:type built-in)
  :config
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;;;;; face-remap
;; changes in the appearance of a face.
;; [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html][face remapping]]
(use-package face-remap
  :blackout (buffer-face-mode . "")
  :straight (:type built-in))


;;;; Security
;;;;; Auth-Source
;; built-in authentication source
;; [[https://www.gnu.org/software/emacs/manual/html_mono/auth.html][auth-source docs]]
(use-package auth-source
  :straight (:type built-in)
  :defer t
  :config
  (setq auth-sources `(,(f-expand "~/.ssh/.authinfo.gpg")
                       ,(f-expand "~/.ssh/.authinfo")
                       macos-keychain-generic
                       macos-keychain-internet)
        auth-source-do-cache t))

;;;;; epa
;; EasyPG assistant Emacs native support for GnuPG implementation of the OpenPGP standard
;; [[https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources][Mastering Emacs Keeping Secrets]]
(use-package epa
  :ensure t
  :straight (:type built-in)
  :config
  (setq epa-replace-original-text 'ask)
  ;;(epa-file-enable)
  )

;;;;; epq
;; EasyPG Emacs native support for GnuPG implementation of the OpenPGP standard
;; [[https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources][Mastering Emacs Keeping Secrets]]
(use-package epg
  :straight (:type built-in)
  :config
  (setq epg-pinentry-mode 'loopback))

;;;;; Gnutls
;; GnuTLS is a library that establishes encrypted SSL or TLS connections.
;; [[https://www.gnu.org/software/emacs/manual/html_mono/emacs-gnutls.html][Emacs GnuTLS]]
(use-package gnutls
  :straight (:type built-in)
  :config
  (setq gnutls-verify-error t
        gnutls-min-prime-bits 2048
        tls-checktrust gnutls-verify-error)
  (gnutls-available-p))

;;;;; keychain-environment
;; - set up any SSH or GPG keychains that the Keychain tool has set up for us
;; - https://github.com/tarsius/keychain-environment
(use-package keychain-environment
  :ensure-system-package keychain
  :hook (emacs-startup . keychain-refresh-environment))

;;;;; Emacs-lock
;; built-in package to lock buffer from kill and/or Emacs from exiting
;; [[https://www.emacswiki.org/emacs/ProtectingBuffers][Emacs-Lock Wiki]]
(use-package emacs-lock
  :blackout ""
  :straight (:type built-in))


;;;; help
;;;;; help
;; help mode settings
;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/help-mode.el][help-mode.el]]
(use-package help
  :straight (:type built-in)
  :init
  (add-hook 'help-mode-hook #'visual-line-mode)
  (setq help-window-select 'always)
  (advice-add 'help-window-display-message :override #'ignore))

;;;;; which-key
;; - minibuffer keybinding prompts
;; - https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key")
  :blackout t
  :hook (emacs-startup . which-key-mode)
  :bind* (("C-h h" . which-key-show-top-level)
          ("C-h M-m" . which-key-show-major-mode))
  :commands which-key-mode
  :config
  (setq which-key-use-C-h-commands t
        which-key-separator " "
        which-key-prefix-prefix "+")
  (which-key-setup-side-window-bottom) )

;;;;; discover
;; discover more of Emacs using context menus
;; [[https://github.com/mickeynp/discover.el]]
(use-package discover
  :straight t
  :hook (emacs-startup . global-discover-mode))

;;;;; helpful
;; helpful is an improved help-fns & help-fns+
;; https://github.com/Wilfred/helpful
(use-package helpful
  :straight (helpful :type git :host github :repo "Wilfred/helpful")
  :bind* ( ("C-h C-d" . helpful-at-point)
           ("C-h c" . helpful-command)
           ("C-h k" . helpful-key) ; C-h k
           ("C-h v" . helpful-variable) ; C-h v
           ("C-h f" . helpful-callable) ; C-f v
           ("C-h F" . helpful-function)
           ("C-h M" . helpful-macro))  )


;;; user interface
;;;; themes
;;;;; wombat theme
;; previous preferred theme
                                        ;(load-theme 'wombat)

;;;;; tron-legacy-theme
;; current preferred theme
;; [[https://github.com/ianyepan/tron-legacy-emacs-theme][tron-legacy-theme]]
(use-package tron-legacy-theme
  :demand t
                                        ;:hook (emacs-startup . (lambda () (load-theme 'tron-legacy)))
  :preface
  (setq tron-legacy-theme-vivid-cursor t)
  (setq tron-legacy-theme-dark-fg-bright-comments t)
  (setq tron-legacy-theme-softer-bg nil)
  :config
  (load-theme 'tron-legacy))


;;;; frames
;;;;; frame
;; built-in frame package
(use-package frame
  :straight (:type built-in)
  :bind* (; super combo
          ("s-4" . dired-other-frame)
          ("s-5" . make-frame-command)
          ("s-6" . delete-other-frames)
          ("s-w" . delete-frame)
                                        ; C-x combo
          ("C-x w" . delete-frame)
                                        ; C-q combo
          ("C-q m" . sej/frame-recentre)
          ("C-q F" . toggle-frame-fullscreen)
          ("C-q <up>" . sej/frame-resize-full)
          ("C-q <left>" . sej/frame-resize-l)
          ("C-q <right>" . sej/frame-resize-r)
          ("C-q <S-left>" . sej/frame-resize-l2)
          ("C-q <S-right>" . sej/frame-resize-r2)
                                        ; Alt Meta combo
          ("A-M-m" . sej/frame-recentre)
          ("<A-M-left>" . sej/frame-resize-l)
          ("<A-M-right>" . sej/frame-resize-r)
                                        ; Hyper Control HJKL combo
          ("H-C-f" . toggle-frame-fullscreen)
          ("H-C-j" . sej/frame-resize-full)
          ("H-C-h" . sej/frame-resize-l)
          ("H-C-l" . sej/frame-resize-r)
          ("H-C-S-h" . sej/frame-resize-l2)
          ("H-C-S-l" . sej/frame-resize-r2))
  :init
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1
        frame-title-format '("Emacs - %b")
        icon-title-format frame-title-format
        undelete-frame-mode t
        )

  (blink-cursor-mode -1)
  (unless (display-graphic-p)
    (menu-bar-mode -1))

  ;; Don't open a file in a new frame
  (when (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))

  ;; Resize frame to left half after startup
  (if (display-graphic-p)
      (add-hook 'emacs-startup-hook 'sej/frame-resize-l) )

  (defun sej/frame-resize-full ()
    "Set frame full height and 1/2 wide, position at screen left."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)  (- (display-pixel-width) (if sys/macp (eval 13) (eval 25)))
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height) sej/menu-height)) 1))

  (defun sej/frame-resize-l ()
    "Set frame full height and 1/2 wide, position at screen left."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 14)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height) sej/menu-height)) 1))

  (defun sej/frame-resize-l2 ()
    "Set frame full height and 1/2 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height) sej/menu-height)) 1)  )

  (defun sej/frame-resize-r ()
    "Set frame full height and 1/2 wide, position at screen right."
    (interactive)
    (set-frame-position (selected-frame) (- (truncate (/ (display-pixel-width) 2)) 0) 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 14)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height) sej/menu-height)) 1)  )

  (defun sej/frame-resize-r2 ()
    "Set frame full height and 1/2 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height)) sej/menu-height) 1)  )

  (when sys/mac-x-p
    (setq ns-use-native-fullscreen nil))

  (defun sej/frame-recentre (&optional frame)
    "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
    (interactive)
    (unless (eq 'maximised (frame-parameter nil 'fullscreen))
      (let* ((frame (or (and (boundp 'frame)
                             frame)
                        (selected-frame)))
             (frame-w (frame-pixel-width frame))
             (frame-h (frame-pixel-height frame))
             ;; frame-monitor-workarea returns (x y width height) for the monitor
             (monitor-w (nth 2 (frame-monitor-workarea frame)))
             (monitor-h (nth 3 (frame-monitor-workarea frame)))
             (center (list (/ (- monitor-w frame-w) 2)
                           (/ (- monitor-h frame-h) 2))))
        (apply 'set-frame-position (flatten-list (list frame center)))))))

;;;;; fringe
;; fringe-mode
;; [[https://www.emacswiki.org/emacs/TheFringe][The Fringe wiki]]
;; [[https://emacsredux.com/blog/2015/01/18/customizing-the-fringes/][Customizing the Fringes]]
(use-package fringe
  :straight (:type built-in)
  :if window-system
  :init
  (fringe-mode '(4 . 4)))


;;;; buffers
;;;;; buffer key-bindngs
(bind-keys*
 ("s-s" . save-buffer)
 ("s-y" . bury-buffer)
 ("C-c y" . bury-buffer)
 ("C-c r" . revert-buffer)
 ("s-r" . revert-buffer)
 ("C-x k" . kill-this-buffer)
 ("s-n" . bs-cycle-next) ; buffer cycle next
 ("s-p" . bs-cycle-previous))

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
  (browse-url sej-homepage))
(bind-key* "C-q h" 'sej/browse-homepage)

;;;;; sej/quit-and-kill-auxiliary-windows
(defun sej/quit-and-kill-auxiliary-windows ()
  "Kill buffer and its window on quitting."
  (local-set-key (kbd "q") 'kill-buffer-and-window))
(add-hook 'special-mode 'sej/quit-and-kill-auxiliary-windows)
(add-hook 'compilation-mode-hook 'sej/quit-and-kill-auxiliary-windows)


;;;; scratch buffer
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
(bind-key* "C-q C-s" 'sej/create-scratch-buffer)

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
(use-package window
  :straight (:type built-in)
  :bind* (("s-0" . delete-window)
          ("s-1" . delete-other-windows)
          ("s-2" . split-window-vertically)
          ("s-3" . split-window-right)
          ("s-7" .  (lambda () (interactive)
                      (save-excursion
                        (other-window 1)
                        (quit-window))))

          ;; wind move to multifram window
          ("M-'" . next-multiframe-window)

          ;; movement complementary to windmove / windswap
          ("A-h" . left-char)
          ("A-j" . next-line)
          ("A-k" . previous-line)
          ("A-l" . right-char)

          ;;scroll window up/down by one line
          ("A-n" ("scroll up 1" . (lambda () (interactive) (scroll-up 1))))
          ("A-p" ("scroll down 1" . (lambda () (interactive) (scroll-down 1))))
          )
  :init
  (setq window-combination-resize t
        even-window-sizes 'height-only
        window-sides-vertical nil))

;;;;; ace-window
;; - quickly selecting a window to switch to
;; - C-u prefex to move window
;; - C-u C-u prefex to delete window
;; - https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight (ace-window :type git :host github :repo "abo-abo/ace-window")
  :bind* (("C-x o" . ace-window)
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
  :straight (:type built-in)
  :hook (emacs-startup . winner-mode)
  :commands (winner-undo winner-redo)
  :bind* (("C-c <left>" . winner-undo)
          ("C-c <right>" . winner-redo))
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

;;;;; zoom
;; - replacement for golden-ratio
;; - for managing window sizes
;; - [[https://github.com/cyrus-and/zoom][zoom]]
(use-package zoom
  :hook (emacs-startup . zoom-mode)
  :bind* ("C-x +" . zoom) ; override the key binding of balance-windows
  :blackout
  :custom
  (zoom-mode t)
  (zoom-size '(0.6818 . 0.6818)) ; resize window using the golden ratio
                                        ;(temp-buffer-resize-mode t) ; preserve completion buffer
  (zoom-ignored-major-modes '(reb-mode )) ; ignore these major modes
                                        ;(zoom-ignored-buffer-names '("zoom.el" "init.el")) ; ignore these buffer names
  (zoom-ignored-buffer-name-regexps '("^*calc" "^*makey-key" "^magit:")) ; ignore related windows
  (zoom-ignore-predicates nil)
                                        ; '((lambda () (> (count-lines (point-min) (point-max)) 20)))) ; ignore any buffer less than x lines
  :config
  (defun size-callback () ; resize the buffer according to frame size
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t '(0.5 . 0.5))))
  )


;;;; tabs
;;;;; tab-bar
;; - built-in tabs for virtual desktops
;; - C-x t prefix
;; - [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html][tab bars Emacs manual]]
(use-package tab-bar
  :straight (:type built-in)
  :bind (("M-["  . tab-bar-history-back)
         ("M-]" . tab-bar-history-forward))
  :config
  (setq tab-bar-close-button-show t)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show 2)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode t)
  (tab-bar-history-mode t))


;;;; mode-line
;;;;; doom-modeline
;; - A fancy and fast mode-line inspired by minimalism design
;; - https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :preface
  (setq doom-modeline-hud t
        doom-modeline-project-detection 'auto))

;;;;; minions
;; implements a nested menu that gives access to all minor modes
;; [[https://github.com/tarsius/minions][minions]]
(use-package minions
  :init
  (setq doom-modeline-minor-modes t)
  (minions-mode 1))

;;;;; all-the-icons
;; - NOTE: Must run `M-x all-the-icons-install-fonts' manually on Windows
;; - https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :if (display-graphic-p))


;;; text manipulation
;;;; text manipulation settings
;;;;; saveplace
;; automatically save place in files so return to same place in next session
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/saveplace.el
(use-package saveplace
  :straight (:type built-in)
  :hook (emacs-startup . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t))


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
;;;;; isearch
;; built-in search function
;; [[https://github.com/VernonGrant/discovering-emacs/blob/main/show-notes/3-making-incremental-search-work-for-you.md][good tutorial]]
(use-package isearch
  :straight (:type built-in)
  :init
  (setq search-highlight t
        search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil
        isearch-lazy-highlight t
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        isearch-yank-on-move 'shift
        isearch-allow-scroll 'unlimited
        isearch-repeat-on-direction-change t
        lazy-highlight-initial-delay 0.5
        lazy-highlight-no-delay-length 3
        search-ring-max 30
        regexp-search-ring-max 30
        isearch-wrap-pause t)
  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

;;;;; Anzu
;; good query replace search
;; [[https://github.com/emacsorphanage/anzu][anzu.el]]
(use-package anzu
  :blackout t
  :bind*  (([remap query-replace] . anzu-query-replace-regexp)
           ([remap query-replace-regexp] . anzu-query-replace))
  :init
  (defalias 'qr #'anzu-query-replace)
  (defalias 'qrr #'anzu-query-replace-regexp)
  :config
  (global-anzu-mode))

;;;;; consult-ag
;; - searching with the silver searcher, consult style
;; - [[https://github.com/yadex205/consult-ag][consult-ag.el]]
(use-package consult-ag
  :after consult
  :commands consult-ag
  :bind (:map search-map
              ("a" . consult-ag)))

;;;;; re-builder
;; - set built in regex helper to string format
;; - https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package re-builder
  :straight (re-builder :type built-in)
  :config (setq reb-re-syntax 'read))

;;;;; bookmark+
;; - enhancements to the built-in bookmark package
;; - [[https://www.emacswiki.org/emacs/BookmarkPlus#toc1][bookmarks+]]
(use-package bookmark+
  :init
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28
  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)
  :config
  (setq bookmark-save-flag +1))


;;;; completion
;;;;; abbrev
;; - for inserting abbreviations
;; - https://www.emacswiki.org/emacs/AbbrevMode
(use-package abbrev
  :straight (abbrev :type built-in)
  :hook ((emacs-startup org-mode) . abbrev-mode)
  :blackout
  :config
  (setq abbrev-file-name             ;; tell emacs where to read abbrev
        (nl-var-expand "abbrev_defs") only-global-abbrevs nil)    ;; definitions from...

  (define-abbrev-table
    'org-mode-abbrev-table
    '(("orgh" "" sej/org-header 0)
      ("orgl" "" sej/org-wrap-elisp 0)
      ("orgs" "" sej/org-wrap-source 0))))

;;;;; dabbrev
;; built-in package to let you write just a few characters of words you've written
;; earlier to be able to expand them.
(use-package dabbrev
  :straight (:type built-in)
  :commands (dabbrev-expand
             dabbrev-completion)
  :init
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
        dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|="
        dabbrev-backward-only nil
        dabbrev-case-distinction 'case-replace
        dabbrev-case-fold-search t
        dabbrev-case-replace 'case-replace
        dabbrev-check-other-buffers t
        dabbrev-eliminate-newlines t
        dabbrev-upcase-means-case-search t))

;;;;; hippie-expand
;; - built-in package to expand at point in various ways
;; - https://www.emacswiki.org/emacs/HippieExpand

(use-package hippie-expand
  :straight (hippie-expand :type built-in)
  :bind* ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          cape-elisp-symbol
          try-expand-list-all-buffers
          try-expand-line-all-buffers)))

;;;;; vertico
;; - alternative to ivy, ido, helm
;; - [[https://github.com/minad/vertico][vertico]]
(use-package vertico
  :demand t
  :hook (emacs-startup . vertico-mode)
  :config
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;;;;; vertico-directory
;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  :demand t
  :load-path "straight/build/vertico/extensions/"
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;;; corfu
;; small completion program similar to company
;; [[https://github.com/minad/corfu][corfu]]
(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("<tab>" . corfu-complete)
              ("M-m" . corfu-move-to-minibuffer)
              ("<escape>". corfu-quit)
              ("<return>" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . 'corfu-show-location))
  :hook ((emacs-startup . global-corfu-mode)
         ((eshell-mode markdown-mode) . (lambda ()
                                          (setq-local corfu-auto nil)
                                          (corfu-mode))))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-separator ?\s)             ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)           ;; t, 'separator, nil Never quit
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  (corfu-echo-documentation nil)
  :config
  (defun corfu-move-to-minibuffer ()
    "Move \"popup\" completion candidates to minibuffer.
Useful if you want a more robust view into the recommend candidates."
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (use-package corfu-history
    :straight nil
    :load-path "straight/build/corfu/extensions"
    :demand t
    :init
    (savehist-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)
    :config
    (corfu-history-mode t))
  (use-package corfu-indexed
    :straight nil
    :load-path "straight/build/corfu/extensions"
    :demand t
    :config
    (corfu-indexed-mode t))
  (use-package corfu-popupinfo
    :straight nil
    :load-path "straight/build/corfu/extensions"
    :demand t
    :config
    (corfu-popupinfo-mode t))
  (use-package corfu-terminal
    :unless (display-graphic-p)
    :init
    (corfu-terminal-mode +1)  )
  (use-package corfu-doc-terminal
    :straight (corfu-doc-terminal :type git
                                  :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
    :unless (display-graphic-p)
    :init
    (corfu-doc-terminal-mode +1) ) )

;;;;; cape
;; completion at point extensions
;; [[https://github.com/minad/cape][cape]]
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package cape
  :demand t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind* (("C-c p p" . completion-at-point) ;; capf
          ("C-c p t" . complete-tag)        ;; etags
          ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
          ("C-c p h" . cape-history)
          ("C-c p f" . cape-file)
          ("C-c p k" . cape-keyword)
          ("C-c p s" . cape-elisp-symbol)
          ("C-c p e" . cape-elisp-block)
          ("C-c p a" . cape-abbrev)
          ("C-c p i" . cape-dict)
          ("C-c p l" . cape-line)
          ("C-c p w" . cape-dict)
          ("C-c p \\" . cape-tex)
          ("C-c p _" . cape-tex)
          ("C-c p ^" . cape-tex)
          ("C-c p &" . cape-sgml)
          ("C-c p r" . cape-rfc1345))
  :init
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

;;;;; marginalia
;; Enable richer annotations using the Marginalia package
;; [[https://github.com/minad/marginalia][marginalia]]
(use-package marginalia
  :ensure t
  :hook (emacs-startup . marginalia-mode)
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-max-relative-age 0))

;;;;; orderless
;; provides an orderless completion style that divides the pattern into space-separated components,
;; and matches candidates that match all of the components in any order.
;; [[https://github.com/oantolin/orderless][orderless]]
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;;; prescient
;; sorts and filters lists of candidates
;; [[https://github.com/radian-software/prescient.el][prescient]]
(use-package prescient
  :straight (:type git :host github :repo "radian-software/prescient.el" :files ("*.el"))
  :demand t
  :hook (emacs-startup . prescient-persist-mode)
  :init
  (add-to-list 'completion-styles 'prescient)
  :config
  (use-package vertico-prescient
    :load-path "straight/build/prescient"
    :hook (vertico-mode . vertico-prescient-mode))
  (use-package corfu-prescient
    :load-path "straight/build/prescient"
    :hook (corfu-mode . corfu-prescient-mode)))

;;;;; embark
;; acting on targets
;; [[https://github.com/oantolin/embark/][embark]]
(use-package embark
  :ensure t
  :bind*  (("C-." . embark-act)        ;; pick some comfortable binding
           ("M-." . embark-dwim)        ;; good alternative: M-.
           ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
           ("C-s-e" . embark-export)
           ("H-e" . embark-export))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;; consult
;; - completing read
;; - [[https://github.com/minad/consult][consult]]
(use-package consult
  :demand t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind* (;; C-c bindings (mode-specific-map)
          ("C-c b" . consult-bookmark)
          ("C-c k" . consult-kmacro)
          ("C-c h" . consult-history)
          ("C-c M-x" . consult-mode-command)
          ;; C-x bindings (ctl-x-map)
          ("C-x C-r" . consult-recent-file)
          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
          ("C-x p b" . consult-project-buffer)      ;; switch to buffer within project
          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
          ;; Custom M-# bindings for fast register access
          ("C-q C-y" . consult-register-load)
          ("C-q C-w" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
          ("C-q C-r" . consult-register)
          ;; Other custom bindings
          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
          ;; M-g bindings (goto-map)
          ("M-g e" . consult-compile-error)
          ("M-g f" . consult-flymake)
          ("M-g g" . consult-goto-line)             ;; orig. goto-line
          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          :map search-map
          ("a" . consult-ag)                    ;;if executable ag "Silver-Searcher" exists
          ("f" . consult-find)
          ("L" . consult-locate)
          ("g" . consult-grep)
          ("G" . consult-git-grep)
          ("r" . consult-ripgrep)
          ("l" . consult-line)
          ("m" . consult-multi-occur)
          ("k" . consult-keep-lines)
          ("u" . consult-focus-lines)
          ;; isearch integration
          :map isearch-mode-map
          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
          :map consult-narrow-map
          ("?" . consult-narrow-help)
          ;; Minibuffer history
          :map minibuffer-local-map
          ("M-s" . consult-history)                 ;; orig. next-matching-history-element
          ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"


  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'project-root)
  ;;;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;;;; 5. No project support
  ;; (setq consult-project-function nil)

  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    ;; below is ideal but some do not have .texti files yet
    ;;(consult-info "vertico" "consult" "marginalia" "orderless" "embark" "corfu" "cape" "tempel")
    (consult-info "consult" "marginalia" "orderless" "embark" "corfu" "cape" "tempel")  )

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))


;;;; indentation
;;;;; indentation settings
(setq-default indent-tabs-mode nil
              fill-column 160)

;;;;; dtrt-indent
;; - automatically set the right indent for other people's files
;; - https://github.com/jscheid/dtrt-indent
(use-package dtrt-indent
  :hook (emacs-startup . dtrt-indent-mode)
  :blackout)

;;;;; sej/indent-buffer
;; - bound to C-c <tab>
(defun sej/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key* "C-q <tab>" 'sej/indent-buffer)


;;;; history packages
;;;;; undo-fu
;; - Simple, stable linear undo with redo for Emacs.
;; - https://gitlab.com/ideasman42/emacs-undo-fu
(use-package undo-fu
  :blackout t
  :bind* (("C-z" . undo-fu-only-undo)
          ("C-S-z" . undo-fu-only-redo))
  :config (setq undo-fu-allow-undo-in-region t))

;;;;; undo-fu-session
;; - Save & recover undo steps between Emacs sessions.
;; - https://gitlab.com/ideasman42/emacs-undo-fu-session
(use-package undo-fu-session
  :after undo-fu
  :hook (emacs-startup . global-undo-fu-session-mode))

;;;;; vundo
;; - visual undo displays the undo history as a tree
;; - [[https://github.com/casouri/vundo][vundo]]
(use-package vundo
  :blackout t
  :bind ("C-/" . vundo))

;;;;; recentf
;; - recent file history list settings
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el
(use-package recentf
  :straight (recentf :type built-in)
  :hook (emacs-startup . recentf-mode)
  :init
  (setq recentf-max-saved-items 2000
        recentf-max-menu-items 100
        recentf-auto-cleanup 'never
        recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'"))  )

;;;;; savehist
;; - recent buffer history settings
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el
(use-package savehist
  :straight (savehist :type built-in)
  :hook (emacs-startup . savehist-mode)
  :custom
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t "Allow commands in minibuffers.")
  (history-length 10000)
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)
                                 "each varible is perssted accross Emacs sessions.")
  (savehist-autosave-interval 300)
  (savehist-save-minibuffer-history t))


;;;; movement
;;;;; mwim
;; - better than crux for C-e mwim-end
;; - will cycle between end of code and end-of-code plus comments
;; - https://github.com/alezost/mwim.el
(use-package mwim
  :bind* (("C-a" . mwim-beginning) ; C-a
          ("C-e" . mwim-end))) ; C-e better than crux

;;;;; avy
;; - Jump to things in Emacs tree-style
;; - https://github.com/abo-abo/avy
(use-package avy
  :bind* (("H-'" . avy-goto-char)
          ("M-g l" . avy-goto-line)
          ("H-l" . avy-goto-line)
          ("M-g w" . avy-goto-word-1)
          ("H-w" . avy-goto-word-1)
          ("H-y" . avy-copy-line))
  :config (setq avy-background t))

;;;;; goto-chg
;; - goto the last changes made in buffer
;; - https://github.com/emacs-evil/goto-chg
(use-package goto-chg
  :bind* (("H-." . goto-last-change)
          ("H-," . goto-last-change-reverse)) )

;;;;; beginend
;; - smart moves redefining M-< and M-> for some modes
;; - [[https://github.com/DamienCassou/beginend]]
(use-package beginend               ; smart M-< & M->
  :straight t
  :ensure t
  :demand t
  :blackout beginend-global-mode
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
            (blackout mode))
  (beginend-global-mode))

;;;;; subword
;; - Handling capitalized subwords in a nomenclature
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/subword.el
(use-package subword
  :straight (subword :type built-in)
  :blackout t
  :hook (emacs-startup . subword-mode)
  :config
  ;; this makes forward-word & backward-word understand snake & camel case
  (setq c-subword-mode t)
  (global-subword-mode t))

;;;;; string inflection
;; - underscore -> UPCASE -> Camelcase conversion
;; - https://github.com/akicho8/string-inflection
(use-package string-inflection
  :bind* (("M-u" . string-inflection-all-cycle)))


;;;; regions
;;;;; easy-kill-extras
;; - This package contains extra functions for easy-kill/easy-mark.
;; - Kill & Mark things easily
;; - https://github.com/leoliu/easy-kill
;; - https://github.com/knu/easy-kill-extras.el
(use-package easy-kill-extras
  :bind* (("M-w" . easy-kill) ; M-w
          ([remap mark-sexp] . easy-mark-sexp) ; C-M-<SPC>
          ("M-@" . easy-mark-word) ; M-@
          ([remap zap-to-char] . easy-mark-to-char)) ; M-z
  :init
  (setq easy-kill-alist '((?w word           " ")
                          (?s sexp           "\n")
                          (?l list           "\n")
                          (?d defun          "\n\n")
                          (?D defun-name     " ")
                          (?e line           "\n")
                          (?b buffer "")
                          (?^ backward-line-edge "")
                          (?$ forward-line-edge "")
                          (?< buffer-before-point "")
                          (?> buffer-after-point "")
                          (?f string-to-char-forward "")
                          (?F string-up-to-char-forward "")
                          (?t string-to-char-backward "")
                          (?T string-up-to-char-backward ""))))

;;;;; delsel
;; - Do not delete selection if you insert
;; - https://github.com/typester/emacs/blob/master/lisp/delsel.el
(use-package delsel
  :straight (:type built-in)
  :hook (emacs-startup . delete-selection-mode))

;;;;; rect
;; - Rectangle
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/rect.el
(use-package rect
  :straight (:type built-in))

;;;;; drag-stuff
;; - Drag stuff (lines, words, region, etc...) around
;; - https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :blackout
  :hook (emacs-startup . drag-stuff-global-mode)
  :bind* (("M-<down>" . drag-stuff-down)
          ("C-H-n" . drag-stuff-down)
          ("M-<up>" . drag-stuff-up)
          ("C-H-p" . drag-stuff-up)
          ("H-S-p" . drag-stuff-up))
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode))

;;;; url actions
;;;;; sej/url-insert
;; - improved from jcs (Irreal) blog to copy url from safari and paste at point
;; - https://irreal.org/blog/?p=2895
  (when sys/macp
  (defun sej/url-insert (desc)
    "Retrieve URL from current Safari page and prompt for description.
      Insert an Org link at point."
    (interactive "sLink Description (None to display url): ")
    (let ((link (do-applescript "tell application \"Safari\" to return URL of document 1")))
      (if (> (length desc) 0)
      (insert (format "[[%s][%s]]" (org-trim link) desc))
      (insert (format "[[%s]]" (org-trim link)))) ))

      (bind-key* "C-H-u" 'sej/url-insert))

;;;;; sej/url-git-clone-from-clipboard
;; - from Alvaro Ramirez function to git clone from url in clipboard mods by me
;; - [[http://xenodium.com/emacs-clone-git-repo-from-clipboard/][git-clone-from-clipboard-url]]
(when sys/macp
  (defun sej/url-git-clone-from-clipboard ()
    "Clone git URL in clipboard asynchronously and open in Dired when finished."
    (interactive)
    (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
    (let* ((url (current-kill 0))
           (download-dir (expand-file-name "~/Projects/"))
           (project-dir (concat (file-name-as-directory download-dir)
                                (file-name-base url)))
           (default-directory download-dir)
           (command (format "git clone %s" url))
           (buffer (generate-new-buffer (format "*%s*" command)))
           (proc))
      (when (file-exists-p project-dir)
        (if (y-or-n-p (format "%s exists, delete? " (file-name-base url)))
            (delete-directory project-dir t)
          (user-error "Bailed")))
      (switch-to-buffer buffer)
      (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
      (with-current-buffer buffer
        (setq default-directory download-dir)
        (shell-command-save-pos-or-erase)
        (require 'shell)
        (shell-mode)
        (view-mode +1))
      (set-process-sentinel proc (lambda (process state)
                                   (let ((output (with-current-buffer (process-buffer process)
                                                   (buffer-string))))
                                     (kill-buffer (process-buffer process))
                                     (if (= (process-exit-status process) 0)
                                         (progn
                                           (message "finished: %s" command)
                                           (dired project-dir))
                                       (user-error (format "%s\n%s" command output))))))
      (set-process-filter proc #'comint-output-filter)))

  (bind-key "C-H-c" 'sej/url-git-clone-from-clipboard))

;;;;; ace-link
;; - Quickly follow links
;; - [[https://github.com/abo-abo/ace-link]]
(use-package ace-link
  :bind (("H-u" . ace-link-addr)
         :map org-mode-map
         ("H-u" . ace-link-org))
  :config (ace-link-setup-default))

;;;;; orglink
;; use Org mode links in other modes
;; [[https://github.com/tarsius/orglink][orglink]]
(use-package orglink
  :blackout t
  :straight (:type git :host github :repo "tarsius/orglink")
  :hook (dashboard-mode . global-orglink-mode)
  ;; :config
  ;; (if sys/freebsdp (message "no orglink")
    ;; (add-hook  'emacs-startup #'global-orglink-mode) )
    ;; (add-hook  'emacs-startup #'global-orglink-mode)
  )

;;;;; restclient
;; - Allows query of a restclient with the results left into the buffer
;; - use GET or POST plus http:10.0.1.1/rest/
;; then use C-c to execute (check examples directory for more)
;; - https://github.com/pashky/restclient.el
(use-package restclient)


;;;; highlighting faces fonts
(add-to-list 'default-frame-alist '(cursor-color . "palegoldenrod"))

;;;;; hl-line
;; - Highlight the current line
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/hl-line.el
(use-package hl-line
  :straight (hl-line :type built-in)
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)
         (dashboard-mode . hl-line-mode))
  :config
  (set-face-attribute hl-line-face nil :underline nil))

;;;;; symbol-overlay
;; - Highlight symbols and move between them
;; - https://github.com/wolray/symbol-overlay
(use-package symbol-overlay
  :blackout t
  :defines iedit-mode
  :commands (symbol-overlay-get-symbol
             symbol-overlay-assoc
             symbol-overlay-get-list
             symbol-overlay-jump-call)
  :bind (("C-M-;" . iedit-mode) ;; define Iedit mode so as to remove default message
         ("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :bind-keymap ("C-q s" . symbol-overlay-map)
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode-end . symbol-overlay-mode)))

;;;;; highlight-numbers
;; - hightlight-numbers in a special way
;; - https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;;;;; highlight-quoted
;; [[https://github.com/Fanael/highlight-quoted][highlight-quoted]]
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;;;;; highlight-indent-guides
;; - Highlight indentations
;; - https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :if window-system
  :blackout t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack))

;;;;; rainbow-mode
;; - Colorize color names in buffers
;; - https://github.com/tcrayford/emacs/blob/master/vendor/rainbow-mode.el
(use-package rainbow-mode
  :blackout t
  :hook (prog-mode . rainbow-mode)
  :config
  (setq rainbow-ansi-colors nil
        rainbow-x-colors nil)
  (defun sej/rainbow-mode-in-themes ()
    (when-let* ((file (buffer-file-name))
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))

  (add-hook 'emacs-lisp-mode-hook #'sej/rainbow-mode-in-themes)

  (define-key ctl-x-x-map "c" #'rainbow-mode)) ; C-x x c

;;;;; hl-todo
;; - Highlight TODO and similar keywords in comments and strings
;; - https://github.com/tarsius/hl-todo
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:box t :inherit))))
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
  ;; (setq hl-todo-keyword-faces
  ;;       '(("HOLD" . "#d0bf8f")
  ;;         ("TODO" . "#cc9393")
  ;;         ("NEXT" . "#dca3a3")
  ;;         ("THEM" . "#dc8cc3")
  ;;         ("PROG" . "#7cb8bb")
  ;;         ("OKAY" . "#7cb8bb")
  ;;         ("DONT" . "#5f7f5f")
  ;;         ("FAIL" . "#8c5353")
  ;;         ("DONE" . "#afd8af")
  ;;         ("NOTE" . "#d0bf8f")
  ;;         ("MAYBE" . "#d0bf8f")
  ;;         ("KLUDGE" . "#d0bf8f")
  ;;         ("HACK" . "#d0bf8f")
  ;;         ("TEMP" . "#d0bf8f")
  ;;         ("FIXME" . "#cc9393")
  ;;         ("XXXX*" . "#cc9393")))
  (push 'org-mode hl-todo-include-modes))

;; ;;;;; diff-hl
;; ;; - Highlight uncommitted changes
;; ;; - https://github.com/dgutov/diff-hl
;; (use-package diff-hl
;;   :custom-face
;;   (diff-hl-change ((t (:background "#46D9FF"))))
;;   (diff-hl-delete ((t (:background "#ff6c6b"))))
;;   (diff-hl-insert ((t (:background "#98be65"))))
;;   :bind (:map diff-hl-command-map
;;               ("SPC" . diff-hl-mark-hunk))
;;   :hook ((emacs-startup . global-diff-hl-mode)
;;          (dired-mode . diff-hl-dired-mode))
;;   :config
;;   ;; Highlight on-the-fly
;;   (diff-hl-flydiff-mode 1)
;;   ;; Integration with magit
;;   (with-eval-after-load 'magit
;;     (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;     (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;;;;; volatile-highlights
;; - Highlight some buffer region operations
;; - https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :blackout t
  :hook (emacs-startup . volatile-highlights-mode))

;;;;; Pulsar
;; [[https://protesilaos.com/emacs/pulsar][pulsar]]
;; small package that temporarily highlights the current line after a given function is invoked
;; additional to built-in pulse
(use-package pulsar
  :bind* (("C-q p p" . pulsar-pulse-line)
          ("C-q p h" . pulsar-highlight-line))
  :hook ((emacs-startup . pulsar-global-mode)
         ((consult-after-jump
           bookmark-after-jump
           magit-diff-visit-file) . pulsar-recenter-top)
         ((consult-after-jump
           imenu-after-jump) . pulsar-reveal-entry)
         (next-error . pulsar-pulse-line-red))
  :config
  ;; Check the default value of `pulsar-pulse-functions'.  That is where
  ;; you add more commands that should cause a pulse after they are
  ;; invoked
  (setq pulsar-pulse-functions (-union pulsar-pulse-functions '(pop-to-mark-command
                                                                flymake-goto-next-error
                                                                flymake-goto-prev-error
                                                                recenter-top-bottom)))
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow))

;;;;; paren
;; - show paren mode
;; - [[https://www.emacswiki.org/emacs/ShowParenMode][paren wiki]]
(use-package paren
  :straight (:type built-in)
  :hook (emacs-startup . show-paren-mode)
  :config
  (setq show-paren-delay 0
        show-paren-style 'parenthesis ; parenthesis, expression, mixed
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t
        show-paren-context-when-offscreen 'child-frame))

;;;;; hideshow
;; built-in mode to hideshow blocks
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html][hideshow minor mode]]
(use-package hideshow
  :blackout (hs-minor-mode . "")
  :straight (:type built-in)
  :hook (prog-mode . hs-minor-mode))

;;;;; outline outshine pretty-outlines
;; - program modes outline much like org-mode "C-c @"" prefix
;; - [[https://www.emacswiki.org/emacs/OutlineMinorMode][outline-minor-mode wiki]]
;; - [[https://github.com/alphapapa/outshine][outshine]]
(use-package outshine
  :blackout t
  :hook ((prog-mode          . outline-minor-mode)
         (outline-minor-mode . outshine-mode))
  :bind ("M-S-<return>" . outshine-insert-heading)
  :config
  (blackout 'outline-minor-mode)
  (setq outline-minor-mode-use-buttons nil
        outline-minor-mode-use-margins nil)
  (custom-theme-set-faces
   'user
   `(outline-1 ((t (:height 1.8 :foreground "#c8d8e3" :weight bold))))
   `(outline-2 ((t (:height 1.5 :foreground "#268bd2" :weight bold))))
   `(outline-3 ((t (:height 1.2 :foreground "#2aa198" :weight bold))))
   `(outline-4 ((t (:height 1.05 :foreground "#818e96" :weight bold)))))  )

(use-package pretty-outlines
  :straight (pretty-outlines :type git :host github :repo "sejgit/pretty-outlines"
                             :files ("*.el"))
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
  (setq pretty-outlines-bullets-bullet-list '(#x2022 ))  )


;;; programming
;;;;; prog-mode
;; - generalized program mode
;; - Prettify Symbols
;; - e.g. display “lambda” as “λ”
(use-package prog-mode
  :no-require t
  :straight (:type built-in)
  :init
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
  (setq prettify-symbols-unprettify-at-point 'right-edge
        global-prettify-symbols-mode t
        lisp-prettify-symbols-alist prettify-symbols-alist)
  ;;(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'prog-mode-hook #'show-paren-mode)
  (add-hook 'prog-mode-hook #'prettify-symbols-mode)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

;;;;; tree-sitter
;; Emacs Lisp binding for tree-sitter, an incremental parsing library.
;; Important this is using the built-in Emacs version tree-sit.el
;; [[https://github.com/emacs-tree-sitter/elisp-tree-sitter][elisp-tree-sitter]]
(use-package treesit
  ;; Emacs Lisp binding for tree-sitter, an incremental parsing library.
  ;; Important this is using the built-in Emacs version tree-sit.el
  ;; [[https://github.com/emacs-tree-sitter/elisp-tree-sitter][elisp-tree-sitter]]
  :straight (:type built-in)
  :config
  (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter_intel" "~/.emacs.d/tree-sitter_m1"))  )

(use-package treesit-auto
  ;; some auto features for tree-sit
  ;; [[https://github.com/renzmann/treesit-auto][treesit-auto]]
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  ;; standard movement using treesitter
  ;; [[https://github.com/mickeynp/combobulate][combobulate]]
  ;; use C-c o o for menu
  :straight (:type git :host github :repo "mickeynp/combobulate")
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((                              ;c-ts-mode
                                        ;c++-ts-mode
                                        ;c-or-c++-ts-mode
                                        ;toml-ts-mode
                                        ;csharp-ts-mode
          css-ts-mode
                                        ;java-ts-mode
          js-ts-mode
                                        ;json-ts-mode
          python-ts-mode
                                        ;ruby-ts-mode
                                        ;bash-ts-mode
          yaml-ts-mode
          typescript-ts-mode
          tsx-ts-mode) . combobulate-mode))

;;;;; eglot
;; - simple client for Language Server Protocol servers
;; - https://github.com/joaotavora/eglot
;; - [[https://joaotavora.github.io/eglot/#Eglot-and-LSP-Servers][good how to use post]]
(use-package eglot
  :straight (:type built-in)
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)
         (prog-mode . sej/eglot-ensure-prg))

  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c o" . eglot-code-action-organize-imports)
              ("C-c h" . eglot-help-at-point)
              ("C-c x" . xref-find-definitions))
  :config
  (defun sej/eglot-ensure-prg ()
    "run eglot in all prog except"
    (interactive)
    (if (eq major-mode 'emacs-lisp-mode)
        t
      (eglot-ensure)))

  (add-to-list 'eglot-server-programs
               `((c-mode c++-mode objc-mode cuda-mode c-or-c++-mode
                         c-ts-mode c++-ts-mode c-or-c++-ts-mode) .
                         ,(eglot-alternatives
                           '(("ccls" )
                             ("clang")))))
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives
                                    '(("pyright-langserver" "--stdio")
                                      ("jedi-language-server")
                                      ("pylsp" "pyls") ))))
  (setq help-at-pt-display-when-idle t)
  (setq completion-category-defaults nil)
  (use-package consult-eglot
    :commands consult-eglot-symbols))

;;;;; format-all
;; - auto-format source code in many languages using the same command for all languages
;; - You will need to install external programs to do the formatting
;; - https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :bind* (("C-q f" . format-all-buffer)
          ("A-f" . format-all-buffer)))

;;;;; sh-script
;; shell script mode built-in
;; [[https://www.emacswiki.org/emacs/ShMode][sh-script sh-mode wiki]]
(use-package sh-script
  :straight (:type built-in)
  :preface
  (defun sej/sh-prettify-mode-line ()
    (setq mode-line-process nil)
    (when (eq major-mode 'sh-mode)
      (setq mode-name (capitalize (symbol-name sh-shell)))))

  (defun sh-script-extra-font-lock-is-in-double-quoted-string ()
    "Non-nil if point in inside a double-quoted string."
    (let ((state (syntax-ppss)))
      (eq (nth 3 state) ?\")))

  (defun sh-script-extra-font-lock-match-var-in-double-quoted-string (limit)
    "Search for variables in double-quoted strings."
    (let (res)
      (while
          (and (setq res
                     (re-search-forward
                      "\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
                      limit t))
               (not (sh-script-extra-font-lock-is-in-double-quoted-string))))
      res))

  (defvar sh-script-extra-font-lock-keywords
    '((sh-script-extra-font-lock-match-var-in-double-quoted-string
       (2 font-lock-variable-name-face prepend))))

  (defun sh-script-extra-font-lock-activate ()
    (interactive)
    (font-lock-add-keywords nil sh-script-extra-font-lock-keywords)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (when font-lock-mode
        (with-no-warnings
          (font-lock-fontify-buffer)))))
  :init
  (add-hook 'sh-mode-hook #'sej/sh-prettify-mode-line)
  (add-hook 'sh-mode-hook #'sh-script-extra-font-lock-activate)
  :config
  (setq-default sh-basic-offset 2))

;;;;; tramp
;; - remote editing
;; - https://www.gnu.org/software/tramp/
(use-package tramp
  :straight (tramp :type built-in)
  :init
  (setq tramp-default-method "ssh" ; or scp
        tramp-terminal-type "tramp"
        tramp-verbose 3
        tramp-completion-reread-directory-timeout nil
        tramp-histfile-override "/tmp/tramp_history"
        remote-file-name-inhibit-cache nil
        tramp-chunksize 500
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp)
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        ;; don't generate backups for remote files opened as root (security hazard)
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                    (when (stringp method)
                      (member method '("su" "sudo"))))))))
  :config
  (add-to-list 'tramp-default-user-alist '("\\`localhost\\'" "\\`root\\'" "su")))

(use-package tramp-sh
  :straight (tramp-sh :type built-in)
  :config
  (add-to-list 'tramp-remote-path "/usr/local/sbin")
  (add-to-list 'tramp-remote-path "/opt/java/current/bin")
  (add-to-list 'tramp-remote-path "/opt/gradle/current/bin")
  (add-to-list 'tramp-remote-path "~/bin"))

;;;;; ssh-config-mode
;; A mode to edit SSH config files.
;; [[https://github.com/jhgorrell/ssh-config-mode-el][ssh-config-mode]]
;; add this to file for automatic usage: # -*- mode: ssh-config -*-
(use-package ssh-config-mode)

;;;;; indent-guide
;; - show vertical lines to guide indentation
;; - https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :blackout t)

;;;;; New-Comment
;; built-in library contains functions and variables for commenting and
;; uncommenting source code.
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html][comment commands]]
(use-package newcomment
  :straight (:type built-in)
  :init
  (setq comment-empty-lines t
        comment-fill-column 0
        comment-multi-line t
        comment-style 'multi-line))

;;;;; comment-dwim
;; - replacement for the Emacs built-in command comment-dwim
;; - [[https://www.emacs.dyerdwelling.family/emacs/20230215204855-emacs--commenting-uncommenting/][comment-dwim replacement]]
(defun sej/comment-or-uncomment ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)(region-end))
    (comment-or-uncomment-region
     (line-beginning-position)(line-end-position))))
(bind-keys* ("M-;" . sej/comment-or-uncomment)
            ("H-;" . comment-box))

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
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-shell (getenv "$SHELL")))

;;;;; electric
;; electric indent mode
(use-package electric
  :straight (:type built-in)
  :hook ('prog-mode . electric-indent-mode)
  :init
  (setq-default electric-indent-chars '(?\n ?\^?))
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
        electirc-pair-preserve-balance t
        electric-pair-pairs '((8216 . 8217) (8220 . 8221) (171 . 187))
        electric-pair-skip-self 'electric-pair-default-skip-self
        electric-pair-skip-whitespace nil
        electric-pair-skip-whitespace-chars '(9 10 32)
        electric-quote-context-sensitive t
        electric-quote-paragraph t
        electric-quote-string nil
        electric-quote-replace-double t))

;;;;; elec-pair
;; - Automatic parenthesis pairing
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/elec-pair.el
(use-package elec-pair
  :disabled ;; using smartparens for now
  :straight (elec-pair :type built-in)
  :hook (prog-mode . electric-pair-mode)
  :config
  (electric-layout-mode t)
  (electric-indent-mode t))

;;;;; smartparens
;; - minor mode for dealing with pairs in Emacs
;; - [[https://github.com/Fuco1/smartparens][smartparens]]
(use-package smartparens
  :blackout t
  :bind-keymap* ("C-q (" . smartparens-mode-map)
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

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
  :hook (compilation-filter . my-colorize-compilation-buffer)
  :init
  (setq compilation-message-face 'compilation-base-face
        compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error))

;;;;; make-mode
;; Major mode for editing standard Makefiles
;; [[http://doc.endlessparentheses.com/Fun/makefile-mode.html][makefile docs]]
(use-package make-mode
  :blackout ((makefile-automake-mode . "Makefile")
             (makefile-gmake-mode . "Makefile")
             (makefile-makepp-mode . "Makefile")
             (makefile-bsdmake-mode . "Makefile")
             (makefile-imake-mode . "Makefile"))
  :straight (:type built-in)
  :config (setq-local indent-tabs-mode t))

;;;;; dumb-jump
;; - Jump to definition via `ag'/`rg'/`grep'
;; - https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :hook ((emacs-startup . dumb-jump-mode)
         (xref-backend-functions . dumb-jump-xref-activate))
  :config
  (setq dumb-jump-prefer-searcher 'rg))

;;;;; flymake
;; - built-in emacs syntax checker
;; - https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top
(use-package flymake
  :straight (flymake :type built-in)
  :hook (emacs-startup . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! s" . flymake-start)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! d" . flymake-show-project-diagnostics))
  ("H-[" . flymake-goto-prev-error)
  ("H-]" . flymake-goto-next-error)
  ("H-\\" . flymake-show-buffer-diagnostics)
  :init
  (setq flymake-show-diagnostics-at-end-of-line 'short)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)  )

;;;;; flymake-quickdef
;; - package defines flymake-quickdef-backend to generate backends for flymake
;; - [[https://github.com/karlotness/flymake-quickdef][flymake-quickdef]]
(use-package flymake-quickdef)

;; EXAMPLE BACKEND
;;   (require 'flymake-quickdef)
;; (flymake-quickdef-backend
;;   flymake-proselint-backend
;;   :pre-let ((proselint-exec (executable-find "proselint")))
;;   :pre-check (unless proselint-exec (error "Proselint not found on PATH"))
;;   :write-type 'pipe
;;   :proc-form (list proselint-exec "-")
;;   :search-regexp "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.+\\)$"
;;   :prep-diagnostic
;;   (let* ((lnum (string-to-number (match-string 1)))
;;          (lcol (string-to-number (match-string 2)))
;;          (msg (match-string 3))
;;          (pos (flymake-diag-region fmqd-source lnum lcol))
;;          (beg (car pos))
;;          (end (cdr pos)))
;;     (list fmqd-source beg end :warning msg)))
;;
;; (defun flymake-proselint-setup ()
;;   "Enable flymake backend."
;;   (add-hook 'flymake-diagnostic-functions #'flymake-proselint-backend nil t))

;;;;; flycheck
;; - added in emacs syntax checker
;; - https://www.flycheck.org/en/latest/
(use-package flycheck
  :disabled ;; only using flymake for now
  :bind
  (:map flycheck-mode-map
        ("H-[" . flycheck-previous-error)
        ("C-c ! p" . flycheck-previous-error)
        ("H-]" . flycheck-next-error)
        ("C-c ! n" . flycheck-next-error)
        ("C-c ! l" . flycheck-list-errors)
        ("H-\\" . flycheck-list-errors)        )
  :custom-face
  (flycheck-error ((((class color)) (:underline "Red"))))
  (flycheck-warning ((((class color)) (:underline "Orange"))))
  :config
  (defadvice flycheck-next-error (before wh/flycheck-next-error-push-mark activate)
    (push-mark))
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save
                                              mode-enabled
                                              idle-change
                                              idle-buffer-switch))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-flake8-maximum-line-length 79)
  (setq flycheck-highlighting-mode 'lines)
  (progn    (set-face-attribute 'flycheck-warning nil
                                :inherit 'warning
                                :underline nil)
            (set-face-attribute 'flycheck-error nil
                                :inherit 'error
                                :underline nil)))

;;;;; flycheck-popup-tip
;; Flycheck extension minor-mode for displaying errors from Flycheck using popup.el
;; https://github.com/flycheck/flycheck-popup-tip
(use-package flycheck-popup-tip
  :disabled ;; only using flymake for now
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-pos-tip-display-errors-tty-function #'flycheck-popup-tip-show-popup))

;;;;; flycheck-color-mode-line
;; minor-mode for Flycheck which colors the mode line according to
;; the Flycheck state of the current buffer
;; https://github.com/flycheck/flycheck-color-mode-line
(use-package flycheck-color-mode-line
  :disabled ;; only using flymake for now
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

;;;;; emr
;; a framework for providing language-specific refactoring in Emacs.
;; It includes refactoring commands for a variety of languages
;; Just hit M-RET to access your refactoring tools in any supported mode.
;; https://github.com/emacsmirror/emr
(use-package emr
  ;; Just hit H-r to access your refactoring tools in any supported mode.
  :bind* ( ("C-q r" . emr-show-refactor-menu)
           ("H-r" . emr-show-refactor-menu) ))


;;;; vcs
;;;;; Project
;; built-in project management
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html][Working with projects]]
(use-package project
  :straight (:type built-in)
  :config
  (setq project-file-history-behavior 'relativize))

;;;;; magit
;; interface to the version control system Git
;; https://magit.vc/
(use-package magit
  :bind (("C-x g" . magit-status)
         ("<f12>" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t
        magit-repository-directories '(("~/Projects" . 1)))

  (if (fboundp 'transient-append-suffix)
      ;; Add switch: --tags
      (transient-append-suffix 'magit-fetch
        "-p" '("-t" "Fetch all tags" ("-t" "--tags")))))

;;;;; forge
;; Access Git forges from Magit
;; To start using Forge in a certain repository visit the Magit status buffer
;; for that repository and type N f f (forge-pull). Alternatively you can use M-x
;; forge-add-repostiory, which makes it possible to add a forge repository without
;; pulling all topics and even without having to clone the respective Git repository.
;; https://github.com/magit/forge
(use-package forge
  :after magit)

;;;;; git-gutter-fringe
;; git fringe
;; [[https://github.com/emacsorphanage/git-gutter-fringe][git-gutter-fringe]]
(use-package git-gutter-fringe
  :blackout t
  :hook (prog-mode . global-git-gutter-mode)
  :init (setq git-gutter:lighter ""))

;;;;; magit-todos
;; Show tasks from commit files
;; https://github.com/alphapapa/magit-todos
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
;; Walk through git revisions of a file
;; https://github.com/emacsmirror/git-timemachine
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
  :straight (:type built-in)
  :blackout
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
             smerge-kill-current)  )

;;;;; browse-at-remote
;; - Open github/gitlab/bitbucket page
;; - https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :bind* ((("C-q B" . browse-at-remote)
           ("C-x v B" . browse-at-remote))
          :map vc-prefix-map
          ("B" . browse-at-remote)))

;;;;; gist
;; - gist client
;; - built-in self help
;; [[https://github.com/KarimAziev/igist][igist github]]
(use-package igist
  :bind (("C-q G" . igist-dispatch))
  :straight (igist
             :repo "KarimAziev/igist"
             :type git
             :host github))

;;;;; git-modes
;; - Emacs major modes for various Git configuration files.
;; - gitattributes-mode , gitconfig-mode , gitignore-mode
;; - https://github.com/magit/git-modes
(use-package git-modes)

;;;;; sej/git-blame-line
;; - Runs `git blame` on the current line and adds the commit id to the kill ring
(defun sej/git-blame-line ()
  "Run `git blame` on the current line and add the commit id to the kill ring."
  (interactive)
  (let* ((line-number (save-excursion
                        (goto-char (pos-bol))
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

(bind-keys* ("C-q b" . sej/git-blame-line)
            ("H-b" . sej/git-blame-line))


;;;; lisp
;;;;; lisp settings
;; - eval do what I mean
;; - taken from here [[http://blog.shanderlam.com/][eval-dwim]]
(defun sej/eval-dwim (arg)
  "Call eval command you want (Do What I Mean).
If the region is active and option `transient-mark-mode' is on, call
`eval-region'. Else, call `eval-last-sexp' using (ARG)."
  (interactive "P")
  (if (and transient-mark-mode mark-active)
	  (eval-region (region-beginning) (region-end))
	(eval-last-sexp arg)))

(bind-keys :map emacs-lisp-mode-map
           ("C-<return>" . sej/eval-dwim)
           ("C-x C-e" . sej/eval-dwim)
           ("H-<return>" . eval-buffer))

(bind-key* "C-q C-e" 'toggle-debug-on-error)

;; use flymake
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))

;;;;; eldoc
;; we don't want this minor mode to be shown in the minibuffer, however
;; we use eldoc to show the signature of the function at point in the minibuffer
;; https://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :blackout t
  :straight (:type built-in)
  :preface
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :hook
  ((prog-mode . turn-on-eldoc-mode))
  :init
  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p 3) )

;;;;; elisp-slime-nav
;; turn on elisp-slime-nav
;; M-. works to jump to function definitions
;; M-, to jump back
;; https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav
  :blackout t
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
  :config
  (unbind-key "C-c C-d d")
  (unbind-key "C-c C-d C-d"))

;;;;; sly
;; replacement repla for slime
;; [[https://github.com/joaotavora/sly][sly]]
(use-package sly
  :hook (lisp-mode . sly-mode)
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        sly-mrepl-history-file-name (nl-var-expand "sly-mrepl-history")))

;;;;; eros
;; - eros-mode will show you the result of evaluating an elisp command
;; as an overlay in your elisp buffer. Try it out with C-x C-e or s-<return>
;; - https://github.com/xiongtx/eros
(use-package eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode))

;;;;; ielm
;; - add a nice popup for ielm
;; - https://www.emacswiki.org/emacs/InferiorEmacsLispMode
(use-package ielm
  :straight (:type built-in)
  :bind (("s-i" . sej/ielm-other-window))
  :config
  (add-hook 'inferior-emacs-lisp-mode-hook #'hs-minor-mode)

  (defun sej/ielm-other-window ()
    "Run ielm on other window."
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*ielm*"))
    (call-interactively 'ielm)))

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

;;;;; geiser ( guile ) ( closure )
;; Emacs for guile
;; [[https://www.nongnu.org/geiser/geiser_2.html#Installation][geiser]]
;; [[https://jeko.frama.io/en/emacs.html][guile hacking manual]]
(use-package geiser
  :straight (geiser
             :type git
             :host gitlab
             :repo "jaor/geiser"))


;;;; python
;;;;; python
;; Install:
;; pip3 install -U setuptools
;; brew install pyright
;; YAPF or Black
;; http://wikemacs.org/wiki/Python
(require 'python)
(setq python-indent-guess-ident-offset-verbose nil
      python-indent-offset 4
      comment-inline-offset 2)
(cond
 ((executable-find "ipython")
  (setq python-shell-buffer-name "IPython"
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i") )
 ((executable-find "python3")
  (setq python-shell-interpreter "python3"))
 (t
  (setq python-shell-interpreter "python")))
(defun python-ts-mode-setup ()
 "Python TreeSit mode setup."
  (treesit-font-lock-recompute-features
   '(function variable) '(definition)))
(add-hook 'python-ts-mode-hook #'python-ts-mode-setup)

(define-skeleton python-insert-docstring
  "Insert a Python docstring."
  "This string is ignored!"
  "\"\"\"" - "\n\n    \"\"\"")

;;;;; inferior-python-mode
;; runs a python interpreter as a subprocess of Emacs
;; [[http://doc.endlessparentheses.com/Fun/inferior-python-mode.html][inferior-python-mode]]
(use-package inferior-pyton-mode
  :straight (:type built-in))

;;;;; pyenv-mode
;; integration with the pyenv tool
;; [[https://github.com/pythonic-emacs/pyenv-mode][pyenv-mode]]
(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

;;;;; blacken & yapfify
;; Format the python buffer following YAPF rules
;; There's also blacken if you like it better.
(cond
 ((executable-find "black")
  (use-package blacken  ))
 ((executable-find "yapf")
  (use-package yapfify  )))

;;;;; live-py-mode
;; Live Coding in Python
;; Open any Python file, and activate live-py-mode with M-x live-py-mode.
;; You should see an extra window on the right that shows the results of
;; running your code.
;; https://github.com/donkirkby/live-py-plugin
(use-package live-py-mode)

;;;;; ein
;; Emacs IPython Notebook
;; #BEGIN_SRC ein-python :session localhost :results raw drawer
;; import numpy, math, matplotlib.pyplot as plt
;; %matplotlib inline
;; x = numpy.linspace(0, 2*math.pi)
;; plt.plot(x, numpy.sin(x))
;; #+END_SRC

;; Use M-x ein:connect-to-notebook to submit code from an arbitrary
;; buffer to a running jupyter kernel
;; M-x ein:run launches a jupyter process from emacs
;; M-x ein:login to a running jupyter server
;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :blackout t)

;;;;; pip-requirements
;; major mode for editing pip requirement files
;; https://github.com/Wilfred/pip-requirements.el
(use-package pip-requirements)


;;;; web modes
;;;;; web-mode
;; Major mode for editing web templates
;; http://web-mode.org/
;; https://github.com/fxbois/web-mode
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;;;;; css-eldoc
;; eldoc-mode plugin for CSS
;; https://github.com/zenozeng/css-eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;;;;; json-mode
;; Major mode for editing JSON files.
;; Extends the builtin js-mode to add better syntax highlighting for JSON
;; and some nice editing keybindings.
;; https://github.com/joshwnj/json-mode
(use-package json-mode)

;;;;; js2-mode
;; Improved JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

;;;;; js2-refactor
;; JavaScript refactoring library for Emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :after js2-mode
  :blackout t
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;;;;; mocha
;; Run Mocha or Jasmine tests
;; https://github.com/scottaj/mocha.el
(use-package mocha
  :config (use-package mocha-snippets))

;;;;; skewer-mode
;; Live browser JavaScript, CSS, and HTML interaction
;; M-x run-skewer to attach a browser to Emacs
;; C-x C-e: Evaluate the form before the point and display the result in the
;; minibuffer. If given a prefix argument, insert the result into the
;; current buffer.
;; C-M-x: Evaluate the top-level form around the point.
;; C-c C-k: Load the current buffer.
;; C-c C-z: Select the REPL buffer
;; https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :blackout t
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode)))

;;;;; web-beautify
;; Format HTML, CSS and JavaScript/JSON by js-beautify
;; Insta;; npm -g install js-beautify
;; https://github.com/yasuyk/web-beautify
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
    (bind-key "C-c b" #'qweb-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

;;;;; haml-mode
;; major mode for the haml mark-up language
;; https://github.com/nex3/haml-mode
(use-package haml-mode)

;;;;; php-mode
;; major mode for editing PHP code
;; https://github.com/emacs-php/php-mode
(use-package php-mode
  :mode (("\\.module$" . php-mode)
         ("\\.inc$" . php-mode)
         ("\\.install$" . php-mode)
         ("\\.engine$" . php-mode)
         ("\\.\\(?:php\\|phtml\\)\\'" . php-mode)))

;;;;; yaml-mode
;; YAML major mode support
;; https://www.emacswiki.org/emacs/YamlMode
(use-package yaml-mode
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

;;;;; nxml-mode
;; major mode for editing XML
;; https://www.gnu.org/software/emacs/manual/html_node/nxml-mode/Introduction.html
(use-package nxml-mode
  :straight (nxml-mode :type built-in)
  :mode (("\\.xaml$" . xml-mode)))


;;;; c program modes
;;;;; c-mode
;; C/C++ Mode
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(use-package cc-mode
  :straight (cc-mode :type built-in)
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (
         (c-mode-common . (lambda ()
                            (c-set-style "bsd")
                            (setq c-basic-offset 4) )))
  :init
  ;; Set the default formatting styles for various C based modes
  (setq c-default-style
        '((awk-mode . "awk")
          (other . "java"))))

(use-package c-ts-mode
  :straight (c-ts-mode :type built-in)
  :bind (:map c-ts-base-mode-map
              ("C-c c" . compile))
  :init
  ;; Set the default formatting styles for various C based modes
  (setq c-ts-mode-set-global-style 'gnu)
  (setq c-ts-mode-indent-style 'gnu)
  )

;;;;; ccls
;; c++-mode, objc-mode, cuda-mode
;; [[https://github.com/MaskRay/ccls/wiki/lsp-mode][emacs-ccls]]
(use-package ccls

  :hook ((c-mode c++-mode objc-mode cuda-mode c-or-c++-mode
                 c-ts-mode c++-ts-mode c-or-c++-ts-mode) . (lambda () (require 'ccls)))
  :init
  (setq ccls-executable "ccls")
  (setq ccls-initialization-options
        '(:index (:comments 2)
                 :completion (:detailedLabel t)
                 :clang
                 ,(list
                   :extraArgs
                   ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                    "-isysroot/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                    "-isystem/usr/local/include"]
                   :resourceDir (string-trim
                                 (shell-command-to-string
                                  "clang -print-resource-dir")))))

  :config
  (setq ccls-sem-highlight-method 'font-lock)
  ;; alternatively, (setq ccls-sem-highlight-method 'overlay)

  ;; For rainbow semantic highlighting
  (ccls-use-default-rainbow-sem-highlight)


  (defun eglot-ccls-inheritance-hierarchy (&optional derived)
    "Show inheritance for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
    (interactive "P")
    (if-let* ((res (jsonrpc-request
                    (eglot--current-server-or-lose)
                    :$ccls/inheritance
                    (append (eglot--TextDocumentPositionParams)
                            `(:derived ,(if derived t :json-false))
                            '(:levels 100) '(:hierarchy t))))
              (tree (list (cons 0 res))))
        (with-help-window "*ccls inheritance*"
          (with-current-buffer standard-output
            (while tree
              (pcase-let ((`(,depth . ,node) (pop tree)))
                (cl-destructuring-bind (&key uri range) (plist-get node :location)
                  (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                  (make-text-button (+ (pos-bol 0) depth) (pos-eol 0)
                                    'action (lambda (_arg)
                                              (interactive)
                                              (find-file (eglot--uri-to-path uri))
                                              (goto-char (car (eglot--range-region range)))))
                  (cl-loop for child across (plist-get node :children)
                           do (push (cons (1+ depth) child) tree)))))))
      (eglot--error "Hierarchy unavailable"))))

(use-package eglot-ccls
  :straight (emacs-eglot-ccls
             :type git
             :host codeberg
             :repo "akib/emacs-eglot-ccls")
  :hook (((c-mode c++-mode objc-mode cuda-mode c-or-c++-mode
                  c-ts-mode c++-ts-mode c-or-c++-ts-mode) . eglot-ccls-semhl-mode))
  :init
  (defun sej/eglot-ccls-use-colour ()
    "front end for eglot-ccls-use-default"
    (interactive)
    (eglot-ccls-semhl-use-default-rainbow-highlight)) )

;;;;; clang-format
;; Clang-format emacs integration for use with C/Objective-C/C++.
;; [[https://github.com/sonatard/clang-format][clang-format]]
(use-package clang-format
  :bind (:map c-mode-base-map
              ("C-c v" . clang-format-region)
              ("C-c u" . clang-format-buffer))
  :config
  (setq clang-format-style-option "llvm"))

;;;;; modern-cpp-font-lock
;; Syntax highlighting support for "Modern C++" - until C++20 and Technical Specification
;; [[https://github.com/ludwigpacifici/modern-cpp-font-lock][modern-cpp-font-lock]]
(use-package modern-cpp-font-lock
  :straight (modern-cpp-font-lock
             :type git
             :host github
             :repo "ludwigpacifici/modern-cpp-font-lock")
  :hook (c++-mode . modern-c++-font-lock-mode))

;;;;; csharp-mode
;; mode for editing C# in emacs. It’s based on cc-mode
;; https://github.com/josteink/csharp-mode
(use-package csharp-mode
  :straight (csharp-mode
             :type git
             :host github
             :repo "josteink/csharp-mode"))

;;;;; arduino-mode
;; mode for .ino files only
;; [[https://github.com/stardiviner/arduino-mode/tree/16955f579c5caca223c0ba825075e3573dcf2288][arduino-mode]]
(use-package arduino-mode
  :disabled
  :straight (arduino-mode
             :type git
             :host github
             :repo "sejgit/arduino-mode")
  :hook (arduino-mode . arduino-cli-mode)
  :ensure-system-package arduino-cli
  :config
  (setq arduino-mode-home "~/src/Arduino"
        arduino-executable (expand-file-name "Arduino" "~/Applications/Arduino/Contents/MacOS/")))

;;(remove-hook 'c++mode-hook #'arduino-mode-cli)

;;;;; arduino-cli-mode
;; minor mode for using the excellent new arduino command line interface
;; [[https://github.com/motform/arduino-cli-mode][arduino-cli-mode]]
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
  :hook ((c-mode c++-mode objc-mode cuda-mode c-or-c++-mode
                 c-ts-mode c++-ts-mode c-or-c++-ts-mode) . platformio-conditionally-enable))

;;;;; rust-mode
;; rust language package
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :config (setq rust-format-on-save t))

;;;;; go lang
;; two different ways to configure
;; [[https://arenzana.org/2019/12/emacs-go-mode-revisited/][golang with eglot/lsp]]  [[https://github.com/golang/tools/blob/master/gopls/README.md][glpls documentation]]
;; need to install golang and go get golang/x/tools/gopls
;; [[https://sandyuraz.com/articles/go-emacs/][go-mode native completion]]
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  :config
  (setq compilation-window-height 14)
  (setq compilation-scroll-output t))


;;;; other program modes
;;;;; csv-mode
;; major mode for csv
;; https://www.emacswiki.org/emacs/csv-mode.el
(use-package csv-mode
  :commands (csv-mode
             csv-align-mode)
  :hook (csv-mode . csv-align-mode)
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;;;;; ESS (Emacs Speaks Statistics)
;; ESS configurationEmacs Speaks Statistics
;; Used for R, S-Plus, SAS, Stata and OpenBUGS/JAGS.
;; [[https://ess.r-project.org/][ESS R-project]]
(use-package ess)

;;;;; apple-script
;; for editing apple-script
;; [[https://github.com/tequilasunset/apples-mode][apples-mode]]
(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'")


;;; files
;;;;; ibuffer
;; operate on buffers much in the same manner as Dired.
;; https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :straight (ibuffer :type built-in)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon)
  :commands ibuffer-find-file
  :bind* ( ("C-x C-b" . ibuffer))
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
                              (mark " " (name 16 -1) " " filename)))))  )

;;;;; registers
;; Registers allow you to jump to a file or other location quickly.
;; (i for init.el, r for this file) to jump to it.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html
(set-register ?c '(file . "~/.ssh/custom-post.el"))
(set-register ?d '(file . "~/.dotfiles/"))
(set-register ?e '(file . "~/.emacs.d/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))

;;;;; dashboard
;; all-in-one start-up screen with current files / projects
;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :if (eq sej-dashboard t)
  :straight (dashboard :type git
                       :flavor melpa
                       :files (:defaults "banners" "dashboard-pkg.el")
                       :host github :repo "emacs-dashboard/emacs-dashboard")
  :blackout (dashboard-mode)
  :commands sej/open-dashboard
  :hook (emacs-startup . sej/open-dashboard)
  :bind* (("<f6>" . sej/open-dashboard)
          ("C-q d" . sej/open-dashboard))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner (locate-user-emacs-file "emacs.png"))
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-backend 'project-el) ; use projectile if using
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
    (switch-to-buffer "*dashboard*")
    (hl-line-mode t)) )

;;;;; page-break-lines
;; display ^L page breaks as tidy horizontal lines
;; https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :blackout t
  :hook ((dashboard-mode
          text-mode
          comint-mode
          helpful-mode
          help-mode
          compilation-mode
          emacs-news-view-mode) . page-break-lines-mode))

;;;;; autoinsert
;; mode that comes with Emacs that automagically inserts text into new buffers
;;   based on file extension or the major mode
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/autoinsert.el
(use-package autoinsert
  :straight (:type built-in)
  :commands (auto-insert)
  :hook (emacs-startup . auto-insert-mode)
  :preface
  (defun sej/autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas/expand-snippet (buffer-string) (point-min) (point-max)))
  :init
  (setq auto-insert-directory "~/.emacs.d/templates/")
  :config
  (setq auto-insert 'other
        auto-insert-directory (nl-etc-expand "autoinsert/")))

;;;;; autorevert
;; watch for changes in files on disk
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html][autorevert man]]
(use-package autorevert
  :straight (:type built-in)
  :hook (emacs-startup . global-auto-revert-mode)
  :init
  (setq auto-revert-use-notify t
        auto-revert-avoid-polling t
        auto-revert-verbose nil
        global-auto-revert-non-file-buffers t
        revert-without-query '(".*")))

;;;;; sej/create-non-existent-directory
;; Offer to create parent directories if they do not exist
;; automatically run after save
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun sej/create-non-existent-directory ()
  "Ask to make directory for file if it does not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p? (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'sej/create-non-existent-directory)

;;;;; sudo-edit
;; Open files as sudo
;; https://github.com/nflath/sudo-edit
(unless sys/win32p
  (use-package sudo-edit))

;;;;; vlf-setup
;; vlf lets you handle very large files for viewing
;; VLF operations are grouped under the C-c C-v prefix by default
;; https://github.com/m00natic/vlfi
(use-package vlf-setup
  :straight (vlf :host github
                 :repo "m00natic/vlfi")
  :commands (vlf vlf-occur-load vlf-ediff-files))


;;; dired
;;;;; dired
;; Directory operations
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html#Dired
(use-package dired
  :straight (dired :type built-in)
  :hook (dired-mode . hl-line-mode)
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-AFGhlv --group-directories-first")

  (setq dired-dwim-target t
        dired-auto-revert-buffer #'dired-directory-changed-p
        dired-free-space nil
        dired-mouse-drag-files t)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    ;; Use GNU ls as `gls' from `coreutils' if available.
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (when (executable-find "gls")
      (setq insert-directory-program (executable-find "gls")
            dired-use-ls-dired t) ))  )

;;;;; dired-aux
;; auxiliary functionality of dired
;; https://github.com/jwiegley/emacs-release/blob/master/lisp/dired-aux.el
(use-package dired-aux
  :straight (dired-aux :type built-in)
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)  )

;;;;; dired-x
;; Extra Dired functionality
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/
(use-package dired-x
  :straight (:type built-in)
  :demand t
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

;;;;; all-the-icons-dired
;; - Shows icons in dired buffer
;; - https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :blackout t
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground unspecified))))
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
            ls           (forward-line 1))))))
  (advice-add #'all-the-icons-dired--display :override #'my-all-the-icons-dired--display))

;;;;; quick-preview
;; - Quick-preview provides a nice preview of the thing at point for files.
;; - https://github.com/myuhe/quick-preview.el
(use-package quick-preview
  :bind* ( ("C-q q" . quick-preview-at-point)
           :map dired-mode-map
           ("Q" . quick-preview-at-point)))

;;;;; browse-at-remote
;; - browse file at remote source
;; - https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :bind* ("C-q b" . browse-at-remote))

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
  :defines deft-text-mode
  :bind* ( ("C-q C-d" . deft))
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
  :bind* ( ("C-q g w" . writegood-mode)
           ("C-q g g" . writegood-grade-level)
           ("C-q g e" . writegood-reading-ease))
  :hook (markdown-mode . writegood-mode) )

;;;;; markdown-mode
;; - markdown-mode used a lot on Github
;; - https://jblevins.org/projects/markdown-mode/
;; - https://leanpub.com/markdown-mode
(use-package markdown-mode
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . visual-line-mode)
         (markdown-mode . writegood-mode)
         (markdown-mode . flymake-markdownlint-setup)
         (markdown-mode . flymake-mode))
  :functions writegood-mode
  :commands (markdown-mode gfm-mode)
  :mode   (("README\\.md\\'" . gfm-mode)
           ("github\\.com.*\\.txt\\'" . gfm-mode)
           ("\\.md\\'"          . markdown-mode)
           ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-header-scaling t)
  (setq markdown-command "pandoc -f markdown -t html")

;;;;; markdown-toc
  ;; - Table of contents
  ;; - Inside a markdown file, the first time,
  ;; place yourself where you want to insert the TOC:
  ;; M-x markdown-toc-generate-toc
  ;; - https://github.com/ardumont/markdown-toc
  (use-package markdown-toc))

;;;;; flymake-markdownlint-backend
;; using flymake-quickdef to make a flymake backend for markdownlint-cli
;; brew install markdownlint-cli
(require 'flymake-quickdef)
(flymake-quickdef-backend flymake-markdownlint-backend
  :pre-let ((markdownlint-exec (executable-find "markdownlint")))
  :pre-check (unless markdownlint-exec (error "Markdownlint-cli not found on PATH"))
  :write-type 'pipe
  :proc-form (list markdownlint-exec "-s")
  :search-regexp "^[^:]*:\\([[:digit:]]+\\):*\\([[:digit:]]* *\\)\\(MD[[:digit:]]\\{3\\}\\)\\(.*\\)$"
  :prep-diagnostic
  (let* ((lnum (string-to-number (match-string 1)))
         (lcol (string-to-number (match-string 2)))
         (code (match-string 3))
         (text (match-string 4))
         (pos (flymake-diag-region fmqd-source lnum lcol))
         (beg (car pos))
         (end (cdr pos))
         (msg (format "%s (%s)" code text )))
    (list fmqd-source beg end :warning msg)))

(defun flymake-markdownlint-setup ()
  "Enable flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-markdownlint-backend nil t))

;;;;; markdown-soma
;; realtime preview by eww
;; install soma first in the .cargo directory (my dotfiles has path for this)
;; - [[https://github.com/jasonm23/markdown-soma][markdown-soma]]
(use-package markdown-soma
  :hook (markdown-mode . markdown-soma-mode)
  :bind (:map markdown-mode-command-map
              ("p" . markdown-soma-mode))
  :config
  ;; select css theme
  (setq markdown-soma-custom-css
        (markdown-soma--css-pathname-from-builtin-name "markdown-soma-dark"))
  ;; Change "theme name" to the selected highlightjs theme.
  (setq markdown-soma-highlightjs-theme "github-dark"))

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
  "Delete text in region-rectangle; number (START to END with FORMAT-STRING FROM)."
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
    (cl-loop with column = (current-column)
             while (and (<= (point) end) (not (eobp)))
             for i from from   do
             (move-to-column column t)
             (insert (format format-string i))
             (forward-line 1)))
  (goto-char start))

(bind-keys* ("C-q N" . sej/number-rectangle)
            ("C-x r N" . sej/number-rectangle))

;;;;; flyspell
;; - main spelling package
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
(use-package flyspell
  :straight (:type built-in)
  :blackout t
  :bind* (("s-$" . ispell-word) ; M-$ doesn't work well in osx due to keybindings
          :map ctl-x-x-map
          ("s" . flyspell-mode)
          ("w" . ispell-word))
  :functions
  flyspell-correct-word
  flyspell-goto-next-error
  :ensure-system-package aspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :config
  (setq flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (cond
   ((executable-find "aspell")
    (setq-default ispell-program-name "aspell")
    (push "--sug-mode=ultra" ispell-extra-args))
   ((executable-find "enchant-2")
    (setq-default ispell-program-name "enchant-2"))
   ((executable-find "hunspell")
    (progn (setq-default ispell-program-name "hunspell")
           (setq ispell-really-hunspell t)))   )

  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word) ;;for mac
  (define-key flyspell-mouse-map [mouse-3] #'undefined)

  (setq ispell-personal-dictionary "~/sej.ispell"
        ispell-silently-savep t)

  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (setq ispell-dictionary-alist '(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_GB-ise") nil utf-8)
                                  ("canadian" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_CA") nil utf-8)
                                  ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))

  (setq ispell-dictionary "canadian")
  )

;;;;; dictionary lookup
;; - built in with Emacs 28
;; - [[https://www.masteringemacs.org/article/wordsmithing-in-emacs?utm_source=newsletter&utm_medium=email&utm_campaign=rss&utm_source=Mastering+Emacs+Newsletter&utm_campaign=3a391dbdb1-MASTERING_EMACS_NEW_POSTS&utm_medium=email&utm_term=0_777fab9be9-3a391dbdb1-355707361][mastering Emacs]]
(use-package dictionary
  :straight (:type built-in)
  :bind (("M-#" . dictionary-lookup-definition) ; use esc # in osx
         ("C-#" . dictionary-search))
  :config
  ;; mandatory, as the dictionary misbehaves!
  (setq switch-to-buffer-obey-display-actions t
        dictionary-server "dict.org")
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*" display-buffer-in-side-window
                 (side . left)
                 (width . 50))))

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
      (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf")))    ))

;;;;; pdf-tools
;; - PDF reader
;; - https://github.com/politza/pdf-tools
(when (display-graphic-p)
  (use-package pdf-tools
    :blackout (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :preface
    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when sys/macp
      (setenv "PKG_CONFIG_PATH"
              (mapconcat 'identity '( "/usr/local/lib/pkgconfig"
                                      "/usr/local/Cellar/libffi/3.4.2/lib/pkgconfig"
                                      "/usr/local/Cellar/zlib/1.2.11/lib/pkgconfig") ":")))
    :config
    (pdf-tools-install t nil t t)
    (setq pdf-view-midnight-colors '("#ededed" . "#21242b"))
    (setq pdf-annot-activate-created-annotations t)

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

;;;;; annotate
;; - add annotations to arbitrary files without changing the files themselves.
;; - [[https://github.com/bastibe/annotate.el][annotate]]
(use-package annotate
  :straight (annotate :type git
                      :host github
                      :repo "bastibe/annotate.el")
  :commands (annotate-annotate
             sej/annotate-annotate
             annotate-goto-next-annotation
             annotate-goto-previous-annotation
             annotate-export-annotations
             annotate-integrate-annotations
             annotate-show-annotation-summary)
  :bind* ((("C-q C-a" . sej/annotate-annotate-dwim)
           ("C-q C-S-a" . annotate-show-annotation-summary)
           ("C-q ]" . annotate-goto-next-annotation)
           ("C-q [" . annotate-goto-previous-annotation))
          (:map annotate-mode-map
                ("C-q C-a" . sej/annotate-annotate-dwim)
                ("C-q C-S-a" . annotate-show-annotation-summary)
                ("C-q ]" . annotate-goto-next-annotation)
                ("C-q [" . annotate-goto-previous-annotation)) )
  :config
  (setq annotate-file (expand-file-name "annotations" user-emacs-directory))
  (setq annotate-annotation-column 73)
  (setq annotate-diff-export-context 5)
  (setq annotate-use-messages nil)
  (setq annotate-integrate-marker "")
  (setq annotate-integrate-higlight ?^)
  (setq annotate-fallback-comment "#")
  (setq annotate-blacklist-major-mode '())
  (setq annotate-annotation-max-size-not-place-new-line 50)
  (setq annotate-search-region-lines-delta 4)
  (setq annotate-annotation-position-policy :by-length)
  (setq annotate-summary-ask-query nil)


  (defun sej/annotate-mode ()
    "Toggles `annotate-mode' for the current buffer."
    (interactive)
    (if (bound-and-true-p annotate-mode)
        (annotate-mode -1)
      (annotate-mode 1)))

  (defun sej/annotate-annotate ()
    "Ensure `annotate-mode' is enabled for `annotate-annotate'."
    (unless (bound-and-true-p annotate-mode)
      (annotate-mode 1))
    (annotate-annotate))

  (defun sej/annotate-annotate-dwim (&optional arg)
    "Common points of entry for annotations.
Write an annotation or toggle `annotate-mode' by prefixing this
function with the \\[universal-argument]."
    (interactive "P")
    (if arg
        (sej/annotate-mode)
      (sej/annotate-annotate))))

;;;;; yequake
;; - configurable drop-down Emacs frames.
;; - run a shortcut with:  emacsclient -n -e '(yequake-toggle "org-capture")'
;; - [[https://github.com/alphapapa/yequake][alphapapa/yequake]]
(use-package yequake
  :straight (yequake
             :type git
             :host github
             :repo "alphapapa/yequake")
  :custom
  (yequake-frames
   '(("org-capture"
      (buffer-fns . (yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)))))))


;;;; LaTex
;;;;; AuCTeX
;; GNU TeX in Emacs
;; [[https://www.gnu.org/software/auctex/download-for-macosx.html][AUCTeX ]]
(use-package tex
  :straight auctex
  :hook ((LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . flymake-mode)
         (LaTeX-mode . reftex-mode)))

;;;;; LaTeX preview pane
;; side by side preview
;; [[https://www.emacswiki.org/emacs/LaTeXPreviewPane][latex-preview-pane wiki]]
(use-package latex-preview-pane)

;;;;; auctex-latexmk
;; This library adds LatexMk support to AUCTeX.
;; [[https://github.com/tom-tan/auctex-latexmk][auctex-latexmk]]
(use-package auctex-latexmk
  :after tex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

;;;;; bibtex
;; bibliography editing
;; [[https://lucidmanager.org/productivity/emacs-bibtex-mode/][bibtex]]
(use-package bibtex
  :straight nil
  :init
  (setq-default bibtex-dialect 'biblatex))

;;;;; biblio
;; An extensible Emacs package for browsing and fetching references
;; [[https://github.com/cpitclaudel/biblio.el][biblio]]
(use-package biblio)

;;;;; reftex
;; RefTeX is a package for managing Labels, References, Citations and index entries with GNU Emacs.
;; [[https://www.gnu.org/software/auctex/manual/reftex.html][reftex]]
(use-package reftex
  :init
  (setq reftex-plug-into-AUCTeX t))

;;;;; CDLaTex
;; fast insertion of environment templates and math stuff in LaTeX
;; [[https://github.com/cdominik/cdlatex][cdlatex]]
(use-package cdlatex)


;;;; org
;;;;; org
;; - org mode for keeping notes, maintaining lists, planning
;; - https://orgmode.org/
;; (straight-use-package
;;  '(org-plus-contrib
;;    :repo "https://code.orgmode.org/bzg/org-mode.git"
;;    :local-repo "org"
;;    :files (:defaults "contrib/lisp/*.el")
;;    :includes (org)))

(use-package org
  :straight nil
  :defines
  org-agenda-span
  org-agenda-skip-scheduled-if-deadline-is-shown
  org-agenda-todo-ignore-deadlines
  org-agenda-todo-ignore-scheduled
  org-agenda-sorting-strategy
  org-agenda-skip-deadline-prewarning-if-scheduled
  :mode ("\\.org$" . org-mode)
  :hook ( (org-mode . flyspell-mode)
          (org-mode . writegood-mode)
          (org-mode . visual-line-mode)
          (org-mode . org-num-mode))
  :bind* (( ("C-c l" . org-store-link)
            ("C-c c" . org-capture)
            ("C-c a" . org-agenda) )
          (:map org-mode-map
                ("C-M-\\" . org-indent-region)
                ("S-<left>" . org-shiftleft)
                ("S-<right>" . org-shiftright)
                ("C-c n" . outline-next-visible-heading)
                ("C-c n" . outline-previous-visible-heading) ))
  :config
  (require 'org)
  (require 'org-capture)
  (setq org-ellipsis "⤵")
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
        org-adapt-indentation nil
        org-special-ctrl-a/e nil
        org-special-ctrl-k nil
        org-M-RET-may-split-line '((default . nil))
        org-hide-emphasis-markers nil
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t
        org-default-notes-file org-file-notes
        org-capture-bookmark t
        org-refile-use-outline-path 'file
        org-log-done 'note
        org-todo-keywords '((sequence "TODO(t)" "MAYBE(m)" "WAITING(w@/!)" "|" "CANCELED(c@)" "DONE(d!)")
                            (sequence "DELIGATE(D)" "CHECK(C)" "|" "CANCELED(x)" "VERIFIED(V)"))
        org-todo-keyword-faces '(("TODO" . (:foreground "pink" :weight bold :height 0.5))
                                 ("MAYBE" . (:foreground "blue" :weight bold :height 0.5))
                                 ("WAITING" . (:foreground "blue" :weight bold :height 0.5))
                                 ("DONE" . (:foreground "green" :weight bold :height 0.5))
                                 ("DELIGATE" . (:foreground "blue" :weight bold :height 0.5))
                                 ("VERIFIED" . (:foreground "green" :weight bold :height 0.5))
                                 ("CANCELED" . (:foreground "grey" :weight bold :height 0.5)))
        org-confirm-babel-evaluate nil
        org-startup-folded nil
        org-highlight-latex-and-related '(latex))

;;;;;; tags
  (setq org-tag-alist ; I don't really use those, but whatever
        '(("meeting")
          ("admin")
          ("emacs")
          ("home")
          ("home-routines")
          ("kids")
          ("purchase")
          ("home-automation")
          ("financial")
          ("electronics")
          ("computer")
          ("projects")
          ("3d-printer")
          ("other")))

  (let* ((variable-tuple
          (if (display-graphic-p)
              (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                    ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                    ((x-list-fonts "Verdana")         '(:font "Verdana"))
                    ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                    (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))
            nil            )          )
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.75 :underline nil))))))

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


  (setq org-refile-targets '((org-file-gtd :maxlevel . 3)
                             (org-file-someday :maxlevel . 1))
        org-deadline-warning-days 7 ;warn me of any deadlines in next 7 days
        org-confirm-babel-evaluate nil )

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
                               (shell . t)                               ))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))

;;;;; org-agenda
;; agenda for todo & calendar items
;; [[https://orgmode.org/manual/Agenda-Views.html][org-agenda manual]]
(use-package org-agenda
  :straight nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-agenda-block-separator nil
        org-agenda-diary-file (concat org-directory "/diary.org")
        org-agenda-files `(,org-directory "~/Documents")
        org-agenda-dim-blocked-tasks 'invisible
        org-agenda-inhibit-startup nil
        org-agenda-show-all-dates t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-on-weekday 1 ; Monday
        org-agenda-start-with-log-mode nil
        org-agenda-tags-column -100
        org-agenda-use-time-grid t
        org-agenda-files (list org-directory)
        org-agenda-window-setup (quote current-window) ;open agenda in current window
        org-agenda-span (quote fortnight) ;show me tasks scheduled or due in next fortnight
        org-agenda-skip-scheduled-if-deadline-is-shown t ;don't show tasks as scheduled
                                        ; if they are already shown as a deadline
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        org-agenda-sorting-strategy ;sort tasks in order of when they are due and then by priority
        (quote
         ((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))) )

;;;;; org-src
;; org src block settings
;; [[https://orgmode.org/manual/Working-with-Source-Code.html][working with source code]]
(use-package org-src
  :straight nil
  :config
  (setq org-src-window-setup 'current-window
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0))

;;;;; org-Ox
;; org mode exporter framework
;; [[https://orgmode.org/worg/exporters/ox-overview.html][org-exporter manual]]
(use-package ox
  :straight nil
  :config
  (setq org-export-with-toc nil
        org-export-headline-levels 8
        org-export-backends '(ascii html latex md)
        org-export-dispatch-use-expert-ui nil
        org-export-coding-system 'utf-8
        org-export-exclude-tags '("noexport" "no_export" "ignore")
        org-export-with-author t
        org-export-with-drawers t
        org-export-with-email t
        org-export-with-footnotes t
        org-export-with-latex t
        org-export-with-properties t
        org-export-with-smart-quotes t
        org-html-html5-fancy t
        org-html-postamble nil))

;;;;; ox-latex
;; latex exporter
;; [[https://orgmode.org/manual/LaTeX-Export.html#LaTeX-Export][LaTeX Export from the Org Mode manual]]
(use-package ox-latex
  :straight nil
  :config
  ;; LaTeX Settings
  (setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f")
        org-latex-remove-logfiles t
        org-latex-prefer-user-labels t
        bibtex-dialect 'biblatex))

;;;;; ox-gfm
;; github flavoured markdown exporter for Org mode
;; [[https://github.com/larstvei/ox-gfm][ox-gfm]]
(use-package ox-gfm
  :straight (:host github :repo "larstvei/ox-gfm"))

;;;;; ox-jdf-report
;; exporter for the jdf style report
;; [[https://githubhelp.com/dylanjm/ox-jdf][ox-jdf style report]]
(use-package ox-jdf-report
  :straight (:host github :repo "dylanjm/ox-jdf"))

;;;;; ox-report
;; meeting report exporter
;; [[https://github.com/DarkBuffalo/ox-report][ox-report]]
(use-package ox-report
  :straight (:host github :repo "DarkBuffalo/ox-report"))

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
  (setq org-superstar-special-todo-items t
        org-superstar-prettify-item-bullets t
        org-superstar-remove-leading-stars t)
  ;; :custom
  ;; (org-superstar-headline--bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  )

;;;;; org-fancy-priorities
;; - displays org priorities as custom strings
;; - https://github.com/harrybournis/org-fancy-priorities
(use-package org-fancy-priorities
  :blackout t
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (unless (char-displayable-p ?❗)
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

;;;;; toc-org
;; - Table of contents updated at save to header with TOC tag
;; - https://github.com/snosov1/toc-org
(use-package toc-org
  :blackout t
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode))
  :bind (:map org-mode-map
              ("C-c C-o" . toc-org-mardown-follow-thing-at-point)))

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
  "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code."
  nil
  > "#+BEGIN_SRC emacs-lisp" \n
  > _ \n
  > "#+END_SRC" \n)

;;;;; sej/org-wrap-source skeleton
;; - skeletons are a kind of yasnippet but they don't mess with keybindings
;; - skeleton to wrap generic babel source
(define-skeleton sej/org-wrap-source
  "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type."
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
  :straight (:type built-in)
  :defines (compilation-last-buffer
            eshell-prompt-function)
  :commands (eshell/alias
             eshell-send-input
             eshell-flatten-list
             eshell-interactive-output-p
             eshell-parse-command
             eshell-command
             eshell)
  :defines eshell-mode-map
  :hook  ( (eshell-mode . (lambda ()
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
                                       ("C-l" . eshell/clear) ))))

  :bind* (("C-q S e" . eshell) )

  :config
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
        eshell-history-append t
        ;; history size
        eshell-history-size 1000
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
  (semantic-mode -1)

  ;; NOTE by Prot 2020-06-16: the following two advice-add snippets
  ;; will need to be reviewed to make sure they do not produce
  ;; undesirable side effects.

  ;; syntax highlighting implementation modified from
  ;; https://emacs.stackexchange.com/questions/50385/use-emacs-syntax-coloring-when-not-in-emacs
  ;;
  ;; This command also makes it possible to, e.g., cat an encrypted and/or
  ;; compressed file.
  (defun contrib/eshell-cat-with-syntax-highlight (&rest args)
    "Like `eshell/cat' but with syntax highlighting.
To be used as `:override' advice to `eshell/cat'."
    (setq args (eshell-stringify-list (flatten-tree args)))
    (dolist (filename args)
      (let ((existing-buffer (get-file-buffer filename))
            (buffer (find-file-noselect filename)))
        (eshell-print
         (with-current-buffer buffer
           (if (fboundp 'font-lock-ensure)
               (font-lock-ensure)
             (with-no-warnings
               (font-lock-fontify-buffer)))
           (let ((contents (buffer-string)))
             (remove-text-properties 0 (length contents) '(read-only nil) contents)
             contents)))
        (unless existing-buffer
          (kill-buffer buffer)))))

  (advice-add 'eshell/cat :override #'contrib/eshell-cat-with-syntax-highlight)

  ;; Turn ls results into clickable links.  Especially useful when
  ;; combined with link-hint.  Modified from
  ;; https://www.emacswiki.org/emacs/EshellEnhancedLS
  (define-button-type 'eshell-ls
    'supertype 'button
    'help-echo "RET, mouse-2: visit this file"
    'follow-link t)

  (defun contrib/electrify-ls (name)
    "Buttonise `eshell' ls file names.
Visit them with RET or mouse click.  This function is meant to be
used as `:filter-return' advice to `eshell-ls-decorated-name'."
    (add-text-properties 0 (length name)
                         (list 'button t
                               'keymap button-map
                               'mouse-face 'highlight
                               'evaporate t
                               'action #'find-file
                               'button-data (expand-file-name name)
                               'category 'eshell-ls)
                         name)
    name)

  (advice-add 'eshell-ls-decorated-name :filter-return #'contrib/electrify-ls))

(use-package esh-module
  :straight (:type built-in)
  :config
  (setq eshell-modules-list             ; Needs review
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix)))

(use-package em-dirs
  :straight (:type built-in)
  :after esh-mode
  :config
  (setq eshell-cd-on-directory t))

(use-package em-tramp
  :straight (:type built-in)
  :after esh-mode
  :config
  (setq password-cache t)
  (setq password-cache-expiry 600))

(use-package em-hist
  :straight (:type built-in)
  :after esh-mode
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

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
        (pop-to-buffer next-error-last-buffer))
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
  "Invoke (view-file) on a file (ARGS).  \"less +42 foo\" will go to line 42 for foo."
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
;; - shortcut for Dired in eshell
(defun eshell/d (&rest args)
  "Shortcut of d for Dired in eshell with ARGS."
  (dired (pop args) "."))

;;;;; eshell/magit
;; - function to open magit-status for the current directory
(defun eshell/magit ()
  "Function to open <magit-status> for the current directory."
  (interactive)
  (magit-status default-directory)
  nil)


;;;; shell
;;;;; shell
;; - basic emacs shell ; eshell is much better
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html
(use-package shell
  :commands shell-command
  :hook ((shell-mode . n-shell-mode-hook)
         (shell-mode . ansi-color-for-comint-mode-on)
         (comint-output-filter-functions . comint-strip-ctrl-m)
         (comint-output-filter-functions . comint-truncate-buffer))
  :bind*  ( ("C-q S s" . shell))
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

  (setq system-uses-terminfo t
        ansi-color-for-comint-mode t
        comint-use-prompt-regexp t
        shell-command-prompt-show-cwd nil)

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
  :bind ("C-q p" . shell-pop)
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

;;;;; eat
;; - faster shell integration
;; - [[https://codeberg.org/akib/emacs-eat][eat]]
(use-package eat
  :straight (eat
             :type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  ;; Close the terminal buffer when the shell terminates.
  (setq eat-kill-buffer-on-exit t)
  ;; Enable mouse-support.
  (setq eat-enable-mouse t)
  ;; fixing editing
  (setq eat-term-name "xterm-256color")

  (when (eq system-type 'darwin)
    (define-key eat-semi-char-mode-map (kbd "C-h") #'eat-self-input)
    (define-key eat-semi-char-mode-map (kbd "<backspace>") #'eat-self-input)))

;;;; Other Services
;; a place to put set-ups for Emacs outside services
;;;;; term ansi-term serial-term
;; - built-in basic terminal
;; - [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Terminal-emulator.html#Terminal-emulator][Emacs manual]]
(use-package term
  :straight (:type built-in)
  :commands (term ansi-term serial-term)
  :bind* ( ("C-q S a" . ansi-term)
           ("C-q S S" . serial-term)
           ("C-q S t" . term))
  :config
  (setq term-buffer-maximum-size 9999)
  (setq term-completion-autolist t)
  (setq term-completion-recexact t)
  (setq term-scroll-to-bottom-on-output nil))

;;;;; vterm
;; - fully-fledged terminal emulator inside GNU Emacs
;; - [[https://github.com/akermu/emacs-libvterm][vterm github]]
(use-package vterm
  :commands vterm
  :bind ( ("C-q S v" . vterm))
  :preface
  (setq vterm-install t
        vterm-always-compile-module t)
  :config
  (setq vterm-disable-bold-font nil
        vterm-disable-inverse-video nil
        vterm-use-vterm-prompt-detection-method t
        vterm-disable-underline nil
        vterm-kill-buffer-on-exit t
        vterm-max-scrollback 9999
        vterm-shell "/bin/zsh"
        vterm-term-environment-variable "xterm-256color"))

;;;;; ERC IRC client
;; - set-up of built-in irc client
;; - [[https://www.gnu.org/software/emacs/manual/html_mono/erc.html#Top][ERC]]
(use-package erc
  :straight (:type built-in)
  :bind* ("C-q I" . sej/erc-dwim)
  :config
  ;; from [[https://www.emacswiki.org/emacs/ErcSSL][emacswiki.org erc-tls hack]]
  ;; erc hack for gnutls for client cert.
  (defvar *uconf/erc-certs* nil
    "erc client certs used by gnutls package for :keylist.")

  ;; copied from the gnutls lib but set :keylist to client certs.
  ;; this function is called from `open-network-stream' with :type tls.
  (defun uconf/open-gnutls-stream (name buffer host service &optional nowait)
    (let ((process (open-network-stream
                    name buffer host service
                    :nowait nowait
                    :tls-parameters
                    (and nowait
                         (cons 'gnutls-x509pki
                               (gnutls-boot-parameters
                                :type 'gnutls-x509pki
                                :keylist *uconf/erc-certs* ;;added parameter to pass the cert.
                                :hostname (puny-encode-domain host)))))))
      (if nowait
          process
        (gnutls-negotiate :process process
                          :type 'gnutls-x509pki
                          :keylist *uconf/erc-certs* ;;added parameter to pass the cert.
                          :hostname (puny-encode-domain host)))))

  ;; only set the global variable when used from `erc-tls'.
  (defun uconf/erc-open-tls-stream (name buffer host port)
    (unwind-protect
        (progn
          (setq *uconf/erc-certs* sej-tls-certfile)
          (open-network-stream name buffer host port
                               :nowait t
                               :type 'tls))
      (setq *uconf/erc-certs* nil)))

  (advice-add 'open-gnutls-stream :override #'uconf/open-gnutls-stream)
  (advice-add 'erc-open-tls-stream :override #'uconf/erc-open-tls-stream)

  (setq erc-prompt-for-password nil)

  (defun sej/erc-dwim ()
    "Switch to latest `erc' buffer or log in."
    (interactive)
    (let* ((irc "irc.libera.chat")
           (nick sej-irc-nick)
           (pass sej-irc-pass)
           (bufs (erc-buffer-list)))
      (if bufs
          (erc-track-switch-buffer 1)
        (erc-tls :server irc
                 :port 6697
                 :nick nick
                 :password pass
                 :full-name nick
                 )
        ;;       (erc :server irc
        ;;          :port 6697
        ;;        :nick nick
        ;;      :password pass
        ;;    :full-name nick)
        )))

  ;; Options

  ;; Join the #emacs and #erc channels whenever connecting to libera.
  (setq erc-autojoin-channels-alist '(("libera.net" "#emacs" "#erc" "#emacs-chat" "#emacs-til")))

  ;; If non, nil, this is a list of IRC message types to hide, e.g.:
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; If non, nil, this is a list of IRC networks and message types to hide, e.g.:
  (setq erc-network-hide-list '(("JOIN" "PART" "QUIT")
                                ("OFTC" "JOIN" "PART")))

  ;; If non, nil, this is a list of IRC channels and message types to hide, e.g.:
  (setq erc-channel-hide-list '(("#erc" "JOIN" "PART" "QUIT")
                                ("#emacs" "NICK")))


  ;; Rename server buffers to reflect the current network name instead
  ;; of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
  ;; This is useful when using a bouncer like ZNC where you have multiple
  ;; connections to the same server.
  (setq erc-rename-buffers t)

  ;; Interpret mIRC-style color commands in IRC chats
  (setq erc-interpret-mirc-color t)

  ;; The following are commented out by default, but users of other
  ;; non-Emacs IRC clients might find them useful.
  ;; Kill buffers for channels after /part
  ;; (setq erc-kill-buffer-on-part t)
  ;; Kill buffers for private queries after quitting the server
  ;; (setq erc-kill-queries-on-quit t)
  ;; Kill buffers for server messages after quitting the server
  ;; (setq erc-kill-server-buffer-on-quit t)
  )

;;;;; shr
;; Emacs simple html renderer used by a few tools
;; - [[https://github.com/emacs-mirror/emacs/blob/master/lisp/net/shr.el][shr]]
(use-package shr
  :straight (:type built-in)
  :commands (eww eww-browse-url)
  :config
  (setq shr-use-fonts t)
  (setq shr-use-colors t)
  (setq shr-max-image-proportion 0.2)
  (setq shr-image-animate t)
  (setq shr-width (current-fill-column)))

;; Support the HTML pre tag with proper syntax highlighting.
(use-package shr-tag-pre-highlight
  :straight (:type built-in)
  :disabled t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

;;;;; xwidget webkit
;; NOT USED FOR NOW AS I LIKE USING SAFARI WHEN ON OSX.  LEFT CODE FOR FUTURE REFERENCE
;; use webkit (safari backend) to render urls
;; emacs-plus must be compiled with --with-xwidgets
;; [[https://github.com/veshboo/emacs][config from veshboo webkit]]
;; Built from source on 2023-02-05 at 12:48:48 with: --with-xwidgets --with-no-frame-refocus --with-native-comp --with-poll --with-imagemagick
;; (if sys/mac-x-p
;;     (progn
;;       (setq browse-url-browser-function 'xwidget-webkit-browse-url)
;;       ;; set default url browser
;;       ;; Then, many packages supporting `browse-url` will work with xwidget webkit
;;       ;; For example, try `C-c C-c p` if you are using `markdown-preview`.
;;
;;       ;; search-web withxwidget webkit
;;       (use-package search-web
;;         :bind ("C-c w" . search-web)
;;         :init
;;         (defun browse-url-default-browser (url &rest args)
;;           "Override `browse-url-default-browser' to use `xwidget-webkit' URL ARGS."
;;           (xwidget-webkit-browse-url url args)))
;;
;;       ;; browse to a URL bookmark from Bookmark List
;;       (defvar xwidget-webkit-bookmark-jump-new-session) ;; xwidget.el
;;       (defvar xwidget-webkit-last-session-buffer) ;; xwidget.el
;;       (add-hook 'pre-command-hook
;;                 (lambda ()
;;                   (if (eq this-command #'bookmark-bmenu-list)
;;                       (if (not (eq major-mode 'xwidget-webkit-mode))
;;                           (setq xwidget-webkit-bookmark-jump-new-session t)
;;                         (setq xwidget-webkit-bookmark-jump-new-session nil)
;;                         (setq xwidget-webkit-last-session-buffer (current-buffer))))))
;;       ;; `RET` on a URL bookmark will show the page in the window with current `*Bookmark List*`
;;       ;; It will create a new `xwidget-webkit-mode` buffer if the previous buffer in the selected
;;       ;;   window is not a `xwidget-webkit-mode`.  Otherwise, it will browse in the
;;       ;;   previous `xwidget-webkit-mode` buffer.
;;       ))

;;;;; eww Emacs-web-wowser
;; - Emacs internal web browser
;; - [[https://www.gnu.org/software/emacs/manual/html_mono/eww.html][EWW]]
(use-package eww
  :straight (:type built-in)
  :bind* ( ("C-q W"))
  :config
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%u")
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory "~/.emacs.d/eww-bookmarks/")
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")

  (defun sej/eww-visit-history (&optional arg)
    "Revisit a URL from `eww-prompt-history' using completion.
With \\[universal-argument] produce a new buffer."
    (interactive "P")
    (let ((history eww-prompt-history)  ; eww-bookmarks
          (new (if arg t nil)))
      (eww
       (completing-read "Visit website from history: " history nil t)
       new)))

  ;; eww-view-source
  (defvar prot/eww-mode-global-map
    (let ((map (make-sparse-keymap)))
      (define-key map "s" 'eww-search-words)
      (define-key map "o" 'eww-open-in-new-buffer)
      (define-key map "f" 'eww-open-file)
      (define-key map "w" 'sej/eww-visit-history)
      map)
    "Key map to scope `eww' bindings for global usage.
The idea is to bind this to a prefix sequence, so that its
defined keys follow the pattern of <PREFIX> <KEY>.")
  :bind-keymap ("C-c w" . prot/eww-mode-global-map)
  :bind (:map eww-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("B" . eww-back-url)
              ("N" . eww-next-url)
              ("P" . eww-previous-url)))

;;;;; Message
;; built-in email message editor mode
;; [[https://www.emacswiki.org/emacs/MessageMode][Message Mode wiki]]
(use-package message
  :straight (:type built-in)
  :commands (message-mail)
  :bind* (("C-q M"))
  :config
  (setq send-mail-function 'sendmail-send-it
        sendmail-program "/usr/local/bin/msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        smtpmail-debug-info t
        message-default-mail-headers "Cc: \nBcc: \n"
        message-auto-save-directory (nl-var-expand "mail/drafts")
        user-mail-address sej-mail-address
        user-full-name sej-full-name))

;;;;; Emacs-everywhere
;; allows you to use Emacs editing in any application
;; Use:
;; emacsclient --eval "(emacs-everywhere)"
;; add shortcut for above
;; click "Add Shortcut" and key a shortcut. (I use control-option-command-space)
;; [[https://github.com/tecosaur/emacs-everywhere][emacs-everywhere]]
(use-package emacs-everywhere
  :straight t)


;;; calendar
;;;;; calendar
;; - the built-in calendar
;; - [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html][calendar]]
(use-package calendar
  :straight (:type built-in)
  :bind* ( ("C-q C"))
  :config
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")"))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-christian-all-holidays-flag t)
  (setq calendar-holidays
        (append holiday-local-holidays  ; TODO set local holidays
                holiday-solar-holidays))
  :hook (calendar-today-visible-hook . calendar-mark-today))

;;;;; solar
;; - S in Calendar , Display M-x sunrise-sunset or sunrise-sunset
;; - [[https://github.com/jwiegley/emacs-release/blob/master/lisp/calendar/solar.el][solar.el]]
(use-package solar
  :straight (:type built-in)
  :config
  (setq calendar-latitude 42.838213
        calendar-longitude -83.728748
        calendar-location-name "Fenton, Michigan"))

;;;;; lunar
;; - M in calendar or M-x phases-of-moon
;; - [[https://ftp.gnu.org/old-gnu/Manuals/emacs-21.2/html_node/emacs_423.html][phases of the moon]]
(use-package lunar
  :straight (:type built-in)
  :config
  (setq lunar-phase-names
        '("New Moon"
          "First Quarter Moon"
          "Full Moon"
          "Last Quarter Moon")))

;;;;; TMR (timer)
;; facilities for setting timers using a convenient notation
;; [[https://protesilaos.com/emacs/tmr][tmr manual]]
(use-package tmr
  :bind* (("C-q t" . tmr-prefix-map))
  :config
  ;; Read the `tmr-descriptions-list' doc string
  (setq tmr-descriptions-list 'tmr-description-history)

  (if sys/macp
      (progn
        (defun sej/osx-alert-tmr (timer)
          "function to display TIMER info on osx notification area"
          (interactive)
          (alert
           (tmr--long-description-for-finished-timer timer)
           :title "TMR"
           :severity 'urgent
           :id 'test-alert
           :style 'osx-notifier) )

        (add-to-list 'tmr-timer-finished-functions #'sej/osx-alert-tmr)
        (delete 'tmr-notification-notify tmr-timer-finished-functions))))


;;; ChatGPT
;;;;; openai
;; backend for chatgpt & codegpt
;; [[https://github.com/emacs-openai/openai][emacs-openai]]
(use-package openai
  :straight (openai :type git :host github :repo "emacs-openai/openai")
  :config
  (setq openai-key sej-chatgpt-key
        openai-user sej-chatgpt-user))

(use-package chatgpt
  ;; Emacs code extension to use official OpenAI API
  ;; [[https://github.com/emacs-openai/chatgpt][emacs chatgpt]]
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt")
  :commands chatgpt
  :config
  (setq openai--show-log t))

(use-package codegpt
  ;; Emacs code extension to generate or check code in the official OpenAI API
  ;; [[https://github.com/emacs-openai/codegpt][codegpt]]
  :straight (codegpt :type git :host github :repo "emacs-openai/codegpt")
  :commands codegpt codegpt-custom codegpt-doc codegpt-fix codegpt-expain codegpt-improve)


;;; init.el --- end
(message "init.el ends here")
(provide 'init)
;;; init.el ends here
