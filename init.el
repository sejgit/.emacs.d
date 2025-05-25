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

;;; Commentary:
;;
;; SeJ Emacs configurations.
;;
;; a long journey to here...
;;
;; using Emacs 31
;; on macos installing emacs-plus@31
;; brew install emacs-plus@30 --with-xwidgets --with-native-comp --with-imagemagick --with-dbus
;; or
;; emacs-head@31
;; brew install emacs-head@31 --with-cocoa --with-crash-debug --with-ctags --with-dbus --with-imagemagick --with-mailutils --with-native-comp --with-native-full-aot --with-tree-sitter --with-mps --with-xwidgets
;;

;;; Code:
(message "Emacs start")

;;; initialize environment
;;;;; debug
;; only turned on when needed
(setq debug-on-error nil)
(setq debug-on-event nil)

;;;;; should i even be here
(defconst emacs/>=30p
  ( >= emacs-major-version 30 )
  "Emacs is 30 or above.")
(when (not emacs/>=30p)
  (error "This requires Emacs 29 and above")  )

;;;;; Use-Package set-up
;; https://github.com/jwiegley/use-package
;; https://github.com/emacsmirror/diminish
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; https://github.com/jwiegley/use-package#use-package-ensure-system-package
;; Should set before loading `use-package'
(setq-default use-package-always-defer t
              use-package-compute-statistics t
              use-package-expand-minimally t
              use-package-enable-imenu-support t)

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (setq package-enable-at-startup nil
        load-path (append (list (emacs-path "lisp"))
						  (delete-dups load-path))))

;; part of Emacs built-in as of Emacs29
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package system-packages)
(use-package use-package-ensure-system-package
  :ensure nil
  :after use-package)

;;;;;  Warnings
;; set-up server & suppress warnings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/warnings.el
(require 'warnings)
;; remove warnings for cl depreciated and server already running
(setq warning-suppress-types (quote ((cl) (server) (iedit) (org-element))))
(setq warning-suppress-log-types (quote ((cl) (org-element))))
(setq byte-compile-warnings (quote ((cl-functions))))

;; prevent warnings buffer from poping up during package compile
;; still available in the buffer list
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;;;; during loading clear file-name-handler-alist
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)))

;;;;; system custom constants
;; section for global constants
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
(defvar sej/menu-height -25
  "Menu-height used to calculate frame adjustments.")
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

(defcustom sej-latex-directory "/Library/TeX/texbin"
  "Directory for Latex."
  :type 'string)

;;;;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (emacs-path "custom.el"))

(let ((custom-template-file
       (emacs-path "custom-template.el")))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(load custom-file :noerror-if-file-is-missing)

;;;;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
;; place to hold specific & secret stuff ~/.ssh is best
(progn
  (let ((file
         (emacs-path "custom-post.el")))
    (load file :noerror-if-file-is-missing) )
  (let ((file
         (expand-file-name "custom-post.el" "~/.ssh/")))
    (load file :noerror-if-file-is-missing) ) )

;;;;; Package manager
;; add melpa to already encluded elpa
(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(defun sej/package-install-refresh-contents (&rest args)
  "With first `package-install' of ARGS, `package-refresh-contents' to ensure list is up to date."
  (package-refresh-contents)
  (advice-remove 'package-install 'sej/package-install-refresh-contents))

(advice-add 'package-install :before 'sej/package-install-refresh-contents)

;;;;; exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :demand t
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

;;;;; ultra-scroll
;; [[https://github.com/jdtsmith/ultra-scroll][ultra-scroll]] is a smooth-scrolling package for emacs, with native support for standard builds as well as emacs-mac
(use-package ultra-scroll
  :demand t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;;;;; OSX System specific environment setting
(when sys/macp
  (message "Mac OSX")
  ;; fix path on M1 macs
  (when sys/mac-AA64-p
	(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
	(setq exec-path (append exec-path '("/opt/homebrew/bin"))))
  (unless (find-font (font-spec :name "Iosevka"))
	(shell-command-to-string "brew install font-iosevka"))
  (if (find-font (font-spec :name "Iosevka"))
	  (add-to-list 'default-frame-alist '(font . "iosevka-14")))

;;;;;; OSX Apple keyboard
;; caps lock is control (through karabiner)
;; Fn key do Hyper
;; LControl key do RControl (karabiner) which is Super (emacs)
;; left opt/alt key do emacs Alt modifier
;; right opt/alt key do regular alt key
;; left and right command(apple) key do Meta
;; spacebar acts as super key with other key
;; karabiner.json backup files in dotfiles under .config directory
;; https://github.com/pqrs-org/Karabiner-Elements

(if (boundp 'mac-carbon-version-string) ;; using mac-port?
    ( progn
      (message "Mac-port")
      ;; for emacs-mac-port
      (setq mac-right-command-modifier 'left) ;right command, plus Karabiner
      (setq mac-right-option-modifier 'none) ;Stays as alt key (like å∫ç∂)
      (setq mac-function-modifier 'hyper) ;hyper is function & held tab key (Karabiner)
      (setq mac-control-modifier 'control) ;Karabiner swapped & caps_lock
      (setq mac-right-control-modifier 'super) ; actually left control
      (setq mac-option-modifier 'alt) ; left option is A-alt key
      (setq mac-command-modifier 'meta)) ;left command is meta
  ( progn
    (message "ns-port")
    ;; for regular Emacs port
    (setq ns-right-command-modifier 'left)
    (setq ns-right-option-modifier 'none)
    (setq ns-function-modifier 'hyper)
    (setq ns-control-modifier 'control)
    (setq ns-right-control-modifier 'super)
    (setq ns-option-modifier 'alt)
    (setq ns-command-modifier 'meta)
    ))
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(setq insert-directory-program "gls")

(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.zprofile ; printf $PATH")))
(setq exec-path (split-string (getenv "PATH") ":")))

;;;;; Linux System specific environment setting
(when sys/linuxp
  (message "Linux")
;;;;;; Linux keyboard
  ;; nothing set at this moment
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
  ;; https://www.autohotkey.com/
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
                                "/mingw64/bin/") exec-path)))
;;;;; Blackout
;; Similar to packages like minions, diminish, or delight.
;; You can alter how your minor and major modes show up in the mode-line.
;; https://github.com/raxod502/blackout
(use-package blackout
  :demand t)

;;;;; Alert
;; Alert is a Growl-workalike for Emacs
;; uses a common notification interface and multiple, selectable "styles"
;; https://github.com/jwiegley/alert
(use-package alert
  :config
  (if sys/macp (setq alert-default-style #'osx-notifier))  )

;;;;; Server set-up
;; set-up Emacs server
(use-package emacs
  :when (or sys/macp sys/linuxp sys/freebsdp)
  :hook (emacs-startup . sej/server-mode)
  :init
  (defun sej/server-mode ()
    "Start server-mode without errors"
    (interactive)
    (with-demoted-errors
        "%S -- Server exists -- not starting new one."
      (load "server")
      (unless (server-running-p) (server-start)) ) ) )

;;;;; async
;; A module for doing asynchronous processing in Emacs
;; https://github.com/jwiegley/emacs-async
(use-package async
  :init
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

;;;;; dash
;; A modern list API for Emacs. No 'cl required.
;; https://github.com/magnars/dash.el
(use-package dash
  :commands -map -union)

;;;;; f
;; modern API for working with files and directories in Emacs.
;; https://github.com/rejeep/f.el
(use-package f
  :demand t
  :commands f-read f-join f-exists-p)

;;;;; s
;; The long lost Emacs string manipulation library.
;; https://github.com/magnars/s.el
(use-package s
  :commands s-split)

;;;;; cl-lib
;; Forward cl-lib compatibility library for Emacs<24.3
;; https://elpa.gnu.org/packages/cl-lib.html
(require 'cl-lib)

;;;;; noflet
;; noflet is dynamic, local, advice for Emacs-Lisp code.
;; https://github.com/nicferrier/emacs-noflet/tree/master
(use-package noflet
  :commands (noflet flet))

;;;;; pcre2el
;; RegeXp translator or RegeXp Tools
;; https://github.com/joddie/pcre2el
;; start with rxt-explain
(use-package pcre2el
  :hook (emacs-lisp-mode . rxt-mode))

;;;;; no-littering feature
;; set the default paths for configuration files & persistent data
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "~/.local/share/emacs/")
        no-littering-var-directory (expand-file-name "~/.cache/emacs/")
        temporary-file-directory (expand-file-name "~/.cache/emacs/tmp/")
		create-lockfiles nil)
  (make-directory temporary-file-directory :parents)
  (defalias 'nl-var-expand #'no-littering-expand-var-file-name)
  (defalias 'nl-etc-expand #'no-littering-expand-etc-file-name)

;;;;;; backups
;; Put backup files neatly away
(let ((backup-dir (nl-var-expand "backups/"))
      (auto-saves-dir (nl-var-expand "auto-save/")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,(expand-file-name backup-dir))
                                 auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
                                 auto-save-file-name-transforms `((".*" ,auto-saves-dir t)))
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2   ; and some old ones, too
      vc-make-backup-files t
      backup-by-copying t
      version-control t
	  auto-save-interval 64
	  auto-save-timeout 2))

;;;;; Emacs internal settings
;; a use-package friendly place to put settings
;; no real extra value to putting as setq but feels clean
(use-package emacs
  :demand t
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
  (load-prefer-newer t)
  (inhibit-startup-screen t)

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
  (indent-tabs-mode t)
  (fill-column 78)
  (x-stretch-cursor 1)

;;;;;; long line settings
  (truncate-lines 1)
  (font-lock-maximum-decoration t)
  (truncate-partial-width-windows 1)
  (auto-hscroll-mode 'current-line)

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
;;;;;; global so-long mode
  ;; set-up global so long mode to protect us
  (global-so-long-mode)

;;;;;; whitespace
  ;; Define the whitespace style.
  (setq-default whitespace-style
                '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))

  ;; Whitespace color corrections.
  (require 'color)
  (let* ((ws-lighten 10) ;; Amount in percentage to lighten up.
         (ws-color (color-lighten-name "#5C5655" ws-lighten)))
    (custom-set-faces
     `(whitespace-newline                ((t (:foreground ,ws-color))))
     `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
     `(whitespace-space                  ((t (:foreground ,ws-color))))
     `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
     `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
     `(whitespace-tab                    ((t (:foreground ,ws-color))))
     `(whitespace-trailing               ((t (:foreground ,ws-color))))))

  ;; Make these characters represent whitespace.
  (setq-default whitespace-display-mappings
                '(
                  ;; space -> · else .
                  (space-mark 32 [183] [46])
                  ;; new line -> ¬ else $
                  (newline-mark ?\n [172 ?\n] [36 ?\n])
                  ;; carriage return (Windows) -> ¶ else #
                  (newline-mark ?\r [182] [35])
                  ;; tabs -> » else >
                  (tab-mark ?\t [187 ?\t] [62 ?\t])))

  ;; Don't enable whitespace for.
  (setq-default whitespace-global-modes
                '(not shell-mode
                    help-mode
                    magit-mode
                    magit-diff-mode
                    ibuffer-mode
                    dired-mode
                    occur-mode))

  ;; set whitespace actions.
  (setq-default whitespace-action
                '(cleanup auto-cleanup))

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
  (global-font-lock-mode t) ; turn on syntax highlighting for all buffers
  (global-visual-line-mode t) ; Add proper word wrapping
  ;; Display fringe indicators in `visual-line-mode`.
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;;;;; color codes
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;;;;; Deleting files go to OS's trash folder, but not remote in v30.1
  (setq delete-by-moving-to-trash t)
  (if sys/macp (setq trash-directory "~/.Trash"))
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t)

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
  (defun ensure-new-position (pop-to-mark-command &rest args)
    "When popping the mark, continue popping until we move the cursor."
    (let ((p (point)))
      (when (eq last-command 'save-region-or-current-line)
        (apply pop-to-mark-command args)
        (apply pop-to-mark-command args)
        (apply pop-to-mark-command args))
      (dotimes (i 10)
        (when (= p (point)) (apply pop-to-mark-command args)))))
  (advice-add 'pop-to-mark-command :around #'ensure-new-position)

;;;;;; transpose lines/words/sexps/params global
  (unbind-key "M-t") ;; which used to be transpose-words
  (unbind-key "C-q")
  (unbind-key "M-z")
  (unbind-key "C-h C-h")
  (define-key input-decode-map [?\C-m] [C-m]) ;; fix C-m so not decoded as <ret>
  
  :bind (:prefix-map transpose-map
                      :prefix "M-t"
                      :prefix-docstring "transpose map"
                      ("l" . transpose-lines)
                      ("w" . transpose-words)
                      ("s" . transpose-sexps)
                      ("p" . transpose-params))

;;;;;; special character definitions λ
  :bind (:prefix-map special-char-map
                      :prefix "C-x 8"
                      :prefix-docstring "special char map"
		              ("l" . sej/insert-lambda)
		              ("t" . sej/insert-tm)
		              ("c" . sej/insert-copyright)
		              (">" . sej/insert-rightarrow)
		              ("8" . sej/insert-infinity)
		              ("v" . sej/insert-check))

;;;;;; map bindings sej-C-x bindings
  :bind (:prefix-map sej-C-q-map
                      :prefix "C-q"
                      :prefix-docstring "SeJ Personal C-q key bindings"
                      ("v"   . emacs-version)
                      ("\\"  . align-regexp) ;Align your code in a pretty way.
                      ("D"   . describe-personal-keybindings)
					  ("l"   . sej/toggle-relative-ln))

  :bind (:prefix-map term-map
                      :prefix "C-q S"
                      :prefix-docstring "Term bindings")

  :bind (:prefix-map sej-C-m-map
                      :prefix "<C-m>"
                      :prefix-docstring "Multi-menu")

  :bind (:prefix-map sej-denote-map
					 :prefix "C-,"
					 :prefix-docstring "SeJ Denote key bindings"
					 ("."   . org-time-stamp))

  :bind (:map override-global-map
               ("s-." . pop-to-mark-command)
	           ("M-j" . join-line)
               ("C-x j" . duplicate-dwim)
			   ("M-\\" . cycle-spacing)))  ;; end of emacs

;;;;; sej constants

(defun sej/insert-lambda()
  "Insert ™ for 'special-char-map'."
  (interactive)
  (insert "λ"))

(defun sej/insert-tm()
  "Insert ™ for 'special-char-map'."
  (interactive)
  (insert "™"))

(defun sej/insert-copyright()
  "Insert © for 'special-char-map'."
  (interactive)
  (insert "©"))

(defun sej/insert-rightarrow()
  "Insert → for 'special-char-map'."
  (interactive)
  (insert "→"))

(defun sej/insert-infinity()
  "Insert ∞ for 'special-char-map'."
  (interactive)
  (insert "∞"))

(defun sej/insert-check()
  "Insert ✓ for 'special-char-map'."
  (interactive)
  (insert "✓"))

;;;;; Simple
;; built-in: simple settings
;;
(use-package simple
  :blackout ((visual-line-mode . "")
             (auto-fill-mode . ""))
  :ensure nil
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
;; built-in: minibuffer settings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/minibuffer.el
(use-package minibuffer
  :demand t
  :ensure nil
  :config
  (setq completion-cycle-threshold 7
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
;; built-in: to make buffer names unique but identifiable
(use-package uniquify
  :ensure nil
  :init
  (setq  uniquify-ignore-buffers-re "^\\*"
         uniquify-buffer-name-style 'post-forward-angle-brackets
         uniquify-strip-common-suffix t
         uniquify-after-kill-buffer-p t
         uniquify-separator "/"))

;;;; history packages
;;;;; savehist
;; built-in: recent buffer history settings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el
(use-package savehist
  :ensure nil
  :demand t
  :hook (emacs-startup . savehist-mode)
  :custom
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t "Allow commands in minibuffers.")
  (history-length 10000)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 300)
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)))

;;;;; recentf
;; built-in: recent file history list settings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el
(use-package recentf
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

;;;;; vundo
;; visual undo displays the undo history as a tree
;; https://github.com/casouri/vundo
(use-package vundo
  :blackout t
  :bind ("C-z" . vundo))

;;; general functions / packages

;;;;; sej functions
;; some basic functions

(defun add-all-to-list (var &rest elems)
  "Add all these elements ELEMS to a list VAR rather than one at a time."
  (dolist (elem (reverse elems))
    (add-to-list var elem)))

(defun sej/open-new-line()
  "Open new line without breaking and place cursor there."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun sej/kill-whole-word ()
  "Kill the current word at point."
  (interactive)
  (backward-word)
  (kill-word 1))

(defun sej/open-line-above-and-indent ()
  "Insert a new line above the current one maintaining the indentation."
  (interactive)
  (let ((current-indentation (current-indentation)))
    (beginning-of-line)
    (open-line 1)
    (when (> current-indentation 0)
      (indent-to current-indentation))))

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

(defun sej/toggle-relative-ln ()
  "Toggle line numbers relative to current, which displays absolute line number."
        (interactive)
             (if (and (boundp 'display-line-numbers-mode) display-line-numbers-mode)
              (display-line-numbers-mode -1)
                 (progn
                   (setq display-line-numbers-type 'relative)
                   (display-line-numbers-mode 1))))

;;;;; list-environment
;; environment variables tabulated
;; process environment editor
;; https://github.com/dgtized/list-environment.el
(use-package list-environment
  :commands list-environment)

;;;;; Advice
;; built-in: accept versus warn from the Advice system.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
(use-package advice
  :ensure nil
  :init
  (setq-default ad-redefinition-action 'accept))

;;;;; calc
;; built-in: calculator
;; https://www.gnu.org/software/emacs/manual/html_mono/calc.html
(use-package calc
  :ensure nil
  :bind (:map sej-C-q-map
              ("c" . calc)
              ("C-c" . quick-calc))
  :commands (quick-calc calc)
  :init
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
;; built-in: checker of buffer for style issues
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/checkdoc.el
(use-package checkdoc
  :ensure nil
  :config
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;;;;; face-remap
;; changes in the appearance of a face.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
(use-package face-remap
  :blackout (buffer-face-mode . "")
  :ensure nil  )


;;;; Security
;;;;; Auth-Source
;; built-in: authentication source
;; https://www.gnu.org/software/emacs/manual/html_mono/auth.html
(eval-when-compile
  (require 'auth-source)
  (require 'auth-source-pass))

(setq auth-sources `(,(f-expand "~/.ssh/.authinfo.gpg")
                     ,(f-expand "~/.ssh/.authinfo")
                     macos-keychain-generic
                     macos-keychain-internet)
	  auth-source-do-cache t)

(defun sej/lookup-password (host user port)
  "Lookup password for HOST USER PORT."
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

;;;;; epa
;; built-in: EasyPG assistant for GnuPG implementation of the OpenPGP standard
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(use-package epa
  :ensure nil
  :init
  (setq epa-replace-original-text 'ask)
  ;;(epa-file-enable)
  )

;;;;; epq
;; built-in: EasyPG for GnuPG implementation of the OpenPGP standard
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(use-package epg
  :ensure nil
  :ensure-system-package gpg
  :init
  (setq epg-pinentry-mode 'loopback))

;;;;; Gnutls
;; built-in: GnuTLS is a library that establishes encrypted SSL or TLS connections.
;; https://www.gnu.org/software/emacs/manual/html_mono/emacs-gnutls.html
(use-package gnutls
  :ensure nil
  :init
  (setq gnutls-verify-error t
        gnutls-min-prime-bits 2048
        tls-checktrust gnutls-verify-error)

  (unless (gnutls-available-p)
	(message "installing gnutls...")
	(shell-command-to-string "brew install gnutls")))

;;;;; keychain-environment
;; set up any SSH or GPG keychains that the Keychain tool has set up for us
;; https://github.com/tarsius/keychain-environment
(use-package keychain-environment
  :ensure-system-package keychain
  :hook (emacs-startup . keychain-refresh-environment))

;;;;; Emacs-lock
;; built-in: lock buffer from kill and/or Emacs from exiting
;; https://www.emacswiki.org/emacs/ProtectingBuffers
(use-package emacs-lock
  :blackout ""
  :ensure nil)

;;;; help
;;;;; help
;; built-in: help mode settings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/help-mode.el
(use-package help
  :ensure nil
  :bind (:map help-map
			  ("=" . describe-char)
			  ("j" . describe-face)
			  ("-" . describe-keymap))
  :init
  (add-hook 'help-mode-hook #'visual-line-mode)
  (setq help-window-select 'always
		find-function-C-source-directory "~/src/emacs/src/") ; where I typically have source
  (advice-add 'help-window-display-message :override #'ignore))

;;;;; which-key
;; built-in: minibuffer keybinding prompts
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure nil
  :hook (emacs-startup . which-key-mode)
  :bind (("C-h h" . which-key-show-top-level)
          ("C-h M-m" . which-key-show-major-mode))
  :commands which-key-mode
  :init
  (setq which-key-use-C-h-commands t
        which-key-separator " "
        which-key-prefix-prefix "+"))

;;;;; helpful
;; helpful is an improved help-fns & help-fns+
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind (("C-h ." . helpful-at-point)
          ("C-h x" . helpful-command)
          ("C-h k" . helpful-key) ; C-h k
          ("C-h v" . helpful-variable) ; C-h v
          ("C-h f" . helpful-callable) ; C-f v
          ("C-h F" . helpful-function)
          ("C-h M" . helpful-macro))  )

;;;;; casual
;; triansient based jump screens
;; https://github.com/kickingvegas?tab=repositories
(use-package casual
  :demand t)

(use-package casual-agenda
  :ensure nil
  :after org-agenda
  :bind (:map org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         ("M-j" . org-agenda-clock-goto) ; optional
         ("J" . bookmark-jump)))

(use-package casual-bookmarks
  :ensure nil
  :after bookmark
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump)))

(use-package casual-calc
  :ensure nil
  :after calc
  :bind (:map calc-mode-map ("C-o" . 'casual-calc-tmenu)))

(use-package casual-calendar
  :ensure nil
  :after calendar
  :bind (:map calendar-mode-map ("C-o" . 'casual-calendar)))

(use-package casual-dired
  :ensure nil
  :after dired
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))

(use-package casual-editkit
  :ensure nil
  :bind (:map global-map ("C-o" . 'casual-editkit-main-tmenu)))

(use-package casual-ibuffer
  :ensure nil
  :after ibuffer
  :bind (:map ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked)
         ("}" . ibuffer-forward-next-marked)
         ("[" . ibuffer-backward-filter-group)
         ("]" . ibuffer-forward-filter-group)
         ("$" . ibuffer-toggle-filter-group)))

(use-package casual-info
  :ensure nil
  :bind (:map Info-mode-map ("C-o" . 'casual-info-tmenu)))

(use-package casual-isearch
  :ensure nil
  :after isearch
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package casual-re-builder
  :ensure nil
  :after re-builder
  :bind (:map reb-mode-map ("C-o" . casual-re-builder-tmenu)
			  :map reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu)))

(use-package casual-image
  :ensure nil
  :hook (image-mode . casual-image-tmenu)
  :bind (:map image-mode-map
			  ("C-o" . casual-image-tmenu)))

(use-package casual-avy
  :ensure t ; separate package from casual
  :after avy
  :bind ("H-/" . casual-avy-tmenu))

(use-package casual-symbol-overlay
  :ensure t ; separate package from casual
  :after symbol-overlay
  :bind (:map symbol-overlay-map
			  ("C-o" . casual-symbold-overlay-tmenu)))

;;; user interface
;;;; themes
;;;;; tron-legacy-theme
;; https://github.com/ianyepan/tron-legacy-emacs-theme
(use-package tron-legacy-theme
  :disabled t
  :init
  (setq tron-legacy-theme-vivid-cursor t)
  (setq tron-legacy-theme-dark-fg-bright-comments nil)
  (setq tron-legacy-theme-softer-bg nil)
  :init
  (load-theme 'tron-legacy :no-confirm))

;;;;; modus-themes
;; built-in: theme
;; https://protesilaos.com/emacs/modus-themes#h:1af85373-7f81-4c35-af25-afcef490c111
;; current preferred theme
(use-package emacs ; built-in package
  :bind ("C-c C-t" . modus-themes-toggle)
  :custom
  (require-theme 'modus-themes)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))
  :config
  (load-theme 'modus-vivendi :no-confirm))

;;;; frames
;;;;; frame
;; built-in: frame package
(use-package frame
  :ensure nil
  :bind (; super combo
          ("s-4" . dired-other-frame)
          ("s-5" . make-frame-command)
          ("s-6" . delete-other-frames)
          ("s-w" . delete-frame)
          ;; C-x combo
          ("C-x w" . delete-frame)
          ;; A-M combo
          ("A-M-s-k" . toggle-frame-fullscreen)
          ("A-M-k" . sej/frame-resize-full)
          ("A-M-h" . sej/frame-resize-l)
          ("A-M-l" . sej/frame-resize-r)
          ("A-M-s-h" . sej/frame-resize-l2)
          ("A-M-s-l" . sej/frame-resize-r2)
          ("A-M-H" . sej/frame-resize-l3)
          ("A-M-L" . sej/frame-resize-r3)
          ;; A-M h-j-k-l combo
          ("A-M-s-<up>" . toggle-frame-fullscreen)
          ("A-M-<up>" . sej/frame-resize-full)
          ("A-M-<left>" . sej/frame-resize-l)
          ("A-M-<right>" . sej/frame-resize-r)
          ("A-M-s-<left>" . sej/frame-resize-l2)
          ("A-M-s-<right>" . sej/frame-resize-r2)
          ("A-M-<S-left>" . sej/frame-resize-l3)
          ("A-M-<S-right>" . sej/frame-resize-r3)
          ;; C-q combo
          :map sej-C-q-map
          ("m" . sej/frame-recentre)
          ("F" . toggle-frame-fullscreen)
          ("<up>" . sej/frame-resize-full)
          ("<left>" . sej/frame-resize-l)
          ("<right>" . sej/frame-resize-r)
          ("<s-left>" . sej/frame-resize-l2)
          ("<s-right>" . sej/frame-resize-r2)
          ("<S-left>" . sej/frame-resize-l3)
          ("<S-right>" . sej/frame-resize-r3)
		  )
  :init
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1
        frame-title-format "%F--%b-[%f]--%Z"
        icon-title-format frame-title-format
        undelete-frame-mode t    )

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
    (set-frame-size (selected-frame)
                    (- (display-pixel-width)
                       (if sys/macp (eval 13)
                         (eval 25)))
                    (- (display-pixel-height)
                       (- (frame-outer-height)
                          (frame-inner-height)
                          sej/menu-height))
                    1))

  (defun sej/frame-resize-l ()
    "Set frame full height and 1/2 wide, position at screen left."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)
					(- (truncate (/ (display-pixel-width) 2)) 14)
					(- (display-pixel-height)
					   (- (frame-outer-height)
						  (frame-inner-height) sej/menu-height))
					1))

  (defun sej/frame-resize-l2 ()
    "Set frame full height and 1/2 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)
					(- (truncate (/ (display-pixel-width) 4)) 0)
					(- (display-pixel-height)
					   (- (frame-outer-height)
						  (frame-inner-height) sej/menu-height))
					1))

  (defun sej/frame-resize-l3 ()
    "Set frame full height and 2/3 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)
					(- (truncate (* (/ (display-pixel-width) 3) 2)) 0)
					(- (display-pixel-height)
					   (- (frame-outer-height)
						  (frame-inner-height) sej/menu-height))
					1))

  (defun sej/frame-resize-r ()
    "Set frame full height and 1/2 wide, position at screen right."
    (interactive)
    (set-frame-position (selected-frame)
                        (- (truncate (/ (display-pixel-width) 2)) 0) 0)
    (set-frame-size (selected-frame)
					(- (truncate (/ (display-pixel-width) 2)) 14)
					(- (display-pixel-height)
					   (- (frame-outer-height)
						  (frame-inner-height) sej/menu-height))
					1))

  (defun sej/frame-resize-r2 ()
    "Set frame full height and 1/2 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame)
                        (truncate (* (/ (display-pixel-width) 4) 3))
						0)
    (set-frame-size (selected-frame)
					(- (truncate (/ (display-pixel-width) 4)) 0)
					(- (display-pixel-height)
					   (- (frame-outer-height)
						  (frame-inner-height) sej/menu-height))
					1))

  (defun sej/frame-resize-r3 ()
    "Set frame full height and 2/3 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame)
                        (truncate (* (/ (display-pixel-width) 3) 1))
						0)
    (set-frame-size (selected-frame)
					(- (truncate (* (/ (display-pixel-width) 3) 2)) 0)
					(- (display-pixel-height)
					   (- (frame-outer-height)
						  (frame-inner-height) sej/menu-height))
					1))

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
;; built-in: fringe-mode
;; https://www.emacswiki.org/emacs/TheFringe
;; https://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
(use-package fringe
  :ensure nil
  :if window-system
  :init
  (fringe-mode '(4 . 4)))


;;;; buffers
;;;;; buffer key-bindngs
(bind-keys*
 ("C-<return>" . save-buffer)
 ("s-y" . bury-buffer)
 ("C-c y" . bury-buffer)
 ("C-c r" . revert-buffer)
 ("s-r" . revert-buffer)
 ("C-x k" . kill-current-buffer)
 ("s-n" . bs-cycle-next) ; buffer cycle next
 ("s-p" . bs-cycle-previous))

(setq-default bs-default-configuration "all-intern-last")

;;;;; sej/dos2unix
;; convert the current buffer to UNIX file format
;; not bound
(defun sej/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;;; sej/unix2dos
;; convert the current buffer to DOS file format
;; not bound
(defun sej/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;;;; sej/save-buffer-as-utf8
;; revert a buffer with coding-system and save as utf-8
(defun sej/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

;;;;; sej/browse-homepage
;; - Browse my github homepage
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

;;;;; sej/delete-buffer-contents-no-matter-properties
(defun sej/delete-buffer-contents-no-matter-properties ()
  "Delete the buffer contents no matter the properties."
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer)))

;;;;; sej/remove-all-buffer-properties
(defun sej/remove-all-buffer-properties ()
  "Remove text properties like read-only in the buffer contents."
  (interactive)
(let ((inhibit-read-only t)) (set-text-properties (point-min) (point-max) ())))

;;;; scratch buffer
;;;;; scratch buffer set-up
;; initial message
;; bury don't kill scratch
(setq initial-scratch-message "")
(defun kill-buffer-around-advice (kill-current-buffer &rest args)
  "Bury the *scratch* buffer, but never kill it when using KILL-CURRENT-BUFFER ARGS."
  (let ((buffer-to-kill (buffer-name)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      (apply kill-current-buffer args))))
(advice-add 'kill-current-buffer :around #'kill-buffer-around-advice)

;;;;; sej/create-scratch-buffer
;; as name suggests
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
(bind-key* "C-q C-S" 'sej/create-scratch-buffer)
(bind-key* "C-q C-s" 'scratch-buffer)

;;;;; [[https://github.com/Fanael/persistent-scratch][persistent-scratch]]
;; keep the scratch buffer from session to session
(use-package persistent-scratch
  :commands persistent-scratch-setup-default
  :hook (emacs-startup . sej/persistent-scratch-setup-default)
  :custom
  (persistent-scratch-autosave-interval 30)
  (persistent-scratch-backup-directory nil)
  :init
  :config
  (defun sej/persistent-scratch-setup-default ()
	"Set up persistent scratch and make it `trusted-content'."
	(add-to-list 'trusted-content persistent-scratch-save-file)
	(persistent-scratch-setup-default))
  
	(persistent-scratch-autosave-mode)
  
  (with-demoted-errors "Error: %S"
    (persistent-scratch-setup-default)))

;;;;; Remember ; persistent notes (like persistent-scratch but built-in)
;; using persistent-scratch for lisp & this for notes
(use-package remember
  :ensure nil
  :bind (:map global-map
			  ("C-, M-r" . remember)
			  ("C-, M-n" . remember-notes))
  :init
  (setq remember-notes-initial-major-mode 'org-mode
		; MAYBE TODO remember-data-file "file name" ; could make this the current denote journal monthly file name
		remember-in-new-frame t)

  (defun sej/switch-to-remember-buffer (f)
	(with-selected-frame f
      (remember-notes t)))
  
  (add-hook 'after-make-frame-functions #'sej/switch-to-remember-buffer))

;;;; windows
;;;;; window key-bindings
;; built-in: super versions of C-x window bindings
(use-package window
  :ensure nil
  :bind (("H-0" . delete-window)
          ("H-1" . delete-other-windows)
          ("H-2" . split-window-vertically)
          ("H-3" . split-window-right)
          ("C-x K" . sej/quit-other)

          ;; wind move to multifram window
          ("M-'" . next-multiframe-window)

          ;; movement complementary to windmove / windswap
          ("A-h" . left-char)
          ("A-j" . next-line)
          ("A-k" . previous-line)
          ("A-l" . right-char)

          ;;scroll window up/down by one line
          ("A-n" . sej/scroll-up-one)
          ("A-p" . sej/scroll-down-one) )
  :init
  (setq window-combination-resize t
        even-window-sizes 'height-only
        window-sides-vertical nil)

  (defun sej/quit-other()
    "Quit other window."
    (interactive)
    (save-excursion
      (other-window 1)
      (quit-window)))

  (defun sej/scroll-up-one()
    "Scroll screen up one while keeping point in place."
    (interactive)
    (scroll-up 1))

  (defun sej/scroll-down-one()
    "Scroll screen down one while keeping point in place."
    (interactive)
    (scroll-down 1)))

;;;;; ace-window
;; quickly selecting a window to switch to
;; C-u prefex to move window
;; C-u C-u prefex to delete window
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("M-o" . ace-window))
  :hook (emacs-startup . ace-window-display-mode)
  :custom ((aw-dispatch-always t)
		   (aw-frame-offset '(100 . 0)))
  :config
  (defun sej/hide-frame (window)
	"Hide frame containing window."
	(lower-frame (window-frame window)))

  (add-to-list 'aw-dispatch-alist '(?h sej/hide-frame "Hide Frame")))

;;;;; winner
;; built-in: Restore old window configurations
;; https://www.emacswiki.org/emacs/WinnerMode
(use-package winner
  :ensure nil
  :hook (emacs-startup . winner-mode)
  :commands (winner-undo winner-redo)
  :bind (("C-c <left>" . winner-undo)
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

;;;;; popper
;; minor-mode to tame ephemeral windows [[https://github.com/karthink/popper][link]]
(use-package popper
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("C-~"   . popper-cycle)
         ("H-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     special-mode
     compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  )

;;;; tabs
;;;;; tab-bar
;; built-in: tabs for virtual desktops
;; C-x t prefix
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html
(use-package tab-bar
  :ensure nil
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
;; A fancy and fast mode-line inspired by minimalism design
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)
		 (doom-modeline-before-github-fetch-notification . auth-source-pass-enable)))

;;;;; minions
;; implements a nested menu that gives access to all minor modes
;; https://github.com/tarsius/minions
(use-package minions
  :init
  (setq doom-modeline-minor-modes t)
  (minions-mode 1))

;;;;; nerd-icons
;; https://github.com/rainstormstudio/nerd-icons.el
;; use nerd-icons-install-fonts
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")  )

;;;;; nerd-icons-dired
;; https://github.com/rainstormstudio/nerd-icons-dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;;;; nerd-icons-ibuffer
;; https://github.com/seagle0128/nerd-icons-ibuffer
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;;; nerd-icons-completion
;; icons with completion
;; https://github.com/rainstormstudio/nerd-icons-completion
(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

;;;;; nerd-icons-corfu
;; https://github.com/LuigiPiucco/nerd-icons-corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Remember to add an entry for `t', the library uses that as default.
  )

;;;;; sf.el
;; [[https://lmno.lol/alvaro/emacs-insert-and-render-sf-symbols][Insert and render SF symbols]]
;; note needs apple SF fonts
;; [[https://developer.apple.com/sf-symbols/][see SF Symbols 6]]
;; commands: (sf-symbol-insert) & (sf-symbol-insert-name)
(when (memq system-type '(darwin))
  (set-fontset-font t nil "SF Pro Display" nil 'append)
  (load "~/.emacs.d/lisp/sf.el"))

;;; text manipulation
;;;; text manipulation settings
;;;;; saveplace
;; built-in: automatically save place in files so return to same place in next session
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/saveplace.el
(use-package saveplace
  :ensure nil
  :hook (emacs-startup . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t))


;;;; multi-edit
;;;;; multiple cursors
;; Multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (:map global-map
		   ("C-S-c C-S-c"   . mc/edit-lines)
           ("C->"           . mc/mark-next-like-this)
           ("C-<"           . mc/mark-previous-like-this)
           ("C-c C-<"       . mc/mark-all-like-this)
           ("C-M->"         . mc/skip-to-next-like-this)
           ("C-M-<"         . mc/skip-to-previous-like-this)
           ("s-<mouse-1>"   . mc/add-cursor-on-click)
		   ("<C-m> a"         . mc/edit-beginings-of-lines)
		   ("<C-m> e"         . mc/edit-ends-of-lines)
		   ("<C-m> s"         . mc/mark-next-like-this-symbol)
		   ("<C-m> S"         . mc/mark-all-symbols-like-this)
		   ("<C-m> D"         . mc/mark-all-like-this-in-defun)
		   ("<C-m> d"         . mc/mark-all-dwim)
		   ("<C-m> r"         . mc/mark-all-in-region)
		   ("<C-m> w"         . mc/mark-next-like-this-word)
		   ("<C-m> W"         . mc/mark-all-words-like-this)
           :map mc/keymap
		   ("C-m v"         . mc/vertical-align)
           ("C-|" . mc/vertical-align-with-space)))

;;;; search
;;;;; isearch
;; built-in: search function
;; https://github.com/VernonGrant/discovering-emacs/blob/main/show-notes/3-making-incremental-search-work-for-you.md
(use-package isearch
  :ensure nil
  :bind ( :map minibuffer-local-isearch-map
          ("M-/" . isearch-complete-edit)
          :map isearch-mode-map
          ("C-g" . isearch-cancel) ;instead of `isearch-abort'
          ("M-/" . isearch-complete))
  :init
  (setq search-highlight t
        search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil
        isearch-lazy-highlight t
        isearch-lazy-count t
        isearch-yank-on-move t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        isearch-yank-on-move 'shift
        isearch-allow-scroll 'unlimited
        isearch-repeat-on-direction-change t
        lazy-highlight-initial-delay 0.5
        lazy-highlight-no-delay-length 3
        search-ring-max 30
        regexp-search-ring-max 30
        isearch-wrap-pause t))

;;;;; Anzu
;; good query replace search
;; https://github.com/emacsorphanage/anzu
(use-package anzu
  :bind  (([remap query-replace] . anzu-query-replace-regexp)
           ("C-H-r" . anzu-query-replace)
           ("C-H-S-r" . anzu-query-replace-at-cursor)
           :map isearch-mode-map
           ("C-H-r" . anzu-isearch-query-replace))
  :config
  (global-anzu-mode))

;;;;; consult-ag
;; searching with the silver searcher, consult style
;; https://github.com/yadex205/consult-ag
(use-package consult-ag
  :after consult
  :ensure-system-package ag
  :commands consult-ag
  :bind (("C-S-s" . consult-ag)
         :map search-map
              ("a" . consult-ag)))

;;;;; re-builder
;; built-in: regex helper to string format
;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package re-builder
  :ensure nil
  :config (setq reb-re-syntax 'read))

;;;;; bookmark
;; built-in bookmark package
(use-package bookmark
  :ensure nil
  :demand t
  :init
  (setq bookmark-use-annotations t)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28
  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)
  :config
  (setq bookmark-save-flag +1))

;;;; completion
;;;;; abbrev
;; built-in: for inserting abbreviations
;; https://www.emacswiki.org/emacs/AbbrevMode
(use-package abbrev
  :ensure nil
  :hook ((emacs-startup org-mode) . abbrev-mode)
  :config
  (setq abbrev-file-name             ;; tell emacs where to read abbrev
        (nl-var-expand "abbrev_defs") only-global-abbrevs nil)    ;; definitions from...
  (setq save-abbrevs 'silently)

  (define-abbrev-table
    'org-mode-abbrev-table
    '(("orgh" "" sej/org-header 0)
      ("orgl" "" sej/org-wrap-elisp 0)
      ("orgs" "" sej/org-wrap-source 0))))

;;;;; dabbrev
;; built-in: to let you write just a few characters of words you've written
;; earlier to be able to expand them.
(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand
             dabbrev-completion)
  ;; Use Dabbrev with Corfu!
  ;; Swap M-/ and C-M-/
  :bind ("C-M-/" . dabbrev-expand)
  :init
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
        dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|="
        dabbrev-backward-only nil
        dabbrev-case-distinction 'case-replace
        dabbrev-case-fold-search t
        dabbrev-case-replace 'case-replace
        dabbrev-check-other-buffers t
        dabbrev-eliminate-newlines t
        dabbrev-upcase-means-case-search t)
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;;;;; hippie-expand
;; built-in: expand at point in various ways
;; https://www.emacswiki.org/emacs/HippieExpand

(use-package hippie-expand
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-complete-file-name
          try-complete-file-name-partially
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-list
          try-expand-line
          cape-elisp-symbol
          try-expand-list-all-buffers
          try-expand-line-all-buffers)))

;;;;; [[https://github.com/minad/vertico][vertico]]
;; alternative to ivy, ido, helm
(use-package vertico
  :bind (("C-H-v" . vertico-repeat)
         :map vertico-map
         ("C-j"   . vertico-exit-input)
         ("C-M-n" . vertico-next-group)
         ("C-M-p" . vertico-previous-group))
  :hook ((emacs-startup . vertico-mode)
		 (minibuffer-setup . vertico-repeat-save))
  :config
  (setq vertico-scroll-margin 0) ;; Different scroll margin
  (setq vertico-count 20)        ;; Show more candidates
  (setq vertico-resize t)        ;; Grow and shrink the Vertico minibuffer
  (setq vertico-cycle t)        ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.

  ;; Hide commands in M-x which do not work in the current mode. Vertico
  ;; commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

;;;;;; vertico-repeat
  (use-package vertico-repeat
	:ensure nil
	:demand t
	:config
	(add-to-list 'savehist-additional-variables 'vertico-repeat-history))

;;;;;; vertico-quick
  (use-package vertico-quick
	:ensure nil
    :demand t
    :bind (:map vertico-map
                ("C-o"   . vertico-quick-exit)
                ("C-." . vertico-quick-embark))
    :preface
    (defun vertico-quick-embark (&optional arg)
      "Embark on candidate using quick keys."
      (interactive)
      (when (vertico-quick-jump)
        (embark-act arg))))

;;;;;; vertico-multiform
  ;; vertico forms, with below active to temporarily change them C-i sticky
  ;; M-B -> vertico-multiform-buffer
  ;; M-F -> vertico-multiform-flat
  ;; M-G -> vertico-multiform-grid
  ;; M-R -> vertico-multiform-reverse
  ;; M-U -> vertico-multiform-unobtrusive
  ;; M-V -> vertico-multiform-vertical
  (use-package vertico-multiform
	:ensure nil
    :demand t
    :bind (:map vertico-map
                ("C-i"   . sej/vertico-multiform-toggle-ur)
                ("<tab>" . vertico-insert))
    :custom
    (vertico-multiform-commands
     '((consult-imenu buffer)
       (consult-line buffer)
       (consult-grep buffer)
       (consult-git-grep buffer)
       (consult-ripgrep buffer)
       (consult-yank-pop)
       (embark-bindings buffer)
       (xref-find-references buffer)))
    (vertico-multiform-categories
     '((t reverse)))
    :config
	(defun sej/vertico-multiform-toggle-ur ()
	  "Toggle sticky setting between reverse and unobtrusive."
	  (interactive)
	  (if (equal vertico-multiform-categories '((t unobtrusive)))
		  (progn (setq vertico-multiform-categories '((t reverse)))
				 (vertico-multiform-reverse))
		(progn (setq vertico-multiform-categories '((t unobtrusive)))
			   (vertico-multiform-unobtrusive))))
	
    (vertico-multiform-mode 1))

;;;;;; vertico-directory
;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  :demand t
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("C-<backspace>" . vertico-directory-delete-word)
              ("C-w" . vertico-directory-delete-word)
			  )
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))  ;; end of vertico

;;;;; [[https://github.com/minad/corfu][corfu]]
;; small completion program similar to company
(use-package corfu
  :bind (:map corfu-map
              ("A-<return>" . corfu-complete)
              ("<escape>". corfu-quit)
              ("M-d" . corfu-show-documentation)
              ("M-l" . 'corfu-show-location))
  :hook (emacs-startup . global-corfu-mode)

  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-separator ?\s)             ;; Orderless field separator
  (corfu-quit-at-boundary t)        ;; Never quit at completion boundary
  (corfu-quit-no-match t)           ;; t, 'separator, nil Never quit
  (corfu-preview-current nil)       ;; Disable current candidate preview
  (corfu-preselect 'directory)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)           ;; Use scroll margin
  (corfu-auto-delay 2)
  (corfu-auto-prefix 2)
  (corfu-count 10)
  (corfu-echo-documentation nil)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match t) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.

  :config
  ;; (setq global-corfu-modes '((not markdown-mode) t))
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (setq tab-always-indent 'complete)

  ;; Free the RET key for less intrusive behavior.
  ;; Option 1: Unbind RET completely
  ;; (keymap-unset corfu-map "RET")
  ;; Option 2: Use RET only in shell modes
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send))))

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  (use-package corfu-history
    :ensure nil
    :demand t
    :config
    (add-to-list 'savehist-additional-variables 'corfu-history)
    (corfu-history-mode t))
  (use-package corfu-indexed
    :ensure nil
    :demand t
    :config
    (corfu-indexed-mode t))
  (use-package corfu-popupinfo
    :ensure nil
    :demand t
    :config
    (corfu-popupinfo-mode t))
  (use-package corfu-terminal
    :unless (display-graphic-p)
    :demand t
    :init
    (corfu-terminal-mode +1)  )

  (use-package corfu-doc-terminal
    :vc (:url "https://codeberg.org/akib/emacs-corfu-doc-terminal"
              :rev :newest
              :branch "master")
    :unless (display-graphic-p)
    :demand t
    :init
    (corfu-doc-terminal-mode +1) ) )

;;;;; [[https://github.com/minad/cape][cape]]
;; completion at point extensions
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :demand t
  :bind ("C-H-/" . completion-at-point) ;; capf
  :bind-keymap ("A-/" . cape-prefix-map)
  :hook ((emacs-lisp-mode .  sej/cape-capf-setup-elisp)
         (org-mode . sej/cape-capf-setup-org)
         (eshell-mode . sej/cape-capf-setup-eshell)
         (git-commit-mode . sej/cape-capf-setup-git-commit)
         )

  :custom (cape-dabbrev-min-length 3)
  :config
  (add-all-to-list 'completion-at-point-functions
				   #'cape-dabbrev
				   #'cape-file
				   #'cape-abbrev)

  ;; #'cape-history
  ;; #'cape-keyword
  ;; #'cape-tex
  ;; #'cape-sgml
  ;; #'cape-rfc1345
  ;; #'cape-dict
  ;; #'cape-elisp-symbol
  ;; #'cape-line

  ;; Elisp
  (defun sej/cape-capf-ignore-keywords-elisp (cand)
    "Ignore keywords with forms that begin with \":\" (e.g.:history)."
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))

  (defun sej/cape-capf-setup-elisp ()
    "Set completion at point for elisp."
	(setq-local completion-at-point-functions
                `(,(cape-capf-super
                    #'elisp-completion-at-point
                    #'cape-dabbrev)
                  cape-file)))

  ;; Org
  (defun sej/cape-capf-setup-org ()
    "Set completion at point for org."
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
					#'cape-dict
					#'cape-dabbrev
					#'cape-history
					#'cape-abbrev)
				  cape-file)))

;;git-commit
  (defun sej/cape-capf-setup-git-commit ()
    "Set completion at point in git commits."
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
					#'cape-dict
					#'cape-dabbrev
					#'cape-history
					#'cape-abbrev)
				  cape-file)))

  ;; Eshell
  (defun sej/cape-capf-setup-eshell ()
    "Set completion at point in eshell."
    (setq-local completion-at-point-functions
                `( pcomplete-completions-at-point
				   cape-file
				   cape-dabbrev
				   cape-history
				   cape-abbrev)))

  :config
  ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;;;; [[https://github.com/minad/marginalia][marginalia]]
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :hook (emacs-startup . marginalia-mode)
  :bind (:map completion-list-mode-map
              ("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;;; [[https://github.com/oantolin/orderless][orderless]]
;; provides an orderless completion style that divides the pattern into space-separated components,
;; and matches candidates that match all of the components in any order.
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;;; [[https://github.com/radian-software/prescient.el][prescient]]
;; sorts and filters lists of candidates
(use-package prescient
  :hook (emacs-startup . prescient-persist-mode)
  :init
  (add-to-list 'completion-styles 'prescient))

(use-package vertico-prescient
  :after prescient
  :hook (vertico-mode . vertico-prescient-mode))

(use-package corfu-prescient
  :after prescient
  :hook (corfu-mode . corfu-prescient-mode))

;;;;; [[https://github.com/oantolin/embark/][embark]]
;; acting on targets
(use-package embark
  :bind  (("C-." . embark-act)        ;; pick some comfortable binding
           ("M-." . embark-dwim)        ;; good alternative: M-.
           ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
           ("C-s-e" . embark-export)
           ("H-e" . embark-export)
		   :map embark-collect-mode-map
		   ("C-c C-a" . embark-collect-direct-action-minor-mode))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;;;; [[https://github.com/oantolin/embark/blob/master/embark-consult.el][embark-consult]]
;; embark-consult provides integration between Embark and Consult. The package will be loaded automatically by Embark.
;; 
;; Some of the functionality here was previously contained in Embark itself:
;; 
;; Support for consult-buffer, so that you get the correct actions for each type of entry in consult-buffer’s list.
;; Support for consult-line, consult-outline, consult-mark and  consult-global-mark, so that the insert and save actions
;; don’t include a weird unicode character at the start of the line, and so you can export from them to an occur buffer
;; (where occur-edit-mode works!).
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;; [[https://github.com/minad/consult][consult]]
;; completing read
(use-package consult
  :bind (("H-M-," . consult-recent-xref)
          ;; C-c bindings (mode-specific-map)
          ("C-c k" . consult-kmacro)
          ("C-c h" . consult-history)
          ("C-c M-x" . consult-mode-command)
          ;; Other custom bindings
          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
          ;; C-x bindings (ctl-x-map)
          :map ctl-x-map
          ("C-r" . consult-recent-file)
          ("b" . consult-buffer)                    ;; orig. switch-to-buffer
          ("4 b" . consult-buffer-other-window)     ;; orig. switch-to-buffer-other-window
          ("r b" . consult-bookmark)                ;; orig. bookmark-jump
          ("4 C-r" . find-file-read-only-other-window) ;; orig. nil
          ("5 b" . consult-buffer-other-frame)      ;; orig. switch-to-buffer-other-frame
          ;; Custom sej bindings for fast register access
          :map sej-C-q-map
          ("C-y" . consult-register-load)
          ("C-w" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
          ("C-r" . consult-register)
          ("C-t" . consult-theme)
          ;; M-g bindings (goto-map)
          :map goto-map
          ("e" . consult-compile-error)
          ("f" . consult-flymake)
          ("g" . consult-goto-line)                 ;; orig. goto-line
          ("M-g" . consult-goto-line)               ;; orig. goto-line
          ("o" . consult-org-heading)               ;; Alternative: consult-org-heading
          ("O" . consult-outline)
          ("m" . consult-mark)
          ("k" . consult-global-mark)
          ("i" . consult-imenu)
          ("I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          :map search-map
          ("a" . consult-ag)                        ;; if executable ag "Silver-Searcher" exists
          ("f" . consult-find)
          ("L" . consult-locate)
          ("g" . consult-grep)
          ("G" . consult-git-grep)
          ("r" . consult-ripgrep)
          ("l" . consult-line)
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

  :custom-face
  (consult-file ((t (:inherit font-lock-string-face))))

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview `C-M-.'
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Prefer ripgrep, then ugrep, and fall back to regular grep.
(setq xref-search-program
      (cond
       ((or (executable-find "ripgrep")
            (executable-find "rg"))
        'ripgrep)
       ((executable-find "ugrep")
        'ugrep)
       (t
        'grep)))

:config
(require 'consult-xref)
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
   consult-bookmark consult-recent-file consult-xref consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl"))

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
                 args)))

  (add-to-list 'consult-mode-histories '(org-mode))

  (defvar consult--xref-history nil
    "History for the `consult-recent-xref' results.")

  (defun consult-recent-xref (&optional markers)
    "Jump to a marker in MARKERS list (defaults to `xref--history'.

  The command supports preview of the currently selected marker position.
  The symbol at point is added to the future history."
    (interactive)
    (consult--read
     (consult--global-mark-candidates
      (or markers (flatten-list xref--history)))
     :prompt "Go to Xref: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input consult--xref-history)
     :add-history (thing-at-point 'symbol)
     :state (consult--jump-state)))

  (defvar org-source
	(list :name     "Org Buffer"
          :category 'buffer
          :narrow   ?o
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
			(with-current-buffer (get-buffer-create name)
              (insert "#+title: " name "\n\n")
              (org-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
			(consult--buffer-query :mode 'org-mode :as #'consult--buffer-pair))))

  (add-to-list 'consult-buffer-sources 'org-source 'append)

  (defvar denote-source
	(list :name     "Denote Buffer"
          :category 'buffer
          :narrow   ?d
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
			(denote name))
          :items
          (lambda ()
			(consult--buffer-query :mode 'org-mode :as #'consult--buffer-pair :filter t :include "\\[D\\].*"))))

  (add-to-list 'consult-buffer-sources 'denote-source 'append)) ;; end of consult

;;;;; [[https://github.com/karthink/consult-dir][consult-dir]]
;; Think of it like the shell tools autojump, fasd or z but for Emacs.
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;;;; consult-dir-vertico
;;
(use-package consult-dir-vertico
  :no-require t
  :ensure nil
  :after (consult-dir vertico)
  :defines (vertico-map)
  :bind (:map vertico-map
              ("C-x C-j" . consult-dir)
              ("M-g d"   . consult-dir)
              ("M-s f"   . consult-dir-jump-file)))

;;;;; [[https://github.com/joaotavora/yasnippet][yasnippet]]
;; YASnippet is a template system for Emacs.
(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :commands yas-minor-mode-on
  :init
  (which-key-add-keymap-based-replacements sej-C-q-map "y" "yasnippet")
  :bind ((:map sej-C-q-map
			   ("y d" . yas-load-directory)
			   ("y f" . yas-visit-snippet-file)
			   ("y n" . yas-new-snippet)
			   ("y t" . yas-tryout-snippet)
			   ("y l" . yas-describe-tables)
			   ("y g" . yas-global-mode)
			   ("y m" . yas-minor-mode)
			   ("y r" . yas-reload-all)
			   ("y x" . yas-expand))
		 (:map yas-keymap
			   ("C-i" . yas-next-field-or-maybe-expand)))
   :hook (prog-mode . yas-minor-mode-on)
  :custom
  (yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
    (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :custom-face
  (yas-field-highlight-face ((t (:background "grey30"))))
  :init
  (use-package yasnippet-snippets))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind (:map sej-C-q-map
			  ("y i" . consult-yasnippet)
			  ("y v" . consult-yasnippet-visit-snippet-file)))

;;;; movement
;;;;; [[https://github.com/alezost/mwim.el][mwim]]
;; better than crux for C-e mwim-end
;; will cycle between end of code and end-of-code plus comments
(use-package mwim
  :bind (("C-a" . mwim-beginning) ; C-a Cycles between Absolute and Logical line beginning
          ("C-e" . mwim-end))) ; C-e better than crux

;;;;; [[https://github.com/abo-abo/avy][avy]]
;; Jump to things in Emacs tree-style
;; https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :bind (("H-'" . avy-goto-char-timer)
          ("M-g l" . avy-goto-line)
          ("C-H-." . avy-pop-mark)
          ("H-l" . avy-goto-line)
          ("M-g w" . avy-goto-word-1)
          ("H-w" . avy-goto-word-1)
          ("H-j" . avy-copy-line)
          ("C-M-s" . isearch-forward-other-window)
          ("C-M-r" . isearch-backward-other-window)
          :map isearch-mode-map
          ("H-s" . avy-isearch))
  :config
  (add-to-list 'savehist-additional-variables 'avy-ring)
  (advice-add 'avy-pop-mark :after #'(lambda () (recenter-top-bottom nil)))
  
  (setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
                      ?a ?s ?d ?f ?g ?h ?j
                      ?k ?l ?' ?x ?c ?v ?b
                      ?n ?, ?/))

  (defun avy-show-dispatch-help ()
     "Display help for Avy dispatch."
     (let* ((len (length "avy-action-"))
            (fw (frame-width))
            (raw-strings (mapcar
                          (lambda (x)
                            (format "%2s: %-19s"
                                    (propertize
                                     (char-to-string (car x))
                                     'face 'aw-key-face)
                                    (substring (symbol-name (cdr x)) len)))
                          avy-dispatch-alist))
            (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
            (strings-len (length raw-strings))
            (per-row (floor fw max-len))
            display-strings)
       (cl-loop for string in raw-strings
                for N from 1 to strings-len do
                (push (concat string " ") display-strings)
                (when (= (mod N per-row) 0) (push "\n" display-strings)))
       (message "%s" (apply #'concat (nreverse display-strings)))))

  ;; Kill text
  (defun avy-action-kill-whole-line (pt)
    "Avy kill whole line at PT selected."
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  ;; Copy text
  (defun avy-action-copy-whole-line (pt)
    "Avy copy whole line at PT selected."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

  ;; Yank text
  (defun avy-action-yank-whole-line (pt)
    "Avy yank whole line at PT selected."
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  ;; Transpose/Move text
  (defun avy-action-teleport-whole-line (pt)
    "Avy teleport whole line at PT selected."
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  ;; Mark text
  (defun avy-action-mark-to-char (pt)
    "Avy mark to char at PT selected."
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  ;; Dictionary: define words
  ;; Replace your package manager or preferred dict package
  (defun dictionary-search-dwim (&optional arg)
    "Avy action search for definition of word at point depending on ARG.
If region is active, search for contents of region instead.
If called with a prefix argument, query for word to search."
    (interactive "P")
    (if arg
        (dictionary-search nil)
      (if (use-region-p)
          (dictionary-search (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
        (if (thing-at-point 'word)
            (dictionary-lookup-definition)
          (dictionary-search-dwim '(4))))))

  (defun avy-action-define (pt)
         "Avy action Dictionary search at PT dwim."
         (save-excursion
           (goto-char pt)
           (dictionary-search-dwim))
         (select-window
          (cdr (ring-ref avy-ring 0)))
         t)

  (setf (alist-get ?= avy-dispatch-alist) 'dictionary-search-dwim)

  ;; Get Elisp Help
  ;; Replace with your package manager or help library of choice
  (defun avy-action-helpful (pt)
    "Avy action Helpful at PT."
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  ;; Embark
  (defun avy-action-embark (pt)
    "Avy action Embark on expression at PT."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  ;; Isearch in other windows
  (defun isearch-forward-other-window (prefix)
    "Function to 'isearch-forward' in 'other-window', modified by PREFIX."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

  (defun isearch-backward-other-window (prefix)
         "Function to 'isearch-backward' in 'other-window', modified by PREFIX."
         (interactive "P")
         (unless (one-window-p)
           (save-excursion
             (let ((next (if prefix 1 -1)))
               (other-window next)
               (isearch-backward)
               (other-window (- next)))))))

;;;;; goto-last-change
;; goto the last changes made in buffer
;; https://github.com/camdez/goto-last-change.el
(use-package goto-last-change
  :bind ("H-." . goto-last-change-with-auto-marks)
  :config
  (advice-add 'goto-last-change-with-auto-marks :after #'recenter-top-bottom))

;;;;; beginend
;; smart moves redefining M-< and M-> for some modes
;; https://github.com/DamienCassou/beginend
(use-package beginend               ; smart M-< & M->
  :bind (("M-<" . beginning-of-buffer)
         ("M->" . end-of-buffer))
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (blackout mode))
  (beginend-global-mode))

;;;;; subword
;; built-in: Handling capitalized subwords in a nomenclature
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/subword.el
(use-package subword
  :blackout t
  :hook (emacs-startup . subword-mode)
  :config
  ;; this makes forward-word & backward-word understand snake & camel case
  (setq c-subword-mode t)
  (global-subword-mode t))

;;;;; string inflection
;; underscore -> UPCASE -> Camelcase conversion
;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :bind (("M-u" . string-inflection-all-cycle)))

;;;; regions
;;;;; easy-kill-extras
;; This package contains extra functions for easy-kill/easy-mark.
;; Kill & Mark things easily
;; https://github.com/leoliu/easy-kill
;; https://github.com/knu/easy-kill-extras.el
(use-package easy-kill-extras
  :bind (("M-w" . easy-kill) ; M-w
          ([remap mark-sexp] . easy-mark-sexp) ; C-M-<SPC>
          ("M-@" . easy-mark-word) ; M-@
          ("H-<SPC>" . easy-mark) ; H-<SPC>
          ("M-z" . easy-mark-to-char)) ; M-z
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
;; built-in: Do not delete selection if you insert
;; https://github.com/typester/emacs/blob/master/lisp/delsel.el
(use-package delsel
  :ensure nil
  :hook (emacs-startup . delete-selection-mode))

;;;;; rect
;; built-in: Rectangle
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/rect.el
(use-package rect
  :ensure nil)

;;;;; drag-stuff
;; Drag stuff (lines, words, region, etc...) around
;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :blackout
  :hook (emacs-startup . drag-stuff-global-mode)
  :bind (("M-<down>" . sej/drag-stuff-down)
         ("M-<up>" . sej/drag-stuff-up))
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (defun sej/drag-stuff-up ()
	"Mod of drag-stuff-up which works in org-mode."
    (interactive)
	(if (equal major-mode "org-mode")
    (call-interactively
     (if (org-at-heading-p)
         'org-metaup
       'drag-stuff-up))
	(drag-stuff-up 1)))

  (defun sej/drag-stuff-down ()
	"Mod of drag-stuff-down which works in org-mode."
    (interactive)
	(if (equal major-mode "org-mode")
    (call-interactively
     (if (org-at-heading-p)
         'org-metadown
       'drag-stuff-down))
	(drag-stuff-down 1)))

  (defun sej/indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'sej/drag-stuff-up :after 'sej/indent-region-advice)
  (advice-add 'sej/drag-stuff-down :after 'sej/indent-region-advice))

;;;;; set region writeable
;; handy when text is read-only and cannot be deleted
;; answer to question on stack-overflow
(defun sej/set-region-writeable (begin end)
  "Remove the read-only text property from the marked region of BEGIN END."
  ;; See http://stackoverflow.com/questions/7410125
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;; url actions
;;;;; sej/url-insert
;; improved from jcs (Irreal) blog to copy url from safari and paste at point
;; https://irreal.org/blog/?p=2895
(when sys/macp
  (defun sej/url-insert-safari (desc)
    "Retrieve URL from current Safari page and prompt for description.
      Insert an Org link at point."
    (interactive "sLink Description (None to display url): ")
    (let ((link (do-applescript "tell application \"Safari\" to return URL of document 1")))
      (if (> (length desc) 0)
          (insert (format "[[%s][%s]]" (org-trim link) desc))
        (insert (format "[[%s]]" (org-trim link)))) ))

  (bind-key* "C-H-u" 'sej/url-insert-safari))

(when sys/macp
  (defun sej/url-insert-edge (desc)
    "Retrieve URL from current Edge page and prompt for description.
      Insert an Org link at point."
    (interactive "sLink Description (None to display url): ")
    (let ((link (do-applescript "tell application \"Microsoft Edge\" to return URL of active tab of front window")))
      (if (> (length desc) 0)
          (insert (format "[[%s][%s]]" (org-trim link) desc))
        (insert (format "[[%s]]" (org-trim link)))) ))

  (bind-key* "C-H-i" 'sej/url-insert-edge))

;;;;; sej/url-git-clone-from-clipboard
;; from Alvaro Ramirez function to git clone from url in clipboard mods by me
;; http://xenodium.com/emacs-clone-git-repo-from-clipboard/
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

;;;;; org-mac-link
(use-package org-mac-link
  ;; pull link from many osx apps
  ;; https://gitlab.com/aimebertrand/org-mac-link
  :bind ("C-H-o" . org-mac-link-get-link))

;;;;; ace-link
;; Quickly follow links
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :bind (("H-u" . ace-link-addr)
         :map org-mode-map
         ("H-u" . ace-link-org))
  :config (ace-link-setup-default))

;;;;; orglink
;; use Org mode links in other modes
;; https://github.com/tarsius/orglink
(use-package orglink
  :hook (emacs-startup . global-orglink-mode))

;;;;; restclient
;; Allows query of a restclient with the results left into the buffer
;; use GET or POST plus http:10.0.1.1/rest/
;; then use C-c to execute (check examples directory for more)
;; https://github.com/pashky/restclient.el
(use-package restclient)


;;;; highlighting faces fonts
;;;;; fontaine
;; font management
;; [[https://protesilaos.com/emacs/fontaine]]
(use-package fontaine
  :bind ("C-c f" . fontaine-set-preset)
  :hook (emacs-startup . fontaine-mode)
  :config
(let* ((variable-tuple
        (cond ((x-list-fonts "Atkinson Hyperlegible") "Atkinson Hyperlegible")
              ((x-list-fonts "Iosevka")  "Iosevka")
              ((x-list-fonts "ETBembo")  "ETBembo")
              ((x-list-fonts "Source Sans Pro")  "Source Sans Pro")
              ((x-list-fonts "Lucida Grande")  "Lucida Grande")
              ((x-list-fonts "Verdana")  "Verdana")
              ((x-family-fonts "Sans Serif")  "Sans Serif")
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (fixed-tuple
        (cond    ((x-list-fonts "Iosevka Fixed")   "Iosevka Fixed")
              ((x-list-fonts "Iosevka")   "Iosevka")
              ((x-list-fonts "ETBembo")  "ETBembo")
              ((x-list-fonts "Source Sans Pro")  "Source Sans Pro")
              ((x-list-fonts "Lucida Grande")  "Lucida Grande")
              ((x-list-fonts "Verdana")  "Verdana")
              ((x-family-fonts "Sans Serif")  "Sans Serif")
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (sej-t-text  `(t
                ;; I keep all properties for didactic purposes, but most can be
                ;; omitted.  See the fontaine manual for the technicalities:
                ;; <https://protesilaos.com/emacs/fontaine>.
                :default-family ,fixed-tuple
                :default-weight semilight
                :default-height 140

                :fixed-pitch-family nil ; falls back to :default-family
                :fixed-pitch-weight nil ; falls back to :default-weight
                :fixed-pitch-height 1.0

                :fixed-pitch-serif-family nil ; falls back to :default-family
                :fixed-pitch-serif-weight nil ; falls back to :default-weight
                :fixed-pitch-serif-height 1.0

                :variable-pitch-family ,variable-tuple
                :variable-pitch-weight nil
                :variable-pitch-height 1.0

                :mode-line-active-family ,variable-tuple
                :mode-line-active-weight regular
                :mode-line-active-height 1.0

                :mode-line-inactive-family ,variable-tuple
                :mode-line-inactive-weight regular
                :mode-line-inactive-height 1.0

                :header-line-family nil ; falls back to :default-family
                :header-line-weight nil ; falls back to :default-weight
                :header-line-height 1.0

                :line-number-family nil ; falls back to :default-family
                :line-number-weight nil ; falls back to :default-weight
                :line-number-height 1.0

                :tab-bar-family nil ; falls back to :default-family
                :tab-bar-weight nil ; falls back to :default-weight
                :tab-bar-height 1.0

                :tab-line-family nil ; falls back to :default-family
                :tab-line-weight nil ; falls back to :default-weight
                :tab-line-height 1.0

                :bold-family nil ; use whatever the underlying face has
                :bold-weight bold

                :italic-family nil
                :italic-slant italic

                :line-spacing nil))

       (sej-font-text `((regular) ; like this it uses all the fallback values and is named `regular'
               (small
                :default-height 100)
               (medium
                :default-height 160)
               (large
                :inherit medium
                :default-height 180)
               (presentation
                :default-height 200)
               ,sej-t-text ) ) )
  (setq fontaine-presets sej-font-text) )

;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))  )

;;;;; show-font
;; preview fonts on the system
;; [[https://protesilaos.com/emacs/show-font][manual]], [[https://github.com/protesilaos/show-font][github]], [[https://protesilaos.com/codelog/2024-09-10-emacs-show-font-0-1-0/][pics]]
(use-package show-font
  :init
  (which-key-add-keymap-based-replacements sej-C-q-map "C-f" "show-font")
  :bind (:map sej-C-q-map
			  ("C-f f" . show-font-select-preview)
			  ("C-f l" . show-font-list)
			  ("C-f t" . show-font-tabulated)))

;;;;; symbol-overlay
;; Highlight symbols and move between them
;; https://github.com/wolray/symbol-overlay
(use-package symbol-overlay
  :blackout t
  :defines iedit-mode
  :commands (symbol-overlay-get-symbol
             symbol-overlay-assoc
             symbol-overlay-get-list
             symbol-overlay-jump-call)
  :bind (("C-:" . iedit-mode) ;; define Iedit mode so as to remove default message
         ("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :bind-keymap ("C-q s" . symbol-overlay-map)
  :init
  (which-key-add-key-based-replacements
    "C-q s" "Symbol Overlay")
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . sej/symbol-overlay-mode-off)
         (iedit-mode-end . symbol-overlay-mode)))

(defun sej/symbol-overlay-mode-off()
  "Turn 'symbol-overlay-mode' off."
  (interactive)
  (symbol-overlay-mode -1))

;;;;; highlight-numbers
;; hightlight-numbers in a special way
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;;;;; highlight-quoted
;; [[https://github.com/Fanael/highlight-quoted][highlight-quoted]]
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;;;;; rainbow-mode
;; Colorize color names in buffers
;; https://github.com/emacsmirror/rainbow-mode
(use-package rainbow-mode
  :blackout t
  :hook ((prog-mode . rainbow-mode)
		 (org-mode . rainbow-mode)
		 (conf-space-mode . rainbow-mode))
  :bind (:map sej-C-q-map
			  ("R" . rainbow-mode))
  :config
  (setq rainbow-ansi-colors t
        rainbow-x-colors t))

;;;;; hl-todo
;; Highlight TODO and similar keywords in comments and strings
;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo"
            :rev :newest
            :branch "main")
  :custom-face
  (hl-todo ((t (:box nil :inherit))))
  :bind (:map hl-todo-mode-map
			  ("H-P" . hl-todo-previous)
			  ("H-N" . hl-todo-next)
			  ("C-q T o" . hl-todo-occur)
			  ("C-q T r" . hl-todo-ripgrep) )
  :hook ((emacs-startup . global-hl-todo-mode)
         (prog-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))
  :init
  (which-key-add-key-based-replacements
    "C-q T" "hl-todo")
  :config
  (setq hl-todo-keyword-faces
        '(
          ("DEBUG" . "#7cb8bb")
          ("FIX" . "#ff9393")
          ("HELP" . "#ff0000")
          ("KLUDGE" . "#cc9999")
          ("LATER" . "#2c5353")
          ("MAYBE" . "#d1bf8f")
          ("NOTE" . "#5f6000")
          ("OKAY" . "#5f9000")
          ("TRY" . "#5f7f5f")
          ("TODO" . "#cc9393")
          ("TEST" . "#ff7700")
          ))

  (push 'org-mode hl-todo-include-modes))

;;;;; consult-todo
;; search and jump hl-todo keywords in buffers with consult
;; [[https://github.com/liuyinz/consult-todo]]
(use-package consult-todo
  :bind (:map sej-C-q-map
			  ("T c" . consult-todo)
			  ("T C" . sej/consult-todo-toggle-comment)
			  ("T d" . consult-todo-dir)
			  ("T p" . consult-todo-project)
			  ("T a" . consult-todo-all))
  :config
  (setq consult-todo-narrow
		(let (value)
		  (dolist (element (sort hl-todo-keyword-faces :reverse t) value)
			(setq value (cons
						 (cons
						  (string-to-char (downcase (substring (car element) 0 1)))
						  (car element))
						 value)))))
  (defun sej/consult-todo-toggle-comment ()
	"Function to toggle `consult-todo-only-comment'."
	(interactive)
	(unless (boundp 'consult-todo-only-comment)
	   (setq consult-todo-only-comment nil))
	 (embark-toggle-variable 'consult-todo-only-comment)) )

;;;;; volatile-highlights
;; Highlight some buffer region operations
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :blackout t
  :hook (emacs-startup . volatile-highlights-mode))

;;;;; Pulsar
;; small package that temporarily highlights the current line after a given function is invoked
;; additional to built-in pulse
;; https://protesilaos.com/emacs/pulsar
(use-package pulsar
  :bind (:map sej-C-q-map
               ("P p" . pulsar-pulse-line)
               ("P h" . pulsar-highlight-line))
  :hook ((emacs-startup . pulsar-global-mode)
         ((consult-after-jump
           bookmark-after-jump
           magit-diff-visit-file) . pulsar-recenter-top)
         ((consult-after-jump
           imenu-after-jump) . pulsar-reveal-entry)
         (next-error . pulsar-pulse-line-red))
  :init
  (which-key-add-keymap-based-replacements sej-C-q-map "P" "Pulsar")
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
;; built-in: show paren mode
;; https://www.emacswiki.org/emacs/ShowParenMode
(use-package paren
  :ensure nil
  :hook (emacs-startup . show-paren-mode)
  :config
  (setq show-paren-delay 0
        show-paren-style 'parenthesis ; parenthesis, expression, mixed
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t
        show-paren-context-when-offscreen 'child-frame))

;;;; indentation
;;;;; outli
;; outlining with comments, simpler/updated than outshine
;; https://github.com/jdtsmith/outli
(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli"
            :rev :newest
            :branch "main")
  :hook ((prog-mode text-mode) . outli-mode) ; or whichever modes you prefer
  :config
  (setq outli-speed-commands nil))

;;;;; indent-bars
;; Highlight indentations
;; https://github.com/jdtsmith/indent-bars
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode) ; or whichever modes you prefer(use-package indent-bars
  :custom
  (indent-bars-prefer-character t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
  if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				      list list_comprehension
				      dictionary dictionary_comprehension
				      parenthesized_expression subscript)))  )

;;;;; indentation & outline settings
(setq outline-minor-mode-prefix (kbd "C-'"))
(bind-keys* ("C-S-n" . outline-next-visible-heading)
            ("C-S-p" . outline-previous-visible-heading)
            ("C-n" . next-line)
            ("C-P" . previous-line))

;;;;; dtrt-indent
;; automatically set the right indent for other people's files
;; https://github.com/jscheid/dtrt-indent
(use-package dtrt-indent
  :hook (emacs-startup . dtrt-indent-mode)
  :blackout)

;;;;; sej/indent-buffer
;; - bound to C-q <tab>
(defun sej/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key* "C-q <tab>" 'sej/indent-buffer)

;;; programming

;;;; debug & formatting tools
;;;;; prog-mode
;; built-in: generalized program mode
;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :no-require t
  :ensure nil
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
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
  ;; Make URLs in comments/strings clickable, (Emacs > v22).
  (add-hook 'find-file-hooks 'goto-address-prog-mode))

;;;;; emr
;; a framework for providing language-specific refactoring in Emacs.
;; It includes refactoring commands for a variety of languages
;; https://github.com/emacsmirror/emr
(use-package emr
  ;; Just hit H-r to access your refactoring tools in any supported mode.
  :bind (:map sej-C-q-map
              ("r" . emr-show-refactor-menu)))

;;;;; flymake-ruff
;; ruff linter
;; [[https://github.com/erickgnavar/flymake-ruff][flymake-ruff]]
;; ("ruff" "check" "--quiet" "--stdin-filename=stdin" "-")
(use-package flymake-ruff
  :ensure-system-package ruff
  :hook (python-base-mode . flymake-ruff-load)
  ;; :hook (eglot-managed-mode-hook , flymake-ruff-load)
  )

;;;;; reformatter
;; base reformatter
;; [[https://github.com/purcell/emacs-reformatter][reformatter]]
(use-package reformatter)

;;;;; ruff-format
;; ruff format using reformatter
;; [[https://github.com/scop/emacs-ruff-format?tab=readme-ov-file][ruff-format]]
(use-package ruff-format
  :ensure-system-package ruff
  :after reformatter)

;;;;; format-all
;; auto-format source code in many languages using the same command for all languages
;; You will need to install external programs to do the formatting
;; https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :bind (:map sej-C-q-map
              ("f" . format-all-buffer)))

;;;; server tools
;;;;; tree-sitter
;; built-in: Emacs Lisp binding for tree-sitter, an incremental parsing library.
;; Important this is using the built-in Emacs version tree-sit.el
;; https://github.com/emacs-tree-sitter/elisp-tree-sitter
;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
(use-package treesit
  :ensure nil)

(use-package treesit-auto
  ;; some auto features for tree-sit
  ;; [[https://github.com/renzmann/treesit-auto]]
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  ;; standard movement using treesitter
  ;; https://github.com/mickeynp/combobulate
  ;; use C-c o o for menu
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.

  :vc (:url "https://github.com/mickeynp/combobulate"
            :rev :newest
            :branch "master")
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (c++-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

;;;;; eglot
;; built-in: simple client for Language Server Protocol servers
;; https://github.com/joaotavora/eglot
;; https://joaotavora.github.io/eglot/#Eglot-and-LSP-Servers
(use-package eglot
  :ensure nil
  :commands eglot
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)
         (python-base-mode . eglot-ensure)
         ((c-or-c++-mode objc-mode cuda-mode) . eglot-ensure) )

  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c o" . eglot-code-action-organize-imports)
			  ("C-c C-." . eglot-code-actions)
              ("C-c h" . eglot-help-at-point)
              ("C-c x" . xref-find-definitions)
			  ("M-^" . eglot-find-implementation))
  :custom
  (eglot-autoshutdown t)
  :config
  (defun sej/eglot-ensure-prg ()
    "run eglot in all prog except"
    (interactive)
    (if (eq major-mode 'emacs-lisp-mode)
        t
      (eglot-ensure)))

  (add-to-list 'eglot-server-programs
               `((c-or-c++-mode objc-mode cuda-mode) . ,(eglot-alternatives
                                                         '(("ccls" )
                                                           ("clangd")))))
(setq read-process-output-max (* 1024 1024))

  (add-hook 'eglot-managed-mode-hook
            #'(lambda ()
                ;; Show flymake diagnostics first.
                (setq eldoc-documentation-functions
                      (cons #'flymake-eldoc-function
                            (remove #'flymake-eldoc-function
                                    eldoc-documentation-functions)))))

  ;; basedpyright-langserver [[https://docs.basedpyright.com/latest/installation/ides/][link]]
  ;; Emacs set-up [[https://webbureaucrat.gitlab.io/articles/emacs-for-python-and-poetry-using-basedpyright-langserver/][link]]
  ;; brew install basedpyright
  (add-to-list 'eglot-server-programs
			   '((python-mode python-ts-mode)
               "basedpyright-langserver" "--stdio"))

  (setq help-at-pt-display-when-idle t)
  (setq completion-category-defaults nil)
  (use-package consult-eglot
    :commands consult-eglot-symbols))

;;;; ssh tools
;;;;; tramp
;; built-in: remote editing
;; https://www.gnu.org/software/tramp/
(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh" ; or scp
        tramp-terminal-type "dumb"
        tramp-verbose 10
        tramp-completion-reread-directory-timeout nil
        tramp-histfile-override "/tmp/tramp_history"
		tramp-auto-save-directory "~/.cache/emacs/backups"
        remote-file-name-inhibit-cache nil
        tramp-default-remote-shell "/bin/bash"
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
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (add-to-list 'tramp-default-user-alist '("\\`localhost\\'" "\\`root\\'" "su")))

(use-package tramp-sh
  :ensure nil
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

;;;; commenting tools
;;;;; New-Comment
;; built-in: library contains functions and variables for commenting and
;; uncommenting source code.
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html]]
(use-package newcomment
  :ensure nil
  :bind (("H-;" . comment-box)
		 ("M-;" . comment-line))
  :custom (comment-empty-lines t
							   comment-fill-column 0
							   comment-multi-line t
							   comment-style 'multi-line))

;;;;; ediff
;; built-in: A saner diff
;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-diff-options "-w")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-shell (getenv "$SHELL")))

;;;; paren management
;;;;; electric
;; built-in: electric indent mode
(use-package electric
  :ensure nil
  :hook ('prog-mode . electric-indent-mode)
  :bind (("C-j". sej/open-new-line)
		 ("C-S-J" . sej/open-line-above-and-indent))
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
;; built-in: Automatic parenthesis pairing
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/elec-pair.el
(use-package elec-pair
  :disabled ;; using smartparens for now
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :config
  (electric-layout-mode t)
  (electric-indent-mode t))

;;;;; smartparens
;; minor mode for dealing with pairs in Emacs
;; [[https://github.com/Fuco1/smartparens][smartparens]]
(use-package smartparens
  :init
  (which-key-add-key-based-replacements
    "C-q C-q" "Smartparens")
  :blackout t
  :bind-keymap ("C-q C-q" . smartparens-mode-map)
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

;;;; compile and make tools
;;;;; compile
;; built-in: Compilation Mode
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html
(use-package compile
  :ensure nil
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
;; built-in: Major mode for editing standard Makefiles
;; [[http://doc.endlessparentheses.com/Fun/makefile-mode.html]]
(use-package make-mode
  :ensure nil
  :blackout ((makefile-automake-mode . "Makefile")
             (makefile-gmake-mode . "Makefile")
             (makefile-makepp-mode . "Makefile")
             (makefile-bsdmake-mode . "Makefile")
             (makefile-imake-mode . "Makefile")))

;;;;; flymake
;; built-in: Emacs syntax checker
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top
(use-package flymake
  :ensure nil
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
  (setq flymake-show-diagnostics-at-end-of-line nil)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)  )

;;;; vcs
;;;;; Project
;; built-in: project management
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html]]
(use-package project
  :ensure nil
  :config
  (setq project-file-history-behavior 'relativize))

;;;;; magit
;; interface to the version control system Git
;; https://magit.vc/
(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-log-section-commit-count 30)
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t
        magit-repository-directories '(("~/Projects" . 1)))

                                        ; enter magit full frame
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
                                        ; exit magit restoring frame config
  (setq magit-bury-buffer-function
        #'magit-restore-window-configuration)
  (if (fboundp 'transient-append-suffix)
      ;; Add switch: --tags
      (transient-append-suffix 'magit-fetch
        "-p" '("-t" "Fetch all tags" ("-t" "--tags"))))
  (setopt magit-format-file-function #'magit-format-file-nerd-icons))

;;;;; disproject
;; integration with project.el and allows for dispatching via transient menus
;; [[https://github.com/aurtzy/disproject]]
(use-package disproject
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map
          ("p" . disproject-dispatch))
  :config
  (require 'magit nil 'noerror)
  (require 'magit-todos nil 'noerror))

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
  :bind (:map global-map
		 ("C-H-]" . git-gutter:next-hunk)
		 ("C-H-[" . git-gutter:previous-hunk))
  :hook (emacs-startup . global-git-gutter-mode)
  :init (setq git-gutter:lighter "")
  :config
  (advice-add 'git-gutter:next-hunk :after #'(lambda () (recenter-top-bottom nil))))

;;;;; magit-todos
;; Show tasks from commit files
;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :after magit
  :commands (disproject-dispatch magit-status-mode)
  :hook (magit-status-mode . magit-todos-mode)
  :config
  (magit-todos-mode 1)
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
;; built-in: Resolve diff3 conflicts
;; http://web.mit.edu/Emacs/source/emacs/lisp/smerge-mode.el
(use-package smerge-mode
  :ensure nil
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
;; Open github/gitlab/bitbucket page
;; https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :bind (:map sej-C-q-map
          ("B" . browse-at-remote)
          :map vc-prefix-map
          ("B" . browse-at-remote)))

;;;;; gist
;; gist client
;; built-in self help
;; [[https://github.com/KarimAziev/igist]]
(use-package igist
  :bind (:map sej-C-q-map
              ("G" . igist-dispatch)))

;;;;; git-modes
;; Emacs major modes for various Git configuration files.
;; gitattributes-mode , gitconfig-mode , gitignore-mode
;; https://github.com/magit/git-modes
(use-package git-modes)

;;;;; sej/git-blame-line
;; Runs `git blame` on the current line and adds the commit id to the kill ring
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
;;;;; lisp modes
(use-package lisp-mode
  :ensure nil
  :hook (((emacs-lisp-mode lisp-mode) . (lambda () (add-hook 'after-save-hook #'check-parens nil t)))
		 (emacs-lisp-mode-hook . flymake-mode))
  :bind ((:map global-map
			   ("C-q C-e" . toggle-debug-on-error))
		 (:map emacs-lisp-mode-map
			   ("C-<return>" . sej/eval-dwim)
			   ("C-x C-e" . sej/eval-dwim)
			   ("H-<return>" . eval-buffer)))
  :custom
  (parens-require-spaces t)
  :init
  (dolist (mode '(ielm-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  lisp-interaction-mode
                  lisp-mode
                  emacs-lisp-mode))
    (font-lock-add-keywords
     mode
     '(("(\\(lambda\\)\\>"
        (0 (ignore
            (compose-region (match-beginning 1)
                            (match-end 1) ?λ))))
       ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t)))))
  :config
  ;; taken from here [[http://blog.shanderlam.com/][eval-dwim]]
  (defun sej/eval-dwim (arg)
	"Call eval command you want (Do What I Mean).
     If the region is active and option `transient-mark-mode' is on, call
     `eval-region'. Else, call `eval-last-sexp' using (ARG)."
	(interactive "P")
	(if (and transient-mark-mode mark-active)
		(eval-region (region-beginning) (region-end))
	  (eval-last-sexp arg)))
  
  ;; enable dash for Emacs lisp highlighting
  (eval-after-load "dash" '(dash-enable-font-lock)))

;;;;; eldoc
;; built-in: we don't want this minor mode to be shown in the minibuffer, however
;; we use eldoc to show the signature of the function at point in the minibuffer
;; https://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :blackout t
  :ensure nil
  :preface
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc" display-buffer-at-bottom
                 (window-height . 4)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :hook
  ((prog-mode . turn-on-eldoc-mode))
  :init
  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p 3
		eldoc-echo-area-display-truncation-message nil)
  :config
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

;;;;; elint
(use-package elint
  :commands (elint-initialize elint-current-buffer)
  :bind ("C-q e" . sej/elint-current-buffer)
  :preface
  (defun sej/elint-current-buffer ()
    (interactive)
    (elint-initialize)
    (elint-current-buffer))
  :config
  (add-all-to-list 'elint-standard-variables
                   'current-prefix-arg
                   'command-line-args-left
                   'buffer-file-coding-system
                   'emacs-major-version
                   'window-system))

;;;;; elisp-slime-nav
;; turn on elisp-slime-nav
;; M-. works to jump to function definitions
;; M-, to jump back
;; https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav
  :blackout t
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
  :custom
  (find-function-C-source-directory (expand-file-name "~/src/emacs-dev/src")))

;;;;; sly
;; replacement repla for slime
;; [[https://github.com/joaotavora/sly][sly]]
(use-package sly
  :hook (lisp-mode . sly-mode)
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        sly-mrepl-history-file-name (nl-var-expand "sly-mrepl-history")))

;;;;; eros
;; eros-mode will show you the result of evaluating an elisp command
;; as an overlay in your elisp buffer. Try it out with C-x C-e or s-<return>
;; https://github.com/xiongtx/eros
(use-package eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode))

;;;;; ielm
;; built-in: add a nice popup for ielm
;; https://www.emacswiki.org/emacs/InferiorEmacsLispMode
(use-package ielm
  :ensure nil
  :commands ielm
  :bind (("s-i" . sej/ielm-other-window)
         :map sej-C-q-map
         ("i" . sej/ielm-other-window)
		 :map ielm-map
		 ("<return>" . sej/ielm-return))
  :hook ((ielm-mode . show-paren-mode)
		 (ielm-mode . electric-pair-mode))
  :config
  (defun sej/ielm-other-window ()
    "Run ielm on other window."
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*ielm*"))
    (call-interactively 'ielm))

  (defun sej/ielm-return ()
    (interactive)
    (let ((end-of-sexp (save-excursion
                         (goto-char (point-max))
                         (skip-chars-backward " \t\n\r")
                         (point))))
      (if (>= (point) end-of-sexp)
          (progn
            (goto-char (point-max))
            (skip-chars-backward " \t\n\r")
            (delete-region (point) (point-max))
            (call-interactively #'ielm-return))
        (call-interactively #'paredit-newline)))))

;;;;; sej/remove-elc-on-save
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

;;;;; geiser ( guile ) ( closure )
;; Emacs for guile
;; [[https://www.nongnu.org/geiser/geiser_2.html#Installation][geiser]]
;; [[https://jeko.frama.io/en/emacs.html][guile hacking manual]]
(use-package geiser)

;;;; python
;;;;; python
;; Install:
;; pip3 install -U setuptools
;; brew install pyright
;; optional install basedpyright
;; YAPF or Black
;; http://wikemacs.org/wiki/Python
;; (require 'python)
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
;; built-in: runs a python interpreter as a subprocess of Emacs
;; [[http://doc.endlessparentheses.com/Fun/inferior-python-mode.html][inferior-python-mode]]
(use-package inferior-pyton-mode
:ensure nil  )

;;;;; pipenv
;; a [[https://github.com/pwalsh/pipenv.el][pipenv]] integration
(use-package pipenv
  :hook (python-base-mode . pipenv-mode)
  :init
  (defun pipenv-activate-project ()
    "Activate integration of Pipenv with Project."
    ;; (add-hook
    ;;  'project-
    ;;  ile-after-switch-project-hook
    ;;  (lambda () (funcall pipenv-projectile-after-switch-function))))

    (setq pipenv-with-flycheck nil
          pipenv-with-projectile nil)))

;;;;; pyenv-mode
;; integration with the pyenv tool
;; [[https://github.com/pythonic-emacs/pyenv-mode][pyenv-mode]]
(use-package pyenv-mode
  :hook (python-base-mode . pyenv-mode))

;;;;; pyenv-mode-auto
;; pyenv automatically based on .python-mode
;; https://github.com/emacsattic/pyenv-mode-auto
(use-package pyenv-mode-auto
  :vc (:url "https://github.com/emacsattic/pyenv-mode-auto"
            :rev :newest
            :branch "master"))

;;;;; envrc
;; A GNU Emacs library which uses the direnv tool to determine per-directory/project
;; environment variables and then set those environment variables on a per-buffer basis.
;; https://github.com/purcell/envrc
(use-package envrc
  :hook (after-init . envrc-global-mode))

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
(use-package json-mode
  :mode "\\.json\\'")

;;;;; [[https://github.com/Sterlingg/json-snatcher][json-snatcher]]
;; pull path to value in large JSON
(use-package json-snatcher
  :commands jsons-print-path
  :after json-mode)

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
;; built-in: major mode for editing XML
;; https://www.gnu.org/software/emacs/manual/html_node/nxml-mode/Introduction.html
(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))


;;;; c program modes
;;;;; c-mode
;; built-in: C/C++ Mode
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook ((c-mode-common . (lambda ()
                            (c-set-style "bsd")
                            (setq c-basic-offset 4) )))
  :init
  ;; Set the default formatting styles for various C based modes
  (setq c-default-style
        '((awk-mode . "awk")
		  (java-mode . "gnu")
          (other . "gnu"))))

(use-package c-ts-mode
  :ensure nil
  :bind (:map c-ts-base-mode-map
              ("C-c c" . compile))
  :init
  ;; Set the default formatting styles for various C based modes
  (setq c-ts-mode-set-global-style 'gnu)
  (setq c-ts-mode-indent-style 'gnu)
  )

;;;;; ccls
;; c++-mode, objc-mode, cuda-mode
;; https://github.com/MaskRay/ccls/wiki/lsp-mode
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

;;;;; eglot-ccls
;; specific extensions for Eglot
;; https://codeberg.org/akib/emacs-eglot-ccls
(use-package eglot-ccls
  :vc (:url "https://codeberg.org/akib/emacs-eglot-ccls"
            :rev :newest
            :branch "master")
  :hook (((c-mode c++-mode objc-mode cuda-mode c-or-c++-mode
                  c-ts-mode c++-ts-mode c-or-c++-ts-mode) . eglot-ccls-semhl-mode))
  :init
  (defun sej/eglot-ccls-use-colour ()
    "front end for eglot-ccls-use-default"
    (interactive)
    (eglot-ccls-semhl-use-default-rainbow-highlight)) )

;;;;; clang-format
;; Clang-format emacs integration for use with C/Objective-C/C++.
;; https://github.com/sonatard/clang-format
(use-package clang-format
  :bind (:map c-mode-base-map
              ("C-c v" . clang-format-region)
              ("C-c u" . clang-format-buffer))
  :config
  (setq clang-format-style-option "llvm"))

;;;;; modern-cpp-font-lock
;; Syntax highlighting support for "Modern C++" - until C++20 and Technical Specification
;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;;;;; csharp-mode
;; mode for editing C# in emacs. It’s based on cc-mode
;; https://github.com/josteink/csharp-mode
(use-package csharp-mode)

;;;;; arduino-mode
;; mode for .ino files only
;; https://github.com/stardiviner/arduino-mode/tree/16955f579c5caca223c0ba825075e3573dcf2288
(use-package arduino-mode
  :hook (arduino-mode . arduino-cli-mode)
  :ensure-system-package arduino-cli
  :config
  (setq arduino-mode-home "~/src/Arduino"
        arduino-executable (expand-file-name "Arduino" "~/Applications/Arduino/Contents/MacOS/")))

;;(remove-hook 'c++mode-hook #'arduino-mode-cli)

;;;;; arduino-cli-mode
;; minor mode for using the excellent new arduino command line interface
;; https://github.com/motform/arduino-cli-mode
(use-package arduino-cli-mode
  :hook (c++-mode . arduino-cli-mode)
  :ensure-system-package arduino-cli
  :config
  (setq arduino-cli-warnings 'all
        arduino-cli-verify t
        arduino-cli-default-fqbn "esp8266:esp8266:d1"
        arduino-cli-default-port "/dev/cu.wchusbserial1430"))

;;;;; platformio-mode
;; minor mode which allows quick building and uploading of PlatformIO projects
;;   with a few short key sequences.
;; Code completion can be provided by installing any package compatible with .clang_complete files,
;;   such as irony-mode.
;; To keep the index up to date, run platformio-init-update-workspace (C-c i i)
;;   after installing any libraries.
;; https://github.com/ZachMassia/platformio-mode
(use-package platformio-mode
  :hook ((c-mode c++-mode objc-mode cuda-mode c-or-c++-mode
                 c-ts-mode c++-ts-mode c-or-c++-ts-mode) . platformio-conditionally-enable))

;;;;; rust-mode
;; rust language package
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :config (setq rust-format-on-save t))

;;;;; go lang
;; two different ways to configure
;; https://arenzana.org/2019/12/emacs-go-mode-revisited/
;; https://github.com/golang/tools/blob/master/gopls/README.md
;; need to install golang and go get golang/x/tools/gopls
;; https://sandyuraz.com/articles/go-emacs/
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
;; built-in: operate on buffers much in the same manner as Dired.
;; https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon)
  :commands ibuffer-find-file
  :bind ( ("C-x C-b" . ibuffer))
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
;; :built-in: Registers allow you to jump to a file or other location quickly.
;; (i for init.el, r for this file) to jump to it.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html
(set-register ?t '(file . "~/Documents/orgtodo/"))
(set-register ?d '(file . "~/.dotfiles/"))
(set-register ?e '(file . "~/.emacs.d/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))
(global-set-key (kbd "H-r") `jump-to-register)

;;;;; dashboard
;; all-in-one start-up screen with current files / projects
;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :if (eq sej-dashboard t)
  :blackout (dashboard-mode)
  :commands sej/open-dashboard
  :hook (emacs-startup . sej/open-dashboard-only)
  :bind (("<f6>" . sej/open-dashboard)
         :map sej-C-q-map
          ("d" . sej/open-dashboard))
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

(defun sej/open-dashboard-only ()
    "Move to the dashboard package buffer and make only window in frame."
    (interactive)
    (switch-to-buffer "*dashboard*")
    (hl-line-mode t)
    (delete-other-windows))

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
;; automagically inserts text into new buffers
;;   based on file extension or the major mode
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/autoinsert.el
(use-package autoinsert
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
;; built-in: watch for changes in files on disk
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html]]
(use-package autorevert
  :hook ((emacs-startup . global-auto-revert-mode)
		 (dired-mode . auto-revert-mode))
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
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'sej/create-non-existent-directory)

;;;;; sudo-edit
;; Open files as sudo
;; https://github.com/nflath/sudo-edit
(unless sys/win32p
  (use-package sudo-edit))

;;; dired
;;;;; dired
;; built-in: Directory operations
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html#Dired
(use-package dired
  :ensure nil
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("C-u D" . sej/dired-do-delete-skip-trash)
			  ("z" . sej/dired-get-size))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-AFGhlv --group-directories-first")

  ;; ls-lisp settings
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-ignore-case 't)
  (setq ls-lisp-use-string-collate nil)

  (setq dired-dwim-target t
        dired-auto-revert-buffer #'dired-directory-changed-p
        dired-free-space nil
        dired-mouse-drag-files t)

  (defun sej/dired-do-delete-skip-trash (&optional arg)
    ""Only needed for pre-version 30.1""
    (interactive "P")
    (let ((delete-by-moving-to-trash nil))
      (dired-do-delete arg)))

  (defun sej/dired-get-size ()
  "Display file size in dired."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1)))))))

;;;;; dired-aux
;; built-in: auxiliary functionality of dired
;; https://github.com/jwiegley/emacs-release/blob/master/lisp/dired-aux.el
(use-package dired-aux
  :ensure nil
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)  )

;;;;; dired-x
;; built-in: Extra Dired functionality
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/
(use-package dired-x
  :ensure nil
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

;;;;; diredfl
;; Extra font-lock rules for a more Colourful dired
;; https://github.com/purcell/diredfl
(use-package diredfl)
  ;; :init (diredfl-global-mode 1))

;;;;; dired-subtree
;; The dired-subtree package provides commands to quickly view the contents of a folder with the TAB key.
(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;;;;; dwim-shell-command
;; define functions that apply command-line utilities to current buffer or dired files
;; [[https://github.com/xenodium/dwim-shell-command#my-toolbox]]
(use-package dwim-shell-command
  :if sys/macp
    :ensure-system-package (macosrec . "brew tap xenodium/macosrec && brew install macosrec")
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands))

;;;;; quick-preview
;; Quick-preview provides a nice preview of the thing at point for files.
;; https://github.com/myuhe/quick-preview.el
(use-package quick-preview
  :bind ( :map sej-C-q-map
          ("q" . quick-preview-at-point)
           :map dired-mode-map
           ("Q" . quick-preview-at-point)))

;;;;; browse-at-remote
;; browse file at remote source
;; https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :bind (:map sej-C-q-map
              ("B" . browse-at-remote)))

;;; writing & reading

;;;;; typo
;; [[https://github.com/jorgenschaefer/typoel][typo-mode]] is a buffer-specific minor mode that will change a number of normal keys to make them insert
;; typographically useful unicode characters. Some of those keys can be used repeatedly to cycle through
;; variations. This includes in particular quotation marks and dashes.
(use-package typo
  :diminish
  :bind
  (:map typo-mode-map
        ("-" . self-insert-command))
  :hook
  ((org-mode markdown-mode gnus-message-setup) . typo-mode)
  :config
  (typo-global-mode 1))

;;;;; denote
;; note organization tools
;; https://protesilaos.com/emacs/denote#h:d99de1fb-b1b7-4a74-8667-575636a4d6a4
;; https://github.com/protesilaos/denote?tab=readme-ov-file
;;
;; populate denote main directory `~/Documents/orgtodo' in my case with `.gitignore'
;;  /.DS_Store
;;  /.saves/
;;  /logs/
;;  /personal
;;  .dir-locals-2.el
;;
(use-package denote
  :bind (:map sej-denote-map
			  ("a" . denote-add-links)
			  ("b" . denote-backlinks)
			  ("B" . denote-find-backlink)
			  ("c" . sej/denote-colleagues-new-meeting)
			  ("C" . sej/denote-colleagues-edit)
			  ("C-c" . sej/denote-colleagues-dump)
			  ("d" . denote-date)
			  ("D" . denote-create-any-dir)
			  ("K" . sej/denote-keywords-edit)
			  ("C-k" . sej/denote-keywords-dump)
			  ("l" . denote-link) ; "insert" mnemonic
			  ("L" . denote-find-link)
			  ("n" . denote)
			  ("N" . denote-open-or-create)
			  ("r" . denote-rename-file)
			  ("R" . denote-rename-file-using-front-matter)
			  ("C-R" . denote-region)
			  ("s" . denote-signature)
			  ("S" . denote-subdirectory)
			  ("t" . denote-type)
			  ("T" . denote-template)
			  ("C-d r" . denote-dired-rename-files)
			  ("C-d k" . denote-dired-rename-marked-files-with-keywords)
			  ("C-d f" . denote-dired-rename-marked-files-using-front-matter))
  :init
  (which-key-add-keymap-based-replacements sej-denote-map
    "f"     "Denote-find"
    "C-d"   "Denote dired")
  (which-key-add-key-based-replacements
    "<C-m> n j"     "Denote journal entry"
    "<C-m> n J"     "Denote journal link"
    "<C-m> n R"     "Denote ren w/front-matter"
    "<C-m> n e"     "Denote extract org-tree"
    "<C-m> n h"     "Denote link to heading"
    "<C-m> n c"      "denote-colleagues-meet"
    "<C-m> n C"     "denote-colleagues-edit"
    "<C-m> n C-c"   "denote-colleagues-dump"
    "<C-m> n C-k"   "denote-keywords-dump"
    "<C-m> n C-d k" "Denote dired ren w/keywords"
    "<C-m> n C-d f" "Denote dired ren w/front" )
  :hook
  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  ;;(dired-mode-hook . denote-dired-mode-in-directories)
  ;; see below for function to work with diredfl

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (text-mode-hook . denote-fontify-links-mode-maybe)

  ;; If you want to have Denote commands available via a right click
  ;; context menu, use the following and then enable
  ;; `context-menu-mode'.
  (context-menu-functions . denote-context-menu)

  :custom-face
  (denote-faces-link ((t (:slant italic))))
  (denote-faces-title ((t (:foreground "royal blue" :underline t))))

  :custom
  (denote-directory sej-org-directory)
  (denote-save-buffers t)
  (denote-known-keywords nil)
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil) ; Org is the default, set others here
  (denote-prompts '(title keywords template))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (denote-date-prompt-use-org-read-date t)

  ;; Read this manual for how to specify `denote-templates'.
  (denote-templates
        `((standard . ,(concat "\n\n" "* "))
        (note . ,(concat "\n\n" "- "))
        (journal . ,(concat "#+category: journal" "\n\n" "* Notes\n" (org-insert-timestamp nil) "\n ") )))

  (denote-date-format nil) ; use default ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (denote-backlinks-show-context t)

  ;; Also see `denote-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; directories to use `denote-dired-mode' in.
  (denote-dired-directories
   (list denote-directory
         (thread-last
		   denote-directory (expand-file-name "attachments"))
		 (expand-file-name "~/Documents/denote-personal")
         (expand-file-name "~/Documents/knowledge")))

  ;; apply `denote-dired-directores' fontification to subdirectories
  (denote-dired-directories-include-subdirectories t)

  (savehist-additional-variables (-union savehist-additional-variables
                                         '(denote-file-history
                                           denote-title-history
                                           denote-keyword-history
                                           denote-component-history )))

  :config
;;;;;; play nice with diredfl
  (defun sej/denote-dired-mode-hook()
	"Function to switch off between diredfl-mode and denote-dired-mode."
  (denote-dired-mode-in-directories)
  (if denote-dired-mode
      (dired-hide-details-mode +1)
    (diredfl-mode +1)))

(add-hook 'dired-mode-hook #'sej/denote-dired-mode-hook)

;;;;;; denote keyword functions
  ;; define keywords in text file in denote-directory
  (defvar sej/denote-keywords-p (f-join denote-directory "denote-keywords.txt"))

  (defun sej/denote-keywords-update (&rest _arg)
    "Update keywords from file."
	(setq sej/denote-keywords-p (f-join denote-directory "denote-keywords.txt"))
    (if (f-exists-p sej/denote-keywords-p)
        (progn (setq denote-known-keywords  (s-split "\n" (f-read sej/denote-keywords-p) t))
               (setq org-tag-persistent-alist (-map #'list denote-known-keywords)))
      (setq denote-known-keywords  "defined-in-denote-keywords.txt"))
	(eval nil))

  ;; update 1st time in config
  (sej/denote-keywords-update)

  (defun sej/denote-keywords-edit ()
  "Edit the keywords list."
  (interactive)
  (sej/denote-keywords-update)
  (switch-to-buffer (find-file-noselect sej/denote-keywords-p)))

  (defun sej/denote-keywords-dump ()
    "Dump the current used keywords in denote-directory for refactor purposes."
    (interactive)
    (let (value)
      (dolist (element (denote-directory-files) value)
        (setq value (cons (denote-extract-keywords-from-path element) value)))
      (setq sej/value (sort (delete-dups (apply #'append value))))
      (insert (mapconcat 'identity sej/value "\n"))))

;;;;;; denote colleagues
;; easy way to launch topic specific files either colleagues or topics

(defvar sej/denote-colleagues-p (f-join denote-directory "denote-colleagues.txt")
  "Default file to keep the colleague list, kept in the base variable `denote-directory'." )

(defvar sej/denote-colleagues nil
  "List of names I collaborate with.
There is at least one file in the variable `denote-directory' that has
the name of this person.")

(defvar sej/denote-colleagues-prompt-history nil
  "Minibuffer history for `sej/denote-colleagues-new-meeting'.")

(defun sej/denote-colleagues-edit ()
  "Edit the colleague list by opening `sej/denote-colleagues'."
  (interactive)
  (setq sej/denote-colleagues-p (f-join denote-directory "denote-colleagues.txt"))
  (switch-to-buffer (find-file-noselect sej/denote-colleagues-p)))

(defun sej/denote-colleagues-update (&rest _arg)
  "Update denote colleagues variables."
  (setq sej/denote-colleagues-p (f-join denote-directory "denote-colleagues.txt"))
  (if (f-exists-p sej/denote-colleagues-p)
      (setq sej/denote-colleagues  (s-split "\n" (f-read sej/denote-colleagues-p) t))
	(progn
	  (setq sej/denote-colleagues '("none-defined"))
      (message  "Put colleagues and topics in denote-colleagues.txt"))))

(defun sej/denote-colleagues-prompt ()
  "Prompt with completion for a name among `sej/denote-colleagues', using the last input as the default value."
  (let ((default-value (car sej/denote-colleagues-prompt-history)))
    (completing-read
     (format-prompt "New meeting with COLLEAGUE" default-value)
     sej/denote-colleagues
     nil
	 'confirm
	 nil
     'sej/denote-colleagues-prompt-history
     default-value)))

(defun sej/denote-colleagues-update-file (name)
  "Update the colleagues file with an additional colleague NAME."
  (interactive "sName to add: ")
  (with-temp-file
	  sej/denote-colleagues-p
	(insert (mapconcat 'identity (sort (add-to-list 'sej/denote-colleagues name :APPEND)) "\n"))) )

(defun sej/denote-colleagues-get-file (name)
  "Find file in variable `denote-directory' for NAME colleague.
If there are more than one files, prompt with completion for one among them.
NAME is one among `sej/denote-colleagues', which if not found in the list, is
confirmed and added, calling the function `sej/denote-colleagues-update-file'."
  (if-let* ((files
			 (let* ((testname (denote-sluggify 'title name))
					(files (denote-directory-files testname))
					(RESULT nil))
			   (dolist (VAR files RESULT)
				 (if (string-match (denote-retrieve-filename-title VAR) testname)
					 (setq RESULT (cons VAR RESULT))))))
            (length-of-files (length files)))
      (cond
       ((= length-of-files 1)
        (car files))
       ((> length-of-files 1)
        (completing-read "Select a file: " files nil :require-match)))
    (progn (sej/denote-colleagues-update-file name)
		   (denote name
				   nil
				   denote-file-type
				   denote-directory
				   (denote-parse-date nil)
				   (alist-get 'standard denote-templates "* ")
				   nil))))

(defun sej/denote-colleagues-new-meeting ()
  "Prompt for the name of a colleague and insert a timestamped heading therein.
The name of a colleague corresponds to at least one file in the variable
`denote-directory'.  In case there are multiple files, prompt to choose
one among them and operate therein."
  (declare (interactive-only t))
  (interactive)
  (sej/denote-colleagues-update)
  (let* ((name (sej/denote-colleagues-prompt))
         (file (sej/denote-colleagues-get-file name))
         (time (format-time-string "%F %a")))  ; add %R if you want the time
    (with-current-buffer (find-file file)
      (goto-char (point-max))
	  (org-insert-heading '(16) nil 1)
	  (org-return)
      (org-insert-item)
      (end-of-line 0)
      (org-move-to-column 2))))

(defun sej/filter-journal-lines-from-list (lines)
  "Filter out lines containing '/journal/' from a cons cell LINES and return the titles."
  (let ((filtered-lines '()))
    (while lines
      (let ((line (car lines)))
        (unless (string-match-p (rx "journal") line)
          (push (concat (denote-retrieve-filename-title line) "\n" ) filtered-lines)))
      (setq lines (cdr lines)))
    (sort filtered-lines)))

(defun sej/denote-colleagues-dump ()
  "Dump current used colleagues in denote directory for refactor purposes."
  (interactive)
  (let ((value (sej/filter-journal-lines-from-list (denote-directory-files) )))
    (dolist (element value)
      (insert element))))

;;;;;; create denote files in any directory
(defun denote-create-any-dir ()
  "Create new Denote note in any directory.
Prompt for the directory using minibuffer completion."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory (read-directory-name "New note in: " nil nil :must-match)))
    (call-interactively 'denote)))

;;;;;; rename based on front matter at every save
(defun sej/denote-always-rename-on-save-based-on-front-matter ()
  "Rename the current Denote file, if needed, upon saving the file.
Rename the file based on its front matter, checking for changes in the
title or keywords fields.

Add this function to the `after-save-hook'."
  (let ((denote-rename-confirmations nil)
        (denote-save-buffers t)) ; to save again post-rename
    (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
      (ignore-errors (denote-rename-file-using-front-matter buffer-file-name))
      (message "Buffer saved; Denote file renamed"))))

(add-hook 'after-save-hook #'sej/denote-always-rename-on-save-based-on-front-matter)) ;end of denote use-package

;;;;;; [[https://github.com/protesilaos/denote-journal][denote-journal]]
(use-package denote-journal
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind (:map sej-denote-map
			  ("j" . denote-journal-new-or-existing-entry)
			  ("J" . denote-journal-link-or-create-entry))
  :custom
  (denote-journal-directory (expand-file-name "journal" denote-directory))
  (denote-journal-title-format "%y-%m")
  :config
  ;; replacement function to only use year and month for journal title yy-mm.
  (defun sej/denote-journal--entry-today (&optional date)
	"Return list of files matching a journal for today or optional DATE.
     DATE has the same format as that returned by `denote-parse-date'."
	(interactive)
	(let* ((identifier (format "%s"(format-time-string "%y-%m" date)))
           (files (denote-directory-files identifier))
           (keyword (concat "_" (regexp-quote denote-journal-keyword))))
      (seq-filter
       (lambda (file)
		 (string-match-p keyword file))
       files)))

  ;; (denote-journal--entry-today)
  (advice-add 'denote-journal--entry-today :override #'sej/denote-journal--entry-today))
  
;;;;;; [[https://github.com/protesilaos/denote-silo][denote-silo]]
(use-package denote-silo
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :bind (:map sej-denote-map
			  ("C-s" . denote-silo-open-or-create))
  :config
  (add-to-list 'denote-silo-directories "~/Documents/denote-personal" :APPEND)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)

  (defun sej/denote-silo-update (&rest _arg)
	"Function to update silo & critical variables based on current buffer/file.
Chooses silo based on file being in one of the extras-directories any history since.
Then proceeds to update keywords, colleagues, journal directory & finally refile targets."
	(interactive)
	(let ((file buffer-file-name) existin)
	  (if file
		  (dolist (elem (-union denote-silo-directories denote-silo-directory-history) )
			(if (file-in-directory-p file elem)
				(setq existin elem))
			(if (and existin (not (equal existin denote-directory)))
				(setq denote-directory existin)))
		(sej/denote-keywords-update)
		(sej/denote-colleagues-update)
		(setq denote-journal-directory (expand-file-name "journal" denote-directory)))
	  (setq org-refile-targets `((org-agenda-files . (:maxlevel 3))))))

  (advice-add 'denote-silo-select-silo-then-command :after #'sej/denote-silo-update)
  (advice-add 'denote-silo-open-or-create :after #'sej/denote-silo-update)
  (advice-add 'denote-silo-create-note :after #'sej/denote-silo-update)

  (defvar sej/switch-buffer-functions
	nil
	"A list of functions to be called when the current buffer has been changed.
Each is passed two arguments, the previous buffer and the current buffer.")

  (defvar sej/switch-buffer-functions--last-buffer
	nil
	"The last current buffer.")

  (defun sej/switch-buffer-functions-run ()
	"Run `sej/switch-buffer-functions' if needed.

This function checks the result of `current-buffer', and run
`sej/switch-buffer-functions' when it has been changed from
the last buffer.

This function should be hooked to `post-command-hook'."
	(unless (eq (current-buffer)
				sej/switch-buffer-functions--last-buffer)
      (let ((current (current-buffer))
			(previous sej/switch-buffer-functions--last-buffer))
		(setq sej/switch-buffer-functions--last-buffer current)
		(run-hook-with-args 'sej/switch-buffer-functions previous current))))

  (add-hook 'post-command-hook
			'sej/switch-buffer-functions-run)

  (defun sej/denote-check-for-denote-buffer-switch (_prev curr)
	"Check for denote CURR buffer switch to and update with `sej/denote-silo-update'."
	(interactive)
	(if (string-match "\\[D\\].*" (buffer-name curr))
		(sej/denote-silo-update)))

  (add-hook 'sej/switch-buffer-functions #'sej/denote-check-for-denote-buffer-switch))

;;;;;; consult-denote
;; integrate denote with consult
;; [[https://github.com/protesilaos/consult-denote]]
(use-package consult-denote
  :bind (:map sej-denote-map
                ("f f" . consult-denote-find)
                ("f g" . consult-denote-grep) )
    :config
    (setq consult-denote-grep-command #'consult-ripgrep)
    (consult-denote-mode 1))

;;;;;; denote-refs
;; puts links and back-links after header in denote files
;; put below in .dir-locals.el in denote-directory
;; ((org-mode . ((eval . (denote-refs-mode)))))
;; FIX DEBUG PROBLEM seems to be causing org-parse lock-ups
(use-package denote-refs
  :disabled t
  :vc (     :url "https://codeberg.org/akib/emacs-denote-refs.git"
            :rev :newest
            :branch "master")
  :hook (org-mode . denote-refs-mode)
  :custom
  (denote-refs-update-delay '(2 1 60))) ; needed to allow time for buffer set-up

;;;;;; denote-menu
;; tabed list of denote notes
;; [[https://github.com/namilus/denote-menu]]
(use-package denote-menu
  :hook (denote-menu-mode . sej/denote-menu-setup)
  :bind (:map sej-denote-map
         ("m" . sej/denote-menu-only-categories)
         :map denote-menu-mode-map
         ("c" . denote-menu-clear-filters)
         ("/ r" . denote-menu-filter)
         ("/ k" . denote-menu-filter-by-keyword)
         ("/ o" . denote-menu-filter-out-keyword)
         ("e" . denote-menu-export-to-dired)
		 ("s" . tabulated-list-sort)
		 ("t" . sej/denote-menu-cycle))
  :config (setq denote-menu-title-column-width 45
				denote-menu-keywords-column-width 35)
  (defun sej/denote-menu-setup ()
	"Change the denote menu mode to my liking."
	(interactive)
	(visual-line-mode -1)
	(if denote-menu-show-file-signature
      (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                    ("Signature" ,denote-menu-signature-column-width t)
                                    ("Title" ,denote-menu-title-column-width t)
                                    ("Keywords" ,denote-menu-keywords-column-width t)])

    (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                  ("Title" ,denote-menu-title-column-width t)
                                  ("Keywords" ,denote-menu-keywords-column-width t)]))
	(denote-menu-update-entries)
	(setq tabulated-list-sort-key '("Date" . t))
	(tabulated-list-init-header)
	(tabulated-list-print))

  (defvar sej/denote-menu-toggle-p 0
	"Variable to hold current toggle state.")

  (defun sej/denote-menu-cycle ()
	"Cycle listing All, only-Jounal, no-Journal notes."
	(interactive)
	(list-denotes)
	(cond
	 ((= sej/denote-menu-toggle-p 0)
	  (sej/denote-menu-only-journal))
	 ((= sej/denote-menu-toggle-p 1)
	  (sej/denote-menu-only-categories))
	 (t (sej/denote-menu-all))))

  (defun sej/denote-menu-all ()
	"Cycle listing All, only-Jounal, no-Journal notes."
	(interactive)
	(list-denotes)
	(setq sej/denote-menu-toggle-p 0)
	(setq tabulated-list-sort-key '("Date" . t))
	(denote-menu-clear-filters)
	(tabulated-list-init-header)
	(tabulated-list-print)
	(beginning-of-buffer))
  
  (defun sej/denote-menu-only-journal ()
	"Listing only-Jounal notes."
	(interactive)
	(list-denotes)
	(setq sej/denote-menu-toggle-p 1)
	(denote-menu-filter-by-keyword '("journal"))
	(setq tabulated-list-sort-key '("Date" . t))
	(tabulated-list-init-header)
	(tabulated-list-print)
	(beginning-of-buffer))
  
  (defun sej/denote-menu-only-categories ()
	"Cycle listing only-categories."
	(interactive)
	(list-denotes)
	(setq sej/denote-menu-toggle-p 2)
	(denote-menu-clear-filters)
	(denote-menu-filter-out-keyword '("journal"))
	(setq tabulated-list-sort-key '("Title" . nil))
	(tabulated-list-init-header)
	(tabulated-list-print)
	(beginning-of-buffer))
)

;;;;; markdown-mode
;; markdown-mode used a lot on Github
;; https://jblevins.org/projects/markdown-mode/
;; https://leanpub.com/markdown-mode
(use-package markdown-mode
  :ensure-system-package (multimarkdown . "brew install multimarkdown")
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . visual-line-mode)
         (markdown-mode . flymake-mode))
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
        markdown-fontify-code-blocks-natively t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-header-scaling t)
  ;; (setq markdown-command "pandoc -f markdown -t html")

;;;;; flymake-markdownlint
  (use-package flymake-markdownlint
	:vc (:url "https://codeberg.org/shaohme/flymake-markdownlint")
	:ensure-system-package (markdownlint . "brew install markdownlint-cli")
	:hook (markdown-mode . flymake-markdownlint-setup))

;;;;; markdown-toc
  ;; Table of contents
  ;; Inside a markdown file, the first time,
  ;; place yourself where you want to insert the TOC:
  ;; M-x markdown-toc-generate-toc
  ;; https://github.com/ardumont/markdown-toc
  (use-package markdown-toc))

;;;;; markdown-soma
;; realtime preview by eww
;; below SHOULD happen automagically, here just incase
;; install soma first in the .cargo directory (my dotfiles has path for this)
;; used for live updates for markdown
;; requires install of `rustup` which can happen from `brew` in osx or best is direct in others
;; curl https://sh.rustup.rs -sSf | sh
;; then you will need to cd to elpa/markdown-soma`
;; cargo install --path . # note the . after path
;; this should compile `soma` and install it in the `~/.cargo/bin/` directory
;; my dotfiles in the `.zshenv` file handles adding this directory to the path
;; markdownlint-cli is used for the linting of markdown-mode files
;; [[https://github.com/jasonm23/markdown-soma][markdown-soma]]
(use-package markdown-soma
  :ensure-system-package (rustup . "brew install rustup")
  :vc (:url "https://github.com/jasonm23/markdown-soma")
  ;; :hook (markdown-mode . markdown-soma-mode)
  :bind (:map markdown-mode-command-map
              ("p" . markdown-soma-mode)
              ("r" . markdown-soma-restart)
              ("+" . sej/markdown-soma-mod-plus)
              ("=" . sej/markdown-soma-mod-plus)
              ("-" . sej/markdown-soma-mod-minus) )
  :init
  ;; add to path
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

  :config
  ;; select css theme
  (setq markdown-soma-custom-css
        (markdown-soma--css-pathname-from-builtin-name "markdown-soma-dark"))
  ;; Change "theme name" to the selected highlightjs theme.
  (setq markdown-soma-highlightjs-theme "github-dark")

  (setq sej/markdown-soma-mod-var 0)

  (defun sej/markdown-soma-mod (x)
    "Calibrate  markdown-soma display by X."
    (setcdr x (+ sej/markdown-soma-mod-var (cdr x)))
	(nth 6 (posn-at-point)))

  (defun sej/markdown-soma-mod-plus ()
    "Add to display mod correction."
    (interactive)
    (setq sej/markdown-soma-mod-var (+ sej/markdown-soma-mod-var 5)))

  (defun sej/markdown-soma-mod-minus ()
    "Subtract from display mod correction."
    (interactive)
    (setq sej/markdown-soma-mod-var (- sej/markdown-soma-mod-var 5)))

  (advice-add 'markdown-soma--window-point :filter-return #'sej/markdown-soma-mod)

  (defun sej/markdown-soma-shell-command (x)
  "Advise to execute the soma app with path."
  (interactive)
  (format (concat (file-name-directory (executable-find "soma")) x)))

  (advice-add 'markdown-soma--shell-command :filter-return #'sej/markdown-soma-shell-command)

  (defun sej/markdown-soma-compile-soma ()
	"Compile soma if does not exist."
	(unless (executable-find "soma")
	  (message "compiling soma...")
	  (let ((default-directory "~/.emacs.d/elpa/markdown-soma"))
		(shell-command-to-string (concat (executable-find "cargo") " install --path .")))))

  (advice-add 'markdown-soma-start :before #'sej/markdown-soma-compile-soma))

;;;;; textile-mode
;; textile markup editing major mode
;; https://github.com/juba/textile-mode
(use-package textile-mode
  :mode "\\.textile\\'")

;;;;; [[https://github.com/sensorflo/adoc-mode/wiki][adoc-mode]]
;; adoc-mode is an Emacs major mode for editing AsciiDoc files.
;; It emphasizes on the idea that the document is highlighted

;; so it pretty much looks like the final output.
(use-package adoc-mode
  :mode ("\\.txt\\'"
		 "\\.adoc\\'")
  :hook (adoc-mode . turn-on-auto-fill))

;;;;; sej/number-rectangle
;; Let's say you have a list like:
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
;; standard spelling package
;; use jinx now but this set-up is here in case I move back
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
;; (use-package flyspell
;;   :disabled t
;;
;;   :blackout t
;;   :bind (("s-$" . ispell-word) ; M-$ doesn't work well in osx due to keybindings
;;           :map ctl-x-x-map
;;           ("s" . flyspell-mode)
;;           ("w" . ispell-word))
;;   :functions
;;   flyspell-correct-word
;;   flyspell-goto-next-error
;;   :ensure-system-package aspell
;;   :hook
;;   (prog-mode . flyspell-prog-mode)
;;   (text-mode . flyspell-mode)
;;   :config
;;   (setq flyspell-abbrev-p t
;;         flyspell-use-global-abbrev-table-p t
;;         flyspell-issue-message-flag nil
;;         flyspell-issue-welcome-flag nil)
;;   (cond
;;    ((executable-find "aspell")
;;     (setq-default ispell-program-name "aspell")
;;     (push "--sug-mode=ultra" ispell-extra-args))
;;    ((executable-find "enchant-2")
;;     (setq-default ispell-program-name "enchant-2"))
;;    ((executable-find "hunspell")
;;     (progn (setq-default ispell-program-name "hunspell")
;;            (setq ispell-really-hunspell t)))   )
;;
;;   (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word) ;;for mac
;;   (define-key flyspell-mouse-map [mouse-3] #'undefined)
;;
;;   (setq ispell-personal-dictionary "~/sej.ispell"
;;         ispell-silently-savep t)
;;
;;   (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
;;   (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
;;   (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
;;   (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
;;
;;   (setq ispell-dictionary-alist '(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_GB-ise") nil utf-8)
;;                                   ("canadian" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_CA") nil utf-8)
;;                                   ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))
;;
;;   (setq ispell-dictionary "canadian"))

;;;;; jinx
;; spell checking using enchanted spell checker
;; [[https://github.com/minad/jinx#]]
(when (eval 'sys/macp)
  (use-package jinx
    :after vertico
	:vc (:url "https://github.com/minad/jinx"
            :rev :newest
            :branch "main")
    :ensure-system-package ((enchant-2 . "brew install enchant")
                            (pkg-config . "brew install pkg-config"))
    :init
    ;; (setenv "PKG_CONFIG_PATH" (concat "/usr/local/Homebrew/Library/Homebrew/os/mac/pkgconfig/:" (getenv "PKG_CONFIG_PATH")))
    :hook (emacs-startup . global-jinx-mode)
    :bind (("C-;" . jinx-correct-nearest)
           ("C-M-;" . jinx-correct-all))
    :config
	(setq jinx-languages "en_US en_GB en_CA")
    (vertico-multiform-mode 1)
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))))

(when (eval 'sys/freebsdp)
  (use-package jinx
    :after vertico
    :ensure-system-package ((enchant-2 . "pkg install enchant2")
                            (pkg-config . "pkg install pkgconf"))
    :init
    ;; (setenv "PKG_CONFIG_PATH" (concat "/usr/local/Homebrew/Library/Homebrew/os/mac/pkgconfig/:" (getenv "PKG_CONFIG_PATH")))
    :hook (emacs-startup . global-jinx-mode)
    :bind (("C-;" . jinx-correct-nearest)
           ("C-M-'" . jinx-languages)
           ("C-M-;" . jinx-next))
    :config
    (vertico-multiform-mode 1)
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))))

;;;;; dictionary lookup
;; built-in: with Emacs 28
;; [[https://www.masteringemacs.org/article/wordsmithing-in-emacs?utm_source=newsletter&utm_medium=email&utm_campaign=rss&utm_source=Mastering+Emacs+Newsletter&utm_campaign=3a391dbdb1-MASTERING_EMACS_NEW_POSTS&utm_medium=email&utm_term=0_777fab9be9-3a391dbdb1-355707361][mastering Emacs]]
(use-package dictionary
  :bind (("M-#" . dictionary-lookup-definition) ; use esc # in osx
         ("C-#" . dictionary-search)))

;;;;; sej/pdf-print-buffer-with-faces (ps-print)
;; print file in the current buffer as pdf
;; https://www.emacswiki.org/emacs/PsPrint
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

;;;;; [[https://github.com/vedang/pdf-tools][pdf-tools]]
;; PDF reader
;; NOTE tool MAY NOT (sometimes does) work without manual compilation on first install with OSX!!!
;; goto ~/.emacs.d/elpa/pdf-tools-20240429.407/build/server/
;; ./autobuild
;; ./install-sh
;; sudo make install

  (use-package pdf-tools
    :magic ("%PDF" . pdf-view-mode)
	:custom
	(pdf-tools-handle-upgrades nil)
	;; :init (pdf-loader-install)
	:config (pdf-tools-install)
	(dolist
      (pkg
       '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
                   pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
                   pdf-util pdf-view pdf-virtual))
      (require pkg)))

;;;;; [[https://github.com/jeremy-compostella/pdfgrep][pdfgrep]]
;; grep service and highlighting for pdf
(use-package pdfgrep
  :commands pdfgrep
  :config
  (pdfgrep-mode))

;;;;; nov
;; Epub reader
;; https://github.com/wasamasa/nov.el
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
  :hook (nov-mode . my-nov-setup))

;;;;; annotate
;; add annotations to arbitrary files without changing the files themselves.
;; [[https://github.com/bastibe/annotate.el]]
(use-package annotate
  :commands (annotate-annotate
             sej/annotate-annotate
             annotate-goto-next-annotation
             annotate-goto-previous-annotation
             annotate-export-annotations
             annotate-integrate-annotations
             annotate-show-annotation-summary)
  :bind ((:map sej-C-q-map
               ("C-a" . sej/annotate-annotate-dwim)
               ("C-S-a" . annotate-show-annotation-summary)
               ("]" . annotate-goto-next-annotation)
               ("[" . annotate-goto-previous-annotation))
         (:map annotate-mode-map
               ("C-q C-a" . sej/annotate-annotate-dwim)
               ("C-q C-S-a" . annotate-show-annotation-summary)
               ("C-q ]" . annotate-goto-next-annotation)
               ("C-q [" . annotate-goto-previous-annotation)) )
  :config
  (setq annotate-file (emacs-path "annotations"))
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

;;;; LaTeX
;;;;; [[[[https://www.gnu.org/software/auctex/download-for-macosx.html]]][AuCTeX]] 
;; GNU TeX in Emacs Auctex
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . flymake-mode)
         (LaTeX-mode . reftex-mode)))

;;;;; [[https://www.emacswiki.org/emacs/LaTeXPreviewPane][LaTeX preview pane]]
;; side by side preview
(use-package latex-preview-pane)

;;;;; [[https://github.com/tom-tan/auctex-latexmk][auctex-latexmk]]
;; This library adds LatexMk support to AUCTeX.
(use-package auctex-latexmk
  :after tex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

;;;;; [[https://lucidmanager.org/productivity/emacs-bibtex-mode/][bibtex]]
;; built-in: bibliography editing
(use-package bibtex
  :ensure nil
  :init
  (setq-default bibtex-dialect 'biblatex))

;;;;; [[https://github.com/cpitclaudel/biblio.el][biblio]]
;; An extensible Emacs package for browsing and fetching references
(use-package biblio)

;;;;; [[https://www.gnu.org/software/auctex/manual/reftex.html][reftex]]
;; RefTeX is a package for managing Labels, References, Citations and index entries with GNU Emacs.
(use-package reftex
  :init
  (setq reftex-plug-into-AUCTeX t))

;;;;; [[https://github.com/cdominik/cdlatex][cdlatex]]
;; fast insertion of environment templates and math stuff in LaTeX
(use-package cdlatex)

;;;; org
;;;;; org
;; built-in: mode for keeping notes, maintaining lists, planning
;; <2024-10-19 Sat> mods to mix with denote system
(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :hook ( (org-mode . visual-line-mode)
          ;;(org-mode . org-num-mode) ; TRY remove for now ; TEST for a while
          (org-mode . variable-pitch-mode)
		  (org-mode . org-list-checkbox-radio-mode))
  :bind (( ("C-c l" . org-insert-link)
		   ("C-c S" . org-store-link))
         (:map org-mode-map
			   ("C-,")
               ("C-M-\\" . org-indent-region)
               ("S-<left>" . org-shiftleft)
               ("S-<right>" . org-shiftright)
               ("M-<return>" . org-insert-heading)
               ("C-M-<return>" . org-insert-subheading)
               ("M-S-<return>" . org-insert-todo-heading)
               ("C-\\" . org-insert-item)
			   ("C-c d" . org-check-deadlines)
               ("M-s H" . consult-org-heading)
			   ("<M-DEL>" . sej/kill-whole-word)
			   ("C-c (" . sej/org-fold-hide-drawer-toggle)
			   ("C-c )" . org-fold-hide-drawer-all)
			   ("C-c b" . org-switchb)
			   ("C-c x" . org-todo)
			   ("C-c H-t" . org-todo-yesterday)))
  :config
  ;; get denote up and going
  (require 'denote)
  (require 'denote-journal)
  (require 'denote-silo)
    
  ;; reading man pages in org-mode
  (require 'ol-man)

;; set headline numbering face to something more subtle
  (setq org-num-face 'org-modern-date-inactive)

  ;; archive location setting
  ;; based on year of archive, under heading of file it comes from
  ;; in denote format eg. 20250101T00000000--ARCHIVE.org for 2025
  (setq org-archive-location (concat org-directory (format-time-string "/%Y") "0101T000000--archive.org::* %s"))

  (setq org-ellipsis "⤵"
        org-directory sej-org-directory
        org-insert-heading-respect-content t
        org-replace-disputed-keys t
        org-hide-emphasis-markers t
        org-adapt-indentation nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
		org-ctrl-k-protect-subtree t
        org-M-RET-may-split-line '((default . nil))
        org-return-follows-link t
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t
        org-use-sub-superscripts "{}"
        org-capture-bookmark t
        org-refile-use-outline-path 'file
		org-outline-path-complete-in-steps nil
		org-refile-allow-creating-parent-nodes 'confirm
        org-log-done 'note
		org-log-into-drawer t
		;; ! - timestamp , @ - note
        org-todo-keywords '((sequence "TODO(t!)" "INPROCESS(i@/!)" "WAIT(w@/!)" "DEFER(r@/!)" "|" "DONE(d@/!)")
							(sequence "MAYBE(m@/!)" "|" "DONE(d@/!)")
                            (sequence "DELIGATE(D@/!)" "CHECK(c)" "|" "VERIFIED(v!)")
							(sequence "FIX(f@/!)" "INPROCESS(i@/!)" "|" "FIXED(F@/!)")
							(sequence "|" "CANCELED(x@/!)"))
		
		;; `list-colors-display' for a buffer of colour names
        org-todo-keyword-faces '(
                                 ("MAYBE" . (:foreground  "#9ac8e0"))
								 ("TODO" . (:foreground "pink4"))
								 ("DEFER" . (:foreground "pink"))
                                 ("WAIT" . (:foreground "yellow"))
                                 ("DONE" . (:foreground "green"))
                                 ("DELIGATE" . (:foreground "blue"))
                                 ("CHECK" . (:foreground "yellow"))
                                 ("VERIFIED" . (:foreground "green"))
                                 ("FIX" . (:foreground "pink"))
                                 ("INPROCESS" . (:foreground "yellow"))
                                 ("FIXED" . (:foreground "green"))
                                 ("CANCELED" . (:foreground "grey"))
								 )
        org-startup-folded 'content
        org-startup-indented t
        org-tags-column -100
        org-startup-with-inline-images t
        org-image-actual-width '(300)
        org-highlight-latex-and-related '(latex)
		org-clock-sound t
		org-id-method 'ts)

;;;;;; tags
  ;; defined here for regular topics
  ;; `org-tag-persistent-alist' defined in denote section from current note list
  (setq org-tag-alist
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

;;;;;; buffer-face-modes
(let* ((variable-tuple
        (cond ((x-list-fonts "Atkinson Hyperlegible") '(:family "Atkinson Hyperlegible"))
              ((x-list-fonts "Iosevka")   '(:family "Iosevka"))
              ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (fixed-tuple
        (cond    ((x-list-fonts "Iosevka Fixed")   '(:family "Iosevka Fixed"))
              ((x-list-fonts "Iosevka")   '(:family "Iosevka"))
              ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight regular :foreground ,base-font-color))
       )

  (custom-theme-set-faces
   'user
   `(variable-pitch ((t (,@variable-tuple :weight normal))))
   `(fixed-pitch ((t (,@fixed-tuple))))

   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.0 :foreground "#ff9580"))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1 :foreground "#caa6df"))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2 :foreground "#82b0ec"))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3 :foreground "#88ca9f"))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4 :foreground "#d2b580"))))

   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline t :foreground "#00FFFF"))))
   `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through nil :foreground "#71696A"))))

   `(org-default ((t (,@headline ,@variable-tuple :inherit variable-pitch))))
   `(org-block ((t (:inherit fixed-pitch))))
   `(org-code ((t (:inherit (shadow fixed-pitch)))))
   `(org-document-info ((t (:foreground "dark orange"))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-indent ((t (:inherit (org-hide variable-pitch)))))
   `(org-link ((t (:foreground "royal blue" :underline t))))
   `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-property-value ((t (:inherit fixed-pitch))) t)
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   `(org-done ((t (:foreground "#71696A" :strike-through nil))))
   ) )

;;;;;; prettify org checkboxes with unicode characters
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through nil)))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

;;;;;; personal sej functions
  (defun sej/org-timer-done-alert ()
	"Alert when timer is done."
	(interactive)
	(tmr "0s" "org timer done!"))
  (add-hook 'org-timer-done-hook #'sej/org-timer-done-alert)
  
  (defun sej/get-open-org-file ()
    "Pull list of .org files which are open in buffers."
    (buffer-file-name
     (get-buffer
      (org-icompleting-read "Buffer: "
                            (mapcar 'buffer-name
                                    (org-buffer-list 'files))))))

  (defun sej/org-reformat-buffer ()
    "Format the current org buffer, which often fixes and updates org interpreted data."
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))

(defun sej/org-remove-link ()
  "Replace an org link by its description or if empty its address."
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))
(bind-key "C-c C-H-u" 'sej/org-remove-link)

  (defun sej/org-fold-hide-drawer-toggle ()
	"Toggle drawer of headline in or above."
	(interactive)
	(save-excursion
	  (org-previous-visible-heading 1)
	  (org-update-checkbox-count)
	  (re-search-forward org-drawer-regexp nil 'noerror)
	  (org-fold-hide-drawer-toggle nil 'noerror)))

  ) ; end of use-pacakge for org

;;;;; org-agenda
;; built-in: agenda for todo & calendar items
;; [[https://orgmode.org/manual/Agenda-Views.html][org-agenda manual]]
(use-package org-agenda
  :ensure nil
  :after org
  :preface
  (setq native-comp-jit-compilation-deny-list '(".*org-element.*"))
  :bind (("C-c a" . org-agenda)
		 (:map org-agenda-mode-map
			   ("2" . org-agenda-fortnight-view))
		 (:map sej-denote-map
			   ("C-." . consult-org-agenda)
			   ("C-," . sej/org-agenda-call)))
  :defines
  (org-agenda-span
  org-agenda-skip-scheduled-if-deadline-is-shown
  org-agenda-todo-ignore-deadlines
  org-agenda-todo-ignore-scheduled
  org-agenda-sorting-strategy
  org-agenda-skip-deadline-prewarning-if-scheduled)
  :custom-face
  (org-agenda-calendar-event ((t (:foreground "gray70"))))
  (org-imminent-deadline ((t (:bold nil :foreground "chocolate3"))))
  (org-upcoming-deadline ((t (:bold nil :foreground "peru"))))
  (org-upcoming-distant-deadline ((t (:foreground "chocolate4"))))
  (org-agenda-date ((t (:bold nil :foreground "medium sea green"))))
  (org-agenda-date-weekend ((t (:bold nil :foreground "medium aquamarine"))))
  :config
  ;; get denote up and running
  (require 'denote)
  (require 'denote-journal)
    
  (setq org-agenda-block-separator nil
        org-agenda-diary-file (concat org-directory "/diary.org")
		org-agenda-files `(,org-directory ,denote-journal-directory)
        org-agenda-dim-blocked-tasks t ; other option is 'invisible
        org-agenda-inhibit-startup nil
        org-agenda-show-all-dates t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-on-weekday 1 ; Monday
        org-agenda-start-with-log-mode nil
        org-agenda-tags-column -100
        org-agenda-use-time-grid t
        org-agenda-include-diary t
        org-agenda-window-setup (quote current-window) ;open agenda in current window
        org-agenda-span (quote fortnight) ;show me tasks scheduled or due in next fortnight
        org-agenda-skip-scheduled-if-deadline-is-shown t ;don't show tasks as scheduled
                                        ; if they are already shown as a deadline
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        org-deadline-warning-days 7 ;warn me of any deadlines in next 7 days
		org-agenda-sticky nil) ; FIX change later to t
  
  (defvar sej/org-custom-agenda-orig
	;; stolen shamelessly from prot w/minor mods <2024-12-28 Sat> [[https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/][link]]
	;; [[https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html][custom agenda commands tutorial (not prot)]]
	`((tags-todo "*"
				 ((org-agenda-skip-function
				   `(org-agenda-skip-entry-if 'scheduled 'deadline ;'timestamp
											  'todo '("MAYBE" "CANCELED" "DONE" "VERIFIED" "FIXED")))
                  (org-agenda-block-separator 9472)
                  (org-agenda-overriding-header "Tasks with action needed without a scheduled or deadline date")))
      (agenda "" ((org-agenda-span 1)
                  (org-deadline-warning-days 0)
                  (org-agenda-block-separator 9472)
                  (org-scheduled-past-days 0)
                  (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                  (org-agenda-format-date "%A %-e %B %Y")
                  (org-agenda-overriding-header "Today's agenda")))
      (agenda "" ((org-agenda-start-on-weekday nil)
                  (org-agenda-start-day "+1d")
                  (org-agenda-span 3)
                  (org-deadline-warning-days 0)
                  (org-agenda-block-separator 9472)
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                  (org-agenda-overriding-header "Next three days")))
      (agenda "" ((org-agenda-time-grid nil)
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-start-day "+4d")
                  (org-agenda-span 14)
                  (org-agenda-show-all-dates nil)
                  (org-deadline-warning-days 0)
                  (org-agenda-block-separator 9472)
                  (org-agenda-entry-types '(:deadline :scheduled))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                  (org-agenda-overriding-header "Upcoming deadlines (+14d)"))))
	"Custom agenda for use in `org-agenda-custom-commands'.")

  (defvar sej/org-custom-agenda-ql
	;; stolen shamelessly from prot w/minor mods <2024-12-28 Sat> [[https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/][link]]
	;; [[https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html][custom agenda commands tutorial (not prot)]]
	`((org-ql-block '(and (todo "TODO" "INPRODCESS" "DELEGATE" "CHECK" "FIX")
						  (not (deadline))
						  (not (planning))
						  (not (scheduled))
						  (category )
						  )
					((org-ql-block-header "Tasks with action needed without scheduled or deadline date")))
      (agenda "" ((org-agenda-span 1)
                  (org-deadline-warning-days 0)
                  (org-agenda-block-separator 9472)
                  (org-scheduled-past-days 0)
                  (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                  (org-agenda-format-date "%A %-e %B %Y")
                  (org-agenda-overriding-header "Today's agenda")))
      (agenda "" ((org-agenda-start-on-weekday nil)
                  (org-agenda-start-day "+1d")
                  (org-agenda-span 3)
                  (org-deadline-warning-days 0)
                  (org-agenda-block-separator 9472)
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                  (org-agenda-overriding-header "Next three days")))
      (agenda "" ((org-agenda-time-grid nil)
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-start-day "+4d")
                  (org-agenda-span 14)
                  (org-agenda-show-all-dates nil)
                  (org-deadline-warning-days 0)
                  (org-agenda-block-separator 9472)
                  (org-agenda-entry-types '(:deadline :scheduled))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                  (org-agenda-overriding-header "Upcoming deadlines (+14d)")))
	  ;; FIX stopped work on this as repeating appointments <timestamps> are not handled [[https://github.com/alphapapa/org-ql/issues/159][link]]
	  ;; (org-ql-block '(or (ts :from +1 :to +3)
	  ;; 					 (deadline :from +1 :to +3)
	  ;; 					 (scheduled :from +1 :to +3)
	  ;; 					 (planning :from +1 :to +3)
	  ;; 					 (todo :from +1 :to +3)
	  ;; 					 (habit))
	  ;; 				((org-ql-block-header "Next three days (ql)")))
	  ;; (org-ql-block '(or (deadline :from +4 :to +14)
	  ;; 					 (scheduled :from +4 :to +14)
	  ;; 					 (planning :from +4 :to +14)
	  ;; 					 (habit))
	  ;; 				((org-ql-block-header "Upcoming deadlines (+14d) (ql)")))
	  )
	"Custom agenda for use in `org-agenda-custom-commands'.")

  (setq org-agenda-custom-commands `(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
									 ("d" "Deadlines & scheduled" agenda ""
									  ((org-agenda-span 'month)
									   (org-agenda-time-grid nil)
									   (org-agenda-show-all-dates nil)
									   (org-agenda-entry-types '(:deadline :scheduled))
									   (org-deadline-warning-days 0)
									   (org-agenda-sorting-strategy '(priority-up effort-down))))
									 ("D" "Deadlines & scheduled w/warnings" agenda ""
									  ((org-agenda-span 'month)
									   (org-agenda-time-grid nil)
									   (org-agenda-show-all-dates nil)
									   (org-agenda-entry-types '(:deadline :scheduled))
									   (org-deadline-warning-days 7)
									   (org-agenda-sorting-strategy '(priority-up effort-down))))
									 ("A" "Agenda and top priority tasks"
									  ,sej/org-custom-agenda-orig)
									 ("z" "QL Agenda and top priority tasks"
									  ,sej/org-custom-agenda-ql)
									 ("P" "Plain text daily agenda and top priorities"
									  ,sej/org-custom-agenda-orig
									  ((org-agenda-with-colors nil)
									   (org-agenda-prefix-format "%t %s")
									   (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
									   (org-agenda-fontify-priorities nil)
									   (org-agenda-remove-tags t))
									  ("agenda.txt"))
									 ("w" . "Wine searches")
									 ("wi" tags "+Italian+CATEGORY=\"wine\"")
									 ("wf" tags "+French+CATEGORY=\"wine\"")
									 ("ws" tags "+Spanish+CATEGORY=\"wine\"")
									 ("ww" tags "+CATEGORY=\"wine\"")
									 ))

  (defun sej/org-agenda-before-sorting-filter-function (element)
	"Mod `org-ql-block' agenda string based on `org-agenda-prefix-format' (eventually).

    Needed to add category to line as `org-ql-block' is hardwired to ignore `org-agenda-prefix-format'."
	(if element
		(let* (
			   (category (plist-get (nth 2 (car (object-intervals element))) 'org-category))
			   (type (plist-get (nth 2 (car (object-intervals element))) 'org-agenda-type))
			   (std (plist-get (nth 2 (car (object-intervals element))) 'standard-properties))
			   )
		  (if (and category (equal type 'search))
			  (progn
				(setq element
					  (concat
					   ;;(propertize FIX use this to add properties to the category tag
					   (substring element 0 2)
					   (substring (concat category ":            ") 0 12)
					   (substring element 2 nil)
					   ))))))
	element)
  (setq org-agenda-before-sorting-filter-function #'sej/org-agenda-before-sorting-filter-function)
					   
  ;; display repeaters for dates, scheduled, deadlines
  ;; [[https://whhone.com/posts/org-agenda-repeated-tasks/]]

  (defun sej/org-agenda-repeater ()
	"The repeater shown in org-agenda-prefix for agenda."
	(if (org-before-first-heading-p)
		"┄┄┄┄┄"  ; fill the time grid
	  (let ((rpt (org-get-repeat)))
		(if rpt rpt ""))))
  
  ;; Add `sej/org-agenda-repeater' to the agenda prefix.
;;   This format works similar to a printf format, with the following meaning:
;; 
;;   %c   the category of the item, \"Diary\" for entries from the diary,
;;        or as given by the CATEGORY keyword or derived from the file name
;;   %e   the effort required by the item
;;   %l   the level of the item (insert X space(s) if item is of level X)
;;   %i   the icon category of the item, see `org-agenda-category-icon-alist'
;;   %T   the last tag of the item (ignore inherited tags, which come first)
;;   %t   the HH:MM time-of-day specification if one applies to the entry
;;   %s   Scheduling/Deadline information, a short string
;;   %b   show breadcrumbs, i.e., the names of the higher levels
;;   %(expression) Eval EXPRESSION and replace the control string
;;                 by the result
;; 
;; If the first character after `%' is a question mark, the entire field
;; will only be included if the corresponding value applies to the current
;; entry.
;; 
;; If there is punctuation or whitespace character just before the
;; final format letter, this character will be appended to the field
;; value if the value is not empty.
(setq org-agenda-prefix-format
		`((agenda . " %i %-12c %?t  %-4e%?s%:(sej/org-agenda-repeater)")
		 (todo .    " %i %-12c %-4e")
		 (tags .    " %i %-12c")
		 (search .  " %i %-12c")))

  (defun sej/org-agenda-call (&optional arg)
	"Call org-agenda screen ARG, but default to `A'."
	(interactive)
	(if arg
		(org-agenda nil arg)
	  (org-agenda nil "A")))

  (defun sej/beginning-of-buffer-point (&optional &arg &arg)
	"Move point to top of agenda buffer after generate. Dummy to filter extra args."
	(beginning-of-buffer))
  (advice-add 'org-agenda :after #'sej/beginning-of-buffer-point)

  (defun sej/beginning-of-buffer-view (&optional &arg)
	"Move view to top of agenda buffer after regen. Dummy to filter extra args."
	(scroll-down-command))

  (advice-add 'org-agenda-redo-all :after #'sej/beginning-of-buffer-view)

  )  ; end of org-agenda

;;;;; org-appear
;; make invisible parts of org-mode visible
;; https://github.com/awth13/org-appear
(use-package org-appear
    :hook
    (org-mode . org-appear-mode))

;;;;; org-attach
  ;; directory to store attachments in relative to file
;; below is an absolute dir parallel to denote-directory
(use-package org-attach
  :after (org dired)
  :ensure nil
  :bind (:map dired-mode-map
			  ("C-c C-a" . org-attach-dired-to-subtree))
  :init
  (setq org-attach-id-dir  (expand-file-name "../denote-attachments/" denote-directory)
		org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format org-attach-id-uuid-folder-format))

  (add-to-list 'org-file-apps '("\\.xls\\'". default))

  ;; Tell Org to use Emacs when opening files that end in .md
  (add-to-list 'org-file-apps '("\\.md\\'" . emacs))

  ;; Do the same for .html
  (add-to-list 'org-file-apps '("\\.html\\'" . emacs))

  (defun sej/org-attach-save-file-list-to-property (dir)
	"Save list of attachments to ORG_ATTACH_FILES property."
	(when-let* ((files (org-attach-file-list dir)))
      (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))
  
  (add-hook 'org-attach-after-change-hook #'sej/org-attach-save-file-list-to-property))

;;;;; [[https://github.com/bzg/org-mode/blob/main/lisp/org-attach-git.el][org-attach-git]]
  ;; An extension to org-attach.  If `org-attach-id-dir' is initialized
  ;; as a Git repository, then `org-attach-git' will automatically commit
  ;; changes when it sees them.  Requires git-annex.
(use-package org-attach-git
  :after org
  :ensure nil)

;;;;; [[https://github.com/calvinwyoung/org-autolist][org-autolist]]
;; make return and delete smarter in org lists
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

;;;;; [[https://github.com/alphapapa/org-bookmark-heading][org-bookmark-heading]]
;; allows headings in org files to be bookmarked and jumped to with standard bookmark commands
(use-package org-bookmark-heading
  :after org)

;;;;; org-capture
;; Bookmarks in Safari
;;
;; Org Capture Journal  javascript:location.href='org-protocol://capture?template=x'
;;                               +'&url='+encodeURIComponent(window.location.href)
;;                               +'&title='+encodeURIComponent(document.title)
;;                               +'&body='+encodeURIComponent(window.getSelection()) ;
;; Org Capture Link  javascript:window.location.href='org-protocol://store-link?'
;;                               +'url='+encodeURIComponent(location.href)
;;                               +'&title='+encodeURIComponent(document.title) ;
(use-package org-capture
  :ensure nil
  :after org
  :bind ("C-c c" . org-capture)
  :config
  (defun sej/new-org-capture-file()
    "Function to allow new FILE in 'org-capture'."
    (interactive)
    (let ((file (read-file-name "File name: " (concat sej-org-directory "/"))))
      (let* ((expanded (expand-file-name file))
             (try expanded)
             (dir (directory-file-name (file-name-directory expanded)))
             new)
        (if (file-exists-p expanded)
            (error "Cannot create file %s: file exists" expanded))
        ;; Find the topmost nonexistent parent dir (variable `new')
        (while (and try (not (file-exists-p try)) (not (equal new try)))
          (setq new try
                try (directory-file-name (file-name-directory try))))
        (when (not (file-exists-p dir))
          (make-directory dir t))
        (write-region "" nil expanded t)
        (when new
          (dired-add-file new)
          (message new)
          )))))

;;;;; [[https://github.com/abo-abo/org-download][org-download]]
;; facilitates moving images by drag-drop, clipboard, kill-ring
;; to attach or specified directory
(use-package org-download
  :hook ((emacs-startup . org-download-enable)
		 (dired-mode . org-download-enable)))

;;;;; [[https://orgmode.org/manual/Tracking-your-habits.html][org-habit]]
;; habit logging and tracking
(use-package org-habit
  :after org-agenda
  :ensure nil
  :custom
  (org-habit-preceding-days 1000)
  (org-habit-today-glyph 45)
  :custom-face
  (org-habit-alert-face ((((background light)) (:background "#f5f946"))))
  (org-habit-alert-future-face ((((background light)) (:background "#fafca9"))))
  (org-habit-clear-face ((((background light)) (:background "#8270f9"))))
  (org-habit-clear-future-face ((((background light)) (:background "#d6e4fc"))))
  (org-habit-overdue-face ((((background light)) (:background "#f9372d"))))
  (org-habit-overdue-future-face ((((background light)) (:background "#fc9590"))))
  (org-habit-ready-face ((((background light)) (:background "#4df946"))))
  (org-habit-ready-future-face ((((background light)) (:background "#acfca9"))))
  :config
  (setq org-habit-graph-column 70))

;;;;; org-fragtog
;; LaTeX previews
;; https://github.com/io12/org-fragtog
(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview nil)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;;;;; org-modern
;; https://github.com/minad/org-modern
(use-package org-modern
    :hook
    (
	 (org-mode . org-modern-mode)
    (org-agenda-finalize . org-modern-agenda))
	:custom-face
	(org-modern-symbol ((t (:family "Iosevka Fixed"))))
	(org-modern-label ((t (:family "Iosevka Fixed" :height 1.0 :background ,(face-attribute 'default :background)))))
	(org-modern-tag ((t (:foreground "dark cyan" :background ,(face-attribute 'default :background)))))
	(org-modern-block-name ((t (:family "Iosevka Fixed" :height 1.0 :weight light))))
	(org-modern-date-active ((((background light)) :background "gray90" :foreground "black")
							  (t :background "gray10" :foreground "#00d3d0")))
	(org-modern-time-active ((((background light)) :background "gray35" :foreground "white" :distant-foreground "black")
							  (t :background "gray10" :foreground "#00e9ff" :distant-foreground "white")))
	(org-modern-date-inactive ((((background light)) :background "gray90" :foreground "gray30")
								(t :background "black" :foreground "gray70")))
	(org-modern-time-inactive ((((background light)) :foreground "gray95" :distant-foreground "gray5")
								(t :background "black" :foreground "gray5" :distant-foreground "gray95")))
	(org-modern-progress-complete ((((background light)) :background "gray20" :foreground "white")
									(t :background "gray75" :foreground "black")))
	(org-modern-progress-incomplete ((((background light)) :background "gray90" :foreground "black")
									  (t :background "gray20" :foreground "white")))
:custom
	(org-modern-star 'fold)
	(org-modern-hide-stars nil)
    (org-modern-timestamp t)
    (org-modern-table t)
    (org-modern-keyword nil)
    (org-modern-priority t)
	(org-modern-priority '((?1 . "🎉")
						   (?2 . "😄")
						   (?3 . "🆗")
						   (?4 . "☹️")
						   (?5 . "🤮")
						   ))
	(org-modern-priority-faces '((?A :foreground "red")
								 (?B :foreground "pink")
								 (?C :foreground "yellow")
								 (?D :foreground "beige")
								 (?E :foreground "grey")
								 (?1 :foreground "red")
								 (?2 :foreground "pink")
								 (?3 :foreground "yellow")
								 (?4 :foreground "beige")
								 (?5 :foreground "grey")))
    (org-modern-checkbox '((88 . "☑") (45 . #("□–" 0 2 (composition ((2))))) (32 . "□")))
	(org-modern-todo t)
	(org-modern-todo-faces org-todo-keyword-faces)
    (org-modern-tag t)
	(org-modern-progress 8)
    (org-modern-block-name t)
    (org-modern-keyword t)
    (org-modern-footnote nil)
    (org-modern-internal-target '(" ↪ " t " "))
	(org-modern-radio-target '(" ⛯ " t " "))
	(org-modern-horizontal-rule nil))

;;;;; [[https://github.com/org-noter/org-noter][org-noter]]
;; create notes that are kept in sync as you scroll through the document
(use-package org-noter
  :after (org pdf-view)
  :commands org-noter
  :bind (:map org-noter-notes-mode-map
              ("C-M-i" . org-noter-insert-dynamic-block))
  :custom
  (org-noter-notes-search-path (list org-directory))
  (org-noter-supported-modes '(doc-view-mode pdf-view-mode))
  (org-noter-max-short-selected-text-length 10)
  (org-noter-separate-notes-from-heading t)
  :config
  (use-package org-noter-pdf
	:ensure nil)

  (require 'pdf-macs)
  (defun org-noter-pdf--show-arrow ()
    ;; From `pdf-util-tooltip-arrow'.
    (pdf-util-assert-pdf-window)
    (let* (x-gtk-use-system-tooltips
           (arrow-top  (aref org-noter--arrow-location 2)) ; % of page
           (arrow-left (aref org-noter--arrow-location 3))
           (image-top  (if (floatp arrow-top)
                           (round (* arrow-top  (cdr (pdf-view-image-size)))))) ; pixel location on page (magnification-dependent)
           (image-left (if (floatp arrow-left)
                           (floor (* arrow-left (car (pdf-view-image-size))))))
           (dx (or image-left
                   (+ (or (car (window-margins)) 0)
                      (car (window-fringes)))))
           (dy (or image-top 0))
           (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))
           (vscroll (pdf-util-required-vscroll pos))
           (tooltip-frame-parameters
            `((border-width . 0)
              (internal-border-width . 0)
              ,@tooltip-frame-parameters))
           (tooltip-hide-delay 3))

      (when vscroll
        (image-set-window-vscroll vscroll))
      (setq dy (max 0 (- dy
                         (cdr (pdf-view-image-offset))
                         (window-vscroll nil t)
                         (frame-char-height))))
      (when (overlay-get (pdf-view-current-overlay) 'before-string)
        (let* ((e (window-inside-pixel-edges))
               (xw (pdf-util-with-edges (e) e-width))
               (display-left-margin (/ (- xw (car (pdf-view-image-size t))) 2)))
          (cl-incf dx display-left-margin)))
      (setq dx (max 0 (+ dx org-noter-arrow-horizontal-offset)))
      (pdf-util-tooltip-in-window
       (propertize
        " " 'display (propertize
                      "\u2192" ;; right arrow
                      'display '(height 2)
                      'face `(:foreground
                              ,org-noter-arrow-foreground-color
                              :background
                              ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
                                   (cdr pdf-view-midnight-colors)
                                 org-noter-arrow-background-color))))
       dx dy))))

;;;;; org-pdftools
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

;;;;; org-(ob)babel
(use-package ob
  :after (org ob-go ob-ipython ob-rust ob-restclient)
  :ensure nil
  :demand t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
														   (python     . t)
														   (calc       . t)
														   (shell      . t)
														   (latex      . t)
														   (C          . t)
														   (sql        . t)
														   (makefile   . t)
														   (ein        . t)
														   (perl       . t)
														   (ruby       . t)
														   (js         . t)
														   (css        . t)
														   (restclient . t)
														   (java       . t)))

  (setq org-confirm-babel-evaluate nil))

;;;;; [[https://github.com/pope/ob-go][ob-go]]
;; org-bable functions for go evaluations
(use-package ob-go
  :after org)

;;;;; [[https://github.com/gregsexton/ob-ipython][ob-ipython]]
;; library that allows Org mode to evaluate code blocks using a Jupyter kernel
;; (Python by default)
(use-package ob-ipython
  :after org)

;;;;; [[https://github.com/zweifisch/ob-rust][ob-rust]]
;; org-babel functions for rust evaluation
(use-package ob-rust
  :after org)

;;;;; [[https://github.com/alf/ob-restclient.el][ob-restclient]]
;; #+BEGIN_SRC restclient
;;   GET http://example.com
;; #+END_SRC
;; provides restclient support
(use-package ob-restclient
  :after org)

;;;;; org-Ox
;; built-in: org mode exporter framework
;; [[https://orgmode.org/worg/exporters/ox-overview.html][org-exporter manual]]
(use-package ox
  :ensure nil
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
;; built-in: latex exporter
;; https://orgmode.org/manual/LaTeX-Export.html#LaTeX-Export
(use-package ox-latex
  :ensure nil
  :config
  ;; LaTeX Settings
  (setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f")
        org-latex-remove-logfiles t
        org-latex-prefer-user-labels t
        bibtex-dialect 'biblatex))

;;;;; ox-gfm
;; github flavoured markdown exporter for Org mode
;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm)

;;;;; ox-jdf-report
;; exporter for the jdf style report
;; https://github.com/dylanjm/ox-jdf
(use-package ox-jdf-report
  :vc (:url "https://github.com/dylanjm/ox-jdf"
            :rev :newest
            :branch "master"))

;;;;; ox-report
;; meeting report exporter
;; https://github.com/DarkBuffalo/ox-report
(use-package ox-report)

;;;;; org-pretty-tags
;; Display text or image surrogates for Org mode tags.
;; https://gitlab.com/marcowahl/org-pretty-tags
(use-package org-pretty-tags
  :hook (org-mode . org-pretty-tags-global-mode)
  :config
  (setq org-pretty-tags-surrogate-strings
        `(;; generic tags
		  ("topic" . "☆")
          ("idea" . "💡")
		  ("log" . "📋")
		  ("done" . "✅")
		  ("closed". "🔒")
          ("service" . "✍")
          ("Blog" . "✍")
          ("security" . "🔥")
		  ;; denotes generic togs
		  ("ATTACH" . "📎")
		  ("journal" . "✒️") ("knowledge" . "🤓") ("project" . "👷🛠️") ("routine" . "🧹🔁")
		  ("manual" . "📚") ("datasheet" . "📈") ("tutorial" . "👨‍🎓") ("tool" . "🪛🔧")
		  ("read" . "👀")
		  ("debug" . "🐞")
		  ("family" . "👨‍👩‍👧‍👦")
		  ("kids" . "👶🏻")
		  ("friends" . "🍻")
		  ("travel" . "✈️")
		  ("home" . "🏠")
		  ("emacs" . "ℇ") ("Emacs" . "ℇ")
		  ("computer" . "🖥️")
		  ("financial" . "💰")
		  ("automation" . "⚙️")
		  ("plugin" . "🔌")

		  ;; media & specific tags
          ("media" . "💿")
		  ("music" . "🎶")

		  ;; sports
		  ("hockey" . "🏒")
		  ("swimming" . "🏊‍♀️")
		  
		  ;; wine & specific tags
		  ("wine" . "🍷")
		  ("Red" . "🍷") ("Rose" . "🌹") ("White" . "🥂")
		  ("Champagne" "🍾") ("Prosecco" "🍾") ("Cava" "🍾") ("Sparkling" "🍾")
		  ("Liquor" . "🫒🍸")
		  ("France" . "🇫🇷Fr") ("French" . "🇫🇷Fr")
		  ("Italy" . "🇮🇹It") ("Italian" . "🇮🇹It")
		  ("Spain" . "🇪🇸Sp") ("Spanish" . "🇪🇸Sp")
		  ("Canada" . "🇨🇦Cdn") ("Canadian" . "🇨🇦Cdn")
		  
		  ;; electronics & specific tags
		  ("electronics" . "electronics")
		  ("stereo" . "🎧")
		  ("active" . ,(propertize (nerd-icons-codicon "nf-cod-chip")))
		  ("static" . ,(propertize (nerd-icons-mdicon "nf-md-resistor_nodes")))
		  ("board" . ,(propertize (nerd-icons-mdicon "nf-md-developer_board")))
		  )) )

;;;;; org-protocol
(use-package org-protocol
  :ensure nil
  :after org
  :custom
  (org-protocol-default-template-key "l"))

;;;;; [[https://github.com/alphapapa/org-ql][org-ql]]
;; This package provides a query language for Org files.
;; It offers two syntax styles: Lisp-like sexps and search engine-like keywords.
(use-package org-ql
  :after org
  :demand t
  :commands org-ql-search
  :functions (org-ql-find--buffers
              org-ql-search-directories-files
              org-ql-find
              org-ql-defpred)
  :bind (:map org-mode-map
		 ("C-c C-x o" . org-ql-open-link)
		 :map sej-denote-map
		 ("o /" . org-ql-sparse-tree)
		 ("o a" . org-ql-view)
		 ("o q" . org-ql-find-in-agenda)
		 ("o w" . org-ql-refile-path)
		 ("o W" . org-ql-refile))
  :preface
  (which-key-add-keymap-based-replacements sej-denote-map "o" "org-ql")
  :config
  (eval-when-compile
    (require 'vertico-multiform))
  (add-all-to-list 'vertico-multiform-commands
                   '(org-ql-find)))

;;;;; org-rich-yank
;; Rich text clipboard when yanking code into org buffer
;; consider demand t as lazy loading may not work
;; https://github.com/unhammer/org-rich-yank
(use-package org-rich-yank
  :defer 2
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

;;;;; org-skeleton
;; skeleton template for new org file
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

;;;;;; sej/org-wrap-elisp skeleton
;; skeletons are a kind of yasnippet but they don't mess with keybindings
;; skeleton to wrap elisp babel source

(define-skeleton sej/org-wrap-elisp
  "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code."
  nil
  > "#+BEGIN_SRC emacs-lisp" \n
  > _ \n
  > "#+END_SRC" \n)

;;;;;; sej/org-wrap-source skeleton
;; skeletons are a kind of yasnippet but they don't mess with keybindings
;; skeleton to wrap generic babel source
(define-skeleton sej/org-wrap-source
  "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type."
  "Language: "
  > "#+BEGIN_SRC " str \n
  > _ \n
  > "#+END_SRC" \n)

;;;;; org-src
;; built-in: org src block settings
;; [[https://orgmode.org/manual/Working-with-Source-Code.html][working with source code]]
(use-package org-src
  :ensure nil
  :config
  (setq org-src-window-setup 'current-window
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0))


;;;;; sej/org-log-checklist-item

;; **not currently used**
;; automatically log checklist items into :LOGBOOK:
;; https://emacs.stackexchange.com/questions/75441/how-to-log-changes-to-checklists-in-org-mode
(defun sej/org-log-checklist-item (item)
"Insert clocked ITEM into logbook drawer. Create drawer if it does not exist yet."
  (save-excursion
    (org-previous-visible-heading 1)
    (while (not (= (org-current-level) 1))
      (org-previous-visible-heading 1))
    (forward-line)
    (let* ((element (org-element-at-point))
           (logbookp (string= (org-element-property :drawer-name element)
                              "LOGBOOK")))
      (if logbookp
          (goto-char (org-element-property :contents-end element))
        (org-insert-drawer nil "LOGBOOK"))

	  (insert "- " item " ")
      (org-insert-time-stamp (current-time) t t)
      (when logbookp
        (insert "\n")))))

(defun sej/org-checkbox-item ()
"Retrieve the contents (text) of the item."
  (save-excursion
    (beginning-of-line)
    (search-forward "]")
    (forward-char)
    (buffer-substring-no-properties (point) (line-end-position))))

;; **currently being used**
(defun sej/org-checklist-date-insert (&rest _)
  "Lock checkbox item by adding date time stamp to end of line."
  (interactive)
  (when (org-at-item-checkbox-p)
	(save-excursion
	  (end-of-visible-line)
	  (org-insert-time-stamp (current-time) t t))))

(defun sej/org-checklist-log (&rest _)
  "Advise function to get and log checkbox item when it is checked."
  (interactive)
  (when (org-at-item-checkbox-p)
    (let ((checkedp (save-excursion
                      (beginning-of-line)
                      (search-forward "[")
                      (looking-at-p "X"))))
      (when checkedp
        (sej/org-log-checklist-item (sej/org-checkbox-item))))))

;; need to have org-list-checkbox-radio-mode
(advice-add 'org-toggle-radio-button :after #'sej/org-checklist-date-insert)


;;;;; toc-org
;; Table of contents updated at save to header with TOC tag
;; https://github.com/snosov1/toc-org
(use-package toc-org
  :blackout t
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode))
  :bind (:map org-mode-map
              ("C-c C-o" . toc-org-markdown-follow-thing-at-point)
			  :map org-mode-map
              ("C-c C-o" . toc-org-markdown-follow-thing-at-point)))


;;; shell tools
;;;;; sh-script
;; built-in: shell script mode
;; https://www.emacswiki.org/emacs/ShMode
(use-package sh-script
  :ensure nil
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

;;;; eshell
;;;;; eshell
;; built-in: Emacs command shell ; much better than shell
;; https://www.gnu.org/software/emacs/manual/html_mono/eshell.html
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
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
                            (eshell/alias "l" "ls -l $1")
                            (eshell/alias "gd" "magit-diff-unstaged")
                            (eshell/alias "gds" "magit-diff-staged")
                            (eshell/alias "d" "dired $1")
                            (bind-keys :map eshell-mode-map
                                       ("M-P" . eshell-previous-prompt)
                                       ("M-N" . eshell-next-prompt)
                                       ("M-R" . eshell-previous-matching-input)
                                       ("C-l" . eshell/clear) ))))

  :bind (:map sej-C-q-map
              ("S e" . eshell) )

  :init
  (defvar eshell-visual-commands '("screen" "htop" "ncftp" "elm" "el" "nano" "ssh" "nethack" "dstat" "tail"))
  (defvar eshell-visual-options '("git" "--help" "--paginate"))
  (defvar eshell-visual-subcommands '("git" "log" "diff" "show"))

  :config
  (setenv "PAGER" "cat")

  ;; Visual commands
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
  :ensure nil
  :config
  (setq eshell-modules-list             ; Needs review
        '(eshell-smart
          eshell-alias
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
  :ensure nil
  :after esh-mode
  :config
  (setq eshell-cd-on-directory t))

(use-package em-tramp
  :ensure nil
  :after esh-mode
  :config
  (setq password-cache t)
  (setq password-cache-expiry 600))

(use-package em-hist
  :ensure nil
  :after esh-mode
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

;;;;; eshell-prompt-extras
;; Display extra information for prompt
;; https://github.com/kaihaosw/eshell-prompt-extras
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
;; truncates all eshell buffers after t time (5s)
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

;;;;; eshell/cls
;; clear the eshell buffer / screen
(defun eshell/cls ()
  "Clear the eshell buffer."
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

;;;;; eshell/emacs
;; edit a file in eshell without re-rerunning Emacs
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
;; Compile a file (ARGS) in Emacs.  Use `compile' to do background make.
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
;; A version of `view-file' which properly rets the eshell prompt.
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
;; Invoke `view-file' on a file.  \"less +42 foo\" will go to line 42 in the buffer
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
;; change directory to the project's root
(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

;;;;; eshell/d
;; shortcut for Dired in eshell
(defun eshell/d (&rest args)
  "Shortcut of d for Dired in eshell with ARGS."
  (dired (pop args) "."))

;;;;; eshell/magit
;; function to open magit-status for the current directory
(defun eshell/magit ()
  "Function to open <magit-status> for the current directory."
  (interactive)
  (magit-status default-directory)
  nil)


;;;; shell
;;;;; shell
;; built-in: basic emacs shell ; eshell is much better
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html
(use-package shell
  :ensure nil
  :commands shell-command
  ;; :hook ((shell-mode . n-shell-mode-hook)
  ;;        (shell-mode . ansi-color-for-comint-mode-on)
  ;;        (comint-output-filter-functions . comint-strip-ctrl-m)
  ;;        (comint-output-filter-functions . comint-truncate-buffer))
  :bind  (:map sej-C-q-map
               ("S s" . shell))
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
;; pop-up shell
;; https://github.com/kyagi/shell-pop-el
(use-package shell-pop
  :bind (:map sej-C-q-map
              ("p" . shell-pop))
  :init (let ((val
               (if sys/win32p
                   '("eshell" "*eshell*" (lambda () (eshell)))
                 '("ansi-term" "*ansi-term*"
                   (lambda () (ansi-term shell-pop-term-shell))))))
          (setq shell-pop-shell-type val)))

;;;;; sej/shell-kill-buffers
;; kill shell buffer upon exit
(defun sej/shell-kill-buffer-sentinel (process event)
  "Function to kill shell buffer upon (PROCESS EVENT)."
  (when (memq (process-status process) '(exit signal))
    (kill-buffer)))

;;;;; sej/kill-process-buffer-on-exit
;; make sure processes get killed on Emacs-exit
(defun sej/kill-process-buffer-on-exit ()
  "Function to kill buffer on exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'sej/shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'sej/kill-process-buffer-on-exit))

;;;;; with-editor
;; things that invoke $EDITOR will use the current Emacs
;; https://github.com/magit/with-editor
(use-package with-editor
  :hook ((shell-mode . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))

;;;;; eat
;; faster shell integration
;; [[https://codeberg.org/akib/emacs-eat][eat]]
(use-package eat
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

;;;;; tldr.el
;; connection to the tldr shell command for man pages
;; [[https://github.com/kuanyui/tldr.el]]
(use-package tldr
  :ensure-system-package (tldr . "brew install tlrc")) ; rust version of tldr

;;;; Other Services
;; a place to put set-ups for Emacs outside services
;;;;; term ansi-term serial-term
;; built-in: basic terminal
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Terminal-emulator.html#Terminal-emulator][Emacs manual]]
(use-package term
  :ensure nil
  :commands (term ansi-term serial-term)
  :bind (:map sej-C-q-map
              ("S a" . ansi-term)
              ("S S" . serial-term)
              ("S t" . term))
  :config
  (setq term-buffer-maximum-size 9999)
  (setq term-completion-autolist t)
  (setq term-completion-recexact t)
  (setq term-scroll-to-bottom-on-output nil))

;;;;; vterm
;; fully-fledged terminal emulator inside GNU Emacs
;; [[https://github.com/akermu/emacs-libvterm]]
(use-package vterm
  :commands vterm
  :bind (:map sej-C-q-map
              ("S v" . vterm))
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
;; built-in: irc client
;; [[https://www.gnu.org/software/emacs/manual/html_mono/erc.html#Top]]
(use-package erc
  :ensure nil
  :bind (:map sej-C-q-map
              ("I" . sej/erc-start-or-switch))
  :config
  (setq auth-source-debug t)
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
          (setq *uconf/erc-certs*
				`((,(plist-get (car (auth-source-search :host "irc.libera.chat")) :cert ) ) ) )
			      (open-network-stream name buffer host port
                               :nowait t
                               :type 'tls))
      (setq *uconf/erc-certs* nil)))

  (advice-add 'open-gnutls-stream :override #'uconf/open-gnutls-stream)
  (advice-add 'erc-open-tls-stream :override #'uconf/erc-open-tls-stream)

  (setq erc-prompt-for-password nil)

  (defun sej/erc-buffer-connected-p (buffer)
	"Check if ERC BUFFER is connected."
	(with-current-buffer buffer
      (and (erc-server-process-alive)
           erc-server-connected)))

  (defun sej/erc-start-or-switch ()
	"Connects to ERC, or switch to last active buffer.

This function serves multiple purposes:

1. Check Active Buffers: It iterates through a predefined list of ERC buffers
   to determine if any of them are actively connected to an IRC server.

2. Verify Connection Status: For each buffer, it checks whether the associated
   ERC process is alive and whether there is an established network connection
   to the server. This is done using the `erc-server-process-alive' function and
   the `erc-server-connected' variable.

3. Switch to Active Buffer: If any buffer is found to be actively connected,
   the function switches to that buffer using `erc-track-switch-buffer'.

4. Reconnect if Disconnected: If none of the checked buffers are connected,
   the function prompts the user to reconnect to the IRC server. If the user
   confirms, a new connection is initiated using the `erc' command with the
   server and port specified (`irc.libera.chat` on port 6667)."
	(interactive)
	(let ((erc-buffers '("Libera.Chat" "irc.libera.chat" "irc.libera.chat:6667"))
          (connected nil))
      (dolist (buffer erc-buffers)
		(when (and (get-buffer buffer)
                   (my/erc-buffer-connected-p buffer))
          (setq connected t)))
      (if connected
          (erc-track-switch-buffer 1)
		(when (y-or-n-p "Start ERC? ")
		  (let ((irc "irc.libera.chat"))
			;; TODO need to get back to erc-tls
			(erc :server irc
					 :port 6697
					 :nick (car (auth-source-user-and-password irc))
					 :full-name (car (auth-source-user-and-password irc))
					 :password (cadr (auth-source-user-and-password irc))
					 ) ))
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
;; built-in: Emacs simple html renderer used by a few tools
;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/net/shr.el]]
(use-package shr
  :ensure nil
  :commands (eww eww-browse-url)
  :config
  (setq shr-use-fonts t)
  (setq shr-use-colors t)
  (setq shr-max-image-proportion 0.2)
  (setq shr-image-animate t)
  (setq shr-width (current-fill-column)))

;; Support the HTML pre tag with proper syntax highlighting.
(use-package shr-tag-pre-highlight
  :disabled t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

;;;;; eww Emacs-web-wowser
;; built-in: Emacs internal web browser
;; - [[https://www.gnu.org/software/emacs/manual/html_mono/eww.html][EWW]]
(use-package eww
  :ensure nil
  :bind (:map sej-C-q-map
              ("W" . eww))
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

;;;;; Emacs-everywhere
;; allows you to use Emacs editing in any application
;; Use:
;; emacsclient --eval "(emacs-everywhere)"
;; add shortcut for above
;; click "Add Shortcut" and key a shortcut. (I use control-option-command-space)
;; [[https://github.com/tecosaur/emacs-everywhere][emacs-everywhere]]
(use-package emacs-everywhere)

;;; calendar
;;;;; calendar
;; built-in: calendar
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html]]
(use-package calendar
  :ensure nil
  :bind (:map sej-C-q-map
              ("C" . calendar))
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

;;;;; TMR (timer)
;; facilities for setting timers using a convenient notation
;; [[https://protesilaos.com/emacs/tmr]]
(use-package tmr
  :bind (:map sej-C-q-map
              ("t" . tmr-prefix-map))
  :ensure-system-package ffmpeg
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
           :style 'osx-notifier
		   :persistent 'persistent) )
		(setq tmr-sound-file "/System/Library/Sounds/Blow.aiff")
        (add-to-list 'tmr-timer-finished-functions #'sej/osx-alert-tmr)
        ;; (delete 'tmr-notification-notify tmr-timer-finished-functions)
		)))

;;;;; elfeed
;; rss feeder
;; [[https://github.com/skeeto/elfeed]]
(use-package elfeed
  :bind ("C-q w" . elfeed)
  :config
  ;;(setq elfeed-feeds '(("http://nullprogram.com/feed/" blog emacs)
					   ;; ( "https://planet.emacslife.com/atom.xml" blog emacs)
					   ;; ("https://sachachua.com/blog/category/emacs/feed/index.xml" blog emacs)
					   ;; ("http://nedroid.com/feed/" webcomic))))

;;;;;; Configure Elfeed with org mode
  (use-package elfeed-org
	:demand t
    :config
    (elfeed-org)
    :custom
    (rmh-elfeed-org-files '("~/Documents/orgtodo/20250124T203443--elfeed.org"))))

;;;;; elfeed-webkit
;; render in webkit [[https://github.com/fritzgrabo/elfeed-webkit][link]]
(use-package elfeed-webkit
  :after elfeed
  :bind (:map elfeed-show-mode-map
              ("'" . elfeed-webkit-toggle)))
  ;; :init
  ;; (setq elfeed-webkit-auto-enable-tags '(webkit comics))
  ;; :config
  ;; ;;(elfeed-webkit-auto-toggle-by-tag)

;;; init.el --- end
(message "init.el ends here")
(provide 'init)
;;; init.el ends here
