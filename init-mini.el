;;; init-mini.el --- SeJ Emacs configurations. -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d
;; Version: 0.0.1
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
;; SeJ Emacs mini configurations.
;;
;; for use on smaller systems or installations
;; just do a ln -s ~/.emacs.d/init-mini.el ~/.emacs


;;; Changelog
;;
;; <2023-01-02 Mon> take init.el and start chopping


;;; Code:
(message "Emacs start")

;;; initialize environment
;;;;; debug
;; only turned on when needed
;; (setq debug-on-error t)
;; (setq debug-on-event t)


;;;;; Straight package manager set-up
(setq-default straight-repository-branch "develop"
              straight-fix-org t
              straight-fix-flycheck t
              straight-use-package-by-default t
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

(straight-use-package 'use-package)


;;;;; bind-key
;; Required by `use-package'
(use-package bind-key
  :bind ("s-d" . describe-personal-keybindings))


;;;;; Blackout
;; Similar to packages like minions, diminish, or delight.
;; You can alter how your minor and major modes show up in the mode-line.
;; [[https://github.com/raxod502/blackout][Blackout]]
(use-package blackout
  :demand t
  :straight (:host github :repo "raxod502/blackout"))


;; Auto installing OS system packages
;; ensure-system-package keyword to ensure system binaries exist alongside your package
(use-package use-package-ensure-system-package
  :ensure t)


;;;;; system custom constants
;; - section for global constants
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

(defconst emacs/>27p
  (> emacs-major-version 27)
  "Emacs is 28 or above.")


;;;;; should i even be here
(when (not emacs/>27p)
  (error "This requires Emacs 28 and above")  )


;;;;;  Warnings
;; set-up server & suppress warnings
;; - [[https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/warnings.el][warnings.el]]
  (require 'warnings)
  ;; remove warnings for cl depreciated and server already running
  (setq warning-suppress-types (quote ((cl server iedit))))
  (setq warning-suppress-log-types (quote ((cl) )))
  (setq byte-compile-warnings '(cl-functions))


;;;;; Server set-up
;; set-up Emacs server
(use-package emacs
  :when (or sys/macp sys/linuxp)
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
  :straight t
  :hook (after-init . gcmh-mode)
  :init
  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)
  (add-hook 'suspend-hook #'gcmh-idle-garbage-collect)
  (setq gcmh-idle-delay 10))


;;;;; restart-emacs
;; simple package to restart Emacs within Emacs
;; with a single universal-argument (C-u) Emacs is restarted with --debug-init flag
;; with two universal-argument (C-u C-u) Emacs is restarted with -Q flag
;; with three universal-argument (C-u C-u C-u) the user is prompted for the arguments
;; restart-emacs-start-new-emacs starts new session of Emacs without killing the current one
;; [[https://github.com/iqbalansari/restart-emacs][restart-emacs]]
(use-package restart-emacs
  :straight t
  :init
  (defalias 're #'restart-emacs))


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

(defcustom sej-mail-address "stephenearljenkins@gmail.com"
  "Set user email address."
  :type 'string)

(defcustom sej-proxy "localhost:80"
  "Set network proxy."
  :type 'string)

(defcustom sej-dashboard t
  "Use dashboard at startup or not. If Non-nil, use dashboard, otherwise will restore previous session."
  :type 'boolean)

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

(defcustom sej-irc-nick "nick"
  "Nickname for ERC/IRC."
  :type 'string)

(defcustom sej-irc-pass "pass"
  "Password for ERC/IRC."
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


;;;;; exec-path-from-shell
;; set-up exec-path and hook for server-start
;; [[https://github.com/purcell/exec-path-from-shell][exec-path-from-shell]]
(use-package exec-path-from-shell
  :demand t
  :straight t
  :when (or sys/macp sys/linuxp daemonp)
  :init
  (setq exec-path-from-shell-variables '("DOTFILES"
                                         "EDITOR"
                                         "EMACS"
                                         "PYENV_ROOT"
                                         "IPYTHONDIR"
                                         "PYTHONSTARTUP"
                                         "PYLINTHOME"
                                         "FZF_DEFAULT_COMMAND"
                                         "HIST_IGNORE"
                                         "HISTSIZE"
                                         "SAVEHIST"
                                         "SSH_AUTH_SOCK"
                                         "TREE_SITTER_DIR"
                                         "JAVA_HOME"
                                         "ZSH"
                                         "PATH"
                                         "MANPATH"
                                         "FPATH"
                                         )
        exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))


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
  :demand t
  :straight t)


;;;;; f
;; modern API for working with files and directories in Emacs.
;; [[https://github.com/rejeep/f.el][f.el]]
(use-package f
  :demand t
  :straight t)


;;;;; s
;; The long lost Emacs string manipulation library.
;; [[https://github.com/magnars/s.el][s.el]]
(use-package s
  :demand t
  :straight t)


;;;;; cl-lib
;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/cl-lib.el][cl-lib.el]]
(use-package cl-lib
  :demand t
  :straight (:type built-in))


;;;;; Emacs internal settings
;; - a use-package friendly place to put settings
;;   no real extra value to putting as setq but feels clean
(use-package emacs
  :demand t
  :straight (:type built-in)
  :custom
;;;;;; general
      (inhibit-startup-message t "No splash screen.")
      (inhibit-startup-screen t)
      (inhibit-startup-echo-area-message t)
      (use-file-dialog nil)
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
      (use-dialog-box nil "Use echo areas for yes-no as well as file.")
      (case-fold-search 1 "Ignore case when searching.")
      (echo-keystrokes 0.1 "How quick to display multi-keystrokes.")
      (next-line-add-newlines t "Add a new line when going to the next line.")

;;;;;; whitespace and end-of-buffer settings
      (indicate-empty-lines t)
      (indicate-buffer-boundaries t)
      (show-trailing-whitespace nil)
      (mode-require-final-newline nil)
      (require-final-newline nil)

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

;;;;;; Don't use GTK+ tooltip
      (when (boundp 'x-gtk-use-system-tooltips)
        (setq x-gtk-use-system-tooltips nil))

;;;;;; windows
      (window-divider-mode)      )


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
        track-eol t
        line-move-visual nil
        line-number-mode t
        mode-line-percent-position nil
        save-interprogram-paste-before-kill t
        kill-read-only-ok t
        shift-select-mode nil
        show-trailing-whitespace nil
        set-mark-command-repeat-pop t)

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
        (when (= p (point)) ad-do-it)))))


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
        completion-pcm-complete-word-inserts-delimiters t
        completion-pcm-word-delimiters "-_./:| "
        completion-styles '(partial-completion substring initials flex)
        completion-category-overrides '((file (styles initials basic))
                                        (buffer (styles initials basic))
                                        (info-menu (styles basic)))
        completions-format 'vertical
        read-answer-short t
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
  :straight t
  :init
  (setq no-littering-etc-directory (expand-file-name "~/.local/share/emacs/")
        no-littering-var-directory (expand-file-name "~/.cache/emacs/"))
  (defalias 'nl-var-expand #'no-littering-expand-var-file-name)
  (defalias 'nl-etc-expand #'no-littering-expand-etc-file-name)

  (setq auto-save-file-name-transforms `((".*"
                                          ,(no-littering-expand-var-file-name "auto-save/") t))))


;;;;; OSX System specific environment setting
(when sys/macp
  (setq exec-path (append exec-path '("/usr/local/bin")))
  )


;;;;; Linux System specific environment setting
(when sys/linuxp
  (setq exec-path (append exec-path '("/usr/local/bin")))
  )


;;;;; Microsoft Windows specific environment settings
;; set execution paths
(when sys/win32p
  (setenv "PATH"
          (mapconcat
           #'identity exec-path path-separator))

  ;; set exec-path for latex installation
  (setq exec-path (append (list sej-latex-directory
                                "c:/msys64/mingw64/bin"
                                "/mingw64/bin/") exec-path)))


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
  "Turn off 'sej-mode'."
  (sej-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-sej-mode)


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
          (setq mac-right-command-modifier 'none) ;right command is left alone to mac
          (setq mac-right-option-modifier 'none) ;Stays as alt key (like å∫ç∂)
          (setq mac-function-modifier 'hyper) ;hyper is function & held tab key (Karabiner)
          (setq mac-control-modifier 'control) ;Karabiner swapped & caps_lock
          (setq mac-right-control-modifier 'super) ; actually left control
          (setq mac-option-modifier 'alt) ; left option is A-alt key
          (setq mac-command-modifier 'meta)) ;right command is meta
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


;;;;; sej-mode-map bindings
(unbind-key "C-z")
(unbind-key "M-z")
(global-unset-key (kbd "C-h C-h"))

;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))

(bind-keys :prefix-map sej-mode-cz-map
           :prefix "C-z"
           :prefix-docstring "SeJ Personal cz-key bindings"
           ("v" . emacs-version)
	   ("\\" . align-regexp) ;Align your code in a pretty way.
           )

(bind-keys :map sej-mode-map
	   ("C-c ." . org-time-stamp)
           ("s-." . pop-to-mark-command)
	   ("C-h C-h" . nil)
	   ("A-SPC" . cycle-spacing)
	   ("M-j" . (lambda () (join-line -1)))
           )

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


;;;;; Advice
;; accept versus warn from the Advice system.
;; [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html][Advising Emacs Lisp Functions]]
(use-package advice
  :straight (:type built-in)
  :init
  (setq-default ad-redefinition-action 'accept))


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
  :commands which-key-mode
  :defines sej-mode-map
  :config
  (setq which-key-use-C-h-commands t
        which-key-separator " "
        which-key-prefix-prefix "+")
  (which-key-setup-side-window-bottom))


;;;;; helpful
;; - helpful is an improved help-fns & help-fns+
;; - https://github.com/Wilfred/helpful
(use-package helpful
  :straight (helpful :type git :host github :repo "Wilfred/helpful")
  :bind ( ("C-h C-d" . helpful-at-point)
          ("C-h c" . helpful-command)
          ("C-h C" . helpful-command)
          ("C-h k" . helpful-key) ; C-h k
          ("C-h v" . helpful-variable) ; C-h v
          ("C-h f" . helpful-callable) ; C-f v
          ("C-h M" . helpful-macro))  )


;;; user interface
;;;; themes
;;;;; wombat theme
(use-package emacs
  :straight (:type built-in)
  :ensure t
  :preface
  (load-theme 'wombat))


;;;;; default-text-scale
;; easily adjust the default font size in all Emacs frames
;; [[https://github.com/purcell/default-text-scale][default-text-scale]]
(use-package default-text-scale
  :straight t
  :bind (:map sej-mode-map
              ("C-z +" . default-text-scale-increase)
              ("C-z -" . default-text-scale-decrease)
              ("s-r" . default-text-scale-reset))
  :config
  (setq default-text-scale-amount 20))


;;;; frames
;;;;; frame
;; built-in frame package
(use-package frame
  :straight (:type built-in)
  :bind (:map sej-mode-map
              ("s-4" . dired-other-frame)
              ("s-5" . make-frame-command)
              ("s-6" . delete-other-frames)
              ("s-w" . delete-frame)
              ("C-x w" . delete-frame)
              ("C-z <up>" . sej/frame-resize-full)
              ("H-C-j" . sej/frame-resize-full)
              ("C-z <left>" . sej/frame-resize-l)
              ("H-C-h" . sej/frame-resize-l)
              ("<A-M-left>" . sej/frame-resize-l)
              ("C-z <S-left>" . sej/frame-resize-l2)
              ("H-C-S-h" . sej/frame-resize-l2)
              ("C-z <right>" . sej/frame-resize-r)
              ("H-C-l" . sej/frame-resize-r)
              ("<A-M-right>" . sej/frame-resize-r)
              ("C-z <S-right>" . sej/frame-resize-r2)
              ("H-C-S-l" . sej/frame-resize-r2)
              ("H-C-f" . toggle-frame-fullscreen)
              ("C-z F" . toggle-frame-fullscreen)
              ("A-M-m" . sej/frame-recentre)
              ("C-z m" . sej/frame-recentre))
  :init
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1
        frame-title-format '("Emacs - %b")
        icon-title-format frame-title-format
        frame-resize-pixelwise t
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

;;;;;; mac specific frame settings
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

  (defun sej/frame-resize-full ()
    "Set frame full height and 1/2 wide, position at screen left."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)  (- (display-pixel-width) (if sys/macp (eval 13) (eval 25)))
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1))

  (defun sej/frame-resize-l ()
    "Set frame full height and 1/2 wide, position at screen left."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 0)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1))

  (defun sej/frame-resize-l2 ()
    "Set frame full height and 1/2 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)  )

  (defun sej/frame-resize-r ()
    "Set frame full height and 1/2 wide, position at screen right."
    (interactive)
    (set-frame-position (selected-frame) (- (truncate (/ (display-pixel-width) 2)) 0) 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 0)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)  )

  (defun sej/frame-resize-r2 ()
    "Set frame full height and 1/2 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
    (interactive)
    (set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
    (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                    (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)  )

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
(use-package fringe
  :straight (:type built-in)
  :init
  (set-fringe-mode 5))


;;;; buffers
;;;;; buffer key-bindngs
(define-key sej-mode-map (kbd "s-s") 'save-buffer)

(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)

(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "C-x k") 'kill-this-buffer)


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
(define-key sej-mode-map (kbd "C-z S") 'sej/create-scratch-buffer)
(define-key sej-mode-map (kbd "C-z S") 'sej/create-scratch-buffer)


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
  :bind (:map sej-mode-map
         ("s-0" . delete-window)
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
         ("A-j" . previous-line)
         ("A-k" . next-line)
         ("A-l" . right-char)

         ;;scroll window up/down by one line
         ("A-n" . (lambda () (interactive) (scroll-up 1)))
         ("A-p" . (lambda () (interactive) (scroll-down 1)))
         )
  :init
  (setq display-buffer-alist
        '(;; top side window
          ("\\*\\(Flycheck\\|Package-Lint\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ;; bottom side window
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; ("\\*e?shell.*"
          ;;  (display-buffer-in-side-window)
          ;;  (window-height . 0.16)
          ;;  (side . bottom)
           ;; (slot . 1))
          ;; left side window
          ;; ("\\*helpful.*"
          ;;  (display-buffer-in-side-window)
          ;;  (window-width . 0.30)       ; See the :hook
          ;;  (side . right)
          ;;  (slot . 0)
          ;;  (window-parameters . ((no-other-window . t))))
          ;; ("\\*Help.*"
          ;;  (display-buffer-in-side-window)
          ;;  (window-width . 0.30)       ; See the :hook
          ;;  (side . right)
          ;;  (slot . 0)
          ;;  (window-parameters . ((no-other-window . t))))
          ;; right side window
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . (" "
                                                      mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1))))
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
  :bind (("C-x o" . ace-window)
         ("M-o" . ace-window))
  :custom-face
  (aw-leading-char-face ((t (:inherit error :bold t :height 1.1))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;;;;; golden-ratio
;; - keep windows balanced with in-focus window larger
;; - https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :after which-key
  :hook (emacs-startup . golden-ratio-mode)
  :blackout
  :init
  (golden-ratio-mode 1)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window)
  (setq golden-ratio-auto-scale t)
  (add-to-list 'golden-ratio-exclude-buffer-names which-key-buffer-name)  )


;;;; mode-line
;;;;; doom-modeline
;; - A fancy and fast mode-line inspired by minimalism design
;; - https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :preface
  (setq doom-modeline-hud t
        doom-modeline-project-detection 'auto))



;;;;; all-the-icons
;; - NOTE: Must run `M-x all-the-icons-install-fonts' manually on Windows
;; - https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

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


;;;; search
;;;;; isearch
;; built-in search function
(use-package isearch
  :straight (:type built-in)
  :init
  (setq lazy-highlight-initial-delay 0
        search-highlight t
        search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil
        isearch-lazy-highlight t
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        isearch-yank-on-move 'shift
        isearch-allow-scroll 'unlimited))


;;;;; vertico
;; - alternative to ivy, ido, helm
;; - [[https://github.com/minad/vertico][vertico]]
(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


;;;;; orderless
;; provides an orderless completion style that divides the pattern into space-separated components,
;; and matches candidates that match all of the components in any order.
;; [[https://github.com/oantolin/orderless][orderless]]
(use-package orderless
  :straight t
  :demand t
  :init
  (setq completion-styles '(orderless basic))
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))  )


;;;;; embark
;; acting on targets
;; [[https://github.com/oantolin/embark/][embark]]
(use-package embark
  :straight t
  :bind  (("C-." . embark-act)         ;; pick some comfortable binding
          ("M-." . embark-dwim)        ;; good alternative: M-.
          ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;;;; consult
;; - complementary to selectrum
;; - [[https://github.com/minad/consult][consult]]
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("C-z C-y" . consult-register-load)
         ("C-z C-w" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-z C-M-y" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch

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

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
;;;;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
           (car (project-roots project)))))
;;;;;;; 2. projectile.el (projectile-project-root)
   ;; (autoload 'projectile-project-root "projectile")
   ;; (setq consult-project-root-function #'projectile-project-root)
;;;;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
;;;;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)


;;;;; ag
;; - searching with the silver searcher
;; - https://github.com/Wilfred/ag.el
(when (executable-find "ag")
  (use-package ag
    :commands ag
    :config
    (setq ag-executable (executable-find "ag")))
  (setq-default ag-highlight-search t))


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
(define-key sej-mode-map (kbd "C-z <tab>") 'sej/indent-buffer)


;;;; history packages
;;;;; undo-fu
;; - Simple, stable linear undo with redo for Emacs.
;; - https://gitlab.com/ideasman42/emacs-undo-fu
(use-package undo-fu
  :straight t
  :blackout
  :bind ( ("C-/" . undo-fu-only-undo)
          ("C-S-/" . undo-fu-only-redo))
  :config (setq undo-fu-allow-undo-in-region t))


;;;;; undo-fu-session
;; - Save & recover undo steps between Emacs sessions.
;; - https://gitlab.com/ideasman42/emacs-undo-fu-session
(use-package undo-fu-session
  :straight t
  :after undo-fu
  :hook (emacs-startup . global-undo-fu-session-mode))


;;;;; recentf
;; - recent file history list settings
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el
(use-package recentf
  :straight (recentf :type built-in)
  :hook (emacs-startup . recentf-mode)
  :bind ("C-x C-r" . crux-recentf-find-file)
  :config
  (setq recentf-max-saved-items 2000
        recentf-max-menu-items 100
        recentf-auto-cleanup 'never
        recentf-exclude '((expand-file-name package-user-dir)
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
  (history-length 3000)
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)
                                 "each varible is perssted accross Emacs sessions.")
  (savehist-autosave-interval 300)
  (savehist-save-minibuffer-history t))


;;;; movement
;;;;; crux
;; - a Colection of Rediculously Useful eXtensions
;; - smart moving to beginning of line or to beginning of text on line
;; - https://github.com/bbatsov/crux
(use-package crux
  :bind ( ("C-c o" . crux-open-with)
          ([remap kill-line] . crux-smart-kill-line) ; C-k
          ("C-S-RET" . crux-smart-open-line-above)
          ([(shift return)] . crux-smart-open-line)
          ("C-c n" . crux-cleanup-buffer-or-region)
          ("C-c u" . crux-view-url)
          ("s-k" . crux-duplicate-current-line-or-region)
          ("C-x C-r" . crux-recentf-find-file)
          ("C-c C-k" . crux-duplicate-current-line-or-region)
          ("C-c M-k" . crux-duplicate-and-comment-current-line-or-region)
          ([remap kill-whole-line] . crux-kill-whole-line)
          ("C-<backspace>" . crux-kill-line-backwards)
          ("C-z I" . crux-find-shell-init-file))
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
  :straight t
  :bind ( ("C-a" . mwim-beginning) ; C-a
          ("C-e" . mwim-end))) ; C-e better than crux


;;;;; avy
;; - Jump to things in Emacs tree-style
;; - https://github.com/abo-abo/avy
(use-package avy
  :bind ( ("H-'" . avy-goto-char)
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


;;;; regions
;;;;; easy-kill-extras
;; - This package contains extra functions for easy-kill/easy-mark.
;; - Kill & Mark things easily
;; - https://github.com/leoliu/easy-kill
;; - https://github.com/knu/easy-kill-extras.el
(use-package easy-kill-extras
  :straight t
  :bind (("M-w" . easy-kill) ; M-w
         ("C-M-@" . easy-mark-sexp) ; C-M-@
         ("M-@" . easy-mark-word) ; M-@
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
                          (?T string-up-to-char-backward "")))    )


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
  :bind ( ("M-<down>" . drag-stuff-down)
          ("H-n" . drag-stuff-down)
          ("M-<up>" . drag-stuff-up)
          ("H-p" . drag-stuff-up)
          ("H-S-p" . drag-stuff-up))
  :config
  (drag-stuff-global-mode)
  (drag-stuff-define-keys)
  (add-to-list 'drag-stuff-except-modes 'org-mode))


;;;;; smart-region
;; - Smartly select region, rectangle, multi cursors
;; - remaping set-mark-command to smart-region
;; - https://github.com/uk-ar/smart-region
(use-package smart-region
  :bind ("C-S-<SPC>" . smart-region) ; C-S-SPC
  :config (smart-region-on))


;;;;; smart-hungry-delete
;; - Hungry deletion
;; - https://github.com/hrehfeld/emacs-smart-hungry-delete
(use-package smart-hungry-delete
  :straight t
  :blackout t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :config (smart-hungry-delete-add-default-hooks))


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
  :blackout
  :defines iedit-mode
  :commands (symbol-overlay-get-symbol
             symbol-overlay-assoc
             symbol-overlay-get-list
             symbol-overlay-jump-call)
  :bind (("C-M-;" . iedit-mode) ;; define Iedit mode so as to remove default message
         ("H-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode-end . symbol-overlay-mode)))


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
  :straight t
  :blackout t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack))


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
  (push 'org-mode hl-todo-include-modes))


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
;;  (diff-hl-flydiff-mode 1)

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
  :blackout
  :hook (emacs-startup . volatile-highlights-mode))


;;;;; whitespace
;; - Visualize TAB, (HARD) SPACE, NEWLINE
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/whitespace.el
(use-package whitespace
  :straight (whitespace :type built-in)
  :blackout
  ;; :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
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
  :hook ((imenu-after-jump . my-recenter-and-pulse)
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


;;;;; paren
;; - show paren mode
;; - [[https://www.emacswiki.org/emacs/ShowParenMode][paren wiki]]
(use-package paren
  :straight (:type built-in)
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0
        show-paren-style 'mixed ; parenthesis, expression, mixed
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t))


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


;;;;; sh-script
;; shell script mode built-in
;; [[https://www.emacswiki.org/emacs/ShMode][sh-script sh-mode wiki]]
(use-package company-shell
  :straight)

(use-package sh-script
  :straight (:type built-in)
  :requires company-shell
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
  (use-package company-shell :straight)
  :init
  (add-hook 'sh-mode-hook #'sej/sh-prettify-mode-line)
  (add-hook 'sh-mode-hook #'sh-script-extra-font-lock-activate)
  :config
  (setq-default sh-basic-offset 2)
  (compdef
   :modes '(sh-mode shell-script-mode)
   :capf #'sh-completion-at-point-function
   :company '(company-capf
              company-shell
              company-shell-env
              company-files
              company-dabbrev-code)))


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
(use-package ssh-config-mode
  :straight t)


;;;;; indent-guide
;; - show vertical lines to guide indentation
;; - https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :blackout)


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


;;;;; comment-dwim-2
;; - replacement for the Emacs built-in command comment-dwim
;; - https://github.com/remyferre/comment-dwim-2
(use-package comment-dwim-2
  :bind (([remap comment-dwim] . comment-dwim-2) ; M-;
        ("C-;" . comment-indent) ; C-; trailing comment
         ("C-:" . comment-kill) ; kill trailing comment
         ("C-x C-;" . comment-box) ; box comment
         ))


;;;;; dumb-jump
;; - Jump to definition via `ag'/`rg'/`grep'
;; - https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :straight t
  :hook ((emacs-startup . dumb-jump-mode)
         (xref-backend-functions . dumb-jump-xref-activate))
  :defines sej-mode-map
  :config
  (setq dumb-jump-prefer-searcher 'rg))


;;;; vcs
;;;;; Project
;; built-in project management
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html][Working with projects]]
(use-package project
  :straight (:type built-in))


;;;;; magit
;; - interface to the version control system Git
;; - https://magit.vc/
(use-package magit
  :straight t
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


;;;;; git-gutter-fringe
;; git fringe
;; [[https://github.com/emacsorphanage/git-gutter-fringe][git-gutter-fringe]]
(use-package git-gutter-fringe
  :straight t
  :hook (prog-mode . global-git-gutter-mode))


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
        dabbrev-case-distinction nil
        dabbrev-case-fold-search t
        dabbrev-case-replace nil
        dabbrev-check-other-buffers t
        dabbrev-eliminate-newlines nil
        dabbrev-upcase-means-case-search t))


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
          try-expand-dabbrev-visable
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-list-all-buffers
          try-expand-list
          try-expand-line-all-buffers
          try-expand-line
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-complete-lisp-symbol-partially
          try-compelete-lisp-symbol)))


;;;;; iComplete
;; built-in minibuffer completion helper
;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html][Fast minibuffer selection]]
(use-package icomplete
  :straight (:type built-in)
  :hook (emacs-startup . icomplete-mode)
  :config
  (setq icomplete-delay-completions-threshold 0
        icomplete-max-chars 0
        icomplete-compute-delay 0
        icomplete-show-matches-on-no-input t
        icomplete-hide-common-prefix nil
        icomplete-prospects-height 1
        icomplete-separator " · "
        icomplete-with-completion-tables t
        icomplete-in-buffer t)
  (fido-mode -1))


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

(define-key emacs-lisp-mode-map (kbd "C-<return>") 'sej/eval-dwim)
(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sej/eval-dwim)
(define-key emacs-lisp-mode-map (kbd "H-<return>") 'eval-buffer)

(define-key emacs-lisp-mode-map (kbd "C-c D") 'toggle-debug-on-error)
(global-set-key (kbd "C-z C-e") 'toggle-debug-on-error)

;; use flymake
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))

;;;;; eldoc
;; - we don't want this minor mode to be shown in the minibuffer, however
;; we use eldoc to show the signature of the function at point in the minibuffer
;; - https://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :blackout
  :straight (:type built-in)
  :hook
  ((prog-mode . turn-on-eldoc-mode))
  :init
  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p 3) )


;;;;; elisp-slime-nav
;; - turn on elisp-slime-nav
;; - M-. works to jump to function definitions
;; - M-, to jump back
;; - https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav
  :blackout
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
  :config
  (global-unset-key (kbd "C-c C-d d"))
  (global-unset-key (kbd "C-c C-d C-d")))


;;;;; sly
;; replacement repla for slime
;; [[https://github.com/joaotavora/sly][sly]]
(use-package sly
  :straight t
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
  :bind (:map sej-mode-map
              ("s-i" . sej/ielm-other-window))
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
  )


;;;;; registers
;; - Registers allow you to jump to a file or other location quickly.
;; (i for init.el, r for this file) to jump to it.
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html
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
    :blackout (dashboard-mode)
    :commands sej/open-dashboard
    :hook (emacs-startup . sej/open-dashboard)
    :bind (("<f6>" . sej/open-dashboard)
           (:map sej-mode-map
                 ("C-z d" . sej/open-dashboard)))
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
;; - display ^L page breaks as tidy horizontal lines
;; - https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :blackout t
  :straight t
  :hook ((dashboard-mode
          text-mode
          comint-mode
          helpful-mode
          help-mode
          compilation-mode) . page-break-lines-mode))


;;;;; autoinsert
;; - mode that comes with Emacs that automagically inserts text into new buffers
;;   based on file extension or the major mode
;; - https://github.com/emacs-mirror/emacs/blob/master/lisp/autoinsert.el
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
  :hook (dired-mode . hl-line-mode)
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-aFGhlv --group-directories-first")

  (setq dired-dwim-target t)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    ;; Use GNU ls as `gls' from `coreutils' if available.
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (when (executable-find "gls")
      (setq insert-directory-program (executable-find "gls")
            dired-use-ls-dired t) ))  )


;;;;; dired-aux
;; - auxiliary functionality of dired
;; - https://github.com/jwiegley/emacs-release/blob/master/lisp/dired-aux.el
(use-package dired-aux
  :straight (dired-aux :type built-in)
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)  )


;;;;; dired-x
;; - Extra Dired functionality
;; - https://www.gnu.org/software/emacs/manual/html_node/dired-x/
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
  :defines (sej-mode-map
            eshell-mode-map)
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
                                      ("C-l" . eshell/clear) ) )))

  :bind ( :map sej-mode-map
         ("C-z E" . eshell) )

  :config
  ;; (require 'esh-opt)
  ;; (require 'em-cmpl)
  ;; (require 'em-smart)
  ;; (require 'em-term)
  ;; (require 'em-prompt)

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
;; - shortcut for Dired in eshell
(defun eshell/d (&rest args)
  "Shortcut of d for Dired in eshell with ARGS."
  (dired (pop args) "."))


;;;;; eshell/magit
;; - function to open magit-status for the current directory
(defun eshell/magit ()
  "Function to open 'magit-status' for the current directory."
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
  :bind  (:map sej-mode-map
               ("C-z S" . shell))
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


;;; init-mini.el --- end
(message "init-mini.el ends here")
(provide 'init-mini)
;;; init-mini.el ends here

