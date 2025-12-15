;;; init.el --- SeJ Emacs configurations. -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2025 Stephen Jenkins

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
;; brew install emacs-head@31 --with-cocoa --with-crash-debug --with-ctags --with-dbus --with-imagemagick
;; --with-mailutils --with-native-comp --with-native-full-aot --with-tree-sitter --with-mps --with-xwidgets
;;

;;; Code:
(message "Emacs start")

;;; initialize environment

;;;; Before package

;; Ask the user whether to terminate asynchronous compilations on exit.
;; This prevents native compilation from leaving temporary files in /tmp.
(setq native-comp-async-query-on-exit t)

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(if (boundp 'use-short-answers)
	(setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq undo-limit (* 13 160000)
	  undo-strong-limit (* 13 240000)
	  undo-outer-limit (* 13 24000000))

;;;; Use-Package set-up
;; Ensure use-package is available
(require 'use-package)
(require 'use-package-ensure)
(use-package system-packages)
(use-package use-package-ensure-system-package
  :ensure nil
  :after use-package)

;;;;; Package manager

(require 'package)
(package-initialize)

(defun sej/package-install-refresh-contents (&rest args)
  "With first `package-install' of ARGS, `package-refresh-contents' to ensure list is up to date."
  (package-refresh-contents)
  (advice-remove 'package-install 'sej/package-install-refresh-contents))

(advice-add 'package-install :before 'sej/package-install-refresh-contents)

;;;;; exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :defer 1  ;; Changed from :demand t - defer 1s after startup (saves 0.31s)
  :custom (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))


;;;;;  Warnings
;; set-up server & suppress warnings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/warnings.el
(require 'warnings)
;; remove warnings for cl depreciated and server already running
;; (setq warning-suppress-types (quote ((cl) (server) (iedit) (org-element) (comp) (bytecomp) (files))))
;; (setq warning-suppress-log-types (quote ((cl) (org-element) (comp) (bytecomp) (files))))
;; (setq byte-compile-warnings '(not obsolete cl-functions make-local suspicious lexical free-vars))
;; (setq native-comp-async-report-warnings-errors 'silent)
;; ;; Make warnings visible, but keep noise down
;; ;; Show and log warnings (including package load issues), but not lower levels.
;; (setq warning-minimum-level :warning)
;; (setq warning-minimum-log-level :warning)

;; Ensure *Messages* buffer is never read-only
(with-current-buffer (messages-buffer)
  (setq buffer-read-only nil))

;; prevent warnings buffer from poping up during package compile
;; still available in the buffer list
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;;; during loading clear file-name-handler-alist
;; avoids loader files polluting the file history list
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)))

;;;; system custom constants
(require 'sej-global-constants)

;;;;; OS System specific environmental settins
;; OSX System specific
(when sys/macp
  (require 'sej-os-specific-osx))

;; Linux System specific
(when sys/linuxp
  (require 'sej-os-specific-linux))

;; FreeBSD System specific
(when sys/freebsdp
  (require 'sej-os-specific-bsd))

;; Microsoft Windows specific
(when sys/win32p
  (require 'sej-os-specific-windows))

;;;;; fonts
;; sej/install-iosvka-font
(require 'sej-fonts)

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

;;;;; scratch
;; bury rather than kill, sej/create-scratch-buffer, sej/scratch-save, sej/scratch-restore
(require 'sej-scratch)

;;;;; ultra-scroll
;; [[https://github.com/jdtsmith/ultra-scroll][ultra-scroll]] is a smooth-scrolling package for emacs, with native support for standard builds as well as emacs-mac
(use-package ultra-scroll
  :defer 10
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :custom ((scroll-conservatively 101) ; important!
		   (scroll-margin 0)
		   (fast-but-imprecise-scrolling t)
		   (scroll-error-top-bottom t)
		   (scroll-preserve-screen-position t)
		   (auto-window-vscroll nil)
		   (next-screen-context-lines 0)
		   (hscroll-margin 2)
		   (hscroll-step 1))
  :config
  (ultra-scroll-mode 1))

;;;;; Blackout
;; Similar to packages like minions, diminish, or delight.
;; You can alter how your minor and major modes show up in the mode-line.
;; https://github.com/raxod502/blackout
(use-package blackout)

;;;;; Alert
;; Alert is a Growl-workalike for Emacs
;; uses a common notification interface and multiple, selectable "styles"
;; https://github.com/jwiegley/alert
(use-package alert
  :config
  (if sys/macp (setq alert-default-style #'osx-notifier))  )

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
  :commands (-map -union))

;;;;; f
;; modern API for working with files and directories in Emacs.
;; https://github.com/rejeep/f.el
(use-package f
  :commands (f-read f-join f-exists-p f-expand f-executable?))

;;;;; s
;; The long lost Emacs string manipulation library.
;; https://github.com/magnars/s.el
(use-package s
  :commands (s-split s-trim s-concat))

;;;;; cl-lib
;; Forward cl-lib compatibility library for Emacs<24.3
;; https://elpa.gnu.org/packages/cl-lib.html
(use-package cl-lib
  :commands (cl-loop cl-destructuring-bind cl-incf))

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

;;;;; no-littering & backups
;; set the default paths for configuration files & persistent data
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :custom ((create-lockfiles nil)
		   (backup-by-copying t)    ; Don't delink hardlinks
		   (backup-by-copying-when-linked t)
		   (delete-old-versions t)  ; Clean up the backups
		   (version-control t)      ; Use version numbers on backups,
		   (kept-new-versions 5)    ; keep some new versions
		   (kept-old-versions 5)    ; and some old ones, too
		   (vc-make-backup-files t)
           (make-backup-files t)
		   (auto-save-default t)
		   (auto-save-interval 300)
		   (auto-save-timeout 30)
		   (auto-save-no-message t)
		   (auto-save-include-big-deletions t))
  :init
  (eval-and-compile ; Ensure values don't differ at compile time.
	(setq no-littering-etc-directory user-emacs-directory)
	(setq no-littering-var-directory user-emacs-directory))
  :config
  ;; Put backup files neatly away
  (let ((backup-dir (concat no-littering-var-directory "backups/"))
	    (auto-saves-dir (concat no-littering-var-directory "auto-save/")))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
	    (make-directory dir t)))
    (setq backup-directory-alist `(("." . ,(expand-file-name backup-dir))
                                   auto-save-list-file-prefix ,(concat auto-saves-dir ".saves-")
                                   auto-save-file-name-transforms `((".*" ,auto-saves-dir t)))
          tramp-backup-directory-alist `((".*" . ,backup-dir))
          tramp-auto-save-directory auto-saves-dir)))

;;;;; [[https://github.com/jamescherti/compile-angel.el][compile-angel]]
;; Native compilation enhances Emacs performance by converting Elisp code into
;; native machine code, resulting in faster execution and improved
;; responsiveness.
;;
;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  (push ".*org-element.*" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  (compile-angel-on-load-mode 1))

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
  (display-line-numbers-width 3 "default nil used in display-line-numbers-mode")
  (display-line-numbers-widen t "widen numbers in display-line-numbers-mode")
  (x-underline-at-descent-line t "position underlines at descent line instead of the baseline")
  (remote-file-name-inhibit-cache 50 "number of seconds to cache file name")

;;;;;; whitespace and end-of-buffer settings
  (indicate-empty-lines nil)
  (indicate-buffer-boundaries nil)
  (show-trailing-whitespace nil)
  (mode-require-final-newline nil)
  (require-final-newline t)

;;;;;; tabs, indentation and the TAB key
  (tab-always-indent 'complete)
  (tab-first-completion 'word-or-paren-or-punct)
  (tab-width 4)
  (indent-tabs-mode nil)
  (fill-column 80)
  (x-stretch-cursor 1)
  (read-extended-command-predicate #'command-completion-default-include-p)

;;;;;; long line settings
  (truncate-lines t)
  (font-lock-maximum-decoration t)
  (truncate-partial-width-windows nil)
  (auto-hscroll-mode 'current-line)
  (truncate-string-ellipsis "…")
  (word-wrap t)

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
;;;;;; context-menu-mode
  (when (memq 'context-menu sej-ui-features)
    (when (and (display-graphic-p) (fboundp 'context-menu-mode))
	  (add-hook 'after-init-hook #'context-menu-mode)))

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
  (setq ansi-color-for-comint-mode t
		comint-prompt-read-only t
		comint-buffer-maximum-size 4096)

;;;;;; Automatically visit symlink sources
  (setq find-file-visit-truename t)
  (setq vc-follow-symlinks t)

;;;;;; shorthand for interactive lambdas
  (defmacro λ (&rest body)
    "Shorthand for interactive lambdas (BODY)."
    `(lambda ()
       (interactive)
       ,@body))

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
  :bind ((:prefix-map sej-C-q-map
                      :prefix "C-q"
                      :prefix-docstring "SeJ Personal C-q key bindings"
                      ("v"   . emacs-version)
                      ("\\"  . align-regexp) ;Align your code in a pretty way.
                      ("D"   . describe-personal-keybindings)
		              ("l"   . sej/toggle-relative-ln)))

  :bind (:prefix-map term-map
                     :prefix "C-q S"
                     :prefix-docstring "Term bindings")

  :bind (:prefix-map sej-C-m-map
                     :prefix "<C-m>"
                     :prefix-docstring "Multi-menu")

  :bind (:prefix-map sej-denote-map
		             :prefix "C-,"
		             :prefix-docstring "SeJ Denote key bindings"
		             ("."   . org-timestamp))

  :bind (:map override-global-map
              ("s-." . pop-to-mark-command)
	          ("M-j" . join-line)
              ("C-x j" . duplicate-dwim)
	          ("M-\\" . cycle-spacing)
	          ;; movement complementary to windmove / windswap
	          ("A-h" . left-char)
	          ("A-j" . next-line)
	          ("A-k" . previous-line)
	          ("A-l" . right-char)
	          ;;scroll window up/down by one line
	          ("A-n" . sej/scroll-up-one)
	          ("A-p" . sej/scroll-down-one)
              ("C-j" . sej/open-new-line)
              ("C-S-J" . sej/open-line-above-and-indent)
              ;; url-inserts
              ("C-H-u" . sej/url-insert-safari)
              ("C-H-i" . sej/url-inesert-edge))  )  ;; end of emacs

;;;;; Simple
;; built-in: simple settings
(use-package simple
  :demand t
  :blackout ((visual-line-mode . "")
             (auto-fill-mode . ""))
  :ensure nil
  :custom ((blink-matching-paren 'jump-offscreen)
		   (column-number-mode t)
		   (delete-trailing-lines t)
		   (eval-expression-print-length nil)
		   (eval-expression-print-level nil)
		   (idle-update-delay 1)
		   (kill-buffer-delete-auto-save-files t)
		   (kill-do-not-save-duplicates t)
		   (kill-ring-max 300)
		   (kill-ring-deindent-mode t)
		   (track-eol t)
		   (line-move-visual nil)
		   (line-number-mode t)
		   (save-interprogram-paste-before-kill t)
		   (kill-read-only-ok t)
		   (shift-select-mode nil)
		   (set-mark-command-repeat-pop t)
		   (backward-delete-char-untabify-method nil)))

;;;;; minibuffer
;; built-in: minibuffer settings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/minibuffer.el
(use-package minibuffer
  :ensure nil
  ;;  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  ((minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
   (enable-recursive-minibuffers t)
   (completion-cycle-threshold 7)
   (completion-flex-nospace nil)
   (completion-category-defaults nil)
   (completion-category-overrides
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
	  (info-menu (styles basic))))
   (completions-format 'vertical)
   (read-answer-short t)
   (completion-ignore-case t)
   (read-buffer-completion-ignore-case t)
   (read-file-name-completion-ignore-case t)
   (resize-mini-windows 'grow-only)
   (imenu-auto-rescan t "rescan the buffer for Imenu entries")
   (imenu-max-item-length 160 "prevent truncation of long function names")
   (next-line-add-newlines nil "disable auto-adding a new line at the bottom when scrolling"))
  :hook (after-init . minibuffer-depth-indicate-mode))

;;;;; uniquify
;; built-in: to make buffer names unique but identifiable
(use-package uniquify
  :demand t
  :ensure nil
  :custom ((uniquify-ignore-buffers-re "^\\*")
	       (uniquify-buffer-name-style 'post-forward-angle-brackets)
	       (uniquify-strip-common-suffix t)
	       (uniquify-after-kill-buffer-p t)
	       (uniquify-separator "/")))

;;;;; repeat
;; built-in: bindings to allow easier keys when repeating functions
(use-package repeat
  :ensure nil
  :hook (emacs-startup . repeat-mode)
  :bind (:map ctl-x-map
              :repeat-map window-repeat-map
              ("o" . other-window)
              ("w" . window-configuration-to-register)
              ("0" . delete-window)
              ("1" . zygospore-toggle-delete-other-windows)
              ("2" . split-window-below)
              ("3" . split-window-right)
              ("C-3" . split-only-this-window-horizontally)
              ("M-2" . resplit-windows-vertically)
              ("M-3" . resplit-windows-horizontally)
              ("+" . balance-windows)
              ("-" . shrink-window-if-larger-than-buffer)
              ("^" . enlarge-window)
              ("{" . shrink-window-horizontally)
              ("}" . enlarge-window-horizontally)))

;;;; history packages
;;;;; savehist
;; built-in: recent buffer history settings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode) ;; earlier in startup so available to dashboard
  :custom ((history-delete-duplicates t)
	       (enable-recursive-minibuffers t "Allow commands in minibuffers.")
	       (history-length 10000)
	       (savehist-save-minibuffer-history t)
	       (savehist-autosave-interval 300)
	       (savehist-additional-variables '(mark-ring
										    kill-ring
										    register-alist
										    global-mark-ring
										    search-ring
										    regexp-search-ring
										    extended-command-history))))

;;;;; recentf
;; built-in: recent file history list settings
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el
(use-package recentf
  :hook (emacs-startup . recentf-mode)
  :custom ((recentf-max-saved-items 2000)
	       (recentf-max-menu-items 100)
	       (recentf-auto-cleanup 'never)
	       (recentf-exclude '((expand-file-name package-user-dir)
			                  ".cache"
			                  ".cask"
			                  ".elfeed"
			                  "bookmarks"
			                  "cache"
			                  "persp-confs"
			                  "recentf"
			                  "undo-tree-hist"
			                  "url"
			                  "COMMIT_EDITMSG\\'"))))

;;;;; vundo
;; visual undo displays the undo history as a tree
;; https://github.com/casouri/vundo
(use-package vundo
  :bind ("C-z" . vundo))

;;; general functions / packages

;;;;; sej external functions:
;; sej general functions
;;  sej/scroll-up-one
;;  sej/scroll-down-one
;;  sej/add-all-to-list
;;  sej/open-new-line
;;  sej/kill-whole-word
;;  sej/open-line-above-and-indent
;;  sej/save-macro
;;  sej/exec
;;  sej/exec-with-rc
;;  sej/is-exec
;;  sej/resolve-exec
;;  sej/exec-if-exec
;;  sej/toggle-relative-ln
;;  sej/keyboard-quit-dwim
(require 'sej-general)

;; sej system functions
;;  sej/lookup-password
;;  sej/exec
;;  sej/exec-with-rc
;;  sej/is-exec
;;  sej/resolve-exec
;;  sej/exec-if-exec
;;  sej/url-insert-safari
;;  sej/url-insert-edge
;;  sej/browse-homepage
;;  sej/sej/create-non-existent-directory
(require 'sej-system)

;; sej text manipulation
;;  sej/open-new-line
;;  sej/open-line-above-and-indent
;;  sej/kill-whole-word
;;  sej/indent-buffer
;;  sej/set-region-writeable
;;  sej/number-rectangle
(require 'sej-text-manipulation)

;; sej insert functions
;;  sej/insert-lambda
;;  sej/insert-tm
;;  sej/insert-copyright
;;  sej/insert-rightarrow
;;  sej/insert-infinity
;;  sej/insert-check
;;  sf-symbol-insert-name
;;  sf-symbol-insert
(require 'sej-inserts)


;;;;; [[https://github.com/dgtized/list-environment.el][list-environment]]
;; environment variables tabulated
;; process environment editor
(use-package list-environment
  :commands list-environment)

;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html][Advice]]
;; built-in: accept versus warn from the Advice system.
(use-package advice
  :ensure nil
  :init
  (setq-default ad-redefinition-action 'accept))

;;;;; [[https://www.gnu.org/software/emacs/manual/html_mono/calc.html][calc]]
;; built-in: calculator
(use-package calc
  :ensure nil
  :bind (:map sej-C-q-map
              ("c" . calc)
              ("C-c" . quick-calc))
  :commands (quick-calc calc)
  :custom (setq math-additional-units
		        '((GiB "1024 * MiB" "Giga Byte")
		          (MiB "1024 * KiB" "Mega Byte")
		          (KiB "1024 * B" "Kilo Byte")
		          (B nil "Byte")
		          (Gib "1024 * Mib" "Giga Bit")
		          (Mib "1024 * Kib" "Mega Bit")
		          (Kib "1024 * b" "Kilo Bit")
		          (b "B / 8" "Bit"))))

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/checkdoc.el][checkdoc]]
;; built-in: checker of buffer for style issues
(use-package checkdoc
  :ensure nil
  :config
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;;;; Security
;;;;; [[https://www.gnu.org/software/emacs/manual/html_mono/auth.html][Auth-Source]]
;; built-in: authentication source
(eval-when-compile
  (require 'auth-source)
  (require 'auth-source-pass))

(setq auth-sources `(,(f-expand "~/.ssh/.authinfo.gpg")
                     ,(f-expand "~/.ssh/.authinfo")
                     macos-keychain-generic
                     macos-keychain-internet)
      auth-source-do-cache t)

;; sej/lookup-password (host user port) in sej-general
;;  example (sej/lookup-password "10.0.1.3" "admin" 80)

;;;;; [[https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources][epa]]
;; built-in: EasyPG assistant for GnuPG implementation of the OpenPGP standard
(use-package epa
  :ensure nil)

;;;;; [[https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources][epq]]
;; built-in: EasyPG for GnuPG implementation of the OpenPGP standard
(use-package epg
  :ensure nil
  :ensure-system-package gpg
  :custom (epg-pinentry-mode 'loopback))

;;;;; [[https://www.gnu.org/software/emacs/manual/html_mono/emacs-gnutls.html][Gnutls]]
;; built-in: GnuTLS is a library that establishes encrypted SSL or TLS connections.
(use-package gnutls
  :ensure nil
  :custom ((gnutls-verify-error t)
	       (gnutls-min-prime-bits 2048))
  :init
  (unless (gnutls-available-p)
    (message "installing gnutls...")
    (shell-command-to-string "brew install gnutls")))

;;;;; [[https://github.com/tarsius/keychain-environment][keychain-environment]]
;; set up any SSH or GPG keychains that the Keychain tool has set up for us
(use-package keychain-environment
  :ensure-system-package keychain
  :hook (emacs-startup . keychain-refresh-environment))

;;;;; [[https://www.emacswiki.org/emacs/ProtectingBuffers][Emacs-lock]]
;; built-in: lock buffer from kill and/or Emacs from exiting
(use-package emacs-lock
  :blackout ""
  :ensure nil)

;;;; help
;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/help-mode.el][help]]
;; built-in: help mode settings
(use-package help
  :ensure nil
  :bind (:map help-map
			  ("=" . describe-char)
			  ("j" . describe-face)
			  ("-" . describe-keymap))
  :hook
  (help-mode . visual-line-mode)
  :custom ((help-window-select 'always)
		   (help-enable-completion-autoload nil)
		   (help-enable-autoload nil)
		   (help-enable-symbol-autoload nil)
		   (help-window-select t))
  :init
  (advice-add 'help-window-display-message :override #'ignore)
  (setq apropos-do-all t))

;;;;; [[https://github.com/justbur/emacs-which-key][which-key]]
;; built-in: minibuffer keybinding prompts
(use-package which-key
  :ensure nil
  :hook (emacs-startup . which-key-mode)
  :bind (("C-h h" . which-key-show-top-level)
         ("C-h M-m" . which-key-show-major-mode))
  :commands which-key-mode
  :custom
  ((which-key-use-C-h-commands t)
   (which-key-separator " ")
   (which-key-prefix-prefix "+")
   (which-func-update-delay 1.0)
   (which-key-idle-secondary-delay 0.25)
   (which-key-add-column-padding 1)
   (which-key-max-description-length 40)))

;;;;; [[https://github.com/Wilfred/helpful][helpful]]
;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind (("C-h ." . helpful-at-point)
         ("C-h M" . helpful-macro)
         ([remap describe-command] . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable))
  :custom (helpful-max-buffers 7))

;;;;; [[https://github.com/kickingvegas?tab=repositories][casual]]
;; triansient based jump screens
(use-package casual-suite
  :defer 2
  :config
  (require 'casual-lib))

;;below are part of casual main package
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
  :bind (:map calc-mode-map
	          ("C-o" . casual-calc-tmenu)
	          :map calc-alg-map
	          ("C-o" . casual-calc-tmenu)))

(use-package casual-calendar
  :ensure nil
  :after calendar
  :bind (:map calendar-mode-map
	          ("C-o" . casual-calendar)))

(use-package casual-dired
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
	          ("C-o" . casual-dired-tmenu)
	          ("s" . casual-dired-sort-by-tmenu)
	          ("/" . casual-dired-search-replace-tmenu)))

(use-package casual-editkit
  :ensure nil
  :bind (:map global-map ("C-o" . casual-editkit-main-tmenu)))

(use-package casual-help
  :ensure nil
  :bind (:map help-mode-map
	          ("C-o" . casual-help-tmenu)
	          ("M-[" . help-go-back)
	          ("M-]" . help-go-forward)
	          ("p" . casual-lib-browse-backward-paragraph)
	          ("n" . casual-lib-browse-forward-paragraph)
	          ("P" . help-goto-previous-page)
	          ("N" . help-goto-next-page)
	          ("j" . forward-button)
	          ("k" . backward-button)))

(use-package casual-ibuffer
  :ensure nil
  :after ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . casual-ibuffer-tmenu)
              ("F" . casual-ibuffer-filter-tmenu)
              ("s" . casual-ibuffer-sortby-tmenu)
              ("{" . ibuffer-backwards-next-marked)
              ("}" . ibuffer-forward-next-marked)
              ("[" . ibuffer-backward-filter-group)
              ("]" . ibuffer-forward-filter-group)
              ("$" . ibuffer-toggle-filter-group)
	          ("<double-mouse-1>" . ibuffer-visit-buffer)
              ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window))
  :config
  (require 'hl-line)
  (require 'mouse))

(use-package casual-image
  :ensure nil
  :hook (image-mode . casual-image-tmenu)
  :bind (:map image-mode-map
	          ("C-o" . casual-image-tmenu)))

(use-package casual-info
  :ensure nil
  :bind (:map Info-mode-map
	          ("C-o" . casual-info-tmenu)
	          ;; # Info
	          ;; Use web-browser history navigation bindings
	          ("M-[" . Info-history-back)
	          ("M-]" . Info-history-forward)
	          ;; Bind p and n to paragraph navigation
	          ("p" . casual-info-browse-backward-paragraph)
	          ("n" . casual-info-browse-forward-paragraph)
	          ;; Bind h and l to navigate to previous and next nodes
	          ;; Bind j and k to navigate to next and previous references
	          ("h" . Info-prev)
	          ("j" . Info-next-reference)
	          ("k" . Info-prev-reference)
	          ("l" . Info-next)
	          ;; Bind / to search
	          ("/" . Info-search)
	          ;; Set Bookmark
	          ("B" . bookmark-set))
  :hook ((Info-mode . hl-line-mode)
	     (Info-mode . scroll-lock-mode)))

(use-package casual-isearch
  :ensure nil
  :after isearch
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package casual-man
  :ensure nil
  :bind (:map man-mode-map
	          ("C-o" . casual-man-tmenu)
	          ("n" . casual-lib-browse-forward-paragraph)
	          ("p" . casual-lib-browse-backward-paragraph)
	          ("[" . Man-previous-section)
	          ("]" . Man-next-section)
	          ("j" . next-line)
	          ("k" . previous-line)
	          ("K" . Man-kill)
	          ("o" . casual-man-occur-options)))

(use-package casual-re-builder
  :ensure nil
  :after re-builder
  :bind (:map reb-mode-map
	          ("C-o" . casual-re-builder-tmenu)
	          :map reb-lisp-mode-map
	          ("C-o" . casual-re-builder-tmenu)))

                                        ; separate casual packages
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
  :custom ((tron-legacy-theme-vivid-cursor t)
	       (tron-legacy-theme-dark-fg-bright-comments nil)
	       (tron-legacy-theme-softer-bg nil))
  :init
  (load-theme 'tron-legacy :no-confirm))

;;;;; modus-themes
;; built-in: theme
;; https://protesilaos.com/emacs/modus-themes#h:1af85373-7f81-4c35-af25-afcef490c111
;; current preferred theme
(use-package emacs ; built-in package
  :bind ("C-c C-t" . modus-themes-toggle)
  :custom ((require-theme 'modus-themes)
	       (modus-themes-italic-constructs t)
	       (modus-themes-bold-constructs nil)
	       (modus-themes-mixed-fonts t)
	       (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi)))
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
	     ;; below are meant to match default OSX and my BTT settings in other apps
         ;; full-half-1/3-2/3 w/hjkl combo
         ("A-k" . sej/frame-resize-full)
	     ("A-M-k" . toggle-frame-fullscreen)
         ("H-A-h" . sej/frame-resize-l)
         ("H-A-l" . sej/frame-resize-r)
         ("H-A-M-s-h" . sej/frame-resize-l2)
         ("H-A-M-s-l" . sej/frame-resize-r2)
         ("H-A-M-h" . sej/frame-resize-l3)
         ("H-A-M-l" . sej/frame-resize-r3)
         ;; full-half-1/3-2/3 cursor combo ; fn(hyper) changes cursor to home / end / prior / next
         ("A-<prior>" . sej/frame-resize-full)
	     ("A-M-<prior>" . toggle-frame-fullscreen)
         ("A-<home>" . sej/frame-resize-l)
         ("A-<end>" . sej/frame-resize-r)
         ("A-M-s-<home>" . sej/frame-resize-l2)
         ("A-M-s-<end>" . sej/frame-resize-r2)
         ("A-M-<home>" . sej/frame-resize-l3)
         ("A-M-<end>" . sej/frame-resize-r3)
	     ;; additional
         ("H-A-c" . sej/frame-recentre)
         ("H-A-f" . toggle-frame-fullscreen)
         ("C-M-h" . sej/hide-frame)
         ;; combo if above are not available
         ("s-C" . sej/frame-recentre)
         ("s-F" . toggle-frame-fullscreen)
         ("<S-s-up>" . sej/frame-resize-full)
         ("<S-s-left>" . sej/frame-resize-l)
         ("<S-s-right>" . sej/frame-resize-r)
	     )
  :custom ((window-divider-default-places t)
	       (window-divider-default-bottom-width 1)
	       (window-divider-default-right-width 1)
	       (frame-title-format "%F--%b-[%f]--%Z")
	       (icon-title-format frame-title-format)
	       (undelete-frame-mode t)
	       (window-resize-pixelwise nil))

  :init
  (unless (display-graphic-p)
    (menu-bar-mode -1))

  ;; Don't open a file in a new frame
  (when (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))

  ;; Resize frame to left half after startup
  (if (display-graphic-p)
      (add-hook 'emacs-startup-hook 'sej/frame-resize-l) )

  ;; don't use native full-screen on osx
  (when sys/mac-x-p
    (setq ns-use-native-fullscreen nil))

  ;; sej frame functions
  ;;  sej/frame-resize-full
  ;;  sej/frame-resize-l
  ;;  sej/frame-resize-l2
  ;;  sej/frame-resize-r
  ;;  sej/frame-resize-r2
  ;;  sej/frame-resize-r3
  ;;  sej/frame-recentre
  ;;  sej/hide-frame
  (require 'sej-frames))

;;;; buffers / windows
;;;;; buffer key-bindngs
(bind-keys*
 ("C-<return>" . save-buffer)
 ;; ("s-y" . bury-buffer)
 ("C-c y" . bury-buffer)
 ("C-c r" . revert-buffer)
 ("s-r" . revert-buffer)
 ("C-x k" . kill-current-buffer)
 ("s-]" . bs-cycle-next) ; buffer cycle next
 ("s-[" . bs-cycle-previous)
 ("H-0" . delete-window)
 ("H-1" . delete-other-windows)
 ("H-2" . split-window-vertically)
 ("H-3" . split-window-right)
 ("C-x K" . sej/quit-other)
 ("M-'" . next-multiframe-window) ;; wind move to multifram window
 )

(setq-default bs-default-configuration "all-intern-last")
(setq cutom-buffer-done-kill t)
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)
(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)

;; sej buffer functions:
;;   sej/dos2unix
;;   sej/unix2dos
;;   sej/save-buffer-as-utf8
;;   sej/quit-and-kill-auxiliary-windows
;;   sej/delete-buffer-contents-no-matter-properties
;;   sej/remove-all-buffer-properties
;;   sej/quit-other
(require 'sej-buffers)

;;;;; fringe
;; built-in: fringe-mode
;; https://www.emacswiki.org/emacs/TheFringe
;; https://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
(use-package fringe
  :ensure nil
  :if window-system
  :init
  (fringe-mode '(4 . 4)))


;;;;; [[https://github.com/abo-abo/ace-window][ace-window]]
;; quickly selecting a window to switch to
;; C-u prefex to move window
;; C-u C-u prefex to delete window
(use-package ace-window
  :bind (("C-x o" . sej/ace-two-window)
         ("M-o" . ace-window))
  :hook (emacs-startup . ace-window-display-mode)
  :custom ((aw-dispatch-always nil)
	       (aw-frame-offset '(100 . 0))
	       (aw-minibuffer-flag t))
  :config
  (add-to-list 'aw-dispatch-alist '(?h sej/hide-frame "Hide Frame"))

  (defun sej/ace-two-window (arg)
    "Run full ace windows with two windows."
    (interactive "P")
    (let (dispatch (eval aw-dispatch-always))
      (setq aw-dispatch-always t)
      (ace-window arg)
      (setq aw-dispatch-always dispatch))))

;;;;; [[info:emacs#Window Convenience][winner-mode]]
;; built-in: Restore old window configurations
(use-package winner
  :ensure nil
  :hook (emacs-startup . winner-mode)
  :commands (winner-undo winner-redo)
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo))
  :custom (winner-boring-buffers '("*Completions*"
                                   "*Compile-Log*"
                                   "*inferior-lisp*"
                                   "*Fuzzy Completions*"
                                   "*Apropos*"
                                   "*Help*"
                                   "*cvs*"
                                   "*Buffer List*"
                                   "*Ibuffer*"
                                   "*esh command on file*")))

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/saveplace.el][saveplace]]
;; built-in: automatically save place in files so return to same place in next session
(use-package saveplace
  :ensure nil
  :hook (emacs-startup . save-place-mode)
  :commands (save-place-mode save-place-local-mode)
  :custom ((save-place-forget-unreadable-files t)
           (save-place-limit 600)
           (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))))

;;;;; [[https://github.com/jamescherti/easysession.el][easysession]]
;; The easysession Emacs package is a session manager for Emacs that can persist
;; and restore file editing buffers, indirect buffers/clones, Dired buffers,
;; windows/splits, the built-in tab-bar (including tabs, their buffers, and
;; windows), and Emacs frames. It offers a convenient and effortless way to
;; manage Emacs editing sessions and utilizes built-in Emacs functions to
;; persist and restore frames.
(use-package easysession
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)
  :bind (("C-c s l" . easysession-switch-to)
         ("C-c s s" . easysession-save-as))
  :init
  ;; The depth 102 and 103 have been added to to `add-hook' to ensure that the
  ;; session is loaded after all other packages. (Using 103/102 is particularly
  ;; useful for those using minimal-emacs.d, where some optimizations restore
  ;; `file-name-handler-alist` at depth 101 during `emacs-startup-hook`.)
  ;;(add-hook 'emacs-startup-hook #'easysession-save-mode 103)
  ;;(add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  :custom ((easysession-mode-line-misc-info t)  ; Display the session in the modeline
           (easysession-save-interval (* 10 60)) ; Save every 10 minutes
           (easysession-switch-to-save-session nil)) ; do not save session on switch
  :config
  (add-to-list 'savehist-additional-variables 'easysession--current-session-name)

  (defun my-easysession-visible-buffer-list ()
    "Return a list of all buffers considered visible in the current session.

A buffer is included if it satisfies any of the following:
- It is currently displayed in a visible window.
- It is associated with a visible tab in `tab-bar-mode', if enabled.
- It is the *scratch* buffer (included as a special case).

The returned list contains live buffers only."
    (let ((visible-buffers '()))
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   (or ;; Exception: The scratch buffer
                    (string= (buffer-name buffer) "*scratch*")
                    ;; Windows
                    (get-buffer-window buffer 'visible)
                    ;; Tab-bar windows
                    (and (bound-and-true-p tab-bar-mode)
                         (fboundp 'tab-bar-get-buffer-tab)
                         (tab-bar-get-buffer-tab buffer t nil))))
          (push buffer visible-buffers)))
      visible-buffers))

  (setq easysession-buffer-list-function #'my-easysession-visible-buffer-list)

  (defun my-empty-easysession ()
    "Set up a minimal environment when easysession creates a new session."
    (when (and (boundp 'tab-bar-mode) tab-bar-mode)
      (tab-bar-close-other-tabs))
    (delete-other-windows)
    (scratch-buffer))

  (add-hook 'easysession-new-session-hook #'my-empty-easysession))

;;;;; [[https://github.com/karthink/popper][popper]]
;; minor-mode to tame ephemeral windows
(use-package popper
  ;; EXPERIMENTAL: Changed from :demand t - only needed when using popper features (POPPER)
  :defer t
  :bind (("C-`" . popper-toggle)
         ("C-~" . popper-cycle)
         ("H-`" . popper-toggle-type))
  :custom (popper-reference-buffers '("Output\\*$"
				                      "\\*Async Shell Command\\*"
				                      help-mode
				                      special-mode
				                      compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

;;;; tabs
;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html][tab-bar]]
;; built-in: tabs for virtual desktops
;; C-x t prefix
(use-package tab-bar
  :ensure nil
  :bind (("M-["  . tab-bar-history-back)
         ("M-]" . tab-bar-history-forward))
  :custom ((tab-bar-close-button-show t)
	       (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
	       (tab-bar-close-tab-select 'recent)
	       (tab-bar-new-tab-choice t)
	       (tab-bar-new-tab-to 'right)
	       (tab-bar-position nil)
	       (tab-bar-show 2)
	       (tab-bar-tab-hints nil)
	       (tab-bar-tab-name-function 'tab-bar-tab-name-all))
  :config
  (tab-bar-mode t)
  (tab-bar-history-mode t))

;;;; mode-line
;;;;; [[https://github.com/seagle0128/doom-modeline][doom-modeline]]
;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)
	     (doom-modeline-before-github-fetch-notification . auth-source-pass-enable)))

;;;;; [[https://github.com/tarsius/minions][minions]]
;; implements a nested menu that gives access to all minor modes
(use-package minions
  :init
  (setq doom-modeline-minor-modes t)
  (minions-mode 1))

;;;;; [[https://github.com/rainstormstudio/nerd-icons.el][nerd-icons]]
;; use nerd-icons-install-fonts
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")  )

;;;;; [[https://github.com/rainstormstudio/nerd-icons-dired][nerd-icons-dired]]
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;;;; [[https://github.com/seagle0128/nerd-icons-ibuffer][nerd-icons-ibuffer]]
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;;; [[https://github.com/rainstormstudio/nerd-icons-completion][nerd-icons-completion]]
;; icons with completion
(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

;;;;; [[https://github.com/LuigiPiucco/nerd-icons-corfu][nerd-icons-corfu]]
(use-package nerd-icons-corfu
  :after corfu
  :custom (nerd-icons-corfu-mapping
           '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
	         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
	         (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Remember to add an entry for `t', the library uses that as default.
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; text manipulation

;;;; multi-edit
;;;;; [[https://github.com/magnars/multiple-cursors.el][multiple cursors]]
;; Multiple cursors
(use-package multiple-cursors
  :bind (:map global-map
	          ("C-S-c C-S-c"   . mc/edit-lines)
              ("C->"           . mc/mark-next-like-this)
              ("C-<"           . mc/mark-previous-like-this)
              ("C-c C-<"       . mc/mark-all-like-this)
              ("C-M->"         . mc/skip-to-next-like-this)
              ("C-M-<"         . mc/skip-to-previous-like-this)
              ("A-<mouse-1>"   . mc/add-cursor-on-click)
	          ("A-a"         . mc/edit-beginings-of-lines)
	          ("A-e"         . mc/edit-ends-of-lines)
	          ("A-s"         . mc/mark-next-like-this-symbol)
	          ("A-S"         . mc/mark-all-symbols-like-this)
	          ("A-D"         . mc/mark-all-like-this-in-defun)
	          ("A-d"         . mc/mark-all-dwim)
	          ("A-r"         . mc/mark-all-in-region)
	          ("A-w"         . mc/mark-next-like-this-word)
	          ("A-W"         . mc/mark-all-words-like-this)
              :map mc/keymap
	          ("C-m v"         . mc/vertical-align)
              ("C-|" . mc/vertical-align-with-space)))

;;;; search
;;;;; isearch
;; built-in: search function
;; [[https://github.com/VernonGrant/discovering-emacs/blob/main/show-notes/3-making-incremental-search-work-for-you.md][tutorial: making incremental search work for you]]
(use-package isearch
  :ensure nil
  :bind ( :map minibuffer-local-isearch-map
          ("M-/" . isearch-complete-edit)
          :map isearch-mode-map
          ("C-g" . isearch-cancel) ;instead of `isearch-abort'
          ("M-/" . isearch-complete))
  :custom ((search-highlight t)
	       (search-whitespace-regexp ".*?")
	       (isearch-lax-whitespace t)
	       (isearch-regexp-lax-whitespace nil)
	       (isearch-lazy-highlight t)
	       (isearch-lazy-count t)
	       (isearch-yank-on-move t)
	       (lazy-count-prefix-format "(%s/%s) ")
	       (lazy-count-suffix-format nil)
	       (isearch-yank-on-move 'shift)
	       (isearch-allow-scroll 'unlimited)
	       (isearch-repeat-on-direction-change t)
	       (lazy-highlight-initial-delay 0)
	       (lazy-highlight-no-delay-length 3)
	       (search-ring-max 30)
	       (regexp-search-ring-max 30)
	       (isearch-wrap-pause t)))

;;;;; [[https://github.com/emacsorphanage/anzu][Anzu]]
;; good query replace search
(use-package anzu
  :bind  (([remap query-replace] . anzu-query-replace-regexp)
          ("C-H-r" . anzu-query-replace)
          ("C-H-S-r" . anzu-query-replace-at-cursor)
          :map isearch-mode-map
          ("C-H-r" . anzu-isearch-query-replace))
  :config (global-anzu-mode))

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
  ;; EXPERIMENTAL: Changed from :demand t - only needed when using bookmarks (BOOKMARK)
  :defer t
  :custom ((bookmark-use-annotations t)
	       (bookmark-automatically-show-annotations t)
	       (bookmark-set-fringe-mark t)) ; Emacs28
  :init (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)
  :config (setq bookmark-save-flag +1))

;;;; completion
;;;;; [[https://www.emacswiki.org/emacs/AbbrevMode][abbrev]]
;; built-in: for inserting abbreviations
(use-package abbrev
  :ensure nil
  :hook ((emacs-startup org-mode) . abbrev-mode)
  :custom ((abbrev-file-name             ;; tell emacs where to read abbrev
			(concat no-littering-var-directory "abbrev_defs") only-global-abbrevs nil)    ;; definitions from...
		   (save-abbrevs 'silently))
  :config
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("orgh" "" sej/org-header 0)
      ("orgl" "" sej/org-wrap-elisp 0)
      ("orgs" "" sej/org-wrap-source 0))))

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/dabbrev.el][dabbrev]]
;; built-in: to let you write just a few characters of words you've written
;; earlier to be able to expand them.
(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand
             dabbrev-completion)
  ;; Use Dabbrev with Corfu!
  ;; Swap M-/ and C-M-/
  :bind ("C-M-/" . dabbrev-expand)
  :custom ((dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
	       (dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
	       (dabbrev-backward-o nly nil)
	       (dabbrev-case-distinction 'case-replace)
	       (dabbrev-case-fold-search t)
	       (dabbrev-case-replace 'case-replace)
	       (dabbrev-check-other-buffers t)
	       (dabbrev-eliminate-newlines t)
	       (dabbrev-upcase-means-case-search t))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;;;;; [[https://www.emacswiki.org/emacs/HippieExpand][hippie-expand]]
;; built-in: expand at point in various ways
(use-package hippie-expand
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :custom (hippie-expand-try-functions-list '(try-expand-dabbrev-visible
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
;; alternative selection visual to ivy, ido, helm
(use-package vertico
  :bind (("C-H-v" . vertico-repeat))
  :hook ((emacs-startup . vertico-mode)
	     (minibuffer-setup . vertico-repeat-save))
  :custom ((vertico-scroll-margin 0) ;; Different scroll margin
	       (vertico-count 20)        ;; Show more candidates
	       (vertico-resize t)        ;; Grow and shrink the Vertico minibuffer
	       (vertico-cycle t)        ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.

	       ;; Hide commands in M-x which do not work in the current mode. Vertico
	       ;; commands are hidden in normal buffers.
	       (read-extended-command-predicate #'command-completion-default-include-p)))

;;;;;; vertico-repeat
;; repeat last vertico session C-H-v
(use-package vertico-repeat
  :ensure nil
  :after vertico
  :config (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

;;;;;; vertico-quick
;; quick jump to items in vertico list ala Avy
(use-package vertico-quick
  :ensure nil
  :after vertico
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
  ;; EXPERIMENTAL: Changed from :demand t - extension, loads after vertico (VERTICO-MULTIFORM)
  :after vertico
  :bind (:map vertico-map
              ("C-i"   . sej/vertico-multiform-toggle-ur)
              ("<tab>" . vertico-insert))
  :custom ((vertico-multiform-commands
	        '((consult-imenu buffer)
		      (consult-line buffer)
		      (consult-grep buffer)
		      (consult-git-grep buffer)
		      (consult-ripgrep buffer)
		      (consult-yank-pop)
		      (embark-bindings buffer)
		      (xref-find-references buffer)))
	       (vertico-multiform-categories '((t reverse ))))
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
  ;; EXPERIMENTAL: Changed from :demand t - extension, :after already defers (VERTICO-DIRECTORY)
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
		      ("RET" . vertico-directory-enter)
		      ("DEL" . vertico-directory-delete-char)
		      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;;; [[https://github.com/minad/corfu][corfu]]
;; small completion program similar to company
(use-package corfu
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (eat-mode . corfu-mode))
  :bind (:map corfu-map
              ("A-<return>" . corfu-complete)
              ("<escape>". corfu-quit)
              ("M-d" . corfu-show-documentation)
              ("M-l" . 'corfu-show-location))

  ;; Optional customizations
  :custom ((corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
	       (corfu-auto nil)                    ;; Enable auto completion
	       (corfu-separator ?\s)             ;; Orderless field separator
	       (corfu-quit-at-boundary nil)        ;; Never quit at completion boundary
	       (corfu-quit-no-match nil)           ;; t, 'separator, nil Never quit
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
	       (corfu-preselect-first t))        ; Preselect first candidate?

  :config
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  (global-corfu-mode)

  (setq global-corfu-modes '((not markdown-mode) t))
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

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
    ;; EXPERIMENTAL: Changed from :demand t - extension, loads after corfu (CORFU-HISTORY)
    :after corfu
    :config
    (add-to-list 'savehist-additional-variables 'corfu-history)
    (corfu-history-mode t))
  (use-package corfu-indexed
    :ensure nil
    ;; EXPERIMENTAL: Changed from :demand t - extension, loads after corfu (CORFU-INDEXED)
    :after corfu
    :config
    (corfu-indexed-mode t))
  (use-package corfu-popupinfo
    :ensure nil
    ;; EXPERIMENTAL: Changed from :demand t - extension, loads after corfu (CORFU-POPUPINFO)
    :after corfu
    :config
    (corfu-popupinfo-mode t))
  (use-package corfu-terminal
    :unless (display-graphic-p)
    ;; EXPERIMENTAL: Changed from :demand t - extension, loads after corfu (CORFU-TERMINAL)
    :after corfu
    :init
    (corfu-terminal-mode +1)  )

  (use-package corfu-doc-terminal
    :vc (:url "https://codeberg.org/akib/emacs-corfu-doc-terminal"
              :rev :newest
              :branch "master")
    :unless (display-graphic-p)
    ;; EXPERIMENTAL: Changed from :demand t - extension, loads after corfu (CORFU-DOC-TERMINAL)
    :after corfu
    :init
    (corfu-doc-terminal-mode +1) ) ) ; end of corfu

;;;;; [[https://github.com/minad/cape][cape]]
;; completion at point extensions
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :after corfu
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind (("C-H-/" . completion-at-point)
         ("C-c p" . cape-prefix-map))
  :hook ((emacs-lisp-mode .  sej/cape-capf-setup-elisp)
         (org-mode . sej/cape-capf-setup-org)
         (eshell-mode . sej/cape-capf-setup-eshell)
         (git-commit-mode . sej/cape-capf-setup-git-commit))
  :custom (cape-dabbrev-min-length 3)
  :config
  (sej/add-all-to-list 'completion-at-point-functions
		               #'cape-dabbrev
		               #'cape-file
		               #'cape-abbrev
                       #'cape-elisp-block)

  ;; #'cape-history
  ;; #'cape-keyword
  ;; #'cape-tex
  ;; #'cape-sgml
  ;; #'cape-rfc1345
  ;; #'cape-dict
  ;; #'cape-elisp-symbol
  ;; #'cape-line

  ;; sej cape capf's:
  ;;   sej/cape-capf-ignore-keywords-elisp
  ;;   sej/cape-capf-setup-elisp
  ;;   sej/cape-capf-setup-org
  ;;   sej/cape-capf-setup-git-commit
  ;;   sej/cape-capf-setup-eshell
  (require 'sej-cape)

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
  :commands (marginalia-mode marginalia-cycle)
  :hook (emacs-startup . marginalia-mode)
  :bind (:map completion-list-mode-map
              ("M-A" . marginalia-cycle)
              :map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;;;;; [[https://github.com/oantolin/orderless][orderless]]
;; provides an orderless completion style that divides the pattern into space-separated components,
;; and matches candidates that match all of the components in any order.
(use-package orderless
  :defer 1
  :custom ((completion-styles '(orderless basic))
           (completion-category-defaults nil)
	       (completion-category-overrides '((file (styles basic partial-completion))))))

;;;;; [[https://github.com/radian-software/prescient.el][prescient]]
;; sorts and filters lists of candidates
(use-package prescient
  :hook (emacs-startup . prescient-persist-mode)
  :init (add-to-list 'completion-styles 'prescient))

(use-package vertico-prescient
  :after prescient
  :hook (vertico-mode . vertico-prescient-mode))

(use-package corfu-prescient
  :after prescient
  :hook (corfu-mode . corfu-prescient-mode))

;;;;; [[https://github.com/oantolin/embark/][embark]]
;; acting on targets
(use-package embark
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind  (("C-." . embark-act)        ;; pick some comfortable binding
          ("C-'" . embark-dwim)        ;; good alternative: M-.
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
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;; [[https://github.com/minad/consult][consult]]
;; completing read
(use-package consult
  :bind (("H-M-," . consult-recent-xref)
		 ("C-M-." . sej/project-find-regexp)
		 ("H-q" . consult-register-load)
         ;; C-c bindings (mode-specific-map)
         ("C-c k" . consult-kmacro)
         ("C-c h" . consult-history)
         ("C-c M-x" . consult-mode-command)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; C-x bindings (ctl-x-map)
         :map ctl-x-map
         ("C-r" . consult-recent-file)
		 ("M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("b" . consult-buffer)                    ;; orig. switch-to-buffer
         ("4 C-r" . find-file-read-only-other-window) ;; orig. nil
         ("4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("r i" . consult-register-load)
                                        ;("p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
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
         ("f" . consult-find)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("k" . consult-keep-lines)
         ("l" . consult-line)
         ("L" . consult-locate)
		 ("m" . consult-line-multi)            ;; needed by consult-line to detect isearch
		 ("p" . sej/consult-project-search)
         ("r" . consult-ripgrep)
         ("u" . consult-focus-lines)
         ;; isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s m" . consult-line-multi)            ;; needed by consult-line to detect isearch
		 ("M-s p" . sej/consult-project-search)
         :map consult-narrow-map
         ("?" . consult-narrow-help)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-e" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview `C-M-.'
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun sej/project-find-regexp ()
    "Use `project-find-regexp' with completion."
    (interactive)
    (defvar xref-show-xrefs-function)
    (let ((xref-show-xrefs-function #'consult-xref))
      (if-let ((tap (thing-at-point 'symbol)))
          (project-find-regexp tap)
        (call-interactively #'project-find-regexp))))

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
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

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

  (add-to-list 'consult-buffer-sources 'denote-source 'append)

  (defun sej/consult-project-search ()
    "Search current project files using `consult-ripgrep`."
    (interactive)
    (consult-ripgrep (project-root (project-current t)))) ) ;; end of consult

;;;;; [[https://github.com/karthink/consult-dir][consult-dir]]
;; Think of it like the shell tools autojump, fasd or z but for Emacs.
(use-package consult-dir
  :after vertico
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)
	     :map vertico-map
         ("C-x C-d" . consult-dir)
         ("M-g d"   . consult-dir)
         ("M-s f"   . consult-dir-jump-file))
  :custom (consult-dir-default-command #'dired))

;;;;; [[https://github.com/joaotavora/yasnippet][yasnippet]]
;; YASnippet is a template system for Emacs.
(use-package yasnippet
  :defer 1  ;; Changed from :demand t - defer after startup (saves 0.16s)
  :diminish yas-minor-mode
  :commands yas-minor-mode-on
  :bind (:map sej-C-q-map
	          ("y d" . yas-load-directory)
	          ("y f" . yas-visit-snippet-file)
	          ("y n" . yas-new-snippet)
	          ("y t" . yas-tryout-snippet)
	          ("y l" . yas-describe-tables)
	          ("y g" . yas-global-mode)
	          ("y m" . yas-minor-mode)
	          ("y r" . yas-reload-all)
	          ("y x" . yas-expand))
  :hook (prog-mode . yas-minor-mode-on)
  :custom ((yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
	       (yas-wrap-around-region t)
	       (which-key-add-keymap-based-replacements sej-C-q-map "y" "yasnippet"))
  :custom-face (yas-field-highlight-face ((t (:background "grey30"))))
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
  :custom  (avy-keys '(?q ?e ?r ?y ?u ?o ?p
			              ?a ?s ?d ?f ?g ?h ?j
			              ?k ?l ?' ?x ?c ?v ?b
			              ?n ?, ?/))
  :config
  (add-to-list 'savehist-additional-variables 'avy-ring)
  (advice-add 'avy-pop-mark :after #'(lambda () (recenter-top-bottom nil)))

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

;;;;; [[https://github.com/camdez/goto-last-change.el][goto-last-change]]
;; goto the last changes made in buffer
(use-package goto-last-change
  :bind ("H-." . goto-last-change-with-auto-marks)
  :config (advice-add 'goto-last-change-with-auto-marks :after #'recenter-top-bottom))

;;;;; [[https://github.com/DamienCassou/beginend][beginend]]
;; smart moves redefining M-< and M-> for some modes
(use-package beginend               ; smart M-< & M->
  :bind (("M-<" . beginning-of-buffer)
         ("M->" . end-of-buffer))
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (blackout mode))
  (beginend-global-mode))

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/subword.el][subword]]
;; built-in: Handling capitalized subwords in a nomenclature
(use-package subword
  :blackout t
  :hook (emacs-startup . subword-mode)
  :config
  ;; this makes forward-word & backward-word understand snake & camel case
  (setq c-subword-mode t)
  (global-subword-mode t))

;;;;; [[https://github.com/akicho8/string-inflection][string inflection]]
;; underscore -> UPCASE -> Camelcase conversion
(use-package string-inflection
  :bind (("M-u" . string-inflection-all-cycle)))

;;;; regions
;;;;; [[https://github.com/knu/easy-kill-extras.el][easy-kill-extras]]
;; This package contains extra functions for easy-kill/easy-mark.
;; Kill & Mark things easily
;; https://github.com/leoliu/easy-kill
(use-package easy-kill-extras
  :bind (("M-w" . easy-kill) ; M-w
         ([remap mark-sexp] . easy-mark-sexp) ; C-M-<SPC>
         ("M-@" . easy-mark-word) ; M-@
         ("H-<SPC>" . easy-mark) ; H-<SPC>
         ("M-z" . easy-mark-to-char)) ; M-z
  :custom (easy-kill-alist '((?w word           " ")
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

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/rect.el][rect]]
;; built-in: Rectangle
(use-package rect
  :ensure nil
  :bind ((:map sej-C-q-map
               ("N" . sej/number-rectangle))
         (:map ctl-x-r-map
               ("N" . sej/number-rectangle))))

;;;;; [[https://github.com/rejeep/drag-stuff.el][drag-stuff]]
;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :blackout
  :defer 2  ;; Defer to avoid initialization errors
  :bind (("s-<down>" . sej/drag-stuff-down) ; with Karabiner becomes R-command-n
         ("s-<up>" . sej/drag-stuff-up)) ; with Karabiner becomes R-command-p
  :config
  (drag-stuff-global-mode 1)  ;; Enable after package loads
  (defun sej/drag-stuff-up ()
    "Mod of drag-stuff-up which works in org-mode."
    (interactive)
    (if (equal major-mode #'org-mode)
        (call-interactively
         (if (org-at-heading-p)
             'org-metaup
           'drag-stuff-up))
      (drag-stuff-up 1)))

  (defun sej/drag-stuff-down ()
    "Mod of drag-stuff-down which works in org-mode."
    (interactive)
    (if (equal major-mode #'org-mode)
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

;;;; url actions
;;;;; [[https://gitlab.com/aimebertrand/org-mac-link][org-mac-link]]
(use-package org-mac-link
  ;; pull link from many osx apps
  :bind ("C-H-o" . org-mac-link-get-link))

;;;;; [[https://github.com/abo-abo/ace-link][ace-link]]
;; Quickly follow links
(use-package ace-link
  :bind (("H-u" . ace-link-addr)
         :map org-mode-map
         ("H-u" . ace-link-org))
  :config (ace-link-setup-default))

;;;;; [[https://github.com/tarsius/orglink][orglink]]
;; use Org mode links in other modes
(use-package orglink
  :hook (emacs-startup . global-orglink-mode))

;;;;; [[https://github.com/pashky/restclient.el][restclient]]
;; Allows query of a restclient with the results left into the buffer
;; use GET or POST plus http:10.0.1.1/rest/
;; then use C-c to execute (check examples directory for more)
(use-package restclient)

;;;;; [[https://github.com/federicotdn/verb?tab=readme-ov-file][verb]]
;; HTTP client for Emacs using org format to document
;; develops on the idea of restclient for files you are going to keep as notes
(use-package verb
  :after org
  :init
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;;;; highlighting faces fonts
;;;;; [[https://protesilaos.com/emacs/fontaine][fontaine]]
;; font management
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

;;;;; [[https://github.com/protesilaos/show-font][show-font]]
;; preview fonts on the system
;; [[https://protesilaos.com/emacs/show-font][manual]], [[https://github.com/protesilaos/show-font][github]], [[https://protesilaos.com/codelog/2024-09-10-emacs-show-font-0-1-0/][pics]]
(use-package show-font
  :init
  (which-key-add-keymap-based-replacements sej-C-q-map "C-f" "show-font")
  :bind (:map sej-C-q-map
	          ("C-f f" . show-font-select-preview)
	          ("C-f l" . show-font-list)
	          ("C-f t" . show-font-tabulated)))

;;;;; [[https://github.com/wolray/symbol-overlay][symbol-overlay]]
;; Highlight symbols and move between them
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

;;;;; [[https://github.com/Fanael/highlight-numbers][highlight-numbers]]
;; hightlight-numbers in a special way
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;;;;; [[https://github.com/Fanael/highlight-quoted][highlight-quoted]]
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;;;;; [[https://github.com/emacsmirror/rainbow-mode][rainbow-mode]]
;; Colorize color names in buffers
(use-package rainbow-mode
  :blackout t
  :hook ((prog-mode . rainbow-mode)
	     (org-mode . rainbow-mode)
	     (conf-space-mode . rainbow-mode))
  :bind (:map sej-C-q-map
	          ("R" . rainbow-mode))
  :custom ((rainbow-ansi-colors t)
	       (rainbow-x-colors t)))

;;;;; [[https://github.com/tarsius/hl-todo][hl-todo]]
;; Highlight TODO and similar keywords in comments and strings
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
  :init (which-key-add-key-based-replacements "C-q T" "hl-todo")
  :custom (hl-todo-keyword-faces '(("DEBUG" . "#7cb8bb")
				                   ("FIX" . "red")
				                   ("FIXED" . "forest green")
				                   ("HELP" . "#ff0000")
				                   ("KLUDGE" . "yellow")
				                   ("INPROCESS" . "yellow")
				                   ("LATER" . "#2c5353")
				                   ("WAIT" . "#2c5353")
				                   ("MAYBE" . "#d1bf8f")
				                   ("NOTE" . "#5f6000")
				                   ("OKAY" . "#5f9000")
				                   ("DONE" . "green")
				                   ("TRY" . "#5f7f5f")
				                   ("TODO" . "#cc9393")
				                   ("TEST" . "#ff7700")))
  :config (push 'org-mode hl-todo-include-modes))

;;;;; [[https://github.com/liuyinz/consult-todo][consult-todo]]
;; search and jump hl-todo keywords in buffers with consult
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

;;;;; [[https://github.com/k-talo/volatile-highlights.el][volatile-highlights]]
;; Highlight some buffer region operations
(use-package volatile-highlights
  :blackout t
  :hook (emacs-startup . volatile-highlights-mode))

;;;;; [[https://protesilaos.com/emacs/pulsar][Pulsar]]
;; small package that temporarily highlights the current line after a given function is invoked
;; additional to built-in pulse
(use-package pulsar
  :bind (("C-H-p" . pulsar-pulse-line)
	     ("C-H-h" . pulsar-highlight-dwim))
  :hook ((emacs-startup . pulsar-global-mode)
         ((consult-after-jump
           bookmark-after-jump
           magit-diff-visit-file) . pulsar-recenter-top)
         ((consult-after-jump
           imenu-after-jump) . pulsar-reveal-entry)
         (next-error . pulsar-pulse-line-red))
  :custom ((pulsar-pulse t)
	       (pulsar-delay 0.055)
	       (pulsar-iterations 10)
	       (pulsar-face 'pulsar-magenta)
	       (pulsar-highlight-face 'pulsar-yellow))
  :config
  ;; Check the default value of `pulsar-pulse-functions'.  That is where
  ;; you add more commands that should cause a pulse after they are
  ;; invoked
  (setq pulsar-pulse-functions (-union pulsar-pulse-functions '(pop-to-mark-command
								                                flymake-goto-next-error
								                                flymake-goto-prev-error
								                                recenter-top-bottom))))

;;;; indentation
;;;;; electric-indent
;; built-in: electric indent mode
(use-package electric
  :ensure nil
  :hook ((prog-mode . electric-indent-mode)
         (prog-mode . electric-pair-mode))
  :custom ((electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
           (electirc-pair-preserve-balance t)
           (electric-pair-pairs '((8216 . 8217) (8220 . 8221) (171 . 187)))
           (electric-pair-skip-self 'electric-pair-default-skip-self)
           (electric-pair-skip-whitespace nil)
           (electric-pair-skip-whitespace-chars '(9 10 32))
           (electric-quote-context-sensitive t)
           (electric-quote-paragraph t)
           (electric-quote-string nil)
           (electric-quote-replace-double t))
  :init (setq-default electric-indent-chars '(?\n ?\^?)))

;;;;; [[https://github.com/Malabarba/aggressive-indent-mode][agressive-indent]]
;; more agressive indentation
(use-package aggressive-indent
  :hook ((emacs-lisp-mode css-mode) . aggressive-indent-mode))

;;;;; [[https://github.com/jdtsmith/outli][outli]]
;; outlining with comments, simpler/updated than outshine
(use-package outli
  :vc (:url "https://github.com/jdtsmith/outli"
            :rev :newest
            :branch "main")
  :hook ((prog-mode text-mode) . outli-mode) ; or whichever modes you prefer
  :custom (outli-speed-commands nil))

;;;;; [[https://github.com/jdtsmith/indent-bars][indent-bars]]
;; Highlight indentations
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode) ; or whichever modes you prefer(use-package indent-bars
  :custom ((indent-bars-prefer-character t)
		   (indent-bars-treesit-support t)
		   (indent-bars-treesit-ignore-blank-lines-types '("module"))
		   ;; Add other languages as needed
		   (indent-bars-treesit-scope '((python function_definition class_definition for_statement
										        if_statement with_statement while_statement)))
		   ;; wrap may not be needed if no-descend-list is enough
		   (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
										       list list_comprehension
										       dictionary dictionary_comprehension
										       parenthesized_expression subscript)))))

;;;;; [[https://github.com/jscheid/dtrt-indent][dtrt-indent]]
;; automatically set the right indent for other people's files
(use-package dtrt-indent
  :hook (emacs-startup . dtrt-indent-mode)
  :blackout)

;;;;; outline settings
;; The built-in outline-minor-mode provides structured code folding in modes
;; such as Emacs Lisp and Python, allowing users to collapse and expand sections
;; based on headings or indentation levels. This feature enhances navigation and
;; improves the management of large files with hierarchical structures.
(use-package outline
  :ensure nil
  :commands outline-minor-mode
  :bind (("C-S-n" . outline-next-visible-heading)
         ("C-S-p" . outline-previous-visible-heading)
         ("C-n" . next-line)
         ("C-P" . previous-line))
  :hook  ((emacs-lisp-mode . outline-minor-mode)
          ;; Use " ▼" instead of the default ellipsis "..." for folded text to make
          ;; folds more visually distinctive and readable.
          (outline-minor-mode
           .
           (lambda()
             (let* ((display-table (or buffer-display-table (make-display-table)))
                    (face-offset (* (face-id 'shadow) (ash 1 22)))
                    (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
               (set-display-table-slot display-table 'selective-display value)
               (setq buffer-display-table display-table))))))

;; The outline-indent Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; In addition to code folding, *outline-indent* allows:
;; - Moving indented blocks up and down
;; - Indenting/unindenting to adjust indentation levels
;; - Inserting a new line with the same indentation level as the current line
;; - Move backward/forward to the indentation level of the current line
;; - and other features.
(use-package outline-indent
  :commands outline-indent-minor-mode
  :hook ((python-mode-hook . outline-indent-minor-mode)
         (python-ts-mode-hook . outline-indent-minor-mode)
         (yaml-mode-hook . outline-indent-minor-mode)
         (yaml-ts-mode-hook . outline-indent-minor-mode))
  :custom (outline-indent-ellipsis " ▼") )

;;;; paren management
;;;;; [[https://www.emacswiki.org/emacs/ShowParenMode][paren]]
;; built-in: show paren mode
(use-package paren
  :ensure nil
  :hook (emacs-startup . show-paren-mode)
  :custom ((show-paren-delay 0)
           (show-paren-highlight-openparen t)
           (show-paren-style 'parenthesis) ; parenthesis, expression, mixed
           (show-paren-when-point-in-periphery t)
           (show-paren-when-point-inside-paren t)
           (show-paren-context-when-offscreen 'child-frame)))

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/elec-pair.el][elec-pair]]
;; built-in: Automatic parenthesis pairing
(use-package elec-pair
  :disabled ;; using smartparens for now
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :config
  (electric-layout-mode t)
  (electric-indent-mode t))

;;;;; [[https://github.com/Fuco1/smartparens][smartparens]]
;; minor mode for dealing with pairs in Emacs
(use-package smartparens
  :init (which-key-add-key-based-replacements "C-q C-q" "Smartparens")
  :blackout t
  :bind-keymap ("C-q C-q" . smartparens-mode-map)
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))


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

;;;;; [[https://github.com/emacsmirror/emr][emr]]
;; a framework for providing language-specific refactoring in Emacs.
;; It includes refactoring commands for a variety of languages
(use-package emr
  ;; Just hit H-r to access your refactoring tools in any supported mode.
  :bind (:map sej-C-q-map
              ("r" . emr-show-refactor-menu)))

;;;;; [[https://github.com/erickgnavar/flymake-ruff][flymake-ruff]]
;; ruff linter
;; ("ruff" "check" "--quiet" "--stdin-filename=stdin" "-")
(use-package flymake-ruff
  :ensure-system-package ruff
  ;; :hook (python-base-mode . flymake-ruff-load)
  :hook (eglot-managed-mode-hook , flymake-ruff-load)
  )

;;;;; [[https://github.com/purcell/emacs-reformatter][reformatter]]
;; base reformatter
(use-package reformatter)

;;;;; [[https://github.com/scop/emacs-ruff-format?tab=readme-ov-file][ruff-format]]
;; ruff format using reformatter
(use-package ruff-format
  :ensure-system-package ruff
  :after reformatter)

;;;;; [[https://github.com/lassik/emacs-format-all-the-code][format-all]]
;; auto-format source code in many languages using the same command for all languages
;; You will need to install external programs to do the formatting
(use-package format-all
  :bind (:prefix-map format-all-map
		             :prefix "C-q f"
		             :prefix-docstring "format-all-menu"
		             ("b" . format-all-buffer)
		             ("r" . format-all-region)))

;;;; server tools
;;;;; [[https://github.com/emacs-tree-sitter/elisp-tree-sitter][tree-sitter]]
;; built-in: Emacs Lisp binding for tree-sitter, an incremental parsing library.
;; Important this is using the built-in Emacs version tree-sit.el
;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
(use-package treesit
  :ensure nil
  :config
  (add-to-list 'treesit-extra-load-path (emacs-path "tree-sitter/"))
  (add-to-list 'treesit-extra-load-path (emacs-path "var/tree-sitter/")))

(use-package treesit-auto
  ;; some auto features for tree-sit
  ;; [[https://github.com/renzmann/treesit-auto]]
  ;; EXPERIMENTAL: Changed from :demand t - can load when tree-sitter is needed (TREESIT-AUTO)
  :defer t
  :custom (treesit-auto-install 'prompt)
  :config
  (setq treesit-language-source-alist
	    '((markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
		               "split_parser" "tree-sitter-markdown/src"))
	      (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
			                  "split_parser" "tree-sitter-markdown-inline/src"))))

  ;; Set the maximum level of syntax highlighting for Tree-sitter modes
  (setq treesit-font-lock-level 4)

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
  :custom (combobulate-key-prefix "C-c o")
  :hook (prog-mode . combobulate-mode))

;;;;; [[https://github.com/joaotavora/eglot][eglot]]
;; built-in: simple client for Language Server Protocol servers
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
  :custom ((eglot-autoshutdown t)
		   (read-process-output-max (* 1024 1024))
		   (help-at-pt-display-when-idle t)
		   (completion-category-defaults nil)
		   (eglot-sync-connect 0)
		   (eglot-extend-to-xref t))
  :config
  ;; Eglot optimization
  ;; This reduces log clutter to improves performance.
  (setq jsonrpc-event-hook nil)
  ;; Reduce memory usage and avoid cluttering *EGLOT events* buffer
  (setq eglot-events-buffer-size 0)  ; Deprecated
  (setq eglot-events-buffer-config '(:size 0 :format short))

  (setq eglot-report-progress nil)  ; Prevent minibuffer spam

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
  (setq-default eglot-workspace-configuration
				'(:basedpyright (
								 :typeCheckingMode "standard"
								 )
								:basedpyright.analysis (
												        :diagnosticSeverityOverrides (
																	                  :reportUnusedCallResult "none"
																	                  )
												        :inlayHints (
													                 :callArgumentNames :json-false
													                 ))))
  (use-package consult-eglot
    :commands consult-eglot-symbols))

;;;; ssh tools
;;;;; [[https://www.gnu.org/software/tramp/][tramp]]
;; built-in: remote editing
(use-package tramp
  :ensure nil
  :defer t  ;; Changed from :demand t - only load when needed (saves 0.49s)
  :custom ((tramp-default-method "ssh") ; or scp
	       (tramp-terminal-type "dumb")
	       (tramp-verbose 10)
	       (tramp-completion-reread-directory-timeout nil)
	       (tramp-histfile-override "/tmp/tramp_history")
	       (tramp-auto-save-directory "~/.cache/emacs/backups")
	       (tramp-inhibit-errors-if-setting-file-attributes-fail t)
           ;; 		   (remote-file-name-inhibit-cache nil)
           ;; 		   (remote-file-name-inhibit-locks t)
	       (tramp-use-scp-direct-remote-copying t)
           (remote-file-name-inhibit-auto-save-visited t)
	       (tramp-copy-size-limit (* 1024 1024))
           ;; 		   (connection-local-set-profile-variables 'remote-direct-async-process
           ;; 												   '((tramp-direct-async-process . t)))
           ;; 		   (connection-local-set-profiles '(:application tramp :protocol "scp")
           ;; 										  'remote-direct-async-process)
           ;; 		   (magit-tramp-pipe-stty-settings 'pty)
           ;; 		   (tramp-default-remote-shell "/bin/bash")
	       (tramp-chunksize 500)
           ;; 		   (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
           ;; 									  vc-ignore-dir-regexp
           ;; 									  tramp-file-name-regexp))
	       ;; use the settings in ~/.ssh/config instead of Tramp's
	       ;; (tramp-use-ssh-controlmaster-options nil)
	       ;; don't generate backups for remote files opened as root (security hazard)
	       ;; (backup-enable-predicate (lambda (name)
	       ;; 							  (and (normal-backup-enable-predicate name)
	       ;; 								   (not (let ((method (file-remote-p name 'method)))
	       ;; 										  (when (stringp method)
	       ;; 											(member method '("su" "sudo")))))))))
	       )
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (add-to-list 'tramp-default-user-alist '("\\`localhost\\'" "\\`root\\'" "su"))
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compile-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))
  (remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
  (remove-hook 'find-file-hook 'forge-bug-reference-setup)
                                        ;(require 'tramp-sh)
  )

;;;;; [[https://github.com/jhgorrell/ssh-config-mode-el][ssh-config-mode]]
;; A mode to edit SSH config files.
;; add this to file for automatic usage: # -*- mode: ssh-config -*-
(use-package ssh-config-mode)

;;;; commenting tools
;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html][New-comment]]
;; built-in: library contains functions and variables for commenting and
;; uncommenting source code.
(use-package newcomment
  :ensure nil
  :bind (("H-;" . comment-box)
		 ("M-;" . comment-line))
  :custom ((comment-empty-lines t)
		   (comment-fill-column 0)
		   (comment-multi-line t)
		   (comment-style 'multi-line)))

;;;;; [[https://www.gnu.org/software/emacs/manual/html_mono/ediff.html][ediff]]
;; built-in: A saner diff
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :custom ((ediff-diff-options "-w")
	       (ediff-window-setup-function 'ediff-setup-windows-plain)
	       (ediff-split-window-function 'split-window-horizontally)
	       (ediff-shell (getenv "$SHELL"))))

;;;; compile and make tools
;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html][compile]]
;; built-in: Compilation Mode
(use-package compile
  :ensure nil
  :preface
  ;; ANSI Coloring
  (with-eval-after-load 'compile
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter nil t))
  :custom ((compilation-message-face 'compilation-base-face)
		   (compilation-always-kill t)
		   (compilation-ask-about-save nil)
		   (compilation-scroll-output 'first-error)) )

;;;;; [[http://doc.endlessparentheses.com/Fun/makefile-mode.html][make-mode]]
;; built-in: Major mode for editing standard Makefiles
(use-package make-mode
  :ensure nil
  :blackout ((makefile-automake-mode . "Makefile")
             (makefile-gmake-mode . "Makefile")
             (makefile-makepp-mode . "Makefile")
             (makefile-bsdmake-mode . "Makefile")
             (makefile-imake-mode . "Makefile")))

;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html][flymake]]
;; built-in: Emacs syntax checker
(use-package flymake
  :ensure nil
  :hook (emacs-startup . flymake-mode)
  :bind (("H-[" . flymake-goto-prev-error)
	     ("H-]" . flymake-goto-next-error)
	     ("H-\\" . flymake-show-buffer-diagnostics))
  :custom ((flymake-show-diagnostics-at-end-of-line 'short)
	       (flymake-fringe-indicator-position 'right-fringe)
	       (flymake-suppress-zero-counters t)
	       (flymake-start-on-flymake-mode t)
	       (flymake-no-changes-timeout .5)
	       (flymake-start-on-save-buffer t)
	       (flymake-proc-compilation-prevents-syntax-check t)
	       (flymake-wrap-around nil)))

;;;; vcs
;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html][Project]]
;; built-in: project management
(use-package project
  :ensure nil
  :custom (project-file-history-behavior 'relativize))

;;;;; [[https://magit.vc/][magit]]
;; interface to the version control system Git
(use-package magit
  :bind (("C-x g" . magit-status))
  :custom ((magit-log-section-commit-count 30)
	       (magit-revision-show-gravatars nil)
	       (magit-diff-refine-hunk t)
	       (magit-repository-directories nil)
	       (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
	       (magit-bury-buffer-function #'magit-restore-window-configuration))
  :config
  ;; gh-auth-switch, gh-auth-status, (adds P @ in Magit)
  (require 'sej-gh-auth-switch)
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  (if (fboundp 'transient-append-suffix)
      (progn
        ;; Add switch: --tags
        (transient-append-suffix 'magit-fetch
          "-p" '("-t" "Fetch all tags" ("-t" "--tags")))
        ;; Add gh auth switch to push menu
        (transient-append-suffix 'magit-push
          "p" '("@" "Switch @ GitHub account" gh-auth-switch))
		;; Add gh auth status to push menu
		(transient-append-suffix 'magit-push
		  "p" '("s" "GitHub account <s>tatus" gh-auth-status))))

  (defun my/magit-format-file-icons-when-small (file stat status &rest args)
    "Only draw nerd icons for FILE when STAT reports a small size.
Falls back to the default formatter for large (likely binary) files."
    (let ((file-size (and (listp stat) (file-attribute-size stat))))
      (if (and file-size (<= file-size (* 200 1024)))
          (apply #'magit-format-file-nerd-icons file stat status args)
        (apply #'magit-format-file-default file stat status args))))

  (setopt magit-format-file-function #'my/magit-format-file-icons-when-small))

;;;;; [[https://github.com/aurtzy/disproject][disproject]]
;; integration with project.el and allows for dispatching via transient menus
(use-package disproject
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map
          ("p" . disproject-dispatch))
  :config
  (require 'magit nil 'noerror)
  (require 'magit-todos nil 'noerror))

;;;;; [[https://github.com/magit/forge][forge]]
;; Access Git forges from Magit
;; To start using Forge in a certain repository visit the Magit status buffer
;; for that repository and type N f f (forge-pull). Alternatively you can use M-x
;; forge-add-repostiory, which makes it possible to add a forge repository without
;; pulling all topics and even without having to clone the respective Git repository.
(use-package forge
  :after magit)

;;;;; [[https://github.com/emacsorphanage/git-gutter-fringe][git-gutter-fringe]]
;; git fringe
(use-package git-gutter-fringe
  :blackout t
  :bind (:map global-map
	          ("C-H-]" . git-gutter:next-hunk)
	          ("C-H-[" . git-gutter:previous-hunk))
  :hook (emacs-startup . global-git-gutter-mode)
  :init (setq git-gutter:lighter "")
  :config
  (advice-add 'git-gutter:next-hunk :after #'(lambda () (recenter-top-bottom nil))))

;;;;; [[https://github.com/alphapapa/magit-todo][magit-todos]]
;; Show tasks from commit files
(use-package magit-todos
  :after magit
  :commands (disproject-dispatch magit-status-mode)
  :hook (magit-status-mode . magit-todos-mode)
  :config
  (magit-todos-mode 1)
  (setq magit-todos-recursive t
        magit-todos-depth 100)
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIX" ))
   '(magit-todos-ignore-file-suffixes '("todo"))
   '(magit-todos-exclude-globs '("*.map" "*.html"))))

;;;;; [[https://github.com/emacsmirror/git-timemachine][ git-timemachine]]
;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

;;;;; [[http://web.mit.edu/Emacs/source/emacs/lisp/smerge-mode.el][smerge-mode]]
;; built-in: Resolve diff3 conflicts
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

;;;;; [[https://github.com/KarimAziev/igist][gist]]
;; gist client
;; built-in self help
(use-package igist
  :bind (:map sej-C-q-map
              ("G" . igist-dispatch)))

;;;;; [[https://github.com/magit/git-modes][git-modes]]
;; Emacs major modes for various Git configuration files.
;; gitattributes-mode , gitconfig-mode , gitignore-mode
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
  :hook (((emacs-lisp-mode lisp-mode) . flymake-mode)
         ((emacs-lisp-mode lisp-mode) . sej/lisp-on-save-funcs))
  :bind ((:map global-map
	           ("C-q C-e" . toggle-debug-on-error))
	     (:map emacs-lisp-mode-map
	           ("C-x C-e" . sej/eval-dwim)
	           ("C-<enter>" . sej/eval-dwim) ; translates to C-H-<return>
	           ("H-<return>" . eval-buffer)))
  :custom (parens-require-spaces t)
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
  ;; enable dash for Emacs lisp highlighting
  (eval-after-load "dash" '(dash-enable-font-lock)))

;;;;; [[https://www.emacswiki.org/emacs/ElDoc][eldoc]]
;; built-in: show the signature of the function at point in the minibuffer
(use-package eldoc
  :blackout t
  :ensure nil
  :preface
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc" display-buffer-at-bottom
                 (window-height . 4)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :hook ((emacs-lisp-mode-hook . eldoc-mode)
         (lisp-interaction-mode-hook . eldoc-mode)
         (ielm-mode-hook . eldoc-mode))
  :custom ((eldoc-idle-delay 0.2)
	       (eldoc-echo-area-use-multiline-p 3)
	       (eldoc-echo-area-display-truncation-message nil))
  :config
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

;;;;; [[https://github.com/purcell/elisp-slime-nav][elisp-slime-nav]]
;; turn on elisp-slime-nav
;; M-. works to jump to function definitions
;; M-, to jump back
(use-package elisp-slime-nav
  :blackout t
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

;;;;; [[https://github.com/Wilfred/elisp-refs][elisp-refs]]
;; Provides functions to find references to functions, macros, variables,
;; special forms, and symbols in Emacs Lisp
(use-package elisp-refs
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

;;;;; [[https://github.com/xiongtx/eros][eros]]
;; eros-mode will show you the result of evaluating an elisp command
;; as an overlay in your elisp buffer. Try it out with C-x C-e or s-<return>
(use-package eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode))

;;;;; [[https://www.emacswiki.org/emacs/InferiorEmacsLispMode][ielm]]
;; built-in: add a nice popup for ielm
(use-package ielm
  :ensure nil
  :commands ielm
  :bind (("s-i" . sej/ielm-other-window)
         :map sej-C-q-map
         ("i" . sej/ielm-other-window))
  :hook ((ielm-mode . show-paren-mode)
	     (ielm-mode . electric-pair-mode))
  :config
  (defun sej/ielm-other-window ()
    "Run ielm on other window."
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*ielm*"))
    (call-interactively 'ielm)))


;;;; python
;;;;; [[http://wikemacs.org/wiki/Python][python]]
;; Install:
;; pip3 install -U setuptools
;; brew install pyright
;; optional install basedpyright
;; YAPF or Black
;; (require 'python)
(setq python-indent-guess-ident-offset-verbose nil
      python-indent-offset 4
      comment-inline-offset 2)

;; Python should use spaces, not tabs (PEP 8)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))

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

;;;;; [[http://doc.endlessparentheses.com/Fun/inferior-python-mode.html][inferior-python-mode]]
;; built-in: runs a python interpreter as a subprocess of Emacs
(use-package inferior-pyton-mode
  :ensure nil  )

;;;;; [[https://github.com/pwalsh/pipenv.el][pipenv]]
;; integration with pipenv
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

;;;;; [[https://github.com/pythonic-emacs/pyenv-mode][pyenv-mode]]
;; integration with the pyenv tool
(use-package pyenv-mode
  :hook (python-base-mode . pyenv-mode))

;;;;; [[https://github.com/emacsattic/pyenv-mode-auto][pyenv-mode-auto]]
;; pyenv automatically based on .python-mode
(use-package pyenv-mode-auto
  :vc (:url "https://github.com/emacsattic/pyenv-mode-auto"
            :rev :newest
            :branch "master"))

;;;;; [[https://github.com/wbolster/emacs-python-pytest][python-pytest]]
(use-package python-pytest
  :bind (:map python-base-mode-map
	          ("C-c t" . python-pytest-dispatch)))

;;;;; [[https://github.com/purcell/envrc][envrc]]
;; A GNU Emacs library which uses the direnv tool to determine per-directory/project
;; environment variables and then set those environment variables on a per-buffer basis.
(use-package envrc
  :defer 3  ;; Defer to reduce startup direnv calls - will still activate when editing files
  :config
  (envrc-global-mode))

;;;;; [[https://github.com/radian-software/apheleia][apheleia]]
;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :ensure t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

;;;;; [[https://github.com/donkirkby/live-py-plugin][live-py-mode]]
;; Live Coding in Python
;; Open any Python file, and activate live-py-mode with M-x live-py-mode.
;; You should see an extra window on the right that shows the results of
;; running your code.
(use-package live-py-mode)

;;;;; [[https://github.com/millejoh/emacs-ipython-notebook][ein]]
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
(use-package ein
  :blackout t)

;;;;; [[https://github.com/Wilfred/pip-requirements.el][pip-requirements]]
;; major mode for editing pip requirement files
(use-package pip-requirements)


;;;; web modes
;;;;; [[https://github.com/fxbois/web-mode][web-mode]]
;; Major mode for editing web templates
;; http://web-mode.org/

(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :custom ((web-mode-markup-indent-offset 2)
	       (web-mode-css-indent-offset 2)
	       (web-mode-code-indent-offset 2)))

;;;;; [[https://github.com/zenozeng/css-eldoc][css-eldoc]]
;; eldoc-mode plugin for CSS
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;;;;; [[https://github.com/joshwnj/json-mode][json-mode]]
;; Major mode for editing JSON files.
;; Extends the builtin js-mode to add better syntax highlighting for JSON
;; and some nice editing keybindings.
(use-package json-mode
  :mode "\\.json\\'")

;;;;; [[https://github.com/Sterlingg/json-snatcher][json-snatcher]]
;; pull path to value in large JSON
(use-package json-snatcher
  :commands jsons-print-path
  :after json-mode)

;;;;; [[https://github.com/mooz/js2-mode][js2-mode]]
;; Improved JavaScript editing mode
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

;;;;; [[https://github.com/magnars/js2-refactor.el][js2-refactor]]
;; JavaScript refactoring library for Emacs
(use-package js2-refactor
  :after js2-mode
  :blackout t
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;;;;; [[https://github.com/yasuyk/web-beautify][web-beautify]]
;; Format HTML, CSS and JavaScript/JSON by js-beautify
;; Insta;; npm -g install js-beautify
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

;;;;; [[https://github.com/nex3/haml-mode][haml-mode]]
;; major mode for the haml mark-up language
(use-package haml-mode)

;;;;; [[https://github.com/emacs-php/php-mode][php-mode]]
;; major mode for editing PHP code
(use-package php-mode
  :mode (("\\.module$" . php-mode)
         ("\\.inc$" . php-mode)
         ("\\.install$" . php-mode)
         ("\\.engine$" . php-mode)
         ("\\.\\(?:php\\|phtml\\)\\'" . php-mode)))

;;;;; [[https://www.emacswiki.org/emacs/YamlMode][yaml-mode]]
;; YAML major mode support
(use-package yaml-mode
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/nxml-mode/Introduction.html][nxml-mode]]
;; built-in: major mode for editing XML
(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))


;;;; c program modes
;;;;; [[https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html][c-mode]]
;; built-in: C/C++ Mode
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
  (setq c-ts-mode-indent-style 'gnu))

;;;;; [[https://github.com/MaskRay/ccls/wiki/lsp-mode][ccls]]
;; c++-mode, objc-mode, cuda-mode
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

;;;;; [[https://codeberg.org/akib/emacs-eglot-ccls][eglot-ccls]]
;; specific extensions for Eglot
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

;;;;; [[https://github.com/sonatard/clang-format][clang-format]]
;; Clang-format emacs integration for use with C/Objective-C/C++.
(use-package clang-format
  :bind (:map c-mode-base-map
              ("C-c v" . clang-format-region)
              ("C-c u" . clang-format-buffer))
  :config
  (setq clang-format-style-option "llvm"))

;;;;; [[https://github.com/ludwigpacifici/modern-cpp-font-lock][modern-cpp-font-lock]]
;; Syntax highlighting support for "Modern C++" - until C++20 and Technical Specification
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;;;;; [[https://github.com/josteink/csharp-mode][csharp-mode]]
;; mode for editing C# in emacs. It’s based on cc-mode
(use-package csharp-mode)

;;;;; [[https://github.com/stardiviner/arduino-mode/tree/16955f579c5caca223c0ba825075e3573dcf2288][arduino-mode]]
;; mode for .ino files only
(use-package arduino-mode
  :hook (arduino-mode . arduino-cli-mode)
  :ensure-system-package arduino-cli
  :config
  (setq arduino-mode-home "~/src/Arduino"
        arduino-executable (expand-file-name "Arduino" "~/Applications/Arduino/Contents/MacOS/")))

;;(remove-hook 'c++mode-hook #'arduino-mode-cli)

;;;;; [[https://github.com/motform/arduino-cli-mode][arduino-cli-mode]]
;; minor mode for using the excellent new arduino command line interface
(use-package arduino-cli-mode
  :hook (c++-mode . arduino-cli-mode)
  :ensure-system-package arduino-cli
  :config
  (setq arduino-cli-warnings 'all
        arduino-cli-verify t
        arduino-cli-default-fqbn "esp8266:esp8266:d1"
        arduino-cli-default-port "/dev/cu.wchusbserial1430"))

;;;;; [[https://github.com/ZachMassia/platformio-mode][platformio-mode]]
;; minor mode which allows quick building and uploading of PlatformIO projects
;;   with a few short key sequences.
;; Code completion can be provided by installing any package compatible with .clang_complete files,
;;   such as irony-mode.
;; To keep the index up to date, run platformio-init-update-workspace (C-c i i)
;;   after installing any libraries.
(use-package platformio-mode
  :hook ((c-mode c++-mode objc-mode cuda-mode c-or-c++-mode
                 c-ts-mode c++-ts-mode c-or-c++-ts-mode) . platformio-conditionally-enable))

;;;;; [[https://github.com/rust-lang/rust-mode][rust-mode]]
;; rust language package
(use-package rust-mode
  :config (setq rust-format-on-save t))

;;;;; [[https://github.com/dominikh/go-mode.el][go-mode]]
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
;;;;; [[https://www.emacswiki.org/emacs/csv-mode.el][csv-mode]]
;; major mode for csv
;;
(use-package csv-mode
  :commands (csv-mode
             csv-align-mode)
  :hook (csv-mode . csv-align-mode)
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;;;;; [[https://ess.r-project.org/][ESS(Emacs Speaks Statistics) R-project]]
;; ESS configurationEmacs Speaks Statistics
;; Used for R, S-Plus, SAS, Stata and OpenBUGS/JAGS.
(use-package ess)

;;;;; [[https://github.com/tequilasunset/apples-mode][apples-mode for apple-script]]
;; for editing apple-script
(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'")


;;; files
;;;;; [[https://www.emacswiki.org/emacs/IbufferMode][ibuffer]]
;; built-in: operate on buffers much in the same manner as Dired.

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

;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html][registers]]
;; :built-in: Registers allow you to jump to a file or other location quickly.
;; (i for init.el, r for this file) to jump to it.
;; below are basic register settings, more are in register-alist which is in save-hist
(set-register ?t '(file . "~/Documents/orgtodo/"))
(set-register ?d '(file . "~/dotfiles/"))
(set-register ?e '(file . "~/.emacs.d/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))
(add-to-list 'savehist-additional-variables 'register-alist)

;;;;; [[https://github.com/emacs-dashboard/emacs-dashboard][dashboard]]
;; all-in-one start-up screen with current files / projects
(use-package dashboard
  :if (eq sej-dashboard t)
  :commands sej/open-dashboard
  :hook (emacs-startup . sej/open-dashboard-only)
  :bind (("<f6>" . sej/open-dashboard)
         :map sej-C-q-map
         ("d" . sej/open-dashboard))
  :custom ((dashboard-startup-banner (emacs-path "emacs.png"))
	       (dashboard-set-init-info t)
	       (dashboard-projects-backend 'project-el) ; alt option: projectile
	       (dashboard-items '((recents  . 15)
			                  (bookmarks . 15)
			                  (projects . 10)
			                  (registers . 10)))
	       (dashboard-set-heading-icons t)
	       (dashboard-set-file-icons t))
  :config
  (dashboard-setup-startup-hook)

  (defun sej/open-dashboard-only ()
    "Move to the dashboard package buffer and make only window in frame."
    (interactive)
    (dashboard-insert-startupify-lists)
    (dashboard-initialize)
    (switch-to-buffer "*dashboard*")
    (hl-line-mode t)
    (delete-other-windows))

  (defun sej/open-dashboard ()
    "Move to the dashboard package buffer."
    (interactive)
    (switch-to-buffer "*dashboard*")
    (hl-line-mode t)) )

;;;;; [[https://github.com/purcell/page-break-lines][page-break-lines]]
;; display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :blackout t
  :hook ((dashboard-mode
          text-mode
          comint-mode
          helpful-mode
          help-mode
          compilation-mode
          emacs-news-view-mode) . page-break-lines-mode))

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/autoinsert.el][autoinsert]]
;; automagically inserts text into new buffers
;;   based on file extension or the major mode
(use-package autoinsert
  :commands (auto-insert)
  :hook (emacs-startup . auto-insert-mode)
  :preface
  (defun sej/autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas/expand-snippet (buffer-string) (point-min) (point-max)))
  :custom ((auto-insert-directory "~/.emacs.d/templates/")
	       (auto-insert 'other)
	       (auto-insert-directory (concat no-littering-etc-directory "autoinsert/"))))

;;;;; [[https://github.com/emacs-mirror/emacs/blob/master/lisp/autorevert.el][autorevert]]
;; built-in: watch for changes in files on disk
(use-package autorevert
  :hook ((emacs-startup . global-auto-revert-mode)
	     (dired-mode . auto-revert-mode))
  :custom ((auto-revert-use-notify t)
		   (auto-revert-stop-on-user-input nil)
           (auto-revert-avoid-polling t)
           (auto-revert-verbose t)
           (global-auto-revert-non-file-buffers t)
           (revert-without-query '(".*"))))

;;;;; [[https://github.com/nflath/sudo-edit][sudo-edit]]
;; Open files as sudo
(unless sys/win32p
  (use-package sudo-edit))

;;; dired
;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html][dired]]
;; built-in: Directory operations
(use-package dired
  :ensure nil
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("C-u D" . sej/dired-do-delete-skip-trash)
			  ("z" . sej/dired-get-size))
  :custom ((dired-recursive-deletes 'top) ;; Always delete and copy recursively
		   (dired-recursive-copies 'always)
		   (dired-listing-switches "-AFGhlv --group-directories-first")   ;; Show directory first
		   (dired-dwim-target t)
		   (dired-auto-revert-buffer #'dired-directory-changed-p)
		   (auto-revert-remote-files nil)
		   (dired-free-space nil)
		   (dired-mouse-drag-files t)
		   (dired-isearch-filenames 'dwim)
		   (dired-create-destination-dirs 'ask)
		   (dired-deletion-confirmer 'y-or-n-p)
		   (dired-filter-verbose nil)
		   (dired-clean-confirm-killing-deleted-buffers nil)
		   (dired-omit-verbose nil)
		   (dired-omit-files (concat "\\`[.]\\'"))
		   (dired-vc-rename-file t))
  :config
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

;;;;; [[https://github.com/emacs-lsp/lsp-mode][ls-lisp]]
(use-package ls-lisp
  :ensure nil
  :custom ((ls-lisp-use-insert-directory-program nil)
	       (ls-lisp-ignore-case 't)
	       (ls-lisp-use-string-collate nil)))

;;;;; [[https://github.com/jwiegley/emacs-release/blob/master/lisp/dired-aux.el][dired-aux]]
;; built-in: auxiliary functionality of dired
(use-package dired-aux
  :ensure nil
  :commands (dired-diff
	         dirred-backup-diff
	         dired-compare-directories
	         dired-do-chmod
	         dired-do-chgrp
	         dired-do-chown
	         dired-do-print
	         dired-do-shell-command
	         dired-compress))

;;;;; [[https://www.gnu.org/software/emacs/manual/html_node/dired-x/][dired-x]]
;; built-in: Extra Dired functionality
(use-package dired-x
  :ensure nil
  :commands (dired-x-find-file
	         dired-x-find-file-other-window)
  :custom (dired-x-hands-off-my-keys nil))

;;;;; [[https://github.com/purcell/diredfl][diredfl]]
;; Extra font-lock rules for a more Colourful dired
(use-package diredfl)

;;;;; [[https://github.com/Fuco1/dired-hacks][dired-subtree]]
;; The dired-subtree package provides commands to quickly view the contents of a folder with the TAB key.
(use-package dired-subtree
  :after dired ; Load after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("C-u TAB" . dired-subtree-cycle) ; Cycle expansion levels
              ("M-j" . dired-subtree-next-sibling)
              ("M-k" . dired-subtree-previous-sibling)))

;;;;; [[https://github.com/xenodium/dwim-shell-command#my-toolbox][dwim-shell-command]]
;; define functions that apply command-line utilities to current buffer or dired files
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

;;;;; [[https://github.com/myuhe/quick-preview.el][quick-preview]]
;; Quick-preview provides a nice preview of the thing at point for files.
(use-package quick-preview
  :bind ( :map sej-C-q-map
          ("q" . quick-preview-at-point)
          :map dired-mode-map
          ("Q" . quick-preview-at-point)))

;;;;; [[https://github.com/rmuslimov/browse-at-remote][browse-at-remote]]
;; browse file at remote source
(use-package browse-at-remote
  :bind (:map sej-C-q-map
              ("B" . browse-at-remote)))

;;; writing & reading

;;;;; [[https://github.com/jorgenschaefer/typoel][typo-mode]]
;; [[https://github.com/jorgenschaefer/typoel][typo-mode]] is a buffer-specific minor mode that will change a number of normal keys to make them insert
;; typographically useful unicode characters. Some of those keys can be used repeatedly to cycle through
;; variations. This includes in particular quotation marks and dashes.
(use-package typoel
  :diminish
  :vc (:url "https://github.com/jorgenschaefer/typoel"
            :rev :newest
            :branch "master")
  :hook ((org-mode markdown-mode gnus-message-setup) . typo-mode)
  :config (typo-global-mode 1))

;;;;; [[https://protesilaos.com/emacs/denote#h:d99de1fb-b1b7-4a74-8667-575636a4d6a4][denote]]
;; note organization tools
;; https://github.com/protesilaos/denote?tab=readme-ov-file
;;
;; populate denote main directory `~/Documents/orgtodo' in my case, with `.gitignore'
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
	          ("D" . sej/denote-create-any-dir)
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
	          ("C-d f" . denote-dired-rename-marked-files-using-front-matter)
	          :map search-map
	          ("d" . sej/denote-consult-search)
	          :map isearch-mode-map
	          ("M-s d" . sej/denote-consult-search))
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

  :custom ((denote-directory sej-org-directory)
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
	          (journal . ,(concat "#+category: journal" "\n\n" "* Notes\n"  "\n ") )))

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
						                            denote-component-history ))))
  :config
  ;; sej Denote functions
  ;;  sej/denote-dired-mode-hook
  ;;  sej/denote-keywords-update
  ;;  sej/denote-keywords-edit
  ;;  sej/denote-keywords-dump
  ;;  sej/denote-colleagues-edit
  ;;  sej/denote-colleagues-update
  ;;  sej/denote-colleagues-prompt
  ;;  sej/denote-colleagues-update-file
  ;;  sej/denote-colleagues-get-file
  ;;  sej/denote-colleagues-new-meeting
  ;;  sej/filter-journal-lines-from-list
  ;;  sej/denote-colleagues-dump
  ;;  sej/denote-create-any-dir
  ;;  sej/denote-always-rename-on-save-based-on-front-matter
  ;;  sej/denote-consult-search
  (require 'sej-denote)

  ;; update 1st time in config
  (sej/denote-keywords-update))

;;;;; [[https://github.com/protesilaos/denote-journal][denote-journal]]
(use-package denote-journal
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind (:map sej-denote-map
	          ("j" . denote-journal-new-or-existing-entry)
	          ("J" . denote-journal-link-or-create-entry)
	          ("C-j" . sej/journelly-open))
  :custom ((denote-journal-directory (expand-file-name "journal" denote-directory))
	       (denote-journal-title-format "%y-%m"))
  :config
  ;; (denote-journal--entry-today)
  (advice-add 'denote-journal--entry-today :override #'sej/denote-journal--entry-today))

;;;;; [[https://github.com/protesilaos/denote-silo][denote-silo]]
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

  (advice-add 'denote-silo-select-silo-then-command :after #'sej/denote-silo-update)
  (advice-add 'denote-silo-open-or-create :after #'sej/denote-silo-update)
  (advice-add 'denote-silo-create-note :after #'sej/denote-silo-update)
  (add-hook 'post-command-hook 'sej/switch-buffer-functions-run)
  (add-hook 'sej/switch-buffer-functions #'sej/denote-check-for-denote-buffer-switch))

;;;;; denote-refs
;; puts links and back-links after header in denote files
;; put below in .dir-locals.el in denote-directory
;; ((org-mode . ((eval . (denote-refs-mode)))))
;; FIX DEBUG PROBLEM seems to be causing org-parse lock-ups
(use-package denote-refs
  :disabled t
  :vc (:url "https://codeberg.org/akib/emacs-denote-refs.git"
            :rev :newest
            :branch "master")
  :hook (org-mode . denote-refs-mode)
  :custom (denote-refs-update-delay '(2 1 60))) ; needed to allow time for buffer set-up

;;;;; denote-menu
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
  :custom (( denote-menu-title-column-width 45)
	       (denote-menu-keywords-column-width 35)))

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
  :custom ((markdown-command "multimarkdown")
	       (markdown-enable-wiki-links t)
	       (markdown-italic-underscore t)
	       (markdown-fontify-code-blocks-natively t)
	       (markdown-make-gfm-checkboxes-buttons t)
	       (markdown-gfm-additional-languages '("sh"))
	       ;; (markdown-command "pandoc -f markdown -t html")
	       (markdown-header-scaling t)))

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
(use-package markdown-toc)

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
  :vc (:url "https://github.com/jasonm23/markdown-soma"
	        :rev :newest
	        :branch "master")
  :commands markdown-soma-mode
  ;; :hook (markdown-mode . markdown-soma-mode)
  :bind (:map markdown-mode-command-map
              ("p" . markdown-soma-mode)
              ("r" . markdown-soma-restart)
              ("+" . sej/markdown-soma-mod-plus)
              ("=" . sej/markdown-soma-mod-plus)
              ("-" . sej/markdown-soma-mod-minus) )
  :config
  ;; add to path
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

  (setq markdown-soma-custom-css
	    (markdown-soma--css-pathname-from-builtin-name "markdown-soma-dark")) ;; select css theme
  (setq markdown-soma-highlightjs-theme "github-dark") ;; Change "theme name" to the selected highlightjs theme.
  (setq	sej/markdown-soma-mod-var 0)

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
    :bind (("s-:" . jinx-correct-all)
	       ("C-;" . jinx-correct-nearest)
           ("C-M-;" . jinx-correct-all))
    :custom	(jinx-languages "en_US en_GB en_CA")
    :config
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
  :custom (pdf-tools-handle-upgrades nil)
  :config
  (pdf-tools-install)
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
  :config (pdfgrep-mode))

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
  :custom ((annotate-file (emacs-path "annotations"))
	       (annotate-annotation-column 73)
	       (annotate-diff-export-context 5)
	       (annotate-use-messages nil)
	       (annotate-integrate-marker "")
	       (annotate-integrate-higlight ?^)
	       (annotate-fallback-comment "#")
	       (annotate-blacklist-major-mode '())
	       (annotate-annotation-max-size-not-place-new-line 50)
	       (annotate-search-region-lines-delta 4)
	       (annotate-annotation-position-policy :by-length)
	       (annotate-summary-ask-query nil))
  :config
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
;;;;; [[https://www.gnu.org/software/auctex/download-for-macosx.html][AuCTeX]]
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
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config (auctex-latexmk-setup))

;;;;; [[https://lucidmanager.org/productivity/emacs-bibtex-mode/][bibtex]]
;; built-in: bibliography editing
(use-package bibtex
  :ensure nil
  :custom (bibtex-dialect 'biblatex))

;;;;; [[https://github.com/cpitclaudel/biblio.el][biblio]]
;; An extensible Emacs package for browsing and fetching references
(use-package biblio)

;;;;; [[https://www.gnu.org/software/auctex/manual/reftex.html][reftex]]
;; RefTeX is a package for managing Labels, References, Citations and index entries with GNU Emacs.
(use-package reftex
  :custom (reftex-plug-into-AUCTeX t))

;;;;; [[https://github.com/cdominik/cdlatex][cdlatex]]
;; fast insertion of environment templates and math stuff in LaTeX
(use-package cdlatex)

;;;; org
;;;;; org
;; built-in: mode for keeping notes, maintaining lists, planning
;; <2024-10-19 Sat> mods to mix with denote system
(use-package org
  :ensure nil
  :commands (org-agenda sej/org-agenda-call consult-org-agenda)
  :mode ("\\.org$" . org-mode)
  :hook ( (org-mode . visual-line-mode)
          ;;(org-mode . org-num-mode) ; TRY remove for now ; TEST for a while
          (org-mode . variable-pitch-mode))
  :bind ((:map global-map
               ("C-c l" . org-insert-link)
	           ("C-c S" . org-store-link)
               ("C-c C-H-u" . sej/org-remove-link))
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
  :custom
  ;; set headline numbering face to something more subtle
  (org-num-face 'org-modern-date-inactive)

  ;; archive location setting
  ;; based on year of archive, under heading of file it comes from
  ;; in denote format eg. 20250101T00000000--ARCHIVE.org for 2025
  (org-archive-location (concat org-directory (format-time-string "/%Y") "0101T000000--archive.org::* %s"))

  (org-ellipsis "⤵")
  (org-directory sej-org-directory)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-adapt-indentation nil)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-ctrl-k-protect-subtree t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-return-follows-link t)
  (org-fontify-done-headline t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-capture-bookmark t)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-log-done 'note)
  (org-log-into-drawer t)
  ;; ! - timestamp , @ - note
  ;; use C-u in front to add note
  ;; C-c C-z to add note
  (org-todo-keywords '((sequence "TODO(t!)" "INPROCESS(i@/!)" "WAIT(w@/!)" "DEFER(r@/!)" "|" "DONE(d!)")
		               (sequence "MAYBE(m@/!)" "|" "DONE(d!)")
                       (sequence "DELIGATE(D@/!)" "CHECK(c)" "|" "VERIFIED(v!)")
		               (sequence "FIX(f@/!)" "INPROCESS(i@/!)" "|" "FIXED(F!)")
		               (sequence "MAINT" "|" "MAINTd(M!)")
		               (sequence "|" "CANCELED(x!)")))

  ;; `list-colors-display' for a buffer of colour names
  (org-todo-keyword-faces '(
			                ("DEFER" . (:foreground "pink"))
			                ("TODO" . (:foreground "#cc9393"))
                            ("CANCELED" . (:foreground "grey"))
                            ("CHECK" . (:foreground "yellow"))
                            ("DELIGATE" . (:foreground "blue"))
                            ("DONE" . (:foreground "green"))
                            ("FIX" . (:foreground "red"))
                            ("FIXED" . (:foreground "forest green"))
                            ("INPROCESS" . (:foreground "yellow"))
                            ("MAINT" . (:foreground "turquoise"))
                            ("MAYBE" . (:foreground  "#d1bf8f"))
                            ("VERIFIED" . (:foreground "green"))
                            ("WAIT" . (:foreground "#2c5353"))
			                ))
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-tags-column -115)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-highlight-latex-and-related '(latex))
  (org-clock-sound t)
  (org-id-method 'ts)
  (org-refile-targets '((nil :maxlevel . 9)
			            (org-agenda-files :maxlevel . 5)
			            (org-buffer-list :maxlevel . 2)))

;;;;;; tags
  ;; defined here for regular topics
  ;; `org-tag-persistent-alist' defined in denote section from current note list
  (org-tag-alist '(("meeting")
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

;;;;;; org-requires
  :config
  ;; get denote up and going
  (require 'denote)
  (require 'denote-journal)
  (require 'denote-silo)

  ;; reading man pages in org-mode
  (require 'ol-man)

  ;; personal sej org functions
  ;;  sej/org-timer-done-alert
  ;;  sej/get-open-org-file
  ;;  sej/org-reformat-buffer
  ;;  sej/org-remove-link
  ;;  sej/org-fold-hide-drawer-toggle
  (require 'sej-org)

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
     `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through nil :italic t :foreground "#71696A"))))

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
   'append)) ; end of use-pacakge for org

;;;;; org-agenda
;; built-in: agenda for todo & calendar items
;; [[https://orgmode.org/manual/Agenda-Views.html][org-agenda manual]]
(use-package org-agenda
  :ensure nil
  :after org
  :commands (org-agenda sej/org-agenda-call consult-org-agenda)
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
  :custom ((org-agenda-block-separator nil)
           (org-agenda-diary-file (concat org-directory "/diary.org"))
	       ;; Set org-agenda-files in :config after denote-journal-directory is defined
           (org-agenda-dim-blocked-tasks t) ; other option is 'invisible
           (org-agenda-inhibit-startup nil)
           (org-agenda-show-all-dates t)
           (org-agenda-skip-scheduled-if-done t)
           (org-agenda-start-on-weekday 1) ; Monday
	       (org-agenda-log-mode-items '(closed clock))
           (org-agenda-start-with-log-mode t)
           (org-agenda-use-time-grid t)
           (org-agenda-include-diary t)
	       (org-agenda-tags-column -115) ; still not ideal but works for half screen
           (org-agenda-window-setup (quote current-window)) ;open agenda in current window
           (org-agenda-span-name (quote fortnight)) ;show me tasks scheduled or due in next fortnight
           (org-agenda-skip-scheduled-if-deadline-is-shown t) ;don't show tasks as scheduled if shown as deadline
           (org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
           (org-deadline-warning-days 7) ;warn me of any deadlines in next 7 days
	       (org-agenda-sticky nil)) ; FIX change later to t
  :config
  ;; get denote up and running
  (require 'denote)
  (require 'denote-journal)
  (load-library "org-element.el")
  ;; Set org-agenda-files after denote-journal-directory is defined
  (setq org-agenda-files `(,org-directory ,denote-journal-directory))

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

  ;; display repeaters for dates, scheduled, deadlines
  ;; [[https://whhone.com/posts/org-agenda-repeated-tasks/]]

  ;; Add `sej/org-agenda-repeater' to the agenda prefix.
  (defun sej/org-agenda-repeater ()
    "The repeater shown in org-agenda-prefix for agenda."
    (if (org-before-first-heading-p)
	    "┄┄┄┄┄"  ; fill the time grid
      (let ((rpt (org-get-repeat)))
	    (if rpt rpt ""))))

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
	    `((agenda . "%i %-10c%?t%?-4e%?s%(sej/org-agenda-repeater)")
	      (todo .    "%i %-10c %?-4e")
	      (tags .    "%i %-10c")
	      (search .  "%i %-10c")))

  (defun sej/org-agenda-call (&optional arg)
    "Call org-agenda screen ARG, but default to `A'."
    (interactive)
    (if arg
	    (org-agenda nil arg)
      (org-agenda nil "A")))

  (defun sej/beginning-of-buffer-point (&optional &arg &arg)
    "Move point to top of agenda buffer after generate. Dummy to filter extra args."
    (goto-char (point-min)))
  (advice-add 'org-agenda :after #'sej/beginning-of-buffer-point)

  (defun sej/beginning-of-buffer-view (&optional &arg)
    "Move view to top of agenda buffer after regen. Dummy to filter extra args."
    (goto-char (point-min)))
  (advice-add 'org-agenda-redo-all :after #'sej/beginning-of-buffer-view)

  )  ; end of org-agenda

;;;;; [[https://github.com/awth13/org-appear][org-appear]]
;; make invisible parts of org-mode visible
(use-package org-appear
  :hook (org-mode . org-appear-mode))

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

  (add-hook 'org-attach-after-change-hook #'sej/org-attach-save-file-list-to-property)
  :config
  ;;;;;; [[https://github.com/bzg/org-mode/blob/main/lisp/org-attach-git.el][org-attach-git]]
  ;; An extension to org-attach.  If `org-attach-id-dir' is initialized
  ;; as a Git repository, then `org-attach-git' will automatically commit
  ;; changes when it sees them.  Requires git-annex.
  (require 'org-attach-git))


;;;;; [[https://github.com/calvinwyoung/org-autolist][org-autolist]]
;; make return and delete smarter in org lists
(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode))

;;;;; [[https://github.com/alphapapa/org-bookmark-heading][org-bookmark-heading]]
;; allows headings in org files to be bookmarked and jumped to with standard bookmark commands
(use-package org-bookmark-heading
  :defer t  ;; Changed from :demand t - only load when needed (saves 1.68s!)
  :after org
  :custom (org-bookmark-heading-jump-indirect t))

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
	     (dired-mode . org-download-enable))
  :custom ((org-download-method 'directory)
	       (org-download-image-dir "images")
	       (org-download-heading-lvl nil)
	       (org-download-timestamp "%Y%m%d-%H%M%S_")
	       (org-image-actual-width 300)
	       (org-download-screenshot-method "/usr/local/bin/pngpaste %s"))
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download))

;;;;; org-fragtog
;; LaTeX previews
;; https://github.com/io12/org-fragtog
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  :custom ((org-startup-with-latex-preview nil)
	       (org-format-latex-options
	        (plist-put org-format-latex-options :scale 2)
	        (plist-put org-format-latex-options :foreground 'auto)
	        (plist-put org-format-latex-options :background 'auto))))

;;;;; org-modern
;; https://github.com/minad/org-modern
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
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
  :custom ((org-modern-star 'fold)
	       (org-modern-hide-stars nil)
	       (org-modern-timestamp t)
	       (org-modern-table t)
	       (org-modern-keyword t)
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
	       (org-modern-tag-faces '(("emacs" :foreground "#8b89c5") ("Emacs" :foreground "#8b89c5")
				                   ("org" :foreground "cyan") ("deck" :foreground "brown")))
	       (org-modern-progress 8)
	       (org-modern-block-name t)
	       (org-modern-footnote nil)
	       (org-modern-internal-target '(" ↪ " t " "))
	       (org-modern-radio-target '(" ⛯ " t " "))
	       (org-modern-horizontal-rule nil))
  :config
  ;; copy of org-modern--tag with spaces around ":" removed to allow right justify to work properly
  (defun sej/org-modern--tag ()
    "Prettify headline tags."
    (save-excursion
      (let* ((default-face (get-text-property (match-beginning 1) 'face))
	         (colon-props `(display #(":" 0 1 (face org-hide)) face ,default-face))
	         (beg (match-beginning 2))
	         (end (match-end 2))
	         colon-beg colon-end)
	    (goto-char beg)
	    (while (re-search-forward "::?" end 'noerror)
	      (let ((cbeg (match-beginning 0))
		        (cend (match-end 0)))
	        (when colon-beg
	          ;;(put-text-property colon-end (1+ colon-end) 'display
	          ;;				   (format #(" %c" 1 3 (cursor t)) (char-after colon-end)))
	          ;;(put-text-property (1- cbeg) cbeg 'display
	          ;;				   (string (char-before cbeg) ?\s))
	          (put-text-property
	           colon-end cbeg 'face
	           (if-let* ((faces org-modern-tag-faces)
			             (face (or (cdr (assoc (buffer-substring-no-properties colon-end cbeg) faces))
				                   (cdr (assq t faces)))))
		           `(:inherit (,face org-modern-tag))
		         'org-modern-tag)))
	        (add-text-properties cbeg cend colon-props)
	        (setq colon-beg cbeg colon-end cend))))))

  (advice-add 'org-modern--tag :override #'sej/org-modern--tag))

;;;;; [[https://github.com/org-noter/org-noter][org-noter]]
;; create notes that are kept in sync as you scroll through the document
(use-package org-noter
  :after (org pdf-view)
  :commands org-noter
  :bind (:map org-noter-notes-mode-map
              ("C-M-i" . org-noter-insert-dynamic-block))
  :custom ((org-noter-notes-search-path (list org-directory))
	       (org-noter-supported-modes '(doc-view-mode pdf-view-mode))
	       (org-noter-max-short-selected-text-length 10)
	       (org-noter-separate-notes-from-heading t))
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
  :after org
  :hook (org-mode . sej/ob-start)
  :ensure nil
  :custom (org-confirm-babel-evaluate nil)
  :config
  (require 'ob)
  (defun sej/ob-start ()
    "Bable languages available."
    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp  . t)
							                                 (python      . t)
							                                 (calc        . t)
							                                 (shell       . t)
							                                 (latex       . t)
							                                 (C           . t)
							                                 (sql         . t)
							                                 (makefile    . t)
							                                 (ein         . t)
							                                 (perl        . t)
							                                 (ruby        . t)
							                                 (js          . t)
							                                 (css         . t)
							                                 (restclient  . t)
							                                 (java        . t)
							                                 (applescript . t)))))

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

;;;;; [[https://github.com/stig/ob-applescript.el][ob-applescript]]
;; [[https://plrj.org/2025/06/18/org-babel-and-applescript-with-a-little-vc-use-package/][Org Babel and AppleScript with a little vc-use-package]]
;; example script
;; #+begin_src applescript
;; -- AppleScript goes here; this is a comment
;; display dialog “Hello World”
;; #+end_src
(use-package ob-applescript
  :after org
  :vc (:url "https://github.com/stig/ob-applescript.el"
	        :rev :newest
	        :branch "trunk")
  :init
  (require 'apples-mode))

;;;;; org-Ox
;; built-in: org mode exporter framework
;; [[https://orgmode.org/worg/exporters/ox-overview.html][org-exporter manual]]
(use-package ox
  :ensure nil
  :custom ((org-export-with-toc nil)
	       (org-export-headline-levels 8)
	       (org-export-backends '(ascii html latex md))
	       (org-export-dispatch-use-expert-ui nil)
	       (org-export-coding-system 'utf-8)
	       (org-export-exclude-tags '("noexport" "no_export" "ignore"))
	       (org-export-with-author t)
	       (org-export-with-drawers t)
	       (org-export-with-email t)
	       (org-export-with-footnotes t)
	       (org-export-with-latex t)
	       (org-export-with-properties t)
	       (org-export-with-smart-quotes t)
	       (org-html-html5-fancy t)
	       (org-html-postamble nil)))

;;;;; ox-latex
;; built-in: latex exporter
;; https://orgmode.org/manual/LaTeX-Export.html#LaTeX-Export
(use-package ox-latex
  :ensure nil
  :custom ((org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f"))
	       (org-latex-remove-logfiles t)
	       (org-latex-prefer-user-labels t)
	       (bibtex-dialect 'biblatex)))

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

;;;;; [[https://gitlab.com/marcowahl/org-pretty-tags][org-pretty-tags]]
;; Display text or image surrogates for Org mode tags.
(use-package org-pretty-tags
  :hook (org-mode . org-pretty-tags-global-mode)
  :custom ((org-pretty-tags-surrogate-strings
	        `(;; generic tags
	          ("ATTACH" . "📎")
	          ("automation" . "⚙️")
	          ("basement" . #("󱊾" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0)
					                        font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0)
					                        display (raise 0.0) rear-nonsticky t)))
	          ("batteries" . "🔋⚡") ("battery" . "🔋⚡")
	          ("Blog" . "✍")
	          ("cat" . "🐱") ("cats" . "🐱")
	          ("closed". "🔒")
	          ("computer" . "🖥️")
	          ("datasheet" . "📈")
	          ("debug" . "🐞")
	          ("deck" . "⛩")
	          ("done" . "✅")
	          ("emacs" . "") ("Emacs" . "")
	          ("org" . #("" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0)
				                       font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0)
				                       display (raise 0.0) rear-nonsticky t)))
	          ("family" . "👨‍👩‍👧‍👦")
	          ("financial" . "💰")
	          ("friends" . "🍻")
	          ("furnace" . "🔥")
	          ("garage" . "🚗")
	          ("gtd" . "✅")
	          ("hockey" . "🏒")
	          ("home" . "🏠")
	          ("idea" . "💡")
	          ("journal" . "✒️")
	          ("kids" . "👶🏻")
	          ("kitchen" . "🍽️")
	          ("knowledge" . "🤓")
	          ("laundry" . "🧺")
	          ("log" . "📋")
	          ("maintenance" . "🛠️")
	          ("manual" . "📚")
	          ("media" . "💿")
	          ("meeting" . "👥")
	          ("music" . "🎶")
	          ("outside" . "🌻")
	          ("plugin" . "🔌")
	          ("pond" . "🐟")
	          ("project" . "🚧")
	          ("read" . "👀")
	          ("routine" . "🧹")
	          ("security" . "🔥")
	          ("service" . "🤝")
	          ("sprinkler" . "💦")
	          ("swimming" . "🏊‍♀️")
	          ("tool" . "🔧")
	          ("topic" . "☆")
	          ("travel" . "✈️")
	          ("tutorial" . "👨‍🎓")
	          ("water" . "💧")
	          ("waterfront" . "⚓")

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
	          ))))

;;;;; org-protocol
(use-package org-protocol
  :ensure nil
  :after org
  :custom (org-protocol-default-template-key "l"))

;;;;; org-rich-yank
;; Rich text clipboard when yanking code into org buffer
;; consider demand t as lazy loading may not work
;; https://github.com/unhammer/org-rich-yank
(use-package org-rich-yank
  :defer 2
  :bind (:map org-mode-map
              ("C-S-y" . org-rich-yank)))

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
  :custom ((org-src-window-setup 'current-window)
           (org-src-fontify-natively t)
           (org-src-preserve-indentation t)
           (org-src-tab-acts-natively t)
           (org-edit-src-content-indentation 0)))

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
  (setq-default sh-basic-offset 2)
  (setq sh-indent-after-continuation 'always))

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

  :custom ((eshell-glob-case-insensitive nil)
           (eshell-error-if-no-glob nil)
           (eshell-scroll-to-bottom-on-input nil)
           (eshell-where-to-jump 'begin)
           (eshell-review-quick-commands nil)
           (eshell-smart-space-goes-to-end t)
           (eshell-cmpl-cycle-completions nil)
           ;; auto truncate after 12k lines
           (eshell-buffer-maximum-lines 12000)
           (eshell-history-append t)
           ;; history size
           (eshell-history-size 1000)
           ;; buffer shorthand -> echo foo > #'buffer
           (eshell-buffer-shorthand t)
           ;; my prompt is easy enough to see
           (eshell-highlight-prompt nil)
           ;; treat 'echo' like shell echo
           (eshell-plain-echo-behavior t)
           (eshell-hist-ignoredups t)
           (eshell-save-history-on-exit t)
           (eshell-prefer-lisp-functions nil)
           (eshell-destroy-buffer-when-process-dies t)
	       ;; turn off semantic-mode in eshell buffers
	       (semantic-mode -1))

  :config
  (setenv "PAGER" "cat")

  ;; sej eshell functions
  ;;  contrib/eshell-cat-with-syntax-highlight
  ;;  define-button-type 'eshell-ls
  ;;  contrib/electrify-ls
  ;;  eshell/truncate-eshell-buffers
  ;;  eshell/cls
  ;;  eshell/emacs
  ;;  eshell/ec
  ;;  eshell/view-file
  ;;  eshell/less
  ;;  eshell/cds
  ;;  eshell/d
  ;;  eshell/magit
  (require 'sej-eshell)

  (advice-add 'eshell/cat :override #'contrib/eshell-cat-with-syntax-highlight)
  (advice-add 'eshell-ls-decorated-name :filter-return #'contrib/electrify-ls))

(use-package esh-module
  :ensure nil
  :custom (eshell-modules-list             ; Needs review
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
  :custom (eshell-cd-on-directory t))

(use-package em-tramp
  :ensure nil
  :after esh-mode
  :custom ((password-cache t)
	       (password-cache-expiry 600)))

(use-package em-hist
  :ensure nil
  :after esh-mode
  :custom ((eustom (st-ignoredups t))
	       (eshell-save-history-on-exit t)))

;;;;; eshell-prompt-extras
;; Display extra information for prompt
;; https://github.com/kaihaosw/eshell-prompt-extras
(use-package eshell-prompt-extras
  :after esh-opt
  :defines eshell-highlight-prompt
  :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
  :custom ((eshell-highlight-prompt nil)
	       (eshell-prompt-function 'epe-theme-lambda)
	       (eshell-prompt-funct ion 'epe-theme-dakrone)
	       (epe-git-dirty-char " Ϟ"))
  :init
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  ;; epe-git-dirty-char "*"
  )


;;;; shell
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
  :bind ((:map global-map
               ("C-c t" . sej/eat))
         (:map sej-C-q-map
               ("S e" . sej/eat))
         (:map eat-semi-char-mode-map
               ("C-c C-c" . eat-self-input)
               ("C-c C-j" . eat-semi-char-mode)))
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :custom (;; Close the terminal buffer when the shell terminates.
	       (eat-kill-buffer-on-exit t)
	       ;; Enable mouse-support.
	       (eat-enable-mouse t)
	       ;; Use proper term name
	       (eat-term-name "xterm-256color"))
  :config
  ;; Fix backspace on macOS
  (when (eq system-type 'darwin)
    (define-key eat-semi-char-mode-map (kbd "C-h") #'eat-self-input)
    (define-key eat-semi-char-mode-map (kbd "<backspace>") #'eat-self-input))

  ;; Set environment variables for color support
  (setenv "COLORTERM" "truecolor")
  (setenv "TERM" "xterm-256color")

  ;; Helper function to launch eat with proper shell
  (defun sej/eat ()
    "Launch eat terminal with zsh and proper color support."
    (interactive)
    (let ((eat-buffer (get-buffer "*eat*")))
      (if (and eat-buffer (buffer-live-p eat-buffer))
          (pop-to-buffer eat-buffer)
        (let ((process-environment
               (append '("COLORTERM=truecolor"
                         "TERM=xterm-256color"
                         "CLICOLOR=1"
                         "CLICOLOR_FORCE=1")
                       process-environment)))
          (eat)))))

  ;; Ensure proper colors and environment
  (defun sej/eat-mode-setup ()
    "Setup eat mode with better defaults."
    (setq-local scroll-margin 0)
    (setq-local truncate-lines nil)
    (face-remap-add-relative 'default :background "#000000"))

  (add-hook 'eat-mode-hook #'sej/eat-mode-setup))

;;;;; tldr.el
;; connection to the tldr shell command for summary of man pages
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
  :custom ((term-buffer-maximum-size 9999)
	       (term-completion-autolist t)
	       (term-completion-recexact t)
	       (term-scroll-to-bottom-on-output nil)))

;;;;; ERC IRC client
;; built-in: irc client
;; [[https://www.gnu.org/software/emacs/manual/html_mono/erc.html#Top]]
(use-package erc
  :ensure nil
  :bind (:map sej-C-q-map
              ("I" . sej/erc-start-or-switch))
  :custom ((auth-source-debug t)
	       (erc-prompt-for-password nil))
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
          (setq *uconf/erc-certs*
		        `((,(plist-get (car (auth-source-search :host "irc.libera.chat")) :cert ) ) ) )
	      (open-network-stream name buffer host port
                               :nowait t
                               :type 'tls))
      (setq *uconf/erc-certs* nil)))

  (advice-add 'open-gnutls-stream :override #'uconf/open-gnutls-stream)
  (advice-add 'erc-open-tls-stream :override #'uconf/erc-open-tls-stream)

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
                   (sej/erc-buffer-connected-p buffer))
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
  :custom ((shr-use-fonts t)
	       (shr-use-colors t)
	       (shr-max-image-proportion 0.2)
	       (shr-image-animate t)
	       (shr-width (current-fill-column))))

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
  :custom ((eww-restore-desktop nil)
	       (eww-desktop-remove-duplicates t)
	       (eww-header-line-format "%u")
	       (eww-search-prefix "https://duckduckgo.com/html/?q=")
	       (eww-download-directory "~/Downloads/")
	       (eww-suggest-uris
	        '(eww-links-at-point
	          thing-at-point-url-at-point))
	       (eww-history-limit 150)
	       (eww-use-external-browser-for-content-type
	        "\\`\\(video/\\|audio/\\|application/pdf\\)")
	       (eww-browse-url-new-window-is-tab nil)
	       (eww-form-checkbox-selected-symbol "[X]")
	       (eww-form-checkbox-symbol "[ ]"))
  :config
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

;;;;; TMR (timer)
;; facilities for setting timers using a convenient notation
;; [[https://protesilaos.com/emacs/tmr]]
(use-package tmr
  :bind (:map sej-C-q-map
              ("t" . tmr-prefix-map))
  :ensure-system-package ffmpeg
  :custom (;; Read the `tmr-descriptions-list' doc string
	       (tmr-descriptions-list 'tmr-description-history))
  :config
  (if sys/macp
      (progn
        (defun sej/osx-alert-tmr (&optional timer)
          "function to display TIMER info on osx notification area"
          (interactive)
          (alert
           (if (boundp 'timer) (tmr--long-description-for-finished-timer timer) "test")
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
    ;; EXPERIMENTAL: Changed from :demand t - only needed when using elfeed (ELFEED-ORG)
    :after elfeed
    :config
    (elfeed-org)
    :custom
    (rmh-elfeed-org-files '("~/Documents/orgtodo/20250124T203443--elfeed__emacs.org"))))

;;;;; elfeed-webkit
;; render in webkit [[https://github.com/fritzgrabo/elfeed-webkit][link]]
(use-package elfeed-webkit
  :after elfeed
  :bind (:map elfeed-show-mode-map
              ("'" . elfeed-webkit-toggle)))

;;; init.el --- end
(message "init.el ends here")
(provide 'init)
;;; init.el ends here
