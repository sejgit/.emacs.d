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

;;; Changelog
;;
;; 2019 04 28 Merge from old .emacs.d


;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; debugger
(setq debug-on-error nil)
(setq debug-on-quit nil)

(defvar emacs-start-time (current-time)
  "Time Emacs was started.")

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 80000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))

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

;; Constants
(require 'init-const) ; DONE

;; Custom group definition
(require 'init-custom) ; DONE

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package) ; DONE

;; Preferences
(require 'init-basic) ; DONE
(require 'init-bindings) ; DONE

;; Personal functions
;;   merge of init-funcs & init-misc-defuns
(require 'init-defuns) ;

;; Set-up the user interface
(require 'init-ui) ;
(require 'init-edit) ;
(require 'init-highlight) ;

(require 'init-window) ;
(require 'init-frame-cmds) ;

(require 'init-ivy) ;
;; (require 'init-ido-ivy-helm) ; TODO decide if ivy or helm
(require 'init-company) ;
(require 'init-yasnippet) ; :  add Yasnippets?

(require 'init-registers) ;
(require 'init-dashboard) ;
(require 'init-dired) ;
(require 'init-ibuffer) ;
(require 'init-kill-ring) ;
(require 'init-deft) ;

(require 'init-persp) ;
(require 'init-treemacs) ; TODO keybindings

(require 'init-eshell) ;
(require 'init-shell) ;

(require 'init-org) ; TODO: get back to python3.6/3.7 issue
;; (require 'init-calendar) ; not used leave commented
;; (require 'init-elfeed) ;  not used leave commented

(require 'init-writing) ;
(require 'init-spelling) ;
(require 'init-view) ;

(require 'init-templates) ;
(require 'init-misc-filetypes) ;

;; Programming
(require 'init-tramp) ;
(require 'init-vcs) ;
(require 'init-prog) ;  some more merge from mine

(require 'init-flycheck) ;
(require 'init-projectile) ;
(require 'init-dap) ;

(require 'init-lsp)
(require 'init-lisp) ;

(require 'init-c) ;
(require 'init-python) ;
(require 'init-web) ;
(require 'init-misc-languages) ;
(require 'init-misc-pkgs) ;


;;; init.el ends here
