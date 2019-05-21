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
;; (setq debug-on-error nil)
;; (setq debug-on-quit nil)

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
(require 'init-defuns) ; DONE

;; Set-up the user interface
(require 'init-ui) ; DONE
(require 'init-edit) ; DONE
(require 'init-highlight) ; DONE

(require 'init-window) ; DONE
(require 'init-frame-cmds) ; DONE

(require 'init-ivy) ; DONE
;; (require 'init-helm) ; TODO keep in case decide helm over ivy
(require 'init-company) ; DONE
(require 'init-yasnippet) ; DONE

(require 'init-registers) ; DONE
(require 'init-dashboard) ; DONE
(require 'init-dired) ; DONE
(require 'init-ibuffer) ; DONE
(require 'init-kill-ring) ; DONE
(require 'init-deft) ; DONE

(require 'init-persp) ; DONE

(require 'init-eshell) ; DONE
(require 'init-shell) ; DONE

(require 'init-org) ;
;; (require 'init-calendar) ; not used leave commented
;; (require 'init-elfeed) ;  not used leave commented

(require 'init-writing) ; DONE
(require 'init-spelling) ; DONE
(require 'init-view) ; DONE

(require 'init-templates) ; DONE
(require 'init-misc-filetypes) ; DONE

;; Programming
(require 'init-tramp) ; DONE
(require 'init-vcs) ; DONE
(require 'init-prog) ; DONE

(require 'init-flycheck) ; DONE
(require 'init-projectile) ; DONE
(require 'init-dap) ; DONE

(require 'init-lsp) ; DONE
(require 'init-lisp) ; DONE

(require 'init-c) ; DONE
(require 'init-python) ; DONE
(require 'init-web) ; DONE
(require 'init-misc-languages) ; DONE
(require 'init-misc-pkgs) ; DONE


;;; init.el ends here
