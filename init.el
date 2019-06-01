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
(message "Emacs start")

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Speed up startup
(message "Optimize")
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 80000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 2000000)
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(message "Load Path")
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

(message "First Sets")
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

;; Constants
(message "init-const")
(require 'init-const) ; DONE

;; Custom group definition
(message "init-custom")
(require 'init-custom) ; DONE

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

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(message "init-package")
(require 'init-package) ; DONE

;; Preferences
(message "init-basic")
(require 'init-basic) ; DONE
(message "init-bindings")
(require 'init-bindings) ; DONE

;; Personal functions
;;   merge of init-funcs & init-misc-defuns
(message "init-defuns")
(require 'init-defuns) ; DONE

;; Set-up the user interface
(message "init-ui")
(require 'init-ui) ; DONE
(message "init-edit")
(require 'init-edit) ; DONE
(message "init-highlight")
(require 'init-highlight) ; DONE

(message "init-window")
(require 'init-window); DONE
(message "init-frame-cmds")
(require 'init-frame-cmds) ; DONE

(message "init-ivy")
(require 'init-ivy) ; DONE
;; (message "init-helm")
;; (require 'init-helm) ; TODO keep in case decide helm over ivy
(message "init-company")
(require 'init-company) ; DONE
(message "init-yasnippet")
(require 'init-yasnippet) ; DONE

(message "init-registers")
(require 'init-registers) ; DONE
(message "init-dashboard")
(require 'init-dashboard) ; DONE
(message "init-dired")
(require 'init-dired) ; DONE
(message "init-ibuffer")
(require 'init-ibuffer) ; DONE
(message "init-kill-ring")
(require 'init-kill-ring) ; DONE
(message "init-deft")
(require 'init-deft) ; DONE

(message "init-persp")
(require 'init-persp) ; DONE

(message "init-eshell")
(require 'init-eshell) ; DONE
(message "init-shell")
(require 'init-shell) ; DONE

(message "init-org")
(require 'init-org) ;
;; (require 'init-calendar) ; not used leave commented
;; (require 'init-elfeed) ;  not used leave commented

(message "init-writing")
(require 'init-writing) ; DONE
(message "init-spelling")
(require 'init-spelling) ; DONE
(message "init-view")
(require 'init-view) ; DONE

(message "init-templates")
(require 'init-templates) ; DONE
(message "init-misc-filetypes")
(require 'init-misc-filetypes) ; DONE

;; Programming
(message "init-tramp")
(require 'init-tramp) ; DONE
(message "init-vcs")
(require 'init-vcs) ; DONE
(message "init-prog")
(require 'init-prog) ; DONE

(message "init-flycheck")
(require 'init-flycheck) ; DONE
(message "init-projectile")
(require 'init-projectile) ; DONE
(message "init-dap")
(require 'init-dap) ; DONE

(message "init-lsp")
(require 'init-lsp) ; DONE
(message "init-lisp")
(require 'init-lisp) ; DONE

(message "init-c")
(require 'init-c) ; DONE
(message "init-python")
(require 'init-python) ; DONE
(message "init-web")
(require 'init-web) ; DONE
(message "init-misc-languages")
(require 'init-misc-languages) ; DONE
(message "init-misc-pkgs")
(require 'init-misc-pkgs) ; DONE

(message "init.el ends here")
;;; init.el ends here
