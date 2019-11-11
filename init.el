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
;; 2019 10 20 remove helm stuff; remove most messages
;; 2019 10 22 start to tangle in init-org.org

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

;;;;;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 1024 100))

;;;;;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;; Set deferred timer to reset them
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)))

;; ;; Load path
;; ;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
;; (defun update-load-path (&rest _)
;;   "Update `load-path'."
;;   (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
;;   (push (expand-file-name "lisp" user-emacs-directory) load-path))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'."
;;   (let ((default-directory
;;           (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; (update-load-path)

;; ;; turn on syntax highlightng for all buffers
;; (global-font-lock-mode t)

;; ;; raise the maximum number of logs in the *Messages* buffer
;; (setq message-log-max 16384)

;; ;; wait a bit longer than the default 0.5s before assuming Emacs is idle
;; (setq idle-update-delay 2)

;; ;; make gnutls a bit safer
;; (setq gnutls-min-prime-bits 4096)

;; ;; remove irritating 'got redefined' messages
;; (setq ad-redefinition-action 'accept)

;; ;; figure out current hostname
;; (setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; ;; allow exit without asking to kill processes
;; (setq confirm-kill-processes nil)

;; Constants
;; (require 'init-const) ; org-tangle

;; Custom group definition
;; (require 'init-custom) ; org-tangle

;; ;; add my custom hook ; org-tangle
;; (defvar sej/after-init-hook nil
;;   "Hook called after emacs-init and some time.")

;; (defvar sej/idle-timer 5
;;   "Var to set time in seconds for idle timer.")
;; (when sys/macp
;;   (setq sej/idle-timer 1))

;; (defun sej/run-my-after-init-hook ()
;;   "Function to define when to run my startup hooks"
;;   (interactive)
;;   (message "set-up my hooks")
;;   (run-with-idle-timer sej/idle-timer nil
;;                        (lambda ()
;;                          (message "start running my hooks")
;;                          (run-hooks 'sej/after-init-hook)
;;                          (message "done running my hooks")
;;                          )))

;; (add-hook 'after-init-hook 'sej/run-my-after-init-hook)
;; ;; (remove-hook 'after-init-hook 'sej/run-my-after-init-hook)
;; (add-hook 'emacs-startup-hook 'sej/frame-resize-full)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
;; (require 'init-package) ;org-tangle

;; org-tangle load-file
(org-babel-load-file (concat user-emacs-directory "init-org.org"))

;; Preferences
;; (require 'init-basic) ; org-tangle
;; (require 'init-bindings) ; org-tangle

;; Personal functions
;;   merge of init-funcs & init-misc-defuns
;; (require 'init-defuns) ; org-tangle

;; Set-up the user interface
;; (require 'init-ui) ; org-tangle
;; (require 'init-edit) ; org-tangle
;; (require 'init-highlight) ; org-tangle

;; (require 'init-window) ; org-tangle
(require 'init-frame-cmds)

(require 'init-ivy)

(require 'init-company)
(require 'init-yasnippet)

(require 'init-registers)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-deft)

(require 'init-persp)

(require 'init-eshell)
(require 'init-shell)

(require 'init-org) ;
;; (require 'init-calendar) ; not used leave commented
;; (require 'init-elfeed) ;  not used leave commented

(require 'init-writing)
(require 'init-spelling)
(require 'init-view)

(require 'init-templates)
(require 'init-misc-filetypes)

;; Programming
(require 'init-tramp)
(require 'init-vcs)
(require 'init-prog)

(require 'init-flycheck)
(require 'init-projectile)

(require 'init-lsp)
(require 'init-lisp)

(require 'init-c)
(require 'init-python)
(require 'init-web)
(require 'init-misc-languages)
(require 'init-misc-pkgs)

(message "init.el ends here")
;;; init.el ends here
