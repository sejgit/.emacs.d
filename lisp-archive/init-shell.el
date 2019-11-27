;; init-shell.el --- Initialize shell configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d

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
;; Shell configurations.
;;

;;; Changelog:
;;
;; 2019 05 01 Initialize & Merge from sejgit


;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package shell
  :ensure nil
  :hook (
         ;; interpret and use ansi color codes in shell output windows
         (shell-mode . ansi-color-for-comint-mode-on)
         (shell-mode . n-shell-mode-hook)
         (shell-mode . set-scroll-conservatively))
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

  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  ;; truncate buffers continuously
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)


  ;; Company mode backend for shell functions
  (use-package company-shell
    :after company
    :init (cl-pushnew '(company-shell company-shell-env company-fish-shell)
                      company-backends))

  ;; ANSI & XTERM 256 color support
  (use-package xterm-color
    :defines compilation-environment
    :init
    (setenv "TERM" "xterm-256color")
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))

    (add-hook 'shell-mode-hook
              (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

;; Shell Pop
(use-package shell-pop
  :bind ([f9] . shell-pop)
  :init (let ((val
               (if sys/win32p
                   '("eshell" "*eshell*" (lambda () (eshell)))
                 '("ansi-term" "*ansi-term*"
                   (lambda () (ansi-term shell-pop-term-shell))))))
          (setq shell-pop-shell-type val)))

;; Bash completion
(use-package bash-completion
  :init (bash-completion-setup)
  :hook (shell-mode . compilation-shell-minor-mode)
  :defines explicit-shell-file-name
  :config
  (setq explicit-shell-file-name "bash")
  (setq comint-process-echoes t)
  (setq bash-completion-process-timeout 0.5))

;; this allows GUI Emacs to inherit $PATH
(use-package exec-path-from-shell
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Emacs does not handle less well so use cat
(setenv "PAGER" "cat")

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
      comint-prompt-read-only nil)

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

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message)))))

(defun sudoec (file)
  "A nice helper to sudo-edit a (FILE)."
  (interactive)
  (find-file (concat "/sudo::" (expand-file-name file))))

;; things that invoke $EDITOR will use the current Emacs
(use-package with-editor
  :hook ((shell-mode . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))

;; set up any SSH or GPG keychains that the Keychain tool has set up for us
(use-package keychain-environment
  :hook (sej/after-init . keychain-refresh-environment))

(provide 'init-shell)
;;; init-shell.el ends here
