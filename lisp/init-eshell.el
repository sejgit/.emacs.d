;; init-eshell.el --- Initialize eshell configurations. -*- lexical-binding: t -*-

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
;; Eshell configurations.
;;

;;; Changelog:
;;
;; 2019 05 01 Initialize & Merge from sejgit


;;; Code:

;; Emacs command shell
(use-package eshell
  :ensure nil
  :defines (compilation-last-buffer eshell-prompt-function)
  :commands (eshell/alias
             eshell-send-input eshell-flatten-list
             eshell-interactive-output-p eshell-parse-command
             eshell eshell-command)
  :defines sej-mode-map
  :hook  ((eshell-mode . sej/setup-eshell)
          (eshell-mode . (lambda ()
                           (bind-key "C-l" 'eshell/clear eshell-mode-map)
                           (eshell/alias "f" "find-file $1")
                           (eshell/alias "fo" "find-file-other-window $1")
                           (eshell/alias "d" "dired $1")
                           (eshell/alias "ll" "ls -l")
                           (eshell/alias "la" "ls -al"))))
  :bind (:map sej-mode-map
              ("H-e" . eshell)
              ("C-c e" . eshell))
  :preface

  ;; Visual commands
  (setq eshell-visual-commands '("screen" "htop" "less" "more" "ncftp" "elm"
                                 "nmtui" "alsamixer" "htop" "el" "nano"
                                 "ssh" "nethack" "dstat"))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")
                                    ("vagrant" "ssh")))


;;; Eshell settings
  (defun sej/setup-eshell ()
    "Set-up for eshell function to be called when 'eshell-mode' is entered."
    (interactive)
    ;; turn off semantic-mode in eshell buffers
    (semantic-mode -1)
    (local-set-key (kbd "M-P") 'eshell-previous-prompt)
    (local-set-key (kbd "M-N") 'eshell-next-prompt)
    (local-set-key (kbd "M-R") 'eshell-previous-matching-input))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input))))

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

  (defun sej/truncate-eshell-buffers ()
    "Truncates all eshell buffers"
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (eq major-mode 'eshell-mode)
          (eshell-truncate-buffer)))))

  ;; After being idle for 5 seconds, truncate all the eshell-buffers if
  ;; needed. If this needs to be canceled, you can run `(cancel-timer
  ;; eos/eshell-truncate-timer)'
  (setq sej/eshell-truncate-timer
        (run-with-idle-timer 5 t #'sej/truncate-eshell-buffers))

  (defun eshell/cds ()
    "Change directory to the project's root."
    (eshell/cd (locate-dominating-file default-directory ".git")))

  (defun eshell/d (&rest args)
    (dired (pop args) "."))


  :config

  (require 'em-smart)
  (setq eshell-glob-case-insensitive nil
        eshell-error-if-no-glob nil
        eshell-scroll-to-bottom-on-input nil
        eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)
  ;; Initialize "smart" mode
  ;;(add-hook 'eshell-mode-hook #'eshell-smart-initialize)
  (setenv "PAGER" "cat")
  (require 'esh-opt)
  (require 'em-cmpl)
  (require 'em-prompt)
  (require 'em-term)

  (setq eshell-cmpl-cycle-completions nil
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
        ;; add -lh to the `ls' flags
        eshell-ls-initial-args "-lh")

  ;;  Display extra information for prompt
  ;; See: https://github.com/kaihaosw/eshell-prompt-extras
  (use-package eshell-prompt-extras
    :after esh-opt
    :defines eshell-highlight-prompt
    :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
    :init (setq eshell-highlight-prompt nil
                eshell-prompt-function 'epe-theme-lambda
                eshell-prompt-function 'epe-theme-dakrone
                ;; See eshell-prompt-function below
                eshell-prompt-regexp "^[^#$\n]* [#$] "
                epe-git-dirty-char " Ϟ"
                ;; epe-git-dirty-char "*"
                ))

  ;; Fish-like history autosuggestions
  (use-package esh-autosuggest
    :defines ivy-display-functions-alist
    :preface
    (defun setup-eshell-ivy-completion ()
      (setq-local ivy-display-functions-alist
                  (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                        ivy-display-functions-alist)))
    :bind (:map eshell-mode-map
                ([remap eshell-pcomplete] . completion-at-point))
    :hook ((eshell-mode . esh-autosuggest-mode)
           (eshell-mode . setup-eshell-ivy-completion)))

  ;; Eldoc support
  (use-package esh-help
    :init (setup-esh-help-eldoc))

  ;; So the history vars are defined
  (require 'em-hist)
  (if (boundp 'eshell-save-history-on-exit)
      ;; Don't ask, just save
      (setq eshell-save-history-on-exit t))


  (defun eshell/magit ()
    "Function to open magit-status for the current directory."
    (interactive)
    (magit-status-internal default-directory)
    nil)

  ;; `cd' to frequent directory in eshell
  (use-package eshell-z
    :hook (eshell-mode
           .
           (lambda () (require 'eshell-z)))))

(provide 'init-eshell)
;;; init-eshell.el ends here
