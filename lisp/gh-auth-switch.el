;;; gh-auth-switch.el --- GitHub account switching for Magit -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides interactive commands to switch between GitHub accounts
;; using the gh CLI, useful when pushing to different remotes in Magit.

;;; Code:

(defun gh-auth-switch ()
  "Switch gh authentication account interactively."
  (interactive)
  (let ((output (shell-command-to-string "gh auth switch")))
    (message "%s" (string-trim output))))

(defun gh-auth-status ()
  "Show current GitHub authentication status."
  (interactive)
  (async-shell-command "gh auth status"))

(provide 'gh-auth-switch)
;;; gh-auth-switch.el ends here
