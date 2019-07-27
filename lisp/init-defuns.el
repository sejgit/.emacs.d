;; init-defuns.el --- Define functions. -*- lexical-binding: t -*-

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
;; Define my functions.
;;

;;; Commentary:
;;
;; some functions to add value to my Emacs

;;; ChangeLog
;;
;; 2017 05 17 init SeJ from purcell/.emacs.d
;; 2017 08 29 add copy-from-osx & paste-to-osx
;; 2017 09 08 fixed above for only mac
;; 2017 09 20 deleted unused defuns and renamed to init-misc-defuns.el
;;            move from init-bindings-settings.el
;; 2017 12 21 comment out unused, use crux for some, reorder used at top
;; 2018 01 31 add jcs now sej/insert-url from safari
;;            add my now sej/org-insert-defun
;;            make my functions consistent with sej/
;; 2018 09 24 add executable functions from ohai
;; 2019 07 27 add garbage collection fix

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; from https://gist.github.com/the-kenny/267162
(when sys/macp
  (defun copy-from-osx ()
    "For copying from osx."
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    "For copying to osx TEXT with optional PUSH."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)

  ;; from jcs (Irreal) blog to copy url from safari and paste at point
  (defun sej/insert-url ()
    "Insert URL of current browser page into Emacs buffer."
    (interactive)
    (insert (sej/retrieve-url)))

  ;; from jcs (Irreal) blog helper function from above
  (defun sej/retrieve-url ()
    "Retrieve the URL of the current Safari page as a string."
    (org-trim (shell-command-to-string
               "osascript -e 'tell application \"Safari\" to return URL of document 1'")))
  )

;; as name suggests ; defined as C-c b in above keymappings
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

;; function to edit the curent file as root. attached to C-x C-r in bindings
(defun sej/sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; line numbers when using goto-line M-g M-g or M-g g
(defun sej/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (with-no-warnings (goto-line (read-number "Goto line: "))))
    (display-line-numbers-mode -1)))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun sej/create-non-existent-directory ()
  "Ask to make directory for file if it does not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p? (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'sej/create-non-existent-directory)


;; macro saving
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

;; functions for push and jump to mark
(defun sej/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun sej/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;; executable functions from ohai and modified for my uses
(defun sej/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (s-trim (shell-command-to-string command)))

(defun sej/exec-with-rc (command &rest args)
  "Run a shell command and return a list containing two values: its return
code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun sej/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun sej/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun sej/exec-if-exec (command args)
  "If `command' satisfies `sej/is-exec', run it with `args' and return its
output as per `sej/exec'. Otherwise, return nil."
  (when (sej/is-exec command) (sej/exec (s-concat command " " args))))

;; sej/indent-buffer bound to C-c <tab>
(defun sej/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (text-scale-increase 0)
    (widen)
    (if (and (fboundp 'fancy-narrow-active-p)
             (fancy-narrow-active-p))
        (fancy-widen))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(bind-key "<f5>" #'revert-this-buffer)
(if sys/mac-x-p
    (bind-key "s-r" #'revert-this-buffer))

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of SeJ Emacs."
  (interactive)
  (browse-url sejgit-homepage))

;; Update
(defun update-config ()
  "Update SeJ Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating Emacs configurations...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished. Restart Emacs to complete the process."))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'sej/update-config 'update-config)

(declare-function upgrade-packages 'init-package)
(defalias 'sej/update-packages 'upgrade-packages)
(defun update-sej()
  "Update confgiurations and packages."
  (interactive)
  (update-config)
  (upgrade-packages nil))
(defalias 'sej/update 'update-sej)

(defun update-all()
  "Update dotfiles, org files, Emacs confgiurations and packages, ."
  (interactive)
  (update-sej)
  (update-org)
  (update-dotfiles))
(defalias 'sej/update-all 'update-all)

(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished."))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'sej/update-dotfiles 'update-dotfiles)

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished."))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'sej/update-org 'update-org)

;; Save a file as utf-8
(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

;; Recompile elpa directory
(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

;; Recompile site-lisp directory
(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

;;
;; Network Proxy
;;

(defun proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" sej-proxy)
    (message "No proxy")))

(defun proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,sej-proxy)
                             ("https" . ,sej-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (setq url-http-proxy-basic-auth-storage sej-url-http-proxy-basic-auth-storage)
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (setq url-http-proxy-basic-auth-storage nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

(defvar socks-noproxy)
(defvar socks-server)
(defun proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 1086 5))
  (message "Enable socks proxy."))

(defun proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (message "Disable socks proxy."))

(provide 'init-defuns)
;;; init-defuns.el ends here
