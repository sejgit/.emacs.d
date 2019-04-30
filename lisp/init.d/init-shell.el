;;; init-shell.el --- Initialize shells inside of Emacs

;;; Commentary:
;; shell & Eshell init inside of Emacs

;; ChangeLog
;; 2017 05 26 add standard use-package terminology
;; 2017 08 25 add exec-path-from-shell from EOS
;; 2017 09 06 change name to init-shell
;; 2017 09 20 move bash from init-bash.el to here & delete file
;; 2018 09 27 clean-up

;;; Code:


;; Visual commands
(setq eshell-visual-commands '("screen" "htop" "less" "more" "ncftp" "elm"
				                       "nmtui" "alsamixer" "htop" "el" "nano"
				                       "ssh" "nethack" "dstat"))
(setq eshell-visual-subcommands '(("git" "log" "diff" "show")
				                          ("vagrant" "ssh")))

(add-hook 'shell-mode-hook #'set-scroll-conservatively)
;; truncate buffers continuously
(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
;; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;;; Eshell settings
(add-hook 'eshell-mode-hook #'sej/setup-eshell)

(defun sej/setup-eshell ()
  "Set-up for eshell function to be called when 'eshell-mode' is entered."
  (interactive)
  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  (local-set-key (kbd "M-P") 'eshell-previous-prompt)
  (local-set-key (kbd "M-N") 'eshell-next-prompt)
  (local-set-key (kbd "M-R") 'eshell-previous-matching-input)
  (local-set-key (kbd "M-r") 'helm-eshell-history))


(defun sudoec (file)
  "A nice helper to sudo-edit a (FILE)."
  (interactive)
  (find-file (concat "/sudo::" (expand-file-name file))))

;; eshell
(use-package eshell
  :ensure t
  :commands (eshell eshell-command)
  :defines sej-mode-map
  :bind (:map sej-mode-map
	            ("H-e" . eshell)
              ("C-c e" . eshell))
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
  (defalias 'emacs 'find-file)
  (defalias 'hff 'hexl-find-file)
  (defalias 'sec 'sudoec)
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

  (defalias 'eshell/l 'eshell/ls)
  (defalias 'eshell/ll 'eshell/ls)

  (defun eshell/ec (pattern)
    (if (stringp pattern)
	      (find-file pattern)
      (mapc #'find-file (mapcar #'expand-file-name pattern))))
  (defalias 'e 'eshell/ec)
  (defalias 'ee 'find-file-other-window)

  (defun eshell/d (&rest args)
    (dired (pop args) "."))

  (defun eshell/clear ()
    "Clear the eshell buffer"
    (interactive)
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer)
      (let ((inhibit-read-only t))
	      (erase-buffer)
	      (eshell-send-input))))

  (defun eshell/icat (&rest args)
    "Display image(s) (ARGS)."
    (let ((elems (eshell-flatten-list args)))
      (while elems
	      (eshell-printn
	       (propertize " "
		                 'display (create-image (expand-file-name (car elems)))))
	      (setq elems (cdr elems))))
    nil)

  ;; See eshell-prompt-function below
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] ")

  ;; So the history vars are defined
  (require 'em-hist)
  (if (boundp 'eshell-save-history-on-exit)
      ;; Don't ask, just save
      (setq eshell-save-history-on-exit t))

  ;; See: https://github.com/kaihaosw/eshell-prompt-extras
  (use-package eshell-prompt-extras
    :ensure t
    :init
    (progn
      (setq eshell-highlight-prompt nil
	          epe-git-dirty-char " Ϟ"
	          ;; epe-git-dirty-char "*"
	          eshell-prompt-function 'epe-theme-dakrone)))

  (defun eshell/magit ()
    "Function to open magit-status for the current directory."
    (interactive)
    (magit-status-internal default-directory)
    nil))

(use-package bash-completion
  :ensure t
  :hook (shell-mode . compilation-shell-minor-mode)
  :defines explicit-shell-file-name
  :config
  (setq explicit-shell-file-name "bash")
  (setq comint-process-echoes t)
  (setq bash-completion-process-timeout 0.5))

;; this allows GUI Emacs to inherit $PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; things that invoke $EDITOR will use the current Emacs
(use-package with-editor
  :ensure t
  :hook ((shell-mode . with-editor-export-editor)
	       (eshell-mode . with-editor-export-editor)))

;; set up any SSH or GPG keychains that the Keychain tool has set up for us
(use-package keychain-environment
  :ensure t
  :hook (after-init . keychain-refresh-environment))

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
      (fset 'message old-message))))


(provide 'init-shell)
;;; init-shell.el ends here
