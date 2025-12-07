;;; sej-os-specific-osx.el --- sej Emacs OS Specific OSX -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Provides OS specific constants for OSX
;; sets specific font if available,
;; as well sets up the keyboard for modifiers.

;;; Code:
(when sys/macp
  (message "Mac OSX")
  ;; fix path on M1 macs
  (when sys/mac-AA64-p
	(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
	(setq exec-path (append exec-path '("/opt/homebrew/bin"))))
  ;; Prefer not to run Homebrew on startup. If Iosevka is missing,
  ;; just notify the user and point to `sej/install-iosevka-font'.
  (unless (find-font (font-spec :name "Iosevka"))
	(message "Iosevka font not found; run M-x sej/install-iosevka-font to install it via Homebrew."))
  (when (find-font (font-spec :name "Iosevka"))
	(add-to-list 'default-frame-alist '(font . "iosevka-14")))

	(setq insert-directory-program "gls")

  (if (not (getenv "TERM_PROGRAM"))
	  (setenv "PATH"
			  (shell-command-to-string "source $HOME/.zprofile ; printf $PATH")))
  (setq exec-path (split-string (getenv "PATH") ":"))

  (defun sej/install-iosevka-font ()
	"Install the Iosevka font using Homebrew Cask on macOS.

	 This runs \"brew install --cask font-iosevka\" in a subprocess.
	 It does not change any other system state."
	(interactive)
	(if (not (executable-find "brew"))
		(user-error "Homebrew (brew) not found in PATH – install Homebrew first")
	  (let ((buf (get-buffer-create "*Iosevka Font Install*")))
		(with-current-buffer buf
	  (erase-buffer)
	  (insert "Running: brew install --cask font-iosevka\n\n"))
		(display-buffer buf)
		(let ((proc (start-process-shell-command
				 "iosevka-font-install" buf
				 "brew install --cask font-iosevka")))
	  (set-process-sentinel
	 proc
	 (lambda (p _event)
	  (when (eq (process-status p) 'exit)
	  (if (= (process-exit-status p) 0)
		(message "Iosevka font installation finished successfully.")
	  (message "Iosevka font installation failed; see *Iosevka Font Install* buffer for details.")))))))))

;;;;; OSX Apple keyboard
  ;; caps lock is control (through karabiner)
  ;; Fn key do Hyper
  ;; LControl key do RControl (Karabiner) which is Super (Emacs)
  ;; left opt/alt key do Emacs Alt modifier
  ;; right opt/alt key do regular alt key
  ;; left and right command(apple) key do Meta
  ;; space bar acts as super key with other key
  ;; karabiner.json backup files in dotfiles under .config directory
  ;; https://github.com/pqrs-org/Karabiner-Elements

  (if (boundp 'mac-carbon-version-string) ;; using mac-port?
	  ( progn
		(message "Mac-port")
		;; for emacs-mac-port
		(setq mac-right-command-modifier 'left)  ;right command, plus Karabiner
		(setq mac-right-option-modifier 'meta)   ;right option as meta
		(setq mac-function-modifier 'hyper)      ;hyper is function & held tab key (Karabiner)
		(setq mac-control-modifier 'control)     ;Karabiner swapped & caps_lock
		(setq mac-right-control-modifier 'alt)   ;actually left control
		(setq mac-option-modifier 'meta)         ;left option is meta
		(setq mac-command-modifier 'super))      ;left command is super
	( progn
	  (message "ns-port")
	  ;; for regular Emacs port
	  (setq ns-right-command-modifier 'left)   ;right command, plus Karabiner
	  (setq ns-right-option-modifier 'meta)	 ;right option as meta
	  (setq ns-function-modifier 'hyper)		 ;hyper is function & held tab key (Karabiner)
	  (setq ns-control-modifier 'control)		 ;Karabiner swapped & caps_lock
	  (setq ns-right-control-modifier 'alt)    ;actually left control
	  (setq ns-option-modifier 'meta)			 ;left option is meta
	  (setq ns-command-modifier 'super)		 ;left command is super

	  ;; old version w/command as meta, control as super, option as Alt
	  ;; (setq ns-right-command-modifier 'left)   ;right command, plus Karabiner
	  ;; (setq ns-right-option-modifier 'none)	 ;Stays as alt key (like å∫ç∂)
	  ;; (setq ns-function-modifier 'hyper)		 ;hyper is function & held tab key (Karabiner)
	  ;; (setq ns-control-modifier 'control)		 ;Karabiner swapped & caps_lock
	  ;; (setq ns-right-control-modifier 'super)  ;actually left control
	  ;; (setq ns-option-modifier 'alt)			 ;left option is A-alt key
	  ;; (setq ns-command-modifier 'meta)		 ;left command is meta

	  ;; only needed with old version
	  ;; (global-set-key (kbd "M-`") 'ns-next-frame)
	  ;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
	  )))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-os-specific-osx)
;;; sej-os-specific-osx.el ends here
