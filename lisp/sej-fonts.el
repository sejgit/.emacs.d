;;; sej-fonts.el --- fonts function file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive commands for font management.
;; Currently to install Iosevka font for Homebrew.

;;; Code:

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


;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-fonts)
;;; sej-fonts.el ends here
