;;; sej-os-specific-windows.el --- sej Emacs OS Specific Windows -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Provides OS specific constants for Windows
;; as well sets up the keyboard for modifiers.

;;; Code:
(when sys/win32p
  (message "Microsoft Windows")

;;;;; Windows keyboard
  ;; - CapsLock::LControl through AutoHotkeys
  ;; scroll lock do hyper (tab to scroll lock using AutoHotkeys)
  ;; Left control key do super (LControl::Appskey using AutoHotkeys)
  ;; Left Windows left alone due to win10 taking many keys
  ;; LAlt::Meta
  ;; RAlt::Alt modifier (RAlt::NumLock using Autohotkeys) **only works as tap & release
  ;; Rwin is Alt (not used in current laptop)
  ;; NOTE: only negative of this set-up is RAlt as numlock -> Alt is awkward push & release
  ;; https://www.autohotkey.com/
  (setq w32-pass-lwindow-to-system t
		w32-recognize-altgr nil
		W32-enable-caps-lock nil
		w32-pass-rwindow-to-system nil
		w32-rwindow-modifier 'meta
		w32-apps-modifier 'super
		w32-pass-alt-to-system t
		w32-alt-is-meta t
		w32-scroll-lock-modifier 'hyper
		w32-enable-num-lock nil)
  (w32-register-hot-key [A-])
  (define-key function-key-map (kbd "<kp-numlock>") 'event-apply-alt-modifier)
  (setenv "PATH"
		  (mapconcat
		   #'identity exec-path path-separator))

  ;; set exec-path for latex installation
  (setq exec-path (append (list sej-latex-directory
								"c:/msys64/mingw64/bin"
								"/mingw64/bin/") exec-path)))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-os-specific-windows)
;;; sej-os-specific-windows.el ends here
