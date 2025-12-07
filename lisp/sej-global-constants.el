;;; sej-global-constants.el --- sej Emacs global constants -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Provides interactive commands to switch between GitHub accounts
;; using the gh CLI, useful when pushing to different remotes in Magit.

;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/freebsdp
  (eq system-type 'berkeley-unix)
  "Are we running on a FreeBSD system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running on a graphic Mac system?")

(defconst sys/mac-x86-p
  (and (string= (substring system-configuration 0 6) "x86_64") sys/macp)
  "Are we running X86 Mac system?")

(defconst sys/mac-AA64-p
  (and (string= (substring system-configuration 0 7) "aarch64") sys/macp)
  "Are we running Apple Silicon Mac system?")

;; specific vars for different systems
(defvar sej/menu-height -30
  "Menu-height used to calculate frame adjustments.")
(cond (sys/mac-x86-p
	   (setq sej/menu-height -32)) ;; x86 Intel mac
	  (sys/mac-AA64-p
	   (setq sej/menu-height -37)) ;; Apple silicon mac
	  (t
	   (setq sej/menu-height -30))) ;; default for other

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/freebsd-x-p
  (and (display-graphic-p) sys/freebsdp)
  "Are we running under graphic FreeBSD system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")


;;;;; customization variables set
;; set-up Emacs customizations choices which are then modified by custom.el
(defgroup sej nil
  "SeJ Emacs customizations."
  :group 'convenience)

(defcustom sej-homepage "https://github.com/sejgit/.emacs.d"
  "The Github page of Emacs Owner."
  :type 'string)

(defcustom sej-full-name "Stephen Jenkins"
  "Set user full name."
  :type 'string)

(defcustom sej-mail-address "random@gmail.com"
  "Set user email address."
  :type 'string)

(defcustom sej-dashboard t
  "If Non-nil, use dashboard at start-up, otherwise will restore previous session."
  :type 'boolean)

(defcustom sej-org-directory "~/Documents/orgtodo"
  "Set org directory."
  :type 'string)

(defcustom sej-latex-directory "/Library/TeX/texbin"
  "Directory for Latex."
  :type 'string)


(provide 'sej-global-constants)
;;; sej-global-constants.el ends here
