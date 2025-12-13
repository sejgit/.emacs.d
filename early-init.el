;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2025 Stephen Jenkins

;; Author: Stephen Jenkins
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
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;; 2025-12 Integrated a bunch of updates on early-init from:
;;                Author: James Cherti
;;                URL: https://github.com/jamescherti/minimal-emacs.d
;;

;;; Code:
(message "early-init -- start")

;;;; debug
;; only turned on when needed
(setq debug-on-error t)
(setq debug-on-event t)

;;;; set dirs & src files location

(defvar sej-base-emacs-directory user-emacs-directory
  "Emacs base directory.
Note that this should end with a directory separator.")

(eval-and-compile
  (defsubst emacs-path (path)
	(expand-file-name path sej-base-emacs-directory))

  (setq user-emacs-directory (emacs-path "var/"))
  (setq package-user-dir (emacs-path "elpa"))

  (setq source-directory (expand-file-name "~/src/emacs-dev"))
  (setq find-function-C-source-directory (expand-file-name "~/src/emacs-dev/src"))

  (setq load-path (append load-path (list (emacs-path "lisp"))))
  (delete-dups load-path)
  (setq custom-theme-directory (emacs-path "themes/")))

;;;; should i even be here
(defconst emacs/>=30p
  ( >= emacs-major-version 30 )
  "Emacs is 30 or above.")
(when (not emacs/>=30p)
  (error "This requires Emacs 30 and above")  )


;;;; Garbage collection

;; Backup of `gc-cons-threshold' and `gc-cons-percentage' before startup.
(defvar sej--backup-gc-cons-threshold gc-cons-threshold)
(defvar sej--backup-gc-cons-percentage gc-cons-percentage)

;; Temporarily raise the garbage collection threshold to its maximum value.
;; It will be restored later to controlled values.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(setq garbage-collection-messages nil)

(defun sej--restore-gc-values ()
  "Restore garbage collection values to stored values."
  ;; Increase from 32MB to 100MB for M1 Mac with 16GB RAM
  ;; This prevents GC pauses during scrolling large files
  (setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
  (setq gc-cons-percentage 0.1))  ; GC when heap grows 10%

(add-hook 'after-init-hook #'garbage-collect t)
(add-hook 'emacs-startup-hook #'sej--restore-gc-values 105)

;; GC when Emacs loses focus (less intrusive)
(add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))


;;;; Native compilation and Byte compilation

(if (and (featurep 'native-compile)
		 (fboundp 'native-comp-available-p)
		 (native-comp-available-p))
    ;; Activate `native-compile'
    (setq package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(setq-default native-comp-speed 2
			  native-comp-deferred-compilation t
			  native-comp-async-report-warnings-errors 'silent
			  native-comp-warning-on-missing-source nil)
;; Suppress specific obsolete warnings during native compilation
(setq native-comp-compiler-options
	  '("-O2" "-Wno-deprecated-declarations"))
;; Suppress byte-compile warnings for obsolete functions
(setq byte-compile-warnings '(not obsolete make-local suspicious lexical free-vars))
;; Suppress warnings about lexical binding
(setq byte-compile-error-on-warn nil)
;; Completely silence byte-compile warnings in output
(setq byte-compile-verbose nil)


;;;; Miscellaneous

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

(setq process-adaptive-read-buffering nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;; In PGTK, this timeout introduces latency. Reducing it from the default 0.1
;; improves responsiveness of childframes and related packages.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(when (and (not (daemonp)) (not noninteractive))
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
		inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
		inhibit-startup-buffer-menu t
		inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
				bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
	(setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
	(setq command-line-x-option-alist nil)))


;;;; Performance: Inhibit redisplay
(defun sej--reset-inhibit-redisplay ()
  "Reset inhibit redisplay and force redisplay."
  (setq-default inhibit-redisplay nil)
  (redisplay t)
  (remove-hook 'after-init-hook #'sej--reset-inhibit-redisplay))

(when (and (not (daemonp))
		   (not noninteractive))
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t)
  (add-hook 'after-init-hook #'sej--reset-inhibit-redisplay 99))


;;;; Performance: Inhibit message
(defun sej--reset-inhibit-message ()
  "Reset inhibit message."
  (setq-default inhibit-message nil)
  (remove-hook 'after-init-hook #'sej--reset-inhibit-message))

(when (and (not (daemonp))
		   (not noninteractive))
  (setq-default inhibit-message t)
  (add-hook 'after-init-hook #'sej--reset-inhibit-message))

(when (and (not (daemonp))
		   (not noninteractive))
  (put 'mode-line-format
	   'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (setq mode-line-format nil))))


;;;; Restore values

(defun sej--startup-load-user-init-file (fn &rest args)
  "Advice to reset `mode-line-format'. FN and ARGS are the function and args."
  (unwind-protect
	  ;; Start up as normal
	  (apply fn args)
	;; If we don't undo inhibit-{message, redisplay} and there's an error, we'll
	;; see nothing but a blank Emacs frame.
	(setq-default inhibit-message nil)
	(setq-default inhibit-redisplay nil)
	;; Restore the mode-line
	(unless (default-toplevel-value 'mode-line-format)
	  (setq-default mode-line-format (get 'mode-line-format
										  'initial-value)))))

(advice-add 'startup--load-user-init-file :around
			#'sej--startup-load-user-init-file)


;;;; UI elements

(defvar sej-ui-features '(menu-bar context-menu)
  "List of user interface features to enable in Emacs setup.
This variable holds a list of Emacs UI features that can be enabled:
- context-menu (Enables the context menu in graphical environments.)
- tool-bar (Enables the tool bar in graphical environments.)
- menu-bar (Enables the menu bar in graphical environments.)
- dialogs (Enables both file dialogs and dialog boxes.)
- tooltips (Enables tooltips.)")

(setq frame-title-format "%b – Emacs"
	  icon-title-format "%b – Emacs")

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(unless (memq 'menu-bar sej-ui-features)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (unless (memq window-system '(mac ns))
	(setq menu-bar-mode nil)))

(defun sej--setup-toolbar (&rest _)
  "Setup the toolbar."
  (when (fboundp 'tool-bar-setup)
	(advice-remove 'tool-bar-setup #'ignore)
	(when (bound-and-true-p tool-bar-mode)
	  (funcall 'tool-bar-setup))))

(when (and (not (daemonp))
		   (not noninteractive))
  (when (fboundp 'tool-bar-setup)
	;; Temporarily override the tool-bar-setup function to prevent it from
	;; running during the initial stages of startup
	(advice-add 'tool-bar-setup :override #'ignore)

	(advice-add 'startup--load-user-init-file :after
				#'sej--setup-toolbar)))

(unless (memq 'tool-bar sej-ui-features)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(unless (memq 'tooltips sej-ui-features)
  (when (bound-and-true-p tooltip-mode)
	(tooltip-mode -1)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(unless (memq 'dialogs sej-ui-features)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil))


;;;; frame settings
(modify-all-frames-parameters '((width . 80)
								(height . 50)
								(left . 0)
								(right . 0)
								(left-fringe . 4)
								(right-fringe . 4)
								(internal-border-width . 1)
								(child-frame-border-width . 2)
								(vertical-scroll-bars . nil)
								(horizontal-scroll-bars . nil)
								(tool-bar-lines . 0)
								(ns-appearance . dark)
								(undecorated . t)
								(undecorated-round . t)
								))
(unless (display-graphic-p)
  (add-to-list 'default-frame-alist '(menu-bar-mode . 0) ))


;;;; Security
(setq gnutls-verify-error t)  ; Prompts user if there are certificate issues
(setq tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
(setq gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption


;;;; Package.el
;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(setq use-package-compute-statistics t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)
(setq use-package-verbose nil)
(setq use-package-enable-imenu-support t)
(setq use-package-minimum-reported-time 0.1)
(setq package-quickstart-file
	  (expand-file-name "package-quickstart.el" user-emacs-directory))
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
						 ("gnu"          . "https://elpa.gnu.org/packages/")
						 ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("gnu"    . 99)
								   ("nongnu" . 80)
								   ("melpa"  . 70)
								   ("melpa-stable" . 50)))

(message "early-init -- done")
(provide 'early-init)
;;; early-init.el ends here


