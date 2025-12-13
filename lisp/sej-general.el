;;; sej-general.el --- general functions file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive general purpose commands

;;; Code:

;;;;; scroll up/down
;; NOTE general, window, movement
(defun sej/scroll-up-one()
  "Scroll screen up one while keeping point in place."
  (interactive)
  (scroll-up 1))

(defun sej/scroll-down-one()
  "Scroll screen down one while keeping point in place."
  (interactive)
  (scroll-down 1))

;;;;; lists
;; NOTE lisp
(defun sej/add-all-to-list (var &rest elems)
  "Add all these elements ELEMS to a list VAR rather than one at a time."
  (dolist (elem (reverse elems))
    (add-to-list var elem)))

;;;;; sej/save-macro
;; NOTE general
(defun sej/save-macro (name)
  "Save a macro with NAME as argument; save the macro at the end of your init file."
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (switch-to-buffer nil))

;;;;; line numbers
;; NOTE buffer
(defun sej/toggle-relative-ln ()
  "Toggle line numbers relative to current, which displays absolute line number."
  (interactive)
  (if (and (boundp 'display-line-numbers-mode) display-line-numbers-mode)
      (display-line-numbers-mode -1)
    (progn
      (setq display-line-numbers-type 'relative)
      (display-line-numbers-mode 1))))

;;;;; dwim quit
;; NOTE general
(defun sej/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(global-set-key [remap keyboard-quit] #'sej/keyboard-quit-dwim)

;;;;; [[http://blog.shanderlam.com/][eval-dwim]]
;; NOTE lisp
(defun sej/eval-dwim (arg)
  "Call eval command you want (Do What I Mean).
If the region is active and option `transient-mark-mode' is on, call
     `eval-region'. Else, call `eval-last-sexp' using (ARG)."
  (interactive "P")
  (if (and transient-mark-mode mark-active)
      (eval-region (region-beginning) (region-end))
    (eval-last-sexp arg)))

;;;;; sej/lisp-on-save-funcs
;; NOTE lisp
;; When saving an elisp file, check-parens then remove its compiled version if
;; there is one, as you'll want to recompile it.

(defun sej/lisp-on-save-funcs ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (check-parens)
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))



;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-general)
;;; sej-general.el ends here
