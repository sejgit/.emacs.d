;;; init-js.el --- Some helpful Javascript helpers

;;; Commentary:
;; Part of the Emacs Starter Kit
;; NB: js-mode is part of Emacs since version 23.2 (with an alias
;; javascript-mode). It is derived and updated from Espresso mode.

;;; ChangeLog:
;; 2017 05 09 init copied from part of Emacs Starter Kit
;; 2017 08 30 cleaned up some comments

;;; Code:
(defvar esk-js-mode-hook nil)
(defun run-esk-js-mode-hook ()
  "Make a function to run js hooks."
  (run-hooks 'esk-js-mode-hook))

(defmacro esk-configure-javascript (name)
  "Javascript configuration using (NAME)."
  (let ((sym (intern name))
        (mode (intern (concat name "-mode")))
        (hook (intern (concat name "-mode-hook")))
        (keymap (intern (concat name "-mode-map")))
        (indent (intern (concat name "-indent-level"))))
    `(progn
       (autoload ',mode ,name ,(concat "Start " name "-mode") t)
       (add-to-list 'auto-mode-alist '("\\.js$" . ,mode))
       (add-to-list 'auto-mode-alist '("\\.json$" . ,mode))
       (add-hook ',hook 'moz-minor-mode)
       (add-hook ',hook 'esk-paredit-nonlisp)
       (add-hook ',hook 'run-coding-hook)
       (add-hook ',hook 'run-esk-js-mode-hook)
       (setq ,indent 2)

       (eval-after-load ',sym
         '(progn (define-key ,keymap "{" 'paredit-open-curly)
                 (define-key ,keymap "}" 'paredit-close-curly-and-newline)
                 (define-key ,keymap (kbd ",") 'self-insert-command))))))

(defun pretty-functions ()
  "Pretty JS functions."
  (font-lock-add-keywords
   nil `(("\\(function *\\)("
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1) "ƒ")
                    nil))))))
(add-hook 'esk-js-mode-hook 'pretty-functions)

(if (< (string-to-number emacs-version) 23.2)
    (esk-configure-javascript "espresso")
  (esk-configure-javascript "js"))

(provide 'init-js)
;;; init-js.el ends here
