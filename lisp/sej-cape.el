;;; sej-cape.el --- cape function  file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive commands for Cape completions.

;;; Code:

(defun sej/cape-capf-ignore-keywords-elisp (cand)
  "Ignore keywords with forms that begin with \":\" (e.g.:history)."
  (or (not (keywordp cand))
      (eq (char-after (car completion-in-region--data)) ?:)))

(defun sej/cape-capf-setup-elisp ()
  "Set completion at point for elisp."
  (setq-local completion-at-point-functions
              `(,(cape-capf-super
                  #'elisp-completion-at-point
                  #'cape-dabbrev)
                cape-file)))

;; Org
(defun sej/cape-capf-setup-org ()
  "Set completion at point for org."
  (setq-local completion-at-point-functions
              `(,(cape-capf-super
                  #'cape-dict
                  #'cape-dabbrev
                  #'cape-history
                  #'cape-abbrev)
                cape-file)))

;;git-commit
(defun sej/cape-capf-setup-git-commit ()
  "Set completion at point in git commits."
  (setq-local completion-at-point-functions
              `(,(cape-capf-super
                  #'cape-dict
                  #'cape-dabbrev
                  #'cape-history
                  #'cape-abbrev)
                cape-file)))

;; Eshell
(defun sej/cape-capf-setup-eshell ()
  "Set completion at point in eshell."
  (setq-local completion-at-point-functions
              `( pcomplete-completions-at-point
                 cape-file
                 cape-dabbrev
                 cape-history
                 cape-abbrev)))


;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-cape)
;;; sej-cape.el ends here
