;;; init-templates.el --- templates for auto-insertion

;;; Commentary:
;;to be auto inserted in new files

;;; ChangeLog
;; 2017 05 17 init SeJ
;; 2017 09 01 update ensure / defines

;;; Code:

(use-package autoinsert
  :ensure t
  :hook (find-file . auto-insert)
  :defines
  auto-insert-query
  auto-insert-directory
  :init
  (setq auto-insert-directory "~/.emacs.d/templates/")
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  :config
  (define-auto-insert ".*\\.py[3]?$" "template.py")
  (define-auto-insert ".*\\.el" "template.el")
  )

(provide 'templates)
;;; init-templates.el ends here
