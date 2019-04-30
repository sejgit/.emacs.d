;;; init-deft.el --- Initialize Emacs deft
;;; Commentary:
;; deft settings for Emacs

;;; ChangeLog
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 29 change bindings to sej-mode-map & add H-d

;;; Code:
(use-package deft
  :defines sej-mode-map deft-text-mode
  :bind (:map sej-mode-map
	      ("<f7>" . deft)
	      ("C-c d" . deft)
	      ("H-d" . deft))
  :config
  (if (string-equal system-type "windows-nt")
      (setq deft-directory "C:/Users/NZ891R/gdrive/todo")
    (setq deft-directory "~/gdrive/todo"))
  (setq deft-use-filename-as-title t
	deft-default-extension "org"
	deft-text-mode (quote (org-mode))
	deft-org-mode-title-prefix t
	deft-use-filter-string-for-filename t
	deft-auto-save-interval 0
	deft-recursive t
	deft-extensions (quote ("org" "text" "md" "markdown" "txt"))
	deft-org-mode-title-prefix t))

(provide 'init-deft)
;;; init-deft.el ends here

