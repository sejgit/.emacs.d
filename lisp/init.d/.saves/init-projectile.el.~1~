;;; init-projectile.el --- Projectile init

;;; Commentary:
;; projectile settings

;;; ChangeLog
;; 2017 05 14 SeJ init from purcell/.emacs.d
;; 2017 06 01 simplified & added helm-projectile
;; 2017 08 25 add settings from EOS
;; 2017 08 30 cleanup

;;; Code:

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :config
  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".tern-port")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS")
  (add-to-list 'projectile-globally-ignored-files "GSYMS")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class"))
  
  (use-package helm-projectile
    :ensure t
    :after projectile
    :config
    (setq projectile-completion-system 'helm)
    ;; no fuzziness for projectile-helm
    (setq helm-projectile-fuzzy-match nil)
    (helm-projectile-on))

  (use-package helm-ag
    :ensure t)

  (use-package grep
    :ensure t)


(provide 'init-projectile)
;;; init-projectile.el ends here




