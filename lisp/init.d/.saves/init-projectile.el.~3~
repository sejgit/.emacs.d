;;; init-projectile.el --- Projectile init

;;; Commentary:
;; projectile settings

;;; ChangeLog
;; 2017 05 14 SeJ init from purcell/.emacs.d
;; 2017 06 01 simplified & added helm-projectile
;; 2017 08 25 add settings from EOS
;; 2017 08 30 cleanup
;; 2018 03 19 move helm-projectile to helm init file
;; 2018 08 28 updates for projectile
;; 2018 09 28 add redundant bind for helm-projectile
;;; Code:

(use-package projectile
  :ensure t
  :defines sej-mode-map
  ;;  :diminish projectile-mode
  :bind (:map sej-mode-map
	            ("s-p" . projectile-command-map)
	            ("C-c p" . projectile-command-map))
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
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (setq projectile-project-search-path '("~/Projects/" "~/" "~/Documents/"))
  )

(use-package helm-projectile
  :ensure t
  :after projectile
  :bind (:map sej-mode-map
              ("C-c C-f" . helm-projectile-fuzzy-match))
  :config
  (setq projectile-completion-system 'helm)
  ;; no fuzziness for projectile-helm
  (setq helm-projectile-fuzzy-match nil)
  (helm-projectile-on))

(use-package helm-ag
  :ensure t)

(use-package grep
  :ensure t)

(use-package emr
  :ensure t
  ;; Just hit M-RET to access your refactoring tools in any supported mode.
  :bind (:map sej-mode-map
              ("M-RET" . emr-show-refactor-menu))
  :config
  (add-hook 'prog-mode-hook 'emr-initialize))

(provide 'init-projectile)
;;; init-projectile.el ends here
