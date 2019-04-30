;;; init-ido-ivy-helm.el --- Stephen's emacs init-ido-ivy-helm.el

;;; Commentary:
;; consolidated ido-ivy-helm settings for Emacs, have been picking & choosing from each lately

;;; ChangeLogs:
;; 2016 12 16
;; 2017 01 09 switch from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 16 add recentf
;; 2017 08 24 move smex to this file
;; 2017 08 30 comment out smex in favour of helm, map to sej-mode-map, comment
;; 2017 09 18 back to ido
;; 2017 09 19 combine ido helm swiper smex into init-ido-ivy-helm.el
;; 2017 12 01 update for new use-package
;; 2018 03 17 update focused on helm
;; 2018 03 21 some shortcut adds from http://tuhdo.github.io/helm-intro.html

;;; Code:

(use-package helm
  :ensure t
  :hook ((after-init . helm-mode)
	       (eshell-mode . (lambda ()
			                    (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
			                    (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))
	       )
  :defines (sej-mode-map projectile-mode-map org-mode-map helm-command-map)
  :diminish helm-mode
  :bind (:map sej-mode-map
	            ("M-x" . helm-M-x)
	            ("C-x C-f" . helm-find-files)
	            ("C-x C-r" . helm-recentf)
	            ("C-x C-b" . helm-buffers-list)
	            ("C-x b" . helm-mini)
	            ("C-c h" . helm-command-prefix)
	            ("C-M-z" . helm-resume)
	            ;;("C-x o" . helm-occur)
	            ("M-y" . helm-show-kill-ring)
	            ("C-h i" . helm-info-emacs)
	            ("C-h a" . helm-apropos)
	            ;;   ("C-h m" . helm-man-woman)
	            ("C-h SPC" . helm-all-mark-rings)
	            ("H-SPC" . helm-all-mark-rings)
	            ("s-b" . helm-mini)
	            ("C-x r l" . helm-source-filtered-bookmarks)
	            ("M-s s" . helm-ag)
	            :map helm-command-map
	            ("<tab>" . helm-execute-persistent-action)
	            ("C-i" . helm-execute-persistent-action)
	            ("C-z" . helm-select-action)
	            ("s-f" . helm-multi-files)
	            ("C-t" . helm-imenu)
	            ("A-x" . helm-register)
	            )
  :config
  (require 'helm-config)
  (require 'helm)
  (helm-mode 1)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-inside-p           t ; open helm buffer inside current window, not occupy whole other window
	      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	      helm-echo-input-in-header-line t
	      helm-mode-fuzzy-match t
	      helm-ff-file-name-history-use-recentf t
	      helm-M-x-fuzzy-match t
	      helm-buffers-fuzzy-matching t
	      helm-recentf-fuzzy-match t
	      helm-lisp-fuzzy-completion t
	      helm-apropos-fuzzy-match t
	      helm-completion-in-region-fuzzy-match t
	      helm-echo-input-in-header-line t
	      helm-follow-mode-persistent t
	      helm-split-window-inside-p t
	      )
  (helm-autoresize-mode t))


(use-package helm-swoop
  :after (helm)
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	            ("M-i" . helm-swoop)
	            ("M-I" . helm-swoop-back-to-last-point)
	            ("C-c M-i /" . helm-multi-swoop)
	            ("C-x M-i" . helm-multi-swoop-all)
	            :map isearch-mode-map
	            ("M-i" . helm-swoop-from-isearch)
	            :map helm-swoop-map
	            ("C-p" . helm-previous-line)
	            ("C-n" . helm-next-line)
	            ("M-i" . helm-multi-swoop-all-from-helm-swoop)
	            ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)
	            :map helm-multi-swoop-map
	            ("C-p" . helm-previous-line)
	            ("C-n" . helm-next-line)
	            )
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  ;;(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  ;;(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  ;;(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t
	      ;; If this value is t, split window inside the current window
	      helm-swoop-split-with-multiple-windows nil
	      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
	      helm-swoop-split-direction 'split-window-vertically
	      ;; If you prefer fuzzy matching
	      helm-swoop-use-fuzzy-match t
	      ;; don't auto select the thing at point
	      helm-swoop-pre-input-function (lambda () "" )
	      ;; Always use the previous search for helm. Remember C-<backspace> will delete entire line
	      helm-swoop-pre-input-function
	      (lambda () (if (boundp 'helm-swoop-pattern)
		                   helm-swoop-pattern ""))

	      ))

(use-package smex
  :ensure t)

(use-package helm-smex
  :after (helm smex)
  :ensure t
  :init
  (setq helm-smex-show-bindings t)
  :bind(([remap execute-extended-command] . helm-smex)
	("M-X" . helm-smex-major-mode-commands))
  )

(use-package helm-descbinds
  :after (helm)
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-h b" . helm-descbinds))
  :init (fset 'describe-bindings 'helm-descbinds))

(use-package helm-ag
  :after (helm)
  :ensure t)

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :bind (:map projectile-mode-map
	            ("C-c p /" . (lambda ()
			                       (interactive)
			                       (helm-ag (projectile-project-root))))
              :map sej-mode-map
              ("C-c C-f" . helm-projectile-find-file-dwim)
              ("C-x C-g" . helm-projectile-grep)
	            )
  :config
  (setq projectile-completion-system 'helm)
  ;; no fuzziness for projectile-helm
  (setq helm-projectile-fuzzy-match t)
  (helm-projectile-on))

(use-package flx
  :ensure t)

(use-package helm-flx
  :ensure t
  :after (helm)
  :config
  (helm-flx-mode +1))

(use-package helm-fuzzier
  :ensure t
  :after (helm)
  :config
  (helm-fuzzier-mode +1))

(use-package helm-dash
  :ensure t
  :init
  (setq helm-dash-browser-func 'eww)
  :after (helm))

(provide 'init-ido-ivy-helm)
;;; init-ido-ivy-helm.el ends here


;; (use-package ido
;;   :ensure t
;;   :defer t
;;   :init (defalias 'list-buffers 'ibuffer)
;;   :commands
;;   ido-everywhere
;;   :config
;;   (ido-mode t)
;;   (ido-everywhere t)
;;   (setq ido-enable-flex-matching nil
;;	ido-use-virtual-buffers t
;;	ido-enable-prefix nil
;;         ido-use-filename-at-point 'guess
;;         ido-max-prospects 10
;;	ido-create-new-buffer 'always
;;	ido-ignore-extensions t
;;	ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")))

;; ;; beter finding of M-x
;; (use-package smex
;;   :bind (;; ("M-x" . smex)
;;	 ("M-X" . smex-major-mode-commands))
;;   :config
;;   (setq smex-auto-update 60)
;;   (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))


;; (use-package ivy
;;   :ensure t
;;   :hook (after-init . ivy-mode))

;; (use-package counsel
;;   :ensure t)

;; (use-package swiper
;;   :ensure t
;;   :defines
;;   sej-mode-map
;;   ivy-use-virtual-buffers
;;   ivy-count-format
;;   ivy-display-style
;;   :bind
;;   (:map sej-mode-map
;;	("C-s" . swiper)
;;	("C-c C-s" . isearch-forward)
;;	;;("C-x C-r" . counsel-recentf)
;;	("M-x" . counsel-M-x)
;;	("C-x C-f" . counsel-find-file)
;;	("C-c g" . counsel-git)
;;	("C-c j" . counsel-git-grep)
;;	("C-c k" . counsel-ag)
;;	("H-a" . counsel-ag)
;;	;;("C-x l" . counsel-locate)
;;	("C-c C-r" . ivy-resume))
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq ivy-display-style 'fancy))
