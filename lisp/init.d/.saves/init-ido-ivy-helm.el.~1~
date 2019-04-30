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

;;; Code:

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
;; 	ido-use-virtual-buffers t
;; 	ido-enable-prefix nil
;;         ido-use-filename-at-point 'guess
;;         ido-max-prospects 10
;; 	ido-create-new-buffer 'always
;; 	ido-ignore-extensions t
;; 	ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")))

;; ;; beter finding of M-x
;; (use-package smex
;;   :bind (;; ("M-x" . smex)
;; 	 ("M-X" . smex-major-mode-commands))
;;   :config
;;   (setq smex-auto-update 60)
;;   (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))


(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :defines
  sej-mode-map
  ivy-use-virtual-buffers
  ivy-count-format
  ivy-display-style
  :bind
  (:map sej-mode-map
	("C-s" . swiper)
	("C-c C-s" . isearch-forward)
	;;("C-x C-r" . counsel-recentf)
	("M-x" . counsel-M-x)
	("C-x C-f" . counsel-find-file)
	("C-c g" . counsel-git)
	("C-c j" . counsel-git-grep)
	("C-c k" . counsel-ag)
	("H-a" . counsel-ag)
	;;("C-x l" . counsel-locate)
	("C-c C-r" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy))


(use-package helm
  :ensure t
  :hook (after-init . helm-mode)
  :defines sej-mode-map
  :diminish helm-mode
  :bind
  (:map sej-mode-map
	("C-M-z" . helm-resume)
	;;("C-x C-f" . helm-find-files)
	("C-x o" . helm-occur)
	("M-y" . helm-show-kill-ring)
	("C-h a" . helm-apropos)
	;;   ("C-h m" . helm-man-woman)
	("C-h SPC" . helm-all-mark-rings)
	;;("M-x" . helm-M-x)
	("C-x C-b" . helm-buffers-list)
	("C-x b" . helm-mini)
	))

(use-package helm-swoop
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("M-i" . helm-swoop)
	      ("M-I" . helm-swoop-back-to-last-point)
	      ("C-c M-i" . helm-multi-swoop))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t
        ;; If this value is t, split window inside the current window
        helm-swoop-split-with-multiple-windows t
        ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
        helm-swoop-split-direction 'split-window-vertically
        ;; don't auto select the thing at point
        helm-swoop-pre-input-function (lambda () "")
        ;; If nil, you can slightly boost invoke speed in exchange for text
        ;; color. If I want pretty I'll use helm-occur since it keeps colors
        helm-swoop-speed-or-color nil))

(use-package helm-descbinds
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-h b" . helm-descbinds))
  :init (fset 'describe-bindings 'helm-descbinds))


(provide 'init-ido-ivy-helm)
;;; init-ido-ivy-helm.el ends here



