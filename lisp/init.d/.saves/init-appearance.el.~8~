;;; init-appearance.el --- packages & settings for appearance

;;; Commentary:
;; merging of settings and other package files into an appearance centric file

;;; ChangeLog:
;; 2017 01 06 init SeJ moved from init-misc-pkgs.el
;;                                init-aggressive-indent.el
;;                                init-golden-ratio.el
;; 2017 11 29 clean up use-package and add :hook
;; 2017 12 26 add dimmer
;; 2018 03 19 dimmer percent to fraction
;; 2018 07 22 try out eyeliner
;; 2018 09 24 some chai tips w/ ethan-wspace added
;; 2018 09 28 moved rainbow-mode from init-misc-filetypes
;;; Code:

(use-package ethan-wspace
  :ensure t
  :demand t
  :commands global-ethan-wspace-mode
  :config
  (global-ethan-wspace-mode 1)
  :bind ("C-c w" . ethan-wspace-clean-all)
  :diminish ethan-wspace-mode)

(use-package eyeliner
  :quelpa (eyeliner :fetcher github :repo "dustinlacewell/eyeliner")
  :requires (dash magit spaceline all-the-icons)
  :config
  (require 'eyeliner)
  (eyeliner/install))

(use-package spaceline
  :ensure t)

(use-package dimmer
  :ensure t
  :defer 5
  :config
  (setq dimmer-fraction 0.20)
  (dimmer-mode))

(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode)
  :defines sej-mode-map
  :diminish golden-ratio-mode
  :config
  (setq golden-ratio-auto-scale t)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window)

    (defun sej/helm-alive-p ()
    (if (boundp 'helm-alive-p)
  (symbol-value 'helm-alive-p)))

  (add-to-list 'golden-ratio-inhibit-functions 'sej/helm-alive-p) )

;; hightlight-numbers in a special way
(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

;; dtrt-indent to automatically set the right indent for other people's files
(use-package dtrt-indent
  :ensure t
  :defer 2
  :diminish
  :config
  ;; (setq dtrt-indent-active-mode-line-info "")
  )

(use-package aggressive-indent
  :ensure t
  :hook (after-init . global-aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; Highlight the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :defer 5
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :defer 5
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; rainbow-delimiters-mode - multicoloured brackets
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 1.0))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.0))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.0))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.0))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.0))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.0))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "gray" :height 1.0))))
   '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 1.0))))
   ))

(use-package rainbow-mode
  :ensure t
  :defer 0.1
  :pin gnu
  :hook (after-init . rainbow-mode))

;; show icons for modes
(use-package mode-icons
  :ensure t
  :defines
  mode-icons-desaturate-inactive
  :config
  (setq mode-icons-desaturate-inactive nil)
  (mode-icons-mode))

;; macos only to make appearance of titlebar match theme
(use-package ns-auto-titlebar
  :if (memq window-system '(mac ns))
  :ensure t)


(provide 'init-appearance)
;;; init-appearance.el ends here
