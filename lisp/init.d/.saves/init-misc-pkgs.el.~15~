;;; init-misc-pkgs.el --- miscellaneous settings and a few small packages

;;; Commentary:
;;Lots of small packages not deserving of their own file so far.

;;; ChangeLog:
;; 2017 01 06 init SeJ moved from init-look-and-feel.el the package setups
;;            add google-this ::search google with C-/ return
;;            add volatile-highlights  ::temporarily highlight pasting changes
;;            add rainbow-delimiters ::dired mode for colours
;;            add saveplace ::return to the same place in saved file
;;            add conf-mode :: for editing conf/ini files
;;            remove zenburn-theme ::used from pragmatic Emacs
;;            change from req-package to use-package
;; 2017 01 10 add swiper to M-s from pragmatic Emacs
;;	      add crux to move to biginning of line intelligently
;;	      add avy for efficient movement through search
;;	      move swiper to own file & add ivy-dired-recent-dirs()
;; 2017 01 16 add drag-stuff to move highlighted region around
;;            add beacon mode to highlight cursor when moved
;; 2017 03 30 move magit & pyenv-mode-auto to init-python.el
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 10 add bookmark+
;;            add rpn-calc
;; 2017 05 12 mods from purcell/emacs.d
;; 2017 05 17 add help-fns+.el
;; 2017 05 28 add whole-line-or-region
;; 2017 06 12 add mode-icons
;; 2017 06 19 add no-littering
;; 2017 08 02 add beginend mode
;; 2017 08 22 add midnight mode
;; 2017 08 23 add comments to packages
;;            add expand-region, vlf
;; 2017 08 25 add undo-tree
;; 2017 08 28 add smartscan & dtrt-indent & highlight-numbers
;; 2017 08 30 clean-up, defer, map to sej-mode-map
;; 2017 09 01 turn off for now as not using features
;; 2017 09 04 move indent-guide, page-break-lines, whitespace-cleanup-mode to init-writing.el
;; 2017 09 06 removed no-littering as was messing with backups
;; 2017 09 07 move modes for editing filetypes to init-misc-filetypes.el
;; 2017 09 20 move some packages to init-appearance.el or init-movement.el
;;            move which-key in from init-which-key.el and delete file
;; 2017 11 17 modified which-key to add binds and side-window-right-bottom
;; 2018 06 22 move from fic-mode to hl-todo-mode delete file init-fic-ext.el
;; 2018 07 02 add osx-dictionary to get definitions using built in osx dictionary
;; 2018 08 06 add paredit for use in js2 & json modes
;; 2018 08 07 add try to allow 'trying' a package
;; 2018 09 27 update of which-key ohai tip

;;; Code:

(use-package try
  :ensure t)

(use-package paredit
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
	      ("H-p" . hl-todo-previous)
	      ("H-n" . hl-todo-next)
	      ("H-o" . hl-todo-occur)))

(use-package which-key
  :ensure t
  :demand t
  :hook (after-init . which-key-mode)
  :commands which-key-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
	            ("C-h C-m" . which-key-show-major-mode)
              ("C-h C-k" . which-key-show-top-level))
  :config
  (which-key-setup-minibuffer))

;; undo tree mode to improve undo features remove C-/ in my keymap for use with dabbrev
(use-package undo-tree
  :ensure t
  :defines sej-mode-map
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :bind (:map sej-mode-map ("C-/" . undo-tree-undo))
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
	      (quote (("" . "~/.local/emacs/undo_hist")))))

;; expand selection region larger & smaller
(use-package expand-region
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("s-=" . er/expand-region)
	      ("s--" . er/contract-region)))

;; vlf lets you handle very large files for viewing
(use-package vlf-setup
  :ensure vlf
  :commands (vlf vlf-occur-load vlf-ediff-files))

;; midnight mode to clear buffers at midnight
(use-package midnight
  :ensure t
  :defer 60
  :config
  (customize-set-variable 'midnight-mode t))

;; google-this
(use-package google-this
  :ensure t
  :diminish google-this-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-c g" . google-this)
	      ("s-g" . google-this))
  :config
  (google-this-mode 1))

;; writable grep buffer and apply the changes to files
(use-package wgrep
  :ensure t
  :defer 5
  :init
  (setq-default grep-highlight-matches t
		grep-scroll-output t)
  :config
  (when (eq system-type 'darwin)
    (setq-default locate-command "which")
    (setq exec-path (append exec-path '("/usr/local/bin"))))

  (when (executable-find "ag")
    (use-package ag
      :ensure t)
    (use-package wgrep-ag
      :ensure t)
    (setq-default ag-highlight-search t)
    (define-key sej-mode-map (kbd "M-?") 'ag-project)))

;; helful is an improved help-fns & help-fns+
(use-package helpful
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ;;("C-h f" . helpful-function)
	      ("C-h c" . helpful-command)
	      ("C-h M" . helpful-macro)
	      ("C-h v" . helpful-variable)))

;; operate on current line if region undefined
(use-package whole-line-or-region
  :ensure t
  :defer 5
  :config
  (whole-line-or-region-global-mode t))

;;use osx dictionary when possible
(use-package osx-dictionary
  :if (memq window-system '(mac ns))
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-c d" . osx-dictionary-word-at-point)
	      ("C-c i" . osx-dictionary-search-input)))

(provide 'init-misc-pkgs)
;;; init-misc-pkgs.el ends here
