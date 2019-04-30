;;; template.el --- template for elisp programs

;;; Commentary:
;; to be auto inserted in all new elisp files

;;; Log
;; 2017 05 12 init SeJ

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((((class color)) (:underline "Red"))))
 '(flycheck-warning ((((class color)) (:underline "Orange"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 1.0))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.0))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.0))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.0))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.0))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.0))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "gray" :height 1.0))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 1.0)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (js2-refactor paradox git-gutter+ comment-dwim-2 comment-dwim2 lsp-javascript-typescript javascript-typescript-langserver lsp-css lsp-html lsp-clangd org-plus-contrib lsp-sh lsp-python org define-word powerthesaurus emr anzu lsp-ui company-lsp lsp-mode tern discover-my-major git-gutter-fringe org-projectile-helm yaml-mode writegood-mode whole-line-or-region whitespace-cleanup-mode which-key wgrep-ag web-mode volatile-highlights vlf uptimes undo-tree try toc-org thesaurus textile-mode synosaurus string-inflection spaceline smartparens smart-tab simpleclip shell-pop rainbow-mode rainbow-delimiters quick-preview py-autopep8 poporg pip-requirements php-mode pass paredit osx-dictionary org-dashboard org-bullets olivetti nov neotree mode-icons markdown-mode magit-todos load-dir lispy keychain-environment js2-mode jedi indent-guide highlight-numbers helpful help-fns+ helm-swoop helm-smex helm-projectile helm-fuzzier helm-flycheck helm-flx helm-descbinds helm-dash helm-ag goto-chg google-this golden-ratio gitignore-mode github-clone gitconfig-mode git-timemachine git-messenger git-blamed gist fullframe framemove frame-cmds format-all flycheck-pos-tip flycheck-color-mode-line fic-mode expand-region exec-path-from-shell eshell-prompt-extras emmet-mode elpy elisp-slime-nav dtrt-indent drag-stuff dired-subtree dired-sort dired-rainbow dired-open dired-narrow dired-launch dired-collapse dired+ dimmer diminish diff-hl deft dashboard cyberpunk-theme csv-nav csv-mode crux counsel company-web company-statistics company-shell company-quickhelp company-jedi company-anaconda buffer-move browse-at-remote beginend beacon batch-mode bash-completion auto-compile auctex arduino-mode anaphora all-the-icons-dired aggressive-indent ag adoc-mode a))))
