;;; init-spelling.el --- Initialize emacs spelling settings

;;; Commentary:
;; spelling settings for Emacs

;;; Changelog:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 01 15 add support for thesaurus.el
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 24 change to when statements from writequite.org
;; 2017 09 01 map to sej-mode-map, clean up comments
;; 2017 09 03 add Synosaurus as another Thesaurus option
;; 2018 10 04 remove thesaurus & synosaurus for powerthesaurus (no key required)
;; 2018 10 04 add define-word
;; 2018 10 04 flyspell mouse-map adds
;; 2018 10 17 move osx word definition here & use on osx

;;; Code:

;; main spelling package
(use-package flyspell
  :defines
  sej-mode-map
  :bind
  (:map sej-mode-map
        ("<f8>" . ispell-word)
        ("C-<f8>" . flyspell-mode)
        ("M-<f8>" . flyspell-check-next-highlighted-word)
        ("S-<f8>" . ispell-region))
  :hook (text-mode . flyspell-mode)
  :config
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word) ;;for mac
  (define-key flyspell-mouse-map [mouse-3] #'undefined)

  (setq ispell-personal-dictionary "~/sej.ispell")

  ;; Mostly taken from
  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  (when (executable-find "aspell")
    (setq ispell-program-name (executable-find "aspell")) ;; "/usr/local/bin/aspell"
    (setq ispell-extra-args
          (list "--sug-mode=ultra" ;; ultra|fast|normal|bad-spellers
                "--lang=en_CA"
                "--ignore=4")))

  ;; hunspell
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell"))
    (setq ispell-really-hunspell t)
    (setq ispell-extra-args '("-d en_CA"
                              "-p ~/.flydict")))

  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (setq ispell-dictionary-alist '(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_GB-ise") nil utf-8)
                                  ("canadian" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_CA") nil utf-8)
                                  ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))

  (defun flyspell-check-next-highlighed-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-list-command "list"))

;; PowerThesaurus
(use-package powerthesaurus
  :ensure t
  :bind (:map sej-mode-map
              ("C-c s t" . powerthesaurus-lookup-word-dwim)
              ("s-|" . powerthesaurus-lookup-word-dwim)))



;; Word Definition search for non-osx
(use-package define-word
  :ensure t
  :bind (:map sej-mode-map
              ("C-c s w" . define-word-at-point)
              ("s-\\" . define-word-at-point)))

;; OR

;; pilot osx-dictionary only for osx
;;use osx dictionary when possible
(use-package osx-dictionary
  :if (memq window-system '(mac ns))
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c s w" . osx-dictionary-word-at-point)
              ("s-\\" . osx-dictionary-word-at-point)
              ("C-c s i" . osx-dictionary-search-input)
              ("s-i" . osx-dictionary-search-input)
              ))

(provide 'init-spelling)
;;; init-spelling.el ends here
