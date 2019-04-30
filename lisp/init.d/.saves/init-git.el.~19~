;;; init-git.el --- Git related configuration
;;
;;; Commentary:
;; git related configuration for Emacs
;;
;;; ChangeLog:
;; 2017 05 14 init SeJ from purcell/.emacs.d
;; 2017 06 12 add font-awesome git icon
;; 2017 08 29 map to sej-mode-map & documentation & defer/ensure
;; 2017 08 30 deleted a few packages & documented the rest
;; 2017 09 18 add full screen for magit-status and return to previous on quit
;; 2017 09 20 move init-gist.el to here & delete file
;; 2018 04 02 add magit-repository-directories for magit-list-repositories
;; 2018 06 28 add magit-todos
;; 2018 09 27 some ahai tips  add git-gutter

;;; Code:

;; gist client
(use-package gist
  :ensure t
  :defines sej-mode-map
  :bind
  (:map sej-mode-map
	      ("C-x G" . gist-list)
	      ("H-G" . gist-list)))

;; git on Emacs https://github.com/magit/magit
(use-package magit
  :ensure t
  :defines  sej-mode-map
  :functions
  (magit-tools-mode
   my-vc-git-mode-line-string
   vc-git-mode-line-string)
  :bind
  (:map sej-mode-map
	      ("<f12>" . magit-status)
	      ("C-x g" . magit-status)
	      ("C-x M-g" . magit-dispatch-popup)
	      :map magit-status-mode-map
	      ("C-M-<up>" . magit-section-up)
	      ("q" . magit-quit-session))
  :config

  ;; force magit to open in one window in the current frame when called
  (use-package fullframe
    :ensure t
    :config
    (fullframe magit-status magit-mode-quit-window))

  ;; set directories to search for magit-list-repositories
  (setq magit-repository-directories '(("~/" . 3)))

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)
    (sej/dashboard-fix-registers))

  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen)
    (sej/dashboard-fix-registers))

  (defun sej/remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
	      (setcdr last (cddr last))
	      list)))

  (defun sej/dashboard-fix-registers nil
    "My fix to cleanse the register-alist of magit-fullscreen return info"
    (interactive)
    (let ((count (safe-length register-alist)))
      (while (>= count 0)
	      (if (equal :magit-fullscreen (-first-item (nth count register-alist)))
	          (setq register-alist (sej/remove-nth-element count register-alist))
	        )
	      (setq count (- count 1)))))


  (setq-default magit-diff-refine-hunk t)
  (fullframe magit-status magit-mode-quit-window)
  (add-hook 'git-commit-mode-hook 'goto-address-mode)
  (when (eq system-type 'darwin)
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

  (defun my-vc-git-mode-line-string (orig-fn &rest args)
    "Replace Git in modeline with font-awesome git icon via ORIG-FN and ARGS."
    (let ((str (apply orig-fn args)))
      (concat [#xF1D3] ":" (substring-no-properties str 4))))

  (advice-add #'vc-git-mode-line-string :around #'my-vc-git-mode-line-string)

  ;; WORKAROUND https://github.com/magit/magit/issues/2395
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)
  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode)))

;; M-x git-blamed-mode to turn on view with commits
(use-package git-blamed
  :ensure t
  :after magit)

;; for editing gitignore files
(use-package gitignore-mode
  :ensure t
  :after magit)

;; for editing gitconfig files
(use-package gitconfig-mode
  :ensure t
  :after magit)

;; see your file over time
;; - First do M-x git-timemachine
;; Use the following keys to navigate historic version of the file

;; p Visit previous historic version
;; n Visit next historic version
;; w Copy the abbreviated hash of the current historic version
;; W Copy the full hash of the current historic version
;; g Goto nth revision
;; q Exit the time machine.
;; b Run magit-blame on the currently visited revision (if magit available).
;;
;; Do NOT call git-timemachine-mode or git-timemachine-show-previous-revision
;; or other functions directly!
(use-package git-timemachine
  :ensure t
  :after magit
  )

;; popup to show commit
(use-package git-messenger
  :ensure t
  :bind
  ;; Though see also vc-annotate's "n" & "p" bindings
  (:map sej-mode-map
	      ("C-x M" . git-messenger:popup-message)
	      ("H-m" . git-messenger:popup-message)))

;; easy way to clone a github M-x github-clone (respository directory)
(use-package github-clone
  :ensure t
  :commands github-clone)

;; ;; DONE: move to Melpa when it arrives to list-packages
(use-package magit-todos
  :ensure t
  ;;   :quelpa (magit-todos :fetcher github :repo "alphapapa/magit-todos")
  ;;   :after (a anaphora async dash f hl-todo pcre2el s)
  :hook (magit-mode . magit-todos-mode)
  :defines magit-todos-recursive
  :config
  (setq magit-todos-recursive t
	      magit-todos-depth 100)
  ;;   ;; (setq magit-todos-require-colon t)  -- default - change to nil to catch all
  (magit-todos-mode))

(use-package diff-hl
  :ensure t
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode))

;; Mark uncommitted changes
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(provide 'init-git)
;;; init-git.el ends here
