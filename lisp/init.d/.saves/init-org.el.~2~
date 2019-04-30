;;; init-org --- Stephen's emacs init.org.el file
;;; Commentary:
;; org-mode settings

;;;ChangeLog
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 07 add suggestions from Orgmode for GTD
;; 2017 08 30 map sej-mode-map & comments cleanup
;; 2017 10 19 cleanup of capture & org-agenda files

;;; Code:

(use-package org
  :ensure t
  :defines
  sej-mode-map
  org-capture-bookmark
  org-capture-templates
  org-agenda-window-setup
  org-agenda-span
  org-agenda-skip-scheduled-if-deadline-is-shown
  org-agenda-todo-ignore-deadlines
  org-agenda-todo-ignore-scheduled
  org-agenda-sorting-strategy
  org-agenda-skip-deadline-prewarning-if-scheduled
  :mode ("\\.org$" . org-mode)
  :hook ((org-mode . flyspell-mode)
	 (org-mode . writegood-mode))
  :bind (:map sej-mode-map
	      ("<f1>" . org-mode)
	      ("C-c l" . org-store-link)
	      ("C-c c" . org-capture)
	      ("C-c a" . org-agenda)
	      :map org-mode-map
	      ("C-M-\\" . org-indent-region)
	      ("S-<left>" . org-shiftleft)
	      ("S-<right>" . org-shiftright)
	      ("C-x c o h" . helm-org-headlines)
)
  :config
  (if (string-equal system-type "windows-nt")
      (setq org-directory "C:/Users/NZ891R/gdrive/todo")
    (setq org-directory "~/gdrive/todo"))
  (defconst org-file-inbox (concat org-directory "/inbox.org"))
  (defconst org-file-someday (concat org-directory "/someday.org"))
  (defconst org-file-gtd (concat org-directory "/gtd.org"))
  (defconst org-file-journal (concat org-directory "/journal.org"))
  (defconst org-file-notes (concat org-directory "/notes.org"))
  (setq org-default-notes-file org-file-notes
	org-tags-column 50
	org-capture-bookmark t
	org-refile-use-outline-path 'file
	org-startup-folded 'showeverything
	org-log-done 'note
	org-tags-column 75
	org-log-done t
	org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
			    (sequence "DELIGATE(D)" "CHECK(C)" "|" "VERIFIED(V)")
			    (sequence "|" "CANCELED(x)"))
	org-todo-keyword-faces '(("TODO" . org-warning)
				 ("WAITING" . (:foreground "blue" :weight bold))
				 ("DONE" . (:foreground "green" :weight bold))
				 ("DELIGATE" . (:foreground "blue" :weight bold))
				 ("VERIFIED" . (:foreground "green" :weight bold))
				 ("CANCELED" . (:foreground "grey" :weight bold))))
  (setq org-capture-templates
	'(
	  ("i" "Inbox" entry (file+headline org-file-inbox  "Inbox") "* %i%?\n %U")
	  ("j" "Journal" entry (file+datetree org-file-journal "Journal")  "* %i%?\n %U")
	  ("n" "Notes" entry (file+headline org-file-notes  "Notes") "* %i%?\n %U")
	  ("s" "Someday" entry (file+headline org-file-someday  "Someday") "* %i%?\n %U")
	  ("t" "Todo" entry (file+headline org-file-gtd  "Todo") "* TODO %i%?")
	  ))


  ;; org-mode agenda options
  (setq org-agenda-files (list org-file-inbox org-file-journal org-file-notes org-file-someday org-file-gtd)
	org-refile-targets '((org-file-gtd :maxlevel . 3)
			     (org-file-someday :maxlevel . 1))
	org-agenda-window-setup (quote current-window) ;open agenda in current window
	org-deadline-warning-days 7 ;warn me of any deadlines in next 7 days
	org-agenda-span (quote fortnight) ;show me tasks scheduled or due in next fortnight
	org-agenda-skip-scheduled-if-deadline-is-shown t ;don't show tasks as scheduled if they are already shown as a deadline
	org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
	org-agenda-sorting-strategy ;sort tasks in order of when they are due and then by priority
	(quote
	 ((agenda deadline-up priority-down)
	  (todo priority-down category-keep)
	  (tags priority-down category-keep)
	  (search category-keep))))

  )

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config (org-bullets-mode 1))

(use-package org-dashboard
  :ensure t
  :commands org-dashboard-display)

(provide 'init-org)
;;; init-org.el ends here
