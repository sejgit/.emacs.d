;;; init-org --- Stephen's Emacs Org-mode configuration.  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Org-mode settings
;;

;;;ChangeLog
;;
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 07 add suggestions from Orgmode for GTD
;; 2017 08 30 map sej-mode-map & comments cleanup
;; 2017 10 19 cleanup of capture & org-agenda files
;; 2018 04 01 org babel settings
;; 2018 04 02 add poporg for editing comments in org-mode popup window
;; 2018 07 22 correct mold project documents moved for journal.org
;; 2018 09 28 tips from ohai
;; 2019 04 30 merge

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package org
  ;;:ensure org-plus-contrib
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
  :functions
  sej/org-capture-get-src-block-string
  which-function
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
  (setq org-directory sej-org-directory)
  (defconst org-file-inbox (concat org-directory "/inbox.org"))
  (defconst org-file-someday (concat org-directory "/someday.org"))
  (defconst org-file-gtd (concat org-directory "/gtd.org"))
  (defconst org-file-journal (concat org-directory "/journal.org"))
  (defconst org-file-notes (concat org-directory "/notes.org"))
  (defconst org-file-code (concat org-directory "/snippets.org"))
  (setq org-replace-disputed-keys t
        org-default-notes-file org-file-notes
        org-capture-bookmark t
        org-refile-use-outline-path 'file
        org-log-done 'note
        org-log-done t
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
                            (sequence "DELIGATE(D)" "CHECK(C)" "|" "VERIFIED(V)")
                            (sequence "|" "CANCELED(x)"))
        org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("WAITING" . (:foreground "blue" :weight bold))
                                 ("DONE" . (:foreground "green" :weight bold))
                                 ("DELIGATE" . (:foreground "blue" :weight bold))
                                 ("VERIFIED" . (:foreground "green" :weight bold))
                                 ("CANCELED" . (:foreground "grey" :weight bold)))
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-startup-folded nil
        )

  (define-skeleton org-skeleton
    "Header info for a emacs-org file."
    "Title: "
    "#+TITLE:" str " \n"
    "#+DATE:" '(org-date-from-calendar) " \n"
    "#+AUTHOR: " '(sej-full-name) "\n"
    "#+email: " '(sej-mail-address) "\n"
    "#+INFOJS_OPT: \n"
    "#+BABEL: :session *C* :cache yes :results output graphics :exports both :tangle yes \n"
    "-----\n\n")
  (global-set-key [C-S-f4] 'org-skeleton)

  (setq sej-project-org-capture-list (list
                                      "p" sej-project-org-capture-text 'entry (list 'file+olp+datetree sej-project-org-capture-file "Journal" ) "* %i%?\n %U"))

  (setq org-capture-templates (append
                               '(
                                 ("i" "Inbox" entry (file+headline org-file-inbox  "Inbox") "* %i%?\n %U")
                                 ("j" "Journal" entry (file+olp+datetree org-file-journal "Journal")  "* %i%?\n %U")
                                 ("n" "Notes" entry (file+headline org-file-notes  "Notes") "* %i%?\n %U")
                                 ("s" "Someday" entry (file+headline org-file-someday  "Someday") "* %i%?\n %U")
                                 ;;("t" "Todo" entry (file+headline org-file-gtd  "Todo") "* TODO %i%?")
                                 ("c" "code snippet" entry (file+headline org-file-code "code snippets")
                                  "* %?\n%(my/org-capture-code-snippet \"%F\")")
                                 )
                               (list sej-project-org-capture-list)))

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

  (defun sej/org-capture-get-src-block-string (major-mode)
    "Given a major mode symbol, return the associated org-src block
string that will enable syntax highlighting for that language

E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

    (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
      (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

  (defun sej/org-capture-code-snippet (f)
    (with-current-buffer (find-buffer-visiting f)
      (let ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
            (func-name (which-function))
            (file-name (buffer-file-name))
            (line-number (line-number-at-pos (region-beginning)))
            (org-src-mode (sej/org-capture-get-src-block-string major-mode)))
        (format
         "file:%s::%s
In ~%s~:
#+BEGIN_SRC %s
%s
#+END_SRC"
         file-name
         line-number
         func-name
         org-src-mode
         code-snippet))))

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :config (org-bullets-mode 1))

  (use-package org-fancy-priorities
    :diminish
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (unless (char-displayable-p ?❗)
      (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :disabled ;; turnoff for now with python3.6 issue
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  (use-package org-dashboard)

  (use-package poporg
    :ensure t
    :bind (:map sej-mode-map
                ("C-c s o" . poporg-dwim)))

  (use-package org-projectile-helm
    :ensure t
    :bind (:map sej-mode-map
                ("C-c s c" . org-projectile-capture-for-current-project))
    :config
    (progn
      (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
      (push (org-projectile-project-todo-entry) org-capture-templates)))
  )

(provide 'init-org)
;;; init-org.el ends here
