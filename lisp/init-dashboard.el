;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

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
;; Dashboard configurations.
;;

;;; ChangeLog
;;
;; 2016 12 16 init sej
;; 2017 01 06 change from req-package to use-package
;; 2017 11 30 updates to dashboard-items
;; 2018 07 12 update projects items
;; 2018 07 22 remove hook as part of startup
;; 2018 08 02 fixed crashing (I hope)
;;            added initial-buffer-choice for use in emacsclient
;;            added bind & hook
;; 2018 10 17 edit registers settings
;; 2019 05 02 Initialize & Merge with sejgit
;; 2019 05 06 Clean-up

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Dashboard
(when sej-dashboard
  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :defines (persp-save-dir persp-special-last-buffer sej-mode-map)
    :functions (all-the-icons-faicon
                all-the-icons-material
                open-custom-file
                persp-get-buffer-or-null
                persp-load-state-from-file
                persp-switch-to-buffer
                winner-undo
                widget-forward)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :bind (("<f6>" . open-dashboard)
           (:map sej-mode-map
                 ("C-c s d" . open-dashboard))
           (:map dashboard-mode-map
                 ("H" . browse-homepage)
                 ("R" . restore-session)
                 ("L" . persp-load-state-from-file)
                 ("S" . open-custom-file)
                 ("U" . sej-update)
                 ("q" . quit-dashboard)
                 ("C-p" . widget-backward)
                 ("C-n" . widget-forward)
                 ("<up>" . widget-backward)
                 ("<down>" . widget-forward)
                 ("<tab>" . widget-forward)
                 ("C-i" . widget-forward)
                 ("<backtab>" . widget-backward)
                 ))
    :hook (dashboard-mode . (lambda ()
                              (setq-local frame-title-format "")
                              (setq-local tab-width 1)))
    :init (dashboard-setup-startup-hook)
    :config
    ;; (setq dashboard-banner-logo-title "SeJ EMACS")
    (setq dashboard-startup-banner nil)
    (setq dashboard-center-content nil)
    (setq dashboard-show-shortcuts t)
    (setq dashboard-items '((recents  . 15)
                            (bookmarks . 15)
                            (projects . 10)
                            (registers . 10)))

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
             ;; exclude `treemacs' window
             (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                 2
               1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (if (get-buffer dashboard-buffer-name)
          (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (goto-char (point-min))
      (dashboard-goto-recent-files))

    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
           (message "Error: Unable to restore last session -- %s" err)))
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (funcall (local-key-binding "p")))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (funcall (local-key-binding "m")))

    (defun dashboard-goto-registers ()
      "Go to registers."
      (interactive)
      (funcall (local-key-binding "e")))

    ;; Add heading icons
    (defun dashboard-insert-heading-icon (heading &optional _shortcut)
      (when (display-graphic-p)
        ;; Load `all-the-icons' if it's unavailable
        (unless (featurep 'all-the-icons)
          (require 'all-the-icons nil t))

        (insert (cond
                 ((string-equal heading "Recent Files:")
                  (all-the-icons-octicon "file-text" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Bookmarks:")
                  (all-the-icons-octicon "bookmark" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Registers:")
                  (all-the-icons-octicon "clippy" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
                 ((string-equal heading "Projects:")
                  (all-the-icons-octicon "file-directory" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))))
        (insert " ")))
    (advice-add #'dashboard-insert-heading :before #'dashboard-insert-heading-icon)

    ;; Add file icons
    ;; MUST redefine the sections because of the macro `dashboard-insert-section-list'
    (defmacro dashboard-insert-section-list (section-name list action &rest rest)
      "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST to widget creation."
      `(when (car ,list)
         (mapc (lambda (el)
                 (let ((widget nil))
                   (insert "\n    ")
                   (when (display-graphic-p)
                     (insert (if-let ((path (car (last (split-string ,@rest " - ")))))
                                 (if (file-directory-p path)
                                     (cond
                                      ((and (fboundp 'tramp-tramp-file-p)
                                            (tramp-tramp-file-p default-directory))
                                       (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                                      ((file-symlink-p path)
                                       (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                                      ((all-the-icons-dir-is-submodule path)
                                       (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                                      ((file-exists-p (format "%s/.git" path))
                                       (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                                      (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                                           (apply (car matcher) (list (cadr matcher) :v-adjust 0.01)))))
                                   (all-the-icons-icon-for-file (file-name-nondirectory path)))
                               (all-the-icons-octicon "clippy" :height 1.0 :v-adjust 0.01)
                               ))
                     (insert "\t"))
                   (setq widget
                         (widget-create 'push-button
                                        :action ,action
                                        :mouse-face 'highlight
                                        :button-prefix ""
                                        :button-suffix ""
                                        :format "%[%t%]"
                                        ,@rest))))
               ,list)))

    (defmacro dashboard-insert-shortcut (shortcut-char
                                         search-label
                                         &optional no-next-line)
      "Insert a shortcut SHORTCUT-CHAR for a given SEARCH-LABEL.
Optionally, provide NO-NEXT-LINE to move the cursor forward a line."
      `(progn
         (eval-when-compile (defvar dashboard-mode-map))
         (let ((sym (make-symbol (format "Jump to \"%s\"" ,search-label))))
           (fset sym (lambda ()
                       (interactive)
                       (unless (search-forward ,search-label (point-max) t)
                         (search-backward ,search-label (point-min) t))
                       ,@(unless no-next-line
                           '((forward-line 1)))
                       (back-to-indentation)
                       (if (display-graphic-p) (widget-forward 1))))
           (eval-after-load 'dashboard
             (define-key dashboard-mode-map ,shortcut-char sym)))))

    ;; Recentf
    (defun dashboard-insert-recents (list-size)
      "Add the list of LIST-SIZE items from recently edited files."
      (recentf-mode)
      (dashboard-insert-section
       "Recent Files:"
       recentf-list
       list-size
       "r"
       `(lambda (&rest ignore) (find-file-existing ,el))
       (abbreviate-file-name el)))

    ;; Bookmarks
    (defun dashboard-insert-bookmarks (list-size)
      "Add the list of LIST-SIZE items of bookmarks."
      (require 'bookmark)
      (dashboard-insert-section
       "Bookmarks:"
       (dashboard-subseq (bookmark-all-names)
                         0 list-size)
       list-size
       "m"
       `(lambda (&rest ignore) (bookmark-jump ,el))
       (let ((file (bookmark-get-filename el)))
         (if file
             (format "%s - %s" el (abbreviate-file-name file))
           el))))

    ;; Projectile
    (defun dashboard-insert-projects (list-size)
      "Add the list of LIST-SIZE items of projects."
      (require 'projectile)
      (projectile-load-known-projects)
      (dashboard-insert-section
       "Projects:"
       (dashboard-subseq (projectile-relevant-known-projects)
                         0 list-size)
       list-size
       "p"
       `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
       (abbreviate-file-name el)))

    ;;
    ;; Registers
    ;;
    (defun dashboard-insert-registers (list-size)
      "Add the list of LIST-SIZE items of registers."
      (require 'register)
      (dashboard-insert-section
       "Registers:"
       register-alist
       list-size
       "e"
       (lambda (&rest ignore) (jump-to-register (car el)))
       (format "%c - %s" (car el) (register-describe-oneline (car el)))))

    (defun dashboard-insert-buttons ()
      "Insert buttions after the banner."
      (interactive)
      (with-current-buffer (get-buffer dashboard-buffer-name)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (search-forward dashboard-banner-logo-title nil t)

          (insert "")
          (widget-create 'url-link
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-octicon "mark-github"
                                                           :height 1.1
                                                           :v-adjust 0.0
                                                           :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Homepage" 'face 'font-lock-keyword-face))
                         :help-echo "Browse homepage"
                         :mouse-face 'highlight
                         sej-homepage)
          (insert " ")
          (widget-create 'push-button
                         :help-echo "Restore previous session"
                         :action (lambda (&rest _) (restore-session))
                         :mouse-face 'highlight
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-material "restore"
                                                            :height 1.35
                                                            :v-adjust -0.24
                                                            :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Session" 'face 'font-lock-keyword-face)))
          (insert " ")
          (widget-create 'file-link
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-octicon "tools"
                                                           :height 1.1
                                                           :v-adjust 0.0
                                                           :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Settings" 'face 'font-lock-keyword-face))
                         :help-echo "Open custom file"
                         :mouse-face 'highlight
                         custom-file)
          (insert " ")
          (widget-create 'push-button
                         :help-echo "Update SeJ Emacs"
                         :action (lambda (&rest _) (sej-update))
                         :mouse-face 'highlight
                         :tag (concat
                               (if (display-graphic-p)
                                   (concat
                                    (all-the-icons-material "update"
                                                            :height 1.35
                                                            :v-adjust -0.24
                                                            :face 'font-lock-keyword-face)
                                    (propertize " " 'face 'variable-pitch)))
                               (propertize "Update" 'face 'font-lock-keyword-face)))
          (insert " ")
          (widget-create 'push-button
                         :help-echo "Help (?/h)"
                         :action (lambda (&rest _) (hydra-dashboard/body))
                         :mouse-face 'highlight
                         :tag (concat
                               (if (display-graphic-p)
                                   (all-the-icons-faicon "question"
                                                         :height 1.2
                                                         :v-adjust -0.1
                                                         :face 'font-lock-string-face)
                                 (propertize "?" 'face 'font-lock-string-face))))
          (insert "\n")

          (insert (concat
                   (propertize (format "%d packages loaded in %s"
                                       (length package-activated-list) (emacs-init-time))
                               'face 'font-lock-comment-face)))
          (insert "\n")
          )))
    (add-hook 'dashboard-mode-hook #'dashboard-insert-buttons)

    (defun dashboard-insert-footer ()
      "Insert footer of dashboard."
      (interactive)
      (with-current-buffer (get-buffer dashboard-buffer-name)
        (let ((inhibit-read-only t))
          (goto-char (point-max))

          (insert "\n\n")
          (insert " ")
          (insert (propertize
                   (format "SeJ, %s" (format-time-string "%Y"))
                   'face font-lock-doc-face))
          (insert "\n"))))
    (add-hook 'dashboard-mode-hook #'dashboard-insert-footer)

    (defhydra hydra-dashboard (:color red :hint none)
      "
^Head^               ^Section^            ^Item^                  ^Dashboard^
^^───────────────────^^───────────────────^^──────────────────────^^───────────────
_U_pdate             _r_: Recent Files    _RET_: Open             _<f2>_: Open
_H_omePage           _m_: Bookmarks       _<tab>_/_C-i_: Next       _Q_: Quit
_R_ecover session    _p_: Projects        _<backtab>_: Previous
_L_ist sessions      _e_: Registers       _C-n_: Next line
_S_ettings                                _C-p_: Previous Line
"
      ("<tab>" widget-forward)
      ("C-i" widget-forward)
      ("<backtab>" widget-backward)
      ("RET" widget-button-press :exit t)
      ("g" dashboard-refresh-buffer :exit t)
      ("r" dashboard-goto-recent-files)
      ("p" dashboard-goto-projects)
      ("m" dashboard-goto-bookmarks)
      ("e" dashboard-goto-registers)
      ("H" browse-homepage :exit t)
      ("R" restore-session :exit t)
      ("L" persp-load-state-from-file :exit t)
      ("S" open-custom-file :exit t)
      ("U" sej-update :exit t)
      ("C-n" next-line)
      ("C-p" previous-line)
      ("<f2>" open-dashboard :exit t)
      ("Q" quit-dashboard :exit t)
      ("q" nil "quit")
      ("C-g" nil "quit"))
    (bind-keys :map dashboard-mode-map
               ("h" . hydra-dashboard/body)
               ("?" . hydra-dashboard/body))))

;; display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :config
  (setq global-page-break-lines-mode t)
  )

(provide 'init-dashboard)
;;; init-dashboard.el ends here
