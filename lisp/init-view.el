;;; init-view.el --- init file for view packages. -*- lexical-binding: t -*-

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
;; some packages and settings for viewing files and pdf specifically

;;; ChangeLog
;;
;; 2017 08 23 init SeJ
;; 2018 01 10 add ps-print and sej/pdf print buffer
;; 2019 04 30 merge with init-utils


;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package view
  :defines (View-scrool-half-page-forward View-scrool-half-page-backward)
  :bind (:map view-mode-map (("e" . 'View-scroll-half-page-forward)
                             ("u" . 'View-scroll-half-page-backward)

                             ;; less like
                             ("N" . View-search-last-regexp-backward)
                             ("?" . View-search-regexp-backward?)
                             ("g" . View-goto-line)
                             ("G" . View-goto-line-last)
                             ;; vi/w3m like
                             ("h" . backward-char)
                             ("j" . next-line)
                             ("k" . previous-line)
                             ("l" . forward-char)))
  :config
  (defun View-goto-line-last (&optional line)
    "goto last line"
    (interactive "P")
    (forward-line (line-number-at-pos (point-max)))))

(use-package doc-view
  :bind (:map doc-view-mode-map (("j" . doc-view-next-line-or-next-page)
                                 ("k" . doc-view-previous-line-or-previous-page)
                                 ;; use 'q' to kill the buffer, not just hide it
                                 ("q" . kill-this-buffer))))

(require 'ps-print)
(when (executable-find "ps2pdf")
  (defun sej/pdf-print-buffer-with-faces (&optional filename)
    "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
    (interactive (list (if current-prefix-arg
                           (ps-print-preprint 4)
                         (concat (file-name-sans-extension (buffer-file-name))
                                 ".ps"))))
    (ps-print-with-faces (point-min) (point-max) filename)
    (shell-command (concat "ps2pdf " filename))
    (delete-file filename)
    (message "Deleted %s" filename)
    (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf"))))

;; PDF reader
(when (display-graphic-p)
  (use-package pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward))
    :config
    (setq pdf-view-midnight-colors '("#ededed" . "#21242b"))
    (setq pdf-annot-activate-created-annotations t)

    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when sys/macp
      (setenv "PKG_CONFIG_PATH"
              "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig"))
    (pdf-tools-install t nil t t)

    ;; Recover last viewed position
    (use-package pdf-view-restore
      :hook (pdf-view-mode . pdf-view-restore-mode)
      :init (setq pdf-view-restore-filename
                  (locate-user-emacs-file ".pdf-view-restore")))))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
  :hook (nov-mode . my-nov-setup))

(provide 'init-view)
;;; init-view.el ends here
