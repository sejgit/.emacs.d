;;; init-view.el --- init file for pdf and view packages

;;; Commentary:
;; some packages and settings for viewing files and pdf specifically

;;; ChangeLog
;; 2017 08 23 init SeJ
;; 2018 01 10 add ps-print and sej/pdf print buffer

;;; Code:

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

(provide 'init-view)
;;; init-view.el ends here
