;;; init-view.el --- init file for pdf and view packages

;;; Commentary:
;; some packages and settings for viewing files and pdf specifically

;;; ChangeLog
;; 2017 08 23 init SeJ

;;; Code:

(use-package view
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

(provide 'init-view)
;;; init-view.el ends here
