;;; sej-org.el --- org function file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive commands for use in Orgmode.

;;; Code:

;;;;;; personal sej functions
  (defun sej/org-timer-done-alert ()
    "Alert when timer is done."
    (interactive)
    (tmr "0s" "org timer done!"))
  (add-hook 'org-timer-done-hook #'sej/org-timer-done-alert)

  (defun sej/get-open-org-file ()
    "Pull list of .org files which are open in buffers."
    (buffer-file-name
     (get-buffer
      (org-icompleting-read "Buffer: "
                            (mapcar 'buffer-name
                                    (org-buffer-list 'files))))))

  (defun sej/org-reformat-buffer ()
    "Format the current org buffer, which often fixes and updates org interpreted data."
    (interactive)
    (when (y-or-n-p "Really format current buffer? ")
      (let ((document (org-element-interpret-data (org-element-parse-buffer))))
        (erase-buffer)
        (insert document)
        (goto-char (point-min)))))

  (defun sej/org-remove-link ()
    "Replace an org link by its description or if empty its address."
    (interactive)
    (if (org-in-regexp org-bracket-link-regexp 1)
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description (if (match-end 3)
                               (org-match-string-no-properties 3)
                             (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description))))

  (defun sej/org-fold-hide-drawer-toggle ()
    "Toggle drawer of headline in or above."
    (interactive)
    (save-excursion
      (org-previous-visible-heading 1)
      (org-update-checkbox-count)
      (re-search-forward org-drawer-regexp nil 'noerror)
      (org-fold-hide-drawer-toggle nil 'noerror)))

  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords 'org-mode
                          `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
                             (1 `(face nil
                                       display (space :align-to (- right ,(org-string-width (match-string 2)) 3)))
                                prepend))) t)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-org)
;;; sej-org.el ends here
