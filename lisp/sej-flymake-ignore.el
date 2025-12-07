;;; sej-flymake-ignore.el --- Per-line/region suppression for Flymake -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Provides interactive commands to ignore Flymake diagnostics.
;; Ignores lines containing 'flymake:ignore', ignores the next line if current
;; line has 'flymake:ignore-next-line', and ignores regions between
;; 'flymake:ignore-start' and 'flymake:ignore-end'."

;;; Code:

(defun sej/flymake--should-ignore-pos (pos)
  "Return non-nil if POS is in a line/region marked to ignore Flymake diagnostics.
Ignores lines containing 'flymake:ignore', ignores the next line if current
line has 'flymake:ignore-next-line', and ignores regions between
'flymake:ignore-start' and 'flymake:ignore-end'."
  (save-excursion
    (goto-char pos)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           ;; Current line markers
           (cur-ignore (save-excursion
                         (re-search-forward "\\bflymake:ignore\\b" eol t)))
           (cur-ignore-next (save-excursion
                              (re-search-forward "\\bflymake:ignore-next-line\\b" eol t)))
           ;; Region markers
           (in-ignore-region
            (save-excursion
              (let ((start (save-excursion
                             (when (re-search-backward "\\bflymake:ignore-start\\b" nil t)
                               (point))))
                    (end   (save-excursion
                             (when (re-search-forward "\\bflymake:ignore-end\\b" nil t)
                               (point)))))
                (and start (or (not end) (> end pos))))))
           ;; Next line when current has next-line marker
           (next-line-ignored
            (and cur-ignore-next
                 (let ((next-bol (save-excursion
                                   (forward-line 1)
                                   (line-beginning-position)))
                       (next-eol (save-excursion
                                   (forward-line 1)
                                   (line-end-position))))
                   (and (>= pos next-bol) (<= pos next-eol))))))
      (or cur-ignore in-ignore-region next-line-ignored))))

(defun sej/flymake-filter (report-fn &rest diags)
  "Filter DIAGS, dropping any whose position is marked to ignore.
Call REPORT-FN with the kept diagnostics."
  (let ((kept (seq-filter
               (lambda (d)
                 (let ((beg (flymake-diagnostic-beg d)))
                   (not (sej/flymake--should-ignore-pos beg))))
               diags)))
    (funcall report-fn kept)))

(defun sej/flymake-wrap-backend (backend)
  "Wrap BACKEND so that it filters diagnostics via `sej/flymake-filter'."
  (lambda (report-fn &rest args)
    (apply backend
           (lambda (&rest diags)
             (apply #'sej/flymake-filter report-fn diags))
           args)))

(defun sej/flymake-enable-ignores ()
  "Wrap all Flymake backends in the current buffer with ignore filtering."
  (setq-local flymake-diagnostic-functions
              (mapcar #'sej/flymake-wrap-backend flymake-diagnostic-functions)))

(add-hook 'flymake-mode-hook #'sej/flymake-enable-ignores)

(provide 'sej-flymake-ignore)
;;; sej-flymake-ignore.el ends here
