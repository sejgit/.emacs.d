;;; sej-scratch.el --- scratch functions file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides functions related to *scratch* buffer.
;; *scratch* buffer set-up, creation, no-kill, persistence

;;; Code:

;;;;; scratch buffer set-up
;; initial message
;; bury don't kill scratch
(setq initial-scratch-message "")
(defun kill-buffer-around-advice (kill-current-buffer &rest args)
  "Bury the *scratch* buffer, but never kill it when using KILL-CURRENT-BUFFER ARGS."
  (let ((buffer-to-kill (buffer-name)))
	(if (equal buffer-to-kill "*scratch*")
		(bury-buffer)
	  (apply kill-current-buffer args))))
(advice-add 'kill-current-buffer :around #'kill-buffer-around-advice)

;;;;; sej/create-scratch-buffer
;; as name suggests
(defun sej/create-scratch-buffer nil
  "Create a new scratch buffer to work in (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
		bufname)
	(while (progn
			 (setq bufname (concat "*scratch"
								   (if (= n 0) "" (int-to-string n))
								   "*"))
			 (setq n (1+ n))
			 (get-buffer bufname)))
	(switch-to-buffer (get-buffer-create bufname))
	(lisp-interaction-mode)
	))
(defalias 'create-scratch-buffer 'sej/create-scratch-buffer)
(bind-key* "C-q C-S-s" 'sej/create-scratch-buffer)
(bind-key* "C-q C-s" 'scratch-buffer)

;;;;; Persist all *scratch* buffers across sessions
(defvar sej/scratch-dir (expand-file-name "scratch-buffers/" user-emacs-directory)
  "Directory to persist scratch buffer contents.")

(defun sej/scratch-save ()
  "Save all *scratch* buffers to files in `sej/scratch-dir`."
  (unless (file-exists-p sej/scratch-dir)
    (make-directory sej/scratch-dir t))
  (dolist (buf (buffer-list))
    (let ((bufname (buffer-name buf)))
      (when (string-match "^\\*scratch\\([0-9]*\\)\\*$" bufname)
        (let* ((number (match-string 1 bufname))
               (filename (expand-file-name
                          (concat "scratch" (if (string-empty-p number) "" number) ".txt")
                          sej/scratch-dir)))
          (with-current-buffer buf
            (write-region (point-min) (point-max) filename nil 'silent)))))))

(defun sej/scratch-restore ()
  "Restore all saved scratch buffers from `sej/scratch-dir`."
  (when (file-directory-p sej/scratch-dir)
    (dolist (file (directory-files sej/scratch-dir t "^scratch[0-9]*.txt$"))
      (let* ((basename (file-name-nondirectory file))
             (number (if (string-match "scratch\\([0-9]+\\).txt" basename)
                         (match-string 1 basename)
                       ""))
             (bufname (concat "*scratch" number "*"))
             (buf (get-buffer bufname)))
        ;; Create buffer if it doesn't exist or restore if empty
        (unless buf
          (setq buf (get-buffer-create bufname))
          (with-current-buffer buf
            (lisp-interaction-mode)))
        (when (zerop (buffer-size buf))
          (with-current-buffer buf
            (insert-file-contents file)
            (goto-char (point-max))))))))

(add-hook 'emacs-startup-hook #'sej/scratch-restore)
(add-hook 'kill-emacs-hook #'sej/scratch-save)

(run-with-idle-timer 60 t #'sej/scratch-save)


;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-scratch)
;;; sej-scratch.el ends here
