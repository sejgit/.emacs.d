;;; sej-buffers.el --- buffer functions file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive commands to manipulate buffers

;;; Code:

;;;;; sej/dos2unix
;; convert the current buffer to UNIX file format
;; not bound
(defun sej/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;;; sej/unix2dos
;; convert the current buffer to DOS file format
;; not bound
(defun sej/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;;;; sej/save-buffer-as-utf8
;; revert a buffer with coding-system and save as utf-8
(defun sej/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

;;;;; sej/delete-buffer-contents-no-matter-properties
(defun sej/delete-buffer-contents-no-matter-properties ()
  "Delete the buffer contents no matter the properties."
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer)))

;;;;; sej/remove-all-buffer-properties
(defun sej/remove-all-buffer-properties ()
  "Remove text properties like read-only in the buffer contents."
  (interactive)
  (let ((inhibit-read-only t)) (set-text-properties (point-min) (point-max) ())))

;;;;; sej/quit-and-kill-auxiliary-windows
(defun sej/quit-and-kill-auxiliary-windows ()
  "Kill buffer and its window on quitting."
  (local-set-key (kbd "q") 'kill-buffer-and-window))
(add-hook 'special-mode 'sej/quit-and-kill-auxiliary-windows)
(add-hook 'compilation-mode-hook 'sej/quit-and-kill-auxiliary-windows)

;;;;; sej/quit-other
(defun sej/quit-other()
  "Quit other window."
  (interactive)
  (save-excursion
    (other-window 1)
    (quit-window)))


;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-buffers)
;;; sej-buffers.el ends here
