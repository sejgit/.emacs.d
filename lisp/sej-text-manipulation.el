;;; sej-text-manipulation.el --- text manipulation functions file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive text manipulation commands

;;; Code:

;;;;; line

(defun sej/open-new-line()
  "Open new line without breaking and place cursor there."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun sej/open-line-above-and-indent ()
  "Insert a new line above the current one maintaining the indentation."
  (interactive)
  (let ((current-indentation (current-indentation)))
    (beginning-of-line)
    (open-line 1)
    (when (> current-indentation 0)
      (indent-to current-indentation))))

;;;;; word

(defun sej/kill-whole-word ()
  "Kill the current word at point."
  (interactive)
  (backward-word)
  (kill-word 1))

;;;;; buffer
;; - bound to C-q <tab>
(defun sej/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key* "C-q <tab>" 'sej/indent-buffer)

;;;;; region

(defun sej/set-region-writeable (begin end)
  "Remove the read-only text property from the marked region of BEGIN END."
  ;; See http://stackoverflow.com/questions/7410125
  ;; handy when text is read-only and cannot be deleted
  ;; answer to question on stack-overflow
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;;;; sej/number-rectangle

(defun sej/number-rectangle (start end format-string from)
  "Delete text in region-rectangle; number (START to END with FORMAT-STRING FROM)."
  ;; Let's say you have a list like:
  ;; First Item
  ;; Second Item
  ;; Third Item
  ;; Fourth Item
  ;; And you want to number it to look like:
  ;; 1. First Item
  ;; 2. Second Item
  ;; 3. Third Item
  ;; 4. Fourth Item
  ;; This function allows you to hit ***C-x r N ***and specify the pattern
  ;; and starting offset to number lines in rectangular-selection mode
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: "
                      (if (looking-back "^ *" nil nil) "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (cl-loop with column = (current-column)
             while (and (<= (point) end) (not (eobp)))
             for i from from   do
             (move-to-column column t)
             (insert (format format-string i))
             (forward-line 1)))
  (goto-char start))


;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-text-manipulation)
;;; sej-text-manipulation.el ends here
