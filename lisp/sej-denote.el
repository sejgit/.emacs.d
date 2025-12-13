;;; sej-denote.el --- denote function file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive commands for Denote packages.

;;; Code:

;;;;;; play nice with diredfl
(defun sej/denote-dired-mode-hook()
  "Function to switch off between `diredfl-mode' and `denote-dired-mode'."
  (denote-dired-mode-in-directories)
  (if denote-dired-mode
      (dired-hide-details-mode +1)
    (diredfl-mode +1)))

(add-hook 'dired-mode-hook #'sej/denote-dired-mode-hook)

;;;;;; denote keyword functions
;; define keywords in text file in denote-directory
(defvar sej/denote-keywords-p (f-join denote-directory "denote-keywords.txt"))

(defun sej/denote-keywords-update (&rest _arg)
  "Update keywords from file."
  (setq sej/denote-keywords-p (f-join denote-directory "denote-keywords.txt"))
  (if (f-exists-p sej/denote-keywords-p)
      (progn (setq denote-known-keywords  (s-split "\n" (f-read sej/denote-keywords-p) t))
             (setq org-tag-persistent-alist (-map #'list denote-known-keywords)))
    (setq denote-known-keywords  "defined-in-denote-keywords.txt"))
  (eval nil))

(defun sej/denote-keywords-edit ()
  "Edit the keywords list."
  (interactive)
  (sej/denote-keywords-update)
  (switch-to-buffer (find-file-noselect sej/denote-keywords-p)))

(defun sej/denote-keywords-dump ()
  "Dump the current used keywords in variable `denote-directory' for refactor purposes."
  (interactive)
  (let (value)
    (dolist (element (denote-directory-files) value)
      (setq value (cons (denote-extract-keywords-from-path element) value)))
    (setq sej/value (sort (delete-dups (apply #'append value))))
    (insert (mapconcat 'identity sej/value "\n"))))

;;;;;; denote colleagues
;; easy way to launch topic specific files either colleagues or topics

(defvar sej/denote-colleagues-p (f-join denote-directory "denote-colleagues.txt")
  "Default file to keep the colleague list, kept in the base variable `denote-directory'." )

(defvar sej/denote-colleagues nil
  "List of names I collaborate with.
There is at least one file in the variable `denote-directory' that has
the name of this person.")

(defvar sej/denote-colleagues-prompt-history nil
  "Minibuffer history for `sej/denote-colleagues-new-meeting'.")

(defun sej/denote-colleagues-edit ()
  "Edit the colleague list by opening `sej/denote-colleagues'."
  (interactive)
  (setq sej/denote-colleagues-p (f-join denote-directory "denote-colleagues.txt"))
  (switch-to-buffer (find-file-noselect sej/denote-colleagues-p)))

(defun sej/denote-colleagues-update (&rest _arg)
  "Update denote colleagues variables."
  (setq sej/denote-colleagues-p (f-join denote-directory "denote-colleagues.txt"))
  (if (f-exists-p sej/denote-colleagues-p)
      (setq sej/denote-colleagues  (s-split "\n" (f-read sej/denote-colleagues-p) t))
    (progn
      (setq sej/denote-colleagues '("none-defined"))
      (message  "Put colleagues and topics in denote-colleagues.txt"))))

(defun sej/denote-colleagues-prompt ()
  "Prompt with completion for a name among `sej/denote-colleagues', using the last input as the default value."
  (let ((default-value (car sej/denote-colleagues-prompt-history)))
    (completing-read
     (format-prompt "New meeting with COLLEAGUE" default-value)
     sej/denote-colleagues
     nil
     'confirm
     nil
     'sej/denote-colleagues-prompt-history
     default-value)))

(defun sej/denote-colleagues-update-file (name)
  "Update the colleagues file with an additional colleague NAME."
  (interactive "sName to add: ")
  (with-temp-file
      sej/denote-colleagues-p
    (insert (mapconcat 'identity (sort (add-to-list 'sej/denote-colleagues name :APPEND)) "\n"))) )

(defun sej/denote-colleagues-get-file (name)
  "Find file in variable `denote-directory' for NAME colleague.
If there are more than one files, prompt with completion for one among them.
NAME is one among `sej/denote-colleagues', which if not found in the list, is
confirmed and added, calling the function `sej/denote-colleagues-update-file'."
  (if-let* ((files
             (let* ((testname (denote-sluggify 'title name))
                    (files (denote-directory-files testname))
                    (RESULT nil))
               (dolist (VAR files RESULT)
                 (if (string-match (denote-retrieve-filename-title VAR) testname)
                     (setq RESULT (cons VAR RESULT))))))
            (length-of-files (length files)))
      (cond
       ((= length-of-files 1)
        (car files))
       ((> length-of-files 1)
        (completing-read "Select a file: " files nil :require-match)))
    (progn (sej/denote-colleagues-update-file name)
           (denote name
                   nil
                   denote-file-type
                   denote-directory
                   (denote-parse-date nil)
                   (alist-get 'standard denote-templates "* ")
                   nil))))

(defun sej/denote-colleagues-new-meeting ()
  "Prompt for the name of a colleague and insert a timestamped heading therein.
The name of a colleague corresponds to at least one file in the variable
`denote-directory'.  In case there are multiple files, prompt to choose
one among them and operate therein."
  (declare (interactive-only t))
  (interactive)
  (sej/denote-colleagues-update)
  (let* ((name (sej/denote-colleagues-prompt))
         (file (sej/denote-colleagues-get-file name))
         (time (format-time-string "%F %a")))  ; add %R if you want the time
    (with-current-buffer (find-file file)
      (goto-char (point-max))
      (org-insert-heading '(16) nil 1)
      (org-return)
      (org-insert-item)
      (end-of-line 0)
      (org-move-to-column 2))))

(defun sej/filter-journal-lines-from-list (lines)
  "Filter out lines containing '/journal/' from a cons cell LINES and return the titles."
  (let ((filtered-lines '()))
    (while lines
      (let ((line (car lines)))
        (unless (string-match-p (rx "journal") line)
          (push (concat (denote-retrieve-filename-title line) "\n" ) filtered-lines)))
      (setq lines (cdr lines)))
    (sort filtered-lines)))

(defun sej/denote-colleagues-dump ()
  "Dump current used colleagues in denote directory for refactor purposes."
  (interactive)
  (let ((value (sej/filter-journal-lines-from-list (denote-directory-files) )))
    (dolist (element value)
      (insert element))))

;;;;;; create denote files in any directory
(defun sej/denote-create-any-dir ()
  "Create new Denote note in any directory.
Prompt for the directory using minibuffer completion."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory (read-directory-name "New note in: " nil nil :must-match)))
    (call-interactively 'denote)))

;;;;;; rename based on front matter at every save
(defun sej/denote-always-rename-on-save-based-on-front-matter ()
  "Rename the current Denote file, if needed, upon saving the file.
Rename the file based on its front matter, checking for changes in the
title or keywords fields.

Add this function to the `after-save-hook'."
  (let ((denote-rename-confirmations nil)
        (denote-save-buffers t)) ; to save again post-rename
    (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
      (ignore-errors (denote-rename-file-using-front-matter buffer-file-name))
      (message "Buffer saved; Denote file renamed"))))

(add-hook 'after-save-hook #'sej/denote-always-rename-on-save-based-on-front-matter)

;;;;;; search denote files
;; bind to M-s d ; d for denote
(defun sej/denote-consult-search ()
  "Search all my Denote files using `consult-ripgrep`."
  (interactive)
  (consult-ripgrep denote-directory)) ;end) of denote use-package

;;;;;; denote journal
;; replacement function to only use year and month for journal title yy-mm.
(defun sej/denote-journal--entry-today (&optional date)
  "Return list of files matching a journal for today or optional DATE.
DATE has the same format as that returned by `denote-parse-date'."
  (interactive)
  (let* ((identifier (format "%s"(format-time-string "%y-%m" date)))
         (files (denote-directory-files identifier))
         (keyword (concat "_" (regexp-quote denote-journal-keyword))))
    (seq-filter
     (lambda (file)
       (string-match-p keyword file))
     files)))

(defun sej/journelly-open ()
  "Open Journelly shared file.

    Journelly is an IOS app written by xenodium.com allowing simple journelling in text/org file format."
  (interactive)
  (find-file-other-window (expand-file-name "Journelly.org" denote-journal-directory)))

;;;;;; denote silo

  (defvar sej/switch-buffer-functions
    nil
    "A list of functions to be called when the current buffer has been changed.
Each is passed two arguments, the previous buffer and the current buffer.")

  (defvar sej/switch-buffer-functions--last-buffer
    nil
    "The last current buffer.")

  (defun sej/denote-silo-update (&rest _arg)
    "Function to update silo & critical variables based on current buffer/file.
Chooses silo based on file being in one of the extras-directories any history since.
Then proceeds to update keywords, colleagues, journal directory & finally refile targets."
    (interactive)
    (let ((file buffer-file-name) existin)
      (if file
          (dolist (elem (-union denote-silo-directories denote-silo-directory-history) )
            (if (file-in-directory-p file elem)
                (setq existin elem))
            (if (and existin (not (equal existin denote-directory)))
                (setq denote-directory existin)))
        (sej/denote-keywords-update)
        (sej/denote-colleagues-update)
        (setq denote-journal-directory (expand-file-name "journal" denote-directory)))
      (setq org-refile-targets '((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 5)
                                 (org-buffer-list :maxlevel . 2)))))

  (defun sej/switch-buffer-functions-run ()
    "Run `sej/switch-buffer-functions' if needed.

This function checks the result of `current-buffer', and run
`sej/switch-buffer-functions' when it has been changed from
the last buffer.

This function should be hooked to `post-command-hook'."
    (unless (eq (current-buffer)
                sej/switch-buffer-functions--last-buffer)
      (let ((current (current-buffer))
            (previous sej/switch-buffer-functions--last-buffer))
        (setq sej/switch-buffer-functions--last-buffer current)
        (run-hook-with-args 'sej/switch-buffer-functions previous current))))

  (defun sej/denote-check-for-denote-buffer-switch (_prev curr)
    "Check for denote CURR buffer switch to and update with `sej/denote-silo-update'."
    (interactive)
    (if (string-match "\\[D\\].*" (buffer-name curr))
        (sej/denote-silo-update)))

;;;;;; denote menu
  (defun sej/denote-menu-setup ()
    "Change the denote menu mode to my liking."
    (interactive)
    (visual-line-mode -1)
    (if denote-menu-show-file-signature
        (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                      ("Signature" ,denote-menu-signature-column-width t)
                                      ("Title" ,denote-menu-title-column-width t)
                                      ("Keywords" ,denote-menu-keywords-column-width t)])

      (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                    ("Title" ,denote-menu-title-column-width t)
                                    ("Keywords" ,denote-menu-keywords-column-width t)]))
    (denote-menu-update-entries)
    (setq tabulated-list-sort-key '("Date" . t))
    (tabulated-list-init-header)
    (tabulated-list-print))

  (defvar sej/denote-menu-toggle-p 0
    "Variable to hold current toggle state.")

  (defun sej/denote-menu-cycle ()
    "Cycle listing All, only-Jounal, no-Journal notes."
    (interactive)
    (require 'denote-menu)
    (list-denotes)
    (cond
     ((= sej/denote-menu-toggle-p 0)
      (sej/denote-menu-only-journal))
     ((= sej/denote-menu-toggle-p 1)
      (sej/denote-menu-only-categories))
     (t (sej/denote-menu-all))))

  (defun sej/denote-menu-all ()
    "Cycle listing All, only-Jounal, no-Journal notes."
    (interactive)
    (require 'denote-menu)
    (list-denotes)
    (setq sej/denote-menu-toggle-p 0)
    (setq tabulated-list-sort-key '("Date" . t))
    (denote-menu-clear-filters)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (beginning-of-buffer))

  (defun sej/denote-menu-only-journal ()
    "Listing only-Jounal notes."
    (interactive)
    (require 'denote-menu)
    (list-denotes)
    (setq sej/denote-menu-toggle-p 1)
    (denote-menu-filter-by-keyword '("journal"))
    (setq tabulated-list-sort-key '("Date" . t))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (beginning-of-buffer))

  (defun sej/denote-menu-only-categories ()
    "Cycle listing only-categories."
    (interactive)
    (require 'denote-menu)
    (list-denotes)
    (setq sej/denote-menu-toggle-p 2)
    (denote-menu-clear-filters)
    (denote-menu-filter-out-keyword '("journal"))
    (setq tabulated-list-sort-key '("Title" . nil))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (beginning-of-buffer))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-denote)
;;; sej-denote.el ends here
