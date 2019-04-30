;;; init-misc-defuns.el --- Some Utility functions

;;; Commentary:
;; some functions to add value to my Emacs

;;; ChangeLog
;; 2017 05 17 init SeJ from purcell/.emacs.d
;; 2017 08 29 add copy-from-osx & paste-to-osx
;; 2017 09 08 fixed above for only mac
;; 2017 09 20 deleted unused defuns and renamed to init-misc-defuns.el
;;            move from init-bindings-settings.el
;; 2017 12 21 comment out unused, use crux for some, reorder used at top
;; 2018 01 31 add jcs now sej/insert-url from safari
;;            add my now sej/org-insert-defun
;;            make my functions consistent with sej/
;; 2018 09 24 add executable functions from ohai

;;; Code:

;; from https://gist.github.com/the-kenny/267162
(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    "For copying from osx."
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    "For copying to osx TEXT with optional PUSH."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)

  ;; from jcs (Irreal) blog to copy url from safari and paste at point
  (defun sej/insert-url ()
    "Insert URL of current browser page into Emacs buffer."
    (interactive)
    (insert (sej/retrieve-url)))

  ;; from jcs (Irreal) blog helper function from above
  (defun sej/retrieve-url ()
    "Retrieve the URL of the current Safari page as a string."
    (org-trim (shell-command-to-string
               "osascript -e 'tell application \"Safari\" to return URL of document 1'")))

  )

;; as name suggests ; defined as C-c b in above keymappings
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
    (emacs-lisp-mode)
    ))

;; function to edit the curent file as root. attached to C-x C-r in bindings
(defun sej/sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
       (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; line numbers when using goto-line M-g M-g or M-g g
(defun sej/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
  (display-line-numbers-mode 1)
  (with-no-warnings (goto-line (read-number "Goto line: "))))
    (display-line-numbers-mode -1)))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun sej/create-non-existent-directory ()
  "Ask to make directory for file if it does not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
         (y-or-n-p? (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'sej/create-non-existent-directory)


;; macro saving
(defun sej/save-macro (name)
  "Save a macro.  Take a NAME as argument and save the last defined macro under this name at the end of your init file."
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (switch-to-buffer nil))

;; functions for push and jump to mark
(defun sej/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun sej/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;; executable functions from ohai and modified for my uses
(defun sej/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (s-trim (shell-command-to-string command)))

(defun sej/exec-with-rc (command &rest args)
  "Run a shell command and return a list containing two values: its return
code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun sej/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun sej/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun sej/exec-if-exec (command args)
  "If `command' satisfies `sej/is-exec', run it with `args' and return its
output as per `sej/exec'. Otherwise, return nil."
  (when (sej/is-exec command) (sej/exec (s-concat command " " args))))

;; sej/indent-buffer bound to C-c <tab>
(defun sej/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'init-misc-defuns)
;;; init-misc-defuns.el ends here
