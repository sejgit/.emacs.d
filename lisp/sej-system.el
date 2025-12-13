;;; sej-system.el --- system related functions file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive system related commands.
;; system being outward facing from Emacs to the underlying system.

;;; Code:

;;;;; authentication GPG

(defun sej/lookup-password (host user port)
  "Lookup password for HOST USER PORT."
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

;;;;; exec functions

(defun sej/exec (command)
  "Run a shell COMMAND and return its output as a string, whitespace trimmed."
  (interactive)
  (s-trim (shell-command-to-string command)))

(defun sej/exec-with-rc (command &rest args)
  "Run a shell COMMAND ARGS; return code and its whitespace trimmed output."
  (interactive)
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun sej/is-exec (command)
  "Return non-nil if COMMAND is an executable on the system search path."
  (interactive)
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun sej/resolve-exec (command)
  "If COMMAND is an executable on the system search path.
Return its absolute path.  Otherwise, return nil."
  (interactive)
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun sej/exec-if-exec (command args)
  "If COMMAND `sej/is-exec' run it with ARGS, return per `sej/exec' or nil."
  (interactive)
  (when (sej/is-exec command) (sej/exec (s-concat command " " args))))

;;;;; urls and browsind

;; improved from jcs (Irreal) blog to copy url from safari and paste at point
;; https://irreal.org/blog/?p=2895
(when sys/macp
  (defun sej/url-insert-safari (desc)
    "Retrieve URL from current Safari page and prompt for description.
      Insert an Org link at point."
    (interactive "sLink Description (None to display url): ")
    (let ((link (do-applescript "tell application \"Safari\" to return URL of document 1")))
      (if (> (length desc) 0)
          (insert (format "[[%s][%s]]" (org-trim link) desc))
        (insert (format "[[%s]]" (org-trim link)))) ))

  (defun sej/url-insert-edge (desc)
    "Retrieve URL from current Edge page and prompt for description.
      Insert an Org link at point."
    (interactive "sLink Description (None to display url): ")
    (let ((link (do-applescript "tell application \"Microsoft Edge\" to return URL of active tab of front window")))
      (if (> (length desc) 0)
          (insert (format "[[%s][%s]]" (org-trim link) desc))
        (insert (format "[[%s]]" (org-trim link)))) )))

(defun sej/browse-homepage ()
  "Browse the Github page of SeJ Emacs."
  (interactive)
  (browse-url sej-homepage))
(bind-key* "C-q h" 'sej/browse-homepage)

(defun sej/create-non-existent-directory ()
  "Ask to make directory for file if it does not exist."
  ;; [[http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/][sej/create-non-existent-directory]]
  ;; Offer to create parent directories if they do not exist
  ;; automatically run after save
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'sej/create-non-existent-directory)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-system)
;;; sej-system.el ends here
