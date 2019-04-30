;;; init-tramp.el --- Stephen's emacs init-tramp.el

;;; Commentary:
;; tramp setup.

;;; ChangeLog:
;; 2017 03 14 init SeJ
;; 2017 03 28 fix settings
;; 2017 04 04 get tramp into use-package
;; 2017 05 08 possible changes for darwin
;; 2017 09 01 minor use-package tweaks
;; 2017 09 06 add pass

;;; Code:


(use-package tramp
  :commands
  tramp-default-method
  tramp-default-user
  tramp-default-host
  :init
  (if (eq system-type 'darwin)
      (setq
       tramp-default-method "ssh"
       password-cache-expiry nil)
    (setq
     tramp-default-method "ssh"
     tramp-default-user "pi"
     tramp-default-host "home"
     password-cache-expiry nil)
    )
  (setq tramp-use-ssh-controlmaster-options nil)

  (defadvice tramp-handle-write-region
      (after tramp-write-beep-advice activate)
    "Make tramp beep after writing a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-do-copy-or-rename-file
      (after tramp-copy-beep-advice activate)
    "Make tramp beep after copying a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-insert-file-contents
      (after tramp-insert-beep-advice activate)
    "Make tramp beep after inserting a file."
    (interactive)
    (beep))
  )

(use-package pass
  :ensure t
  :defer 7)

(provide 'init-tramp)
;;; init-tramp.el ends here
