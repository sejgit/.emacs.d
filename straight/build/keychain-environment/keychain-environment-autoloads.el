;;; keychain-environment-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "keychain-environment" "keychain-environment.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from keychain-environment.el

(autoload 'keychain-refresh-environment "keychain-environment" "\
Set ssh-agent and gpg-agent environment variables.

Set the environment variables `SSH_AUTH_SOCK', `SSH_AGENT_PID'
and `GPG_AGENT' in Emacs' `process-environment' according to
information retrieved from files created by the keychain script." t nil)

;;;***

(provide 'keychain-environment-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; keychain-environment-autoloads.el ends here
