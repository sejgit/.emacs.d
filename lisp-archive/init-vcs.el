;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Version control systems.
;;

;;; ChangeLog:
;; 2017 05 14 init SeJ from purcell/.emacs.d
;; 2017 06 12 add font-awesome git icon
;; 2017 08 29 map to sej-mode-map & documentation & defer/ensure
;; 2017 08 30 deleted a few packages & documented the rest
;; 2017 09 18 add full screen for magit-status and return to previous on quit
;; 2017 09 20 move init-gist.el to here & delete file
;; 2018 04 02 add magit-repository-directories for magit-list-repositories
;; 2018 06 28 add magit-todos
;; 2018 09 27 some ahai tips  add git-gutter
;; 2019 04 30 Init & merge


;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Git
(use-package magit
  :bind (("C-x g" . magit-status)
         ("<f12>" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  (if (fboundp 'transient-append-suffix)
      ;; Add switch: --tags
      (transient-append-suffix 'magit-fetch
                               "-p" '("-t" "Fetch all tags" ("-t" "--tags")))))

;; Access Git forges from Magit
(if (executable-find "cc")
    (use-package forge
      :after magit
      :demand))

;; Show tasks
(use-package magit-todos
  :hook (ater-init . magit-todos-mode))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :commands (smerge-mode
             smerge-auto-leave
             smerge-next
             smerge-prev
             smerge-keep-base
             smerge-keep-upper
             smerge-keep-lower
             smerge-keep-all
             smerge-keep-current
             smerge-keep-current
             smerge-diff-base-upper
             smerge-diff-upper-lower
             smerge-diff-base-lower
             smerge-refine
             smerge-ediff
             smerge-combine-with-next
             smerge-resolve
             smerge-kill-current)
  :preface
  (defhydra hydra-smerge
    (:color red :hint none :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^──────────-^^───────────────────^^─────────────────────^^──────────────────
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine               _ZZ_: Save and bury
^^           _RET_: current       _E_diff                _q_: cancel
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (hydra-smerge/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

;; Git related modes
;; gist client
(use-package gist
  :defines sej-mode-map
  :bind  (:map sej-mode-map
               ("C-x G" . gist-list)
               ("H-G" . gist-list)))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)
;; M-x git-blamed-mode to turn on view with commits
(use-package git-blamed)

(provide 'init-vcs)
;;; init-vcs.el ends here
