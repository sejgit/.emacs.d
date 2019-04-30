;;; init-writing.el --- Initialize writing utilities.	-*- lexical-binding: t -*-

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
;; for editing of markdown files & writing helpers

;;; ChangeLog:
;;
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 25 add auto-fill-mode from EOS
;; 2017 09 04 change to init-writing.el & add yaml, elasticsearch, skeleton,
;;              abbrev, thesaurus
;;            numbering rectangles, writing/viewing helpers, highlighting indentation
;; 2017 09 04 move indent-guide, page-break-lines, whitespace-cleanup-mode
;;              from init-misc-pkgs.el
;; 2017 09 07 move YAML mode to init-misc-filetypes.el
;; 2018 06 06 some markdown modes
;; 2018 09 24 comment out whitespace-cleanup-mode due to use of ethan-wspace
;; 2019 04 30 merge init-utils & init-markdown


;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package writegood-mode
  :mode
  (("\\`README\\.md\\'" . gfm-mode)
   ("github\\.com.*\\.txt\\'" . gfm-mode)
   ("\\.md\\'"          . markdown-mode)
   ("\\.markdown\\'"    . markdown-mode)))

;; show vertical lines to guide indentation
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :diminish
  indent-guide-mode)

;; intelligently call whitespace-cleanup on save
;; (use-package whitespace-cleanup-mode
;;   :ensure t
;;   :hook (before-save . whitespace-cleanup)
;;   :config
;;   (global-whitespace-cleanup-mode t))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; markdown-mode used a lot on Github
(use-package markdown-mode
  :defines flycheck-markdown-markdownlint-cli-config
  :preface
  ;; Install: pip install grip
  (defun markdown-preview-grip ()
    "Render and preview with `grip'."
    (interactive)
    (let ((program "grip")
          (port "6419")
          (buffer "*gfm-to-html*"))

      ;; If process exists, kill it.
      (markdown-preview-kill-grip buffer)

      ;; Start a new `grip' process.
      (start-process program buffer program (buffer-file-name) port)
      (sleep-for 1) ; wait for process start
      (browse-url (format "http://localhost:%s/%s.%s"
                          port
                          (file-name-base)
                          (file-name-extension
                           (buffer-file-name))))))

  (defun markdown-preview-kill-grip (&optional buffer)
    "Kill `grip' process."
    (interactive)
    (let ((process (get-buffer-process (or buffer "*gfm-to-html*"))))
      (when process
        (kill-process process)
        (message "Process %s killed" process))))

  ;; Install: npm i -g markdownlint-cli
  (defun set-flycheck-markdownlint ()
    "Set the `mardkownlint' config file for the current buffer."
    (let* ((md-lint ".markdownlint.json")
           (md-file buffer-file-name)
           (md-lint-dir (and md-file
                             (locate-dominating-file md-file md-lint))))
      (setq-local flycheck-markdown-markdownlint-cli-config
                  (concat md-lint-dir md-lint))))
  :bind (:map markdown-mode-command-map
              ("g" .  markdown-preview-grip)
              ("k" .  markdown-preview-kill-grip))
  :hook ((markdown-mode . flyspell-mode)
         (markdown-mode . auto-fill-mode)
         (markdown-mode . set-flycheck-markdownlint)
         (markdown-mode . visual-line-mode)
         (markdown-mode . writegood-mode))
  :functions writegood-mode
  :commands (markdown-mode gfm-mode)
  :mode   (("README\\.md\\'" . gfm-mode)
           ("github\\.com.*\\.txt\\'" . gfm-mode)
           ("\\.md\\'"          . markdown-mode)
           ("\\.markdown\\'"    . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-header-scaling t)
  (setq markdown-command "pandoc --smart -f markdown -t html")

  (when sys/macp
    (let ((typora "/Applications/Typora.app/Contents/MacOS/Typora"))
      (if (file-exists-p typora)
          (setq markdown-open-command typora))))

  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (setq markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<script src='http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre[lang] > code').forEach((code) => {
    code.classList.add(code.parentElement.lang);
    hljs.highlightBlock(code);
  });
});
</script>
")

  ;; Table of contents
  (use-package markdown-toc))


;; Elasticsearch uses asciidoc everywhere for documentation
(use-package adoc-mode
  :ensure t
  )

;; skeletons are a kind of yasnippet but they don't mess with keybindings
(define-skeleton sej/org-header
  "Insert a standard header for org-mode files"
  "Title: "
  "#+TITLE: " str \n
  "#+AUTHOR: " (user-full-name) \n
  "#+EMAIL: " user-mail-address \n
  "#+SETUPFILE: ~/eos/setupfiles/default.setup

| *Author* | {{{author}}} ({{{email}}})    |
| *Date*   | {{{time(%Y-%m-%d %H:%M:%S)}}} |

* Introduction
" \n)

(define-skeleton sej/org-wrap-elisp
  "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code"
  nil
  > "#+BEGIN_SRC emacs-lisp" \n
  > _ \n
  > "#+END_SRC" \n)

(define-skeleton sej/org-wrap-source
  "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type"
  "Language: "
  > "#+BEGIN_SRC " str \n
  > _ \n
  > "#+END_SRC" \n)

;; for inserting abbreviations
(use-package abbrev
  :ensure nil
  :hook (after-init . abbrev-mode)
  :diminish abbrev-mode
  :config
  (define-abbrev-table
    'global-abbrev-table
    '(("<sej" "stephenearljenkins" nil 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<orgh" "" 'sej/org-header 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<orgl" "" 'sej/org-wrap-elisp 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<orgs" "" 'sej/org-wrap-source 0))))


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
;; This function allows you to hit ***C-x r N ***and specify the pattern and starting offset to number lines in rectangular-selection mode:
(defun number-rectangle (start end format-string from)
  "Delete text in the region-rectangle, then number it from (START to END with FORMAT-STRING FROM)."
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
    (loop with column = (current-column)
          while (and (<= (point) end) (not (eobp)))
          for i from from   do
          (move-to-column column t)
          (insert (format format-string i))
          (forward-line 1)))
  (goto-char start))

(define-key sej-mode-map (kbd "C-c s n") 'number-rectangle)

(use-package define-word
  :ensure t
  :bind (("H-d" . define-word-at-point)
         ("H-D" . define-word)))


(provide 'init-writing)
;;; init-writing.el ends here
