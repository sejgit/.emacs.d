;;; init-frame-cmds.el --- Initialize frame-cmds.	-*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d
;; Version: 1.0
;; Keywords: .emacs.d sejgit

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
;; commands to move the frams around and size in Emacs

;;; ChangeLog
;;
;; 2017 01 10 init SeJ
;; 2017 01 17 comment out some irritating binds
;; 2017 08 29 map to sej-mode-map
;; 2017 12 01 update to add :ensure
;; 2018 08 23 update sizes of frames for win-10
;; 2018 09 04 go native for functions removing lisp/files also change name from sej- to sej/
;; 2018 10 05 some adds for bindings and a l2 for double-wide w32 extended screens
;; 2019 05 03 initialize & merge


;;; Code:

(defvar sej-mode-map)
(define-key sej-mode-map (kbd "C-c s <left>") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "C-c s <S-left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "C-c s <right>") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "C-c s <S-right>") 'sej/frame-resize-r2)

(define-key sej-mode-map (kbd "s-<left>") 'sej/frame-resize-l)
(define-key sej-mode-map (kbd "s-<S-left>") 'sej/frame-resize-l2)
(define-key sej-mode-map (kbd "s-<right>") 'sej/frame-resize-r)
(define-key sej-mode-map (kbd "s-<S-right>") 'sej/frame-resize-r2)

;;set frame full height and full wide
;;and position at screen left
(defun sej/frame-resize-full ()
  "Set frame full height and 1/2 wide, position at screen left."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (display-pixel-width)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

;;set frame full height and 1/2 wide
;;and position at screen left
(defun sej/frame-resize-l ()
  "Set frame full height and 1/2 wide, position at screen left."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

;;set frame full height and 1/2 wide
;;and position at screen left of screen in extended monitor display
;;assumes monitors are same resolution
(defun sej/frame-resize-l2 ()
  "Set frame full height and 1/2 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

;;set frame full height and 1/2 wide
;;and position at screen right
(defun sej/frame-resize-r ()
  "Set frame full height and 1/2 wide, position at screen right."
  (interactive)
  (set-frame-position (selected-frame) (- (truncate (/ (display-pixel-width) 2)) 0) 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 2)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )

;;set frame full height and 1/2 wide
;;and position at screen right of left hand screen in extended monitor display
;;assumes monitors are same resolution
(defun sej/frame-resize-r2 ()
  "Set frame full height and 1/2 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
  (set-frame-size (selected-frame)  (- (truncate (/ (display-pixel-width) 4)) 0)
                  (- (display-pixel-height) (- (frame-outer-height) (frame-inner-height))) 1)
  )


(provide 'init-frame-cmds)
;;; init-frame-cmds.el ends here
