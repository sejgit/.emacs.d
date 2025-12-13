;;; sej-frames.el --- frame functions file -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Provides interactive commands for frames.

;;; Code:

(defun sej/frame-resize-full ()
  "Set frame full height and 1/2 wide, position at screen left."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)
                  (- (display-pixel-width)
                     (if sys/macp (eval 13)
                       (eval 25)))
                  (- (display-pixel-height)
                     (- (frame-outer-height)
                        (frame-inner-height)
                        sej/menu-height))
                  1))

(defun sej/frame-resize-l ()
  "Set frame full height and 1/2 wide, position at screen left."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)
                  (- (truncate (/ (display-pixel-width) 2)) 14)
                  (- (display-pixel-height)
                     (- (frame-outer-height)
                        (frame-inner-height) sej/menu-height) )
                  1))

(defun sej/frame-resize-l2 ()
  "Set frame full height and 1/3 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)
                  (- (truncate (/ (display-pixel-width) 3)) 0)
                  (- (display-pixel-height)
                     (- (frame-outer-height)
                        (frame-inner-height) sej/menu-height))
                  1))

(defun sej/frame-resize-l3 ()
  "Set frame full height and 2/3 wide, position at left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame)
                  (- (truncate (* (/ (display-pixel-width) 3) 2)) 0)
                  (- (display-pixel-height)
                     (- (frame-outer-height)
                        (frame-inner-height) sej/menu-height))
                  1))

(defun sej/frame-resize-r ()
  "Set frame full height and 1/2 wide, position at screen right."
  (interactive)
  (set-frame-position (selected-frame)
                      (- (truncate (/ (display-pixel-width) 2)) 0) 0)
  (set-frame-size (selected-frame)
                  (- (truncate (/ (display-pixel-width) 2)) 14)
                  (- (display-pixel-height)
                     (- (frame-outer-height)
                        (frame-inner-height) sej/menu-height))
                  1))

(defun sej/frame-resize-r2 ()
  "Set frame full height and 1/3 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame)
                      (truncate (* (/ (display-pixel-width) 3) 2))
                      0)
  (set-frame-size (selected-frame)
                  (- (truncate (/ (display-pixel-width) 3)) 0)
                  (- (display-pixel-height)
                     (- (frame-outer-height)
                        (frame-inner-height) sej/menu-height))
                  1))

(defun sej/frame-resize-r3 ()
  "Set frame full height and 2/3 wide, position at screen right of left hand screen in extended monitor display assumes monitors are same resolution."
  (interactive)
  (set-frame-position (selected-frame)
                      (truncate (* (/ (display-pixel-width) 3) 1))
                      0)
  (set-frame-size (selected-frame)
                  (- (truncate (* (/ (display-pixel-width) 3) 2)) 0)
                  (- (display-pixel-height)
                     (- (frame-outer-height)
                        (frame-inner-height) sej/menu-height))
                  1))

(defun sej/frame-recentre (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                           frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(defun sej/hide-frame (window)
  "Hide frame containing WINDOW."
  (lower-frame (window-frame window)))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-frames)
;;; sej-frames.el ends here
