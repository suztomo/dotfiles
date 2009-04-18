;;; less.el --- less style view mode

;; Copyright (C) 2005 William Xu

;; Author: William Xu 
;; $Id: less.el,v 0.1 2005/09/07 00:37:49 xwl Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; View file like a simple `less'. Provide limited less keys, mainly j,
;; k, f, b, g, G, etc.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'less)
;;
;; Then use `M-x less-minor-mode' to toggle `less-minor-mode'.

;; There's also a `view-less.el' in XEmacs. But it does too much for me,
;; i just wanna less keys like j, k, f, b, g, G, not to mess with other
;; keys in major mode.

;;; Change Log:

;; v 0.1 [2005/09/07 01:41:01] Initial version.

;; modified by a2c

;;; Code:

;; forrwo key baindings is a2c style if you want to simple original style
;; get from here
;; http://www.emacswiki.org/cgi-bin/wiki/less.el

(define-minor-mode less-minor-mode
  "Toggle less-minor-mode.

With less-minor-mode enabled, you could use `less' like keys to view files.
\\{less-minor-mode-map}."
  nil " Less"
  '(("j" . less-scroll-up-one-line)
    ("k" . less-scroll-down-one-line)
    ("L" . forward-char)
    ("l" . forward-word)
    ("H" . backward-char)
    ("h" . backward-word)
    ("p" . previous-line)
    ("n" . next-line)
    ("J" . less-scroll-up-one-line-slightly)
    ("K" . less-scroll-down-one-line-slightly)
    ("f" . scroll-up)
    ("b" . scroll-down)
    ("o" . other-window)
    ("t" . gtags-find-tag)
    ("i" . imenu)
    ("g" . beginning-of-buffer)
    ("G" . end-of-buffer)
    ("rr" . point-to-register)
    ("rj" . jump-to-register)
    ("rm" . bookmark-set)
    ("rb" . bookmark-jump)
    ("rw" . window-configuration-to-register)
    ("," . find-file)
    ("." . iswitchb-buffer)
    (" " . scroll-up)
    ("^" . beginning-of-line)
    ("$" . end-of-line)
    ("" . scroll-down)
    ("e" . less-quit)
    ("0" . delete-window)
    ("1" . delete-other-windows)
    ("2" . split-window-vertically)
    ("3" . split-window-horizontally)
    ("]" . yic-next-buffer)
    ("[" . yic-prev-buffer)
    )
  (set (make-local-variable 'buffer-read-only) less-minor-mode))

(defun less-scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1)
  (next-line 1))

(defun less-scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1)
  (previous-line 1))

(defun less-scroll-up-one-line-slightly ()
  "Scroll up one line."
  (interactive)
  (scroll-up 2)
  (next-line 2))

(defun less-scroll-down-one-line-slightly ()
  "Scroll down one line."
  (interactive)
  (scroll-down 2)
  (previous-line 2))

(defun less-quit ()
  "Quit `less-minor-mode'."
  (interactive)
  (less-minor-mode -1))

(add-hook 'find-file-hooks 'auto-less-minor-mode)
(defun auto-less-minor-mode ()
  "Auto enter `less-minor-mode' when visiting read-only files. You can
add this to `find-file-hooks'."
  (unless (file-writable-p buffer-file-name)
    (less-minor-mode t)))
(defun less-minor-mode-on ()
  "Turn on `less-minor-mode'."
  (less-minor-mode 1))

(defun less-minor-mode-off ()
  "Turn off `less-minor-mode'."
  (less-minor-mode -1))

;(add-hook 'find-file-hooks 'less-read-only)
;(defun less-read-only ()
;  (if buffer-read-only
;      (less-minor-mode 1)))

(defun less-toggle-read-only (&optional n)
  (interactive "P")
  (if buffer-read-only
      (progn
 (toggle-read-only 1)
 (less-minor-mode -1))
    (progn
      (toggle-read-only -1)
      (less-minor-mode 1))))

;; ----------------------------------------------------------------------
;;     Original yic-buffer.el
;;     From: choo@cs.yale.edu (young-il choo)
;;     Date: 7 Aug 90 23:39:19 GMT
;;
;;     Modified
;;     Atsushi Suga 2006/09/29 02:13:03 (added less-yic-ignore)
;; ----------------------------------------------------------------------

(defun yic-ignore (str)
  (or
   ;;buffers I don't want to switch to 
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^ " str)

   ;;Test to see if the window is visible on an existing visible frame.
   ;;Because I can always ALT-TAB to that visible frame, I never want to 
   ;;Ctrl-TAB to that buffer in the current frame.  That would cause 
   ;;a duplicate top-level buffer inside two frames.
   (memq str                
         (mapcar 
          (lambda (x) 
            (buffer-name 
             (window-buffer 
              (frame-selected-window x))))
          (visible-frame-list)))
   ))

(defun less-yic-ignore (bf)
  (let ((bv (buffer-local-variables bf)) flag)
    (while bv
      (setq var (car bv))
      (if (and (equal (car var) 'less-minor-mode) (cdr var))
   (progn
     (setq flag 't)
     (setq bv '()))
   (setq bv (cdr bv))))
    flag))

(defun yic-next (ls)
  "Switch to next buffer in ls skipping unwanted ones."
  (let* ((ptr ls)
         bf bn go
         )
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      ;(if (null (yic-ignore bn))        ;skip over
      (if (less-yic-ignore bf)
   (setq go bf)
 (setq ptr (cdr ptr))
 )
      )
      ;skip over if buffer is not less-minor-mode
    (if go
        (switch-to-buffer go))))

(defun yic-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (yic-next (reverse (buffer-list))))

(defun yic-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))
;;end of yic buffer-switching methods



(provide 'less)

;;; less.el ends here
