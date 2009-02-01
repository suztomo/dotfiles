;;; physical-line.el --- minor mode for point motion in physical lines
;; $Id: physical-line.el,v 1.35 2007/10/08 03:03:24 ken Exp $

;; Copyright (c) 2000-2002,2007 KAMADA Ken'ichi.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; version 3.2


;;
;; variables
;;

(defvar physical-line-mode nil
  "Non-nil if in physical-line minor mode.")
(make-variable-buffer-local 'physical-line-mode)

(defvar physical-line-slip-backward nil
  "*If non-nil, point slips before a wide-column character.
The position of point is adjusted according to this variable if
target column of vertical motion is within a character.  If nil,
point goes after the character; otherwise point goes before it.")

(defvar physical-line-mode-string " PL"
  "*A string displayed when physical-line minor mode is active.")

(defvar physical-line-mode-load-hook nil
  "*Hook called just after physical-line-mode is loaded.")

(defvar physical-line-active-command-list
  '()
  "*Commands which want to use advised next-line, previous-line, etc.")

(defvar physical-line-goal-column 0
  "Current goal column counting in a physical line.")
(make-variable-buffer-local 'physical-line-goal-column)



;;
;; functions
;;

(defun physical-line-mode (&optional arg)
  "Minor mode for moving point in physical (screen) lines.
`physical-line-mode' toggles physical-line minor mode.
With ARG, turn physical-line mode on if and only if ARG is positive.

In physical-line mode, vertical cursor move is done in physical
(screen) lines."
  (interactive "P")
  (setq physical-line-mode
	(if arg
	    (> (prefix-numeric-value arg) 0)
	  (not physical-line-mode)))
  (force-mode-line-update))


;; --------------------------------
;; basic implementations

(defun physical-line-current-column ()
  "Return the horizontal position of point in a physical (screen) line.
Beginning of line is 0."
  (let ((cur (current-column)))
    (save-excursion
      (physical-line-vertical-motion 0)
      (- cur (current-column)))))

(defun physical-line-next-line (arg)
  "Move cursor vertically down ARG physical-lines.
This function is like `next-line', but counts ARG in physical (screen)
lines."
  (interactive "p")
  (if (and next-line-add-newlines (eq arg 1))
      (condition-case nil
	  (physical-line-line-move 1)
	(end-of-buffer (newline 1)))
    (if (interactive-p)
	(condition-case nil
	    (physical-line-line-move arg)
	  ((beginning-of-buffer end-of-buffer) (ding)))
      (physical-line-line-move arg)))
  nil)

(defun physical-line-previous-line (arg)
  "Move cursor vertically up ARG physical-lines.
This function is like `previous-line', but counts ARG in physical (screen)
lines."
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
	  (physical-line-line-move (- arg))
	((beginning-of-buffer end-of-buffer) (ding)))
    (physical-line-line-move (- arg)))
  nil)

(defun physical-line-line-move (arg)
  "Move cursor vertically down ARG physical-lines.
Move it up if ARG is negative."
  (if (not (memq last-command '(physical-line-next-line
				physical-line-previous-line
				next-line
				previous-line)))
      (setq physical-line-goal-column (physical-line-current-column)))
  (if (not (eq (physical-line-vertical-motion arg) arg))
      (signal (if (< arg 0) 'beginning-of-buffer 'end-of-buffer) nil))
  (let ((base (point)) new
	(target-column (+ (current-column) physical-line-goal-column)))
    (if (and (> (move-to-column target-column) target-column)
	     physical-line-slip-backward)
	(physical-line-backward-composite-char1))
    (setq new (point))
    (physical-line-vertical-motion 0)
    (if (eq base (point))
	(goto-char new)
      (goto-char base)
      (physical-line-end-of-line))))

(defun physical-line-beginning-of-line (&optional n)
  "Move point to the beginning of the current physical-line."
  (interactive "p")
  (or n (setq n 1))
  (physical-line-vertical-motion (1- n))
  nil)

(defun physical-line-end-of-line (&optional n)
  "Move point to the end of the current physical-line."
  (interactive "p")
  (or n (setq n 1))
  (if (and (eq (physical-line-vertical-motion n) n)
	   (not (bobp)))
      (physical-line-backward-composite-char1))
  nil)


;; --------------------------------
;; advice

(defadvice next-line (around physical-line-next-line activate)
  (if (and physical-line-mode
	   (or (interactive-p)
	       (memq this-command physical-line-active-command-list)))
      (physical-line-next-line arg)
    ad-do-it))

(defadvice previous-line (around physical-line-previous-line activate)
  (if (and physical-line-mode
	   (or (interactive-p)
	       (memq this-command physical-line-active-command-list)))
      (physical-line-previous-line arg)
    ad-do-it))



;;
;; environment-specific functions
;;

;; --------------------------------
;; Workaround for Emacs 21.1's buggy vertical-motion

;; ---- with window-system
;; In this comment, a "line" stands for a "physical line".
;; Emacs 21.1's vertical-motion is not reliable.
;;   1. (vertical-motion 0) is not reliable.
;;     1-1. It sometimes moves the point to the previous line.
;;     1-2. It sometimes moves the point to non-beginning of a line.
;;   2. (vertical-motion -1) is not reliable.
;;     2-1. It sometimes moves the point to the beginning of the same
;;          line, and returns 0.
;;     2-2. It sometimes moves up the point by 2 lines, and returns -1.
;;   3. (vertical-motion 1) is not reliable.
;;     3-1. It sometimes moves down the point by 2 lines, and returns 1.
;; As far as I know, (vertical-motion 1) is reliable when the point is
;; at the beginning of a line.

;; Even if with this care, some abnormal behaviors occur in lines
;; with Arbic characters and tabs.  This may be a trouble of display
;; (not of point-moving).
;; Because: in abnormal situatoin,
;;   * (forward-char) and (backward-char) go through different
;;     points on the screen.
;;   * horizontal positions of characters change on M-x recenter.
;; and it goes all right after M-x recenter.

(defun physical-line-e21-1-win-vertical-motion (lines &optional window)
  "A replacement for Emacs 21.1's buggy `vertical-motion'; window-system version.

This function uses Emacs 21.1's buggy `vertical-motion' and provides
\"correct\" behavior of vertical-motion.  This function is also expected
to work with correct vertical-motion.

See also `physical-line-e21-1-tty-vertical-motion'"
  (let ((moved 0))
    (cond
     ((> lines 0)
      (physical-line-e21-1-win-vertical-motion0 window)
      (vertical-motion lines window))
     ((< lines 0)
      (physical-line-e21-1-win-vertical-motion0 window)
      (condition-case nil
	  (while (< lines 0)
	    ;; (backward-char) may be enough here, but...
	    ;; e21-1-win-vertical-motion0's speed up code (*1 below)
	    ;; uses narrowing.  If a edge of the narrowed region is
	    ;; inside a composite character, successive vertical-motion
	    ;; crashes Emacs 21.1 itself.
	    (physical-line-backward-composite-char1)
	    (physical-line-e21-1-win-vertical-motion0 window)
	    (setq lines (1+ lines))
	    (setq moved (1- moved)))
	(beginning-of-buffer nil))
      moved)
     (t
      (physical-line-e21-1-win-vertical-motion0 window)))))

(defun physical-line-e21-1-win-vertical-motion0 (&optional window)
  (let ((cur (point)) prev
	(ploffset 0)
	(inhibit-field-text-motion t)	;; for beginning-of-line
	ret-last-vm)

    ;; [EBug8]
    ;; (narrow-to-region (point) (point)) on a composite character
    ;; illegally displays the character, and then (vertical-motion 1)
    ;; crashes Emacs 21.1.
    (if (bolp)
	0

      (beginning-of-line)

      ;; approach roughly (*1)
      (save-restriction
	(narrow-to-region (point) cur)
	(while (eq (setq ret-last-vm (vertical-motion 9999 window)) 9999)
	  (setq ploffset (+ ploffset 9999)))
	(setq ploffset (+ ploffset ret-last-vm)))
      (beginning-of-line)
      ;; (vertical-motion ploffset window)
      ;; [EBug7]
      ;; Due to Emacs 21.1's bug, ploffset can be 1 when should be 0.
      (if (> ploffset 1)
	  (vertical-motion ploffset window))

      ;; approach accurately
      (setq prev (point)
	    ret-last-vm 1)
      (while (< (point) cur)
	(setq prev (point))
	(setq ret-last-vm (vertical-motion 1 window)))
      (if (not (and (eq (point) cur)		;; (and "already physical-bolp"
		    (eq ret-last-vm 1)))	;;      "not failed")
	  (goto-char prev))))
  0)

;; ---- without window-system
;; As far as I know, (vertical-motion 1) is not reliable without
;; window-system.  (vertical-motion 0) and (vertical-motion -1) are
;; reliable.

(defun physical-line-e21-1-tty-vertical-motion (lines &optional window)
  "A replacement for Emacs 21.1's buggy `vertical-motion'; non window-system version.

See also `physical-line-e21-1-win-vertical-motion'"
  (cond
   ((> lines 0)
    (let ((arglines lines)
	  curpoint nextpoint prevlines)
      (while (and (not (eq prevlines lines))
		  (> lines 0))
	(setq prevlines lines
	      curpoint (point))
	(forward-line)
	(setq nextpoint (point))
	(vertical-motion 0 window)	;; in case of (eobp)
	(while (< curpoint (point))
	  (vertical-motion -1 window)
	  (setq lines (1- lines)))
	(goto-char nextpoint))
      (if (> lines 0)
	  (- arglines lines)
	(vertical-motion lines window)	;; always succeeds
	arglines)))
   (t
    (vertical-motion lines window))))


;; --------------------------------
;; Workaround for Emacs 21.3's buggy vertical-motion

;; ---- with window-system

(defun physical-line-e21-3-win-vertical-motion (lines &optional window)
  ""
  (let ((moved 0))
    (cond
     ((> lines 0)
      (physical-line-e21-3-win-vertical-motion0 window)
      (while (> lines 0)
	(if (physical-line-e21-3-win-vertical-motion-sub1 window)
	    (setq lines (1- lines)
		  moved (1+ moved))
	  (setq lines 0)))
      moved)
     ((< lines 0)
      (physical-line-e21-3-win-vertical-motion0 window)
      (condition-case nil
	  (while (< lines 0)
	    (backward-char)
	    (physical-line-e21-3-win-vertical-motion0 window)
	    (setq lines (1+ lines))
	    (setq moved (1- moved)))
	(beginning-of-buffer nil))
      moved)
     (t
      (physical-line-e21-3-win-vertical-motion0 window)))))

(defun physical-line-e21-3-win-vertical-motion0 (&optional window)
  (let ((cur (point))
	(cureolp (eolp))
	bol next prev offs ret)
    (beginning-of-line)
    (setq bol (point)
	  next bol
	  prev bol
	  offs 1)
    (while (< next cur)
      ;; The behavior of (compute-motion) is weird when invoked from
      ;; the mid-line, but seems ok when invoked at the bol (so far).
      (setq ret (compute-motion bol '(0 . 0) (point-max) (cons 0 offs)
				(window-width) nil window))
      (setq prev next
	    next (car ret)
	    offs (1+ offs)))
    ;; If the length of the previous line is just the same with window-width,
    ;; continuation-glyph is used on 'x11, but (compute-motion) doesn't
    ;; seem to count it.  Hack around this with PREVHPOS of ret.
    (cond
     ;; when the point was at the last of a physical line,
     ;;  - whose length is the same with window-width, and
     ;;  - which is the last physical line of a logical line.
     ((and cureolp
	   (eq (nth 1 ret) (window-width)))
      (goto-char next))
     ;; when the point was at the last logical line.
     ((not (eq (nth 1 ret) 0))
      (goto-char prev))
     ;; when the point was at the last of a physical line, whose length
     ;; is the same with window-width.
     ((and cureolp
	   (eq (nth 3 ret) (window-width)))	;; prevhpos == window-width
      (goto-char (1- next)))
     ((eq next cur)
      (goto-char next))
     (t
      (goto-char prev)))
    0))

(defun physical-line-e21-3-win-vertical-motion-sub1 (&optional window)
  (let (ret)
    ;; '(-1 . 1)?  Huh?  :-p
    (setq ret (compute-motion (point) '(0 . 0) (point-max) '(-1 . 1)
			      (window-width) nil window))
    (cond
     ;; when the point has jumped over a physical line (it is the last
     ;; line of a logical line and its length is the same with window-width).
     ((and (eq (nth 3 ret) (window-width))	;; prevhpos == window-width
	   (not (nth 4 ret)))			;; !cont
      (goto-char (1- (car ret)))
      t)
     ;; when the point was at last physical line, whose length is the same
     ;; with the window-width, and the point is now at the last of it.
     ((and (eq (nth 1 ret) (window-width))
	   (eq (nth 2 ret) 0))
      (goto-char (car ret))
      t)
     ;; if (eq VPOS 0) here, it means the point can't move to the next line.
     ;; (maybe no lines left.)
     ((eq (nth 2 ret) 0)
      (goto-char (car ret))
      nil)
     (t
      (goto-char (car ret))
      t))))


;; --------------------------------
;; Workaround for Emacs 22's buggy vertical-motion

(defun physical-line-e22-win-vertical-motion (lines &optional window)
  (physical-line-e22-win-vertical-motion0 window)
  (vertical-motion lines window))

(defun physical-line-e22-win-vertical-motion0 (&optional window)
  (let ((cur (point))
	(back (point)))
    (vertical-motion 0 window)
    (while (< cur (point))
      (goto-char back)
      (physical-line-backward-composite-char1)
      (setq back (point))
      (vertical-motion 0 window))))


;; --------------------------------
;; for backward-char compatibility

;; Emacs 21.1's forward-char, backward-char, delete-char, etc.
;; seem not to treat a composite character as a unit.
;; I don't know whether this is a bug or a spec, but at least it
;; is incompatible with older Emacsen.
;; Below is a compatible substitution of (backward-char 1).

(defun physical-line-e21-1-backward-composite-char1 ()
  "Backward compatible substitution of (backward-char 1) for Emacs 21.1
`physical-line-e21-1-backward-composite-char1' counts a composite
character as *one* character."
  (let ((prev (1- (point))))
    (if (< prev (point-min))
	(signal 'beginning-of-buffer nil))
    (if (get-char-property prev 'composition)
	(goto-char (previous-single-char-property-change (point) 'composition))
      (goto-char prev))))



;;
;; setup
;;

(if (not (assq 'physical-line-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(physical-line-mode physical-line-mode-string)
		minor-mode-alist)))


;; I'd like to use (version<), but it's not available before 22...
(defalias 'physical-line-vertical-motion
  (cond
   ((eq (string-match "GNU Emacs 22\\." (emacs-version)) 0)
    (if window-system
	(function physical-line-e22-win-vertical-motion)
      (function physical-line-e21-1-tty-vertical-motion)))
   ((eq (string-match "GNU Emacs 21\\.[0-2]\\." (emacs-version)) 0)
    (if window-system
	(function physical-line-e21-1-win-vertical-motion)
      (function physical-line-e21-1-tty-vertical-motion)))
   ((eq (string-match "GNU Emacs 21\\." (emacs-version)) 0)
    (if window-system
	(function physical-line-e21-3-win-vertical-motion)
      (function physical-line-e21-1-tty-vertical-motion)))
   (t
    (function vertical-motion))))

(defalias 'physical-line-backward-composite-char1
  (cond
   ((eq (string-match "GNU Emacs 2[12]\\." (emacs-version)) 0)
    (function physical-line-e21-1-backward-composite-char1))
   (t
    (lambda () (forward-char -1)))))


(run-hooks 'physical-line-mode-load-hook)


(provide 'physical-line)
