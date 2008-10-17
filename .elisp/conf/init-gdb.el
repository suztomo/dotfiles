;; Smart-compile & gdb
(setq gdb-buffer-name-head "*gud")
(setq compilation-buffer-name-head "*com")

(defun gdb-before-refresh-find-buffer (buf-list)
  (let ((buf (car buf-list))
        (last-bufs (cdr buf-list)))
    (if (or (string-equal gdb-buffer-name-head (substring (buffer-name buf) 0 (length gdb-buffer-name-head)))
            (string-equal compilation-buffer-name-head (substring (buffer-name buf) 0 (length compilation-buffer-name-head))))
      buf
      (if last-bufs
        (gdb-before-refresh-find-buffer last-bufs)
        nil))))

(defun gdb-before-refresh-find-gdb-window (win-list gdb-buf)
  (let ((win (car win-list))
        last-win (cdr win-list))
    (if (eq (window-buffer win) gdb-buf)
      win
      (if last-win
          (gdb-before-refresh-find-gdb-window last-win gdb-buf)
          nil))))

(defun gdb-before-refresh ()
  (interactive)
  (let ((gdb-buffer (gdb-before-refresh-find-buffer (buffer-list))))
    (progn
      (if (and (neq (current-buffer) gdb-buffer) (> (length (window-list)) 1))
          (command-execute 'next-multiframe-window)
          nil)
;          (let ((gdb-win (gdb-before-refresh-find-gdb-window (window-list) gdb-buffer)))
;            (if (and (neq gdb-win (selected-window)) (> (length (window-list)) 1))
;                (command-execute 'next-multiframe-window)
;                nil)))
      (if gdb-buffer
        (kill-buffer gdb-buffer)
        nil)
      (command-execute 'gdb))))
(define-key global-map "\M-g" 'gdb-before-refresh)

;; Override anything-isearch-again
(define-key global-map "\M-s" 'smart-compile)
