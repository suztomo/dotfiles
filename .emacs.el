;; (setq load-path (cons "~/dotfiles/.elisp/tuareg" load-path))
;; (setq load-path (cons "~/dotfiles/.elisp/" load-path))

(setq debug-on-error nil)
(add-to-list 'load-path "~/dotfiles/.elisp")
(add-to-list 'load-path "~/dotfiles/.elisp/conf")
(add-to-list 'load-path "~/dotfiles/.elisp/tuareg")

;; C-h to backspace
(global-set-key "\C-h" 'delete-backward-char)

;;(setq auto-mode-alist (cons '("??.ml??w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)


(line-number-mode t)
;(column-number-mode nil)


;; Tabはspace x 4
(setq-default tab-width 4 indent-tabs-mode nil)


;;smart-compile 
(require 'smart-compile)

;; utf-8
(progn
 (set-language-environment 'Japanese)
  (set-terminal-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
;; For yatex mode
;; (setq coding-system-for-read 'mule-utf-8-unix)
  (prefer-coding-system 'utf-8)
 (set-default-coding-systems 'utf-8)
 (set-keyboard-coding-system 'utf-8)
 (set-buffer-file-coding-system 'utf-8-unix)
)



(when (eq window-system 'mac)
  (add-hook 'window-setup-hook
            (lambda ()
;;              (setq mac-autohide-menubar-on-maximize t)
              (set-frame-parameter nil 'fullscreen 'fullboth)
              )))


(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

;; Carbon Emacsの設定で入れられた
(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 )



;;Color
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 80)
   ))



;; Highlight Parens
(load "mic-paren.el")
(require 'mic-paren)
(paren-activate)

;; Visible marked area
(transient-mark-mode t)


;; GNU Global Settings
(load "gtags.el")
;; (autoload 'gtags-mode "gtags" "" t)
;; (setq gtags-mode-hook
;;       '(lambda ()
;;          (local-set-key "\M-t" 'gtags-find-tag)
;;          (local-set-key "\M-r" 'gtags-find-rtag)
;;          (local-set-key "\M-s" 'gtags-find-symbol)
;;          (local-set-key "\C-t" 'gtags-pop-stack)
;;          ))






;; Yatex Mode
(setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
(add-hook 'yatex-mode-hook' (lambda () (setq auto-fill-function nil)))


;; Smooth down key
(progn
 (setq scroll-step 1)
 (setq scroll-conservatively 4))

;; Answer y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; no backup file
;(progn
;  (setq auto-save-list-file-name nil)
;  (setq auto-save-list-file-prefix nil)
;  (setq make-backup-files nil))

;; Show time in status line
(progn
  (setq display-time-24hr-format t)
  (setq display-time-format "%Y-%m-%d(%a) %H:%M")
  (setq display-time-day-and-date t)
  (setq display-time-interval 30)
  (display-time))



;; Ack
;; http://d.hatena.ne.jp/antipop/20080311/1205252552
(defun ack ()
  (interactive)
  (let ((grep-find-command "ack --nocolor --nogroup "))
    (call-interactively 'grep-find)))


;; C-o to move window
;; But it conflicts "Open directory" in Dired-mode....
(global-set-key "\C-o" 'next-multiframe-window)

;; Orikaeshi
(setq truncate-partial-width-windows nil)


;; Pysical Move, (not logical move)
(global-set-key "\C-p" 'previous-window-line)
(global-set-key "\C-n" 'next-window-line)
(defun previous-window-line (n)
  (interactive "p")
  (let ((cur-col
     (- (current-column)
        (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion (- n))
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook)
  )
(defun next-window-line (n)
  (interactive "p")
  (let ((cur-col
     (- (current-column)
        (save-excursion (vertical-motion 0) (current-column))))) 
    (vertical-motion n)
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook)
  )




;; Yasnippet
(require 'yasnippet-bundle)


;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)




(load "init-shell")
(load "init-flymake")
(load "init-js2")
(load "init-jaspace")
(load "init-skk")
(load "init-perl")
;(load "init-cpp")
;(load "init-autosave-enhanced")
(load "init-autosave")
(load "init-anything")
(load "init-revive")
;(load "init-html")



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
