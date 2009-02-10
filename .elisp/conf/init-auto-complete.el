(require 'auto-complete)
(global-auto-complete-mode t)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start 3)

;(define-key ac-complete-mode-map "\t" 'ac-complete)
;(define-key ac-complete-mode-map "\r" nil)

;; (require 'ac-dabbrev)
;; (setq ac-sources
;; (list ac-source-dabbrev
;; ))


