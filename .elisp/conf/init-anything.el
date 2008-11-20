(setq woman-cache-filename (expand-file-name "~/woman_cache"))

(require 'anything-config)

(define-key global-map "\M-f" 'anything)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-p" 'anything-previous-line)

(setq anything-c-adaptive-history-file
      (expand-file-name "~/dotfiles/.elisp/anything/anything-c-adaptive-history"))

; auto-complete.elがあればいらないかな
;(require 'anything-dabbrev-expand)
;(global-set-key "\C-u" 'anything-dabbrev-expand)
;(define-key anything-dabbrev-map "\C-u" 'anything-dabbrev-find-all-buffers)


;; (setq anything-c-locate-db-file "/log/home.simple.locatedb")
;; (setq anything-c-locate-options `("locate" "-d" ,anything-c-locate-db-file "-i" "-r" "--"))

(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-bookmarks
            anything-c-source-file-name-history
;            anything-c-source-file-cache
            anything-c-source-man-pages
;            anything-c-source-info-pages
            anything-c-source-calculation-result
;            anything-c-source-plocate
;            anything-c-source-locate
            anything-c-source-mac-spotlight
            ))
