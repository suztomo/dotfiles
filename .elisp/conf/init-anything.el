(setq woman-cache-filename (expand-file-name "~/woman_cache"))
;(setq anything-c-adaptive-history-file
;      (expand-file-name "~/dotfiles/.elisp/anything/anything-c-adaptive-history"))
(require 'anything-config)

;(defvar anything-c-adaptive-history-file "~/anything-c-adaptive-history"
;  "Path of file where history information is stored.")


(global-set-key [?\C-;] 'anything)

(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-p" 'anything-previous-line)


; auto-complete.elがあればいらないかな
;(require 'anything-dabbrev-expand)
;(global-set-key "\C-u" 'anything-dabbrev-expand)
;(define-key anything-dabbrev-map "\C-u" 'anything-dabbrev-find-all-buffers)


;; (setq anything-c-locate-db-file "/log/home.simple.locatedb")
;; (setq anything-c-locate-options `("locate" "-d" ,anything-c-locate-db-file "-i" "-r" "--"))



; cscope?
; http://d.hatena.ne.jp/tunefs/20070325/p1
; C-c s c : Calling this function
; C-c s G : find global definition (and Move!)
; C-c s d : find global definition
; C-c s C : find called this function
; xcscope.el is available from http://cscope.sourceforge.net/#downloads
(require 'anything-cscope)

(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-bookmarks
            anything-c-source-cscope-global-definition
            anything-c-source-cscope-calling-this-function
;            anything-c-source-file-name-history
;            anything-c-source-file-cache
;            anything-c-source-man-pages
;            anything-c-source-info-pages
            anything-c-source-calculation-result
;            anything-c-source-plocate
;            anything-c-source-locate
;            anything-c-source-mac-spotlight
            ))
