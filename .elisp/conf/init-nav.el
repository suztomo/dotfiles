(setq nav-boring-file-regexps
  (list "\\.pyc$" "\\.o$" "~$" "\\.bak$" "^\\./?$" "/\\."))

;; "^\\./?$" : current directory
;; "^\\.[^/]" : files which begins with dot, e.g., .emacs.el
;; Problem...
;; two "../" appears.

