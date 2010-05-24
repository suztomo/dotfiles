;; -*- Emacs-Lisp -*-
;; AmbientTalk-mode Version 0
;; http://beta-reduction.blogspot.com/2010/01/blog-post.html
(require 'cl)


(defcustom at-indent-level 2
  "*Indentation of AmbientTalk statements with respect to containing block."
  :type 'integer
  :group 'at-indentation-details)


(defvar *at-mode-map* (make-keymap))


;; Colors
(defface fudef-face '((t (:foreground "white" :background "black" :bold t :italic nil))) nil)
(defvar fundef-face 'title-face)


(defvar at-mode-syntax-table
  (let ((st (make-syntax-table)))
   (modify-syntax-entry ?\" "\"" st)
   (modify-syntax-entry ?\  " " st)
   (modify-syntax-entry ?^ "(^" st)
   (modify-syntax-entry ?$ ")$" st)
   (modify-syntax-entry ?/ ". 124b" st)
   (modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table used while in `at-mode'.")

(defvar at-mode-syntax-alist
  (list
   (cons ?\" ".   ")
   (cons ?\\ ".   ")
   (cons ?\  " ")
   (cons ?$ "(^")
   (cons ?^ ")$")
   (cons ?/ ". 14")))

(defvar at-language-keywords
  (list "def" "import" "super"))

(defvar at-language-builtins
  (list "if" "raise" "then" "when" "becomes" "try" "catch" "raise"))


(defun at-mode ()
  (interactive)
  (setq major-mode 'at-mode)
  (setq mode-name "AmbientTalk-mode")
  (make-local-variable 'font-lock-defaults)
  (let ((at-mode-font-lock-keywords)
        '(("def" . fundef-face)
          ("if:" . font-lock-builtin-face)))
    (setq font-lock-defaults
        `(,at-mode-font-lock-keywords ; keywords
          nil ;keyword only
          t ; case-fold
          nil ; syntax-alist
          nil)) ; syntax-begin
    )
  (set-syntax-table at-mode-syntax-table)
)




(defun at-mode ()
  "This is AmbientTalk-mode!"
  (interactive)
  (setf major-mode 'at-mode
        mode-name "at-mode")
  (use-local-map *at-mode-map*)
  (set-syntax-table at-mode-syntax-table)
  (let ((at-mode-font-lock-keywords
         '(("def" . font-lock-keyword-face)
           ("if:" . font-lock-builtin-face)
           ("object:" . font-lock-builtin-face)
         )))

    (setq font-lock-defaults
          `(,at-mode-font-lock-keywords ; keywords
            nil ; keywords-only
            t ; case-fold
            nil ; syntax-alist
            nil))) ;syntax-begin

)


(provide 'at-mode)
