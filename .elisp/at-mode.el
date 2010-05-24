;; -*- Emacs-Lisp -*-
;; AmbientTalk-mode Version 0
;; http://beta-reduction.blogspot.com/2010/01/blog-post.html
;; For major mode coding conventions, see
;;   http://www.gnu.org/s/emacs/manual/html_node/elisp/Major-Mode-Conventions.html#Major-Mode-Conventions
(eval-when-compile (require 'cl))


(defcustom at-indent-level 2
  "*Indentation of AmbientTalk statements with respect to containing block."
  :type 'integer
  :group 'at-indentation-details)

;; keymap for at-mode
(defvar *at-mode-map* (make-keymap))


;; Colors
(defface fudef-face '((t (:foreground "white" :background
                                      "black" :bold t :italic nil))) nil)
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

(defvar at-language-keywords
  (list "def" "import" "super"))

(defvar at-language-builtins
  (list "if" "raise" "then" "when" "becomes" "try" "catch" "raise"))

(defun at-make-keyword-face-pair (name)
  (cons name font-lock-keyword-face))

(defun at-make-builtin-face-pair (name)
  (cons (concat name ":") font-lock-builtin-face))


;; Indent function
;; 
(defun at-indent-line ()
  (interactive)
  (progn
    (indent-to 2)))

(defun at-mode ()
  "AmbientTalk-mode"
  (interactive)
  (kill-all-local-variables)
  (setf major-mode 'at-mode
        mode-name "AmbientTalk") ; used in minibuffer
  (use-local-map *at-mode-map*)
  (set-syntax-table at-mode-syntax-table)
  (let* ((language-keywords
         (mapcar 'at-make-keyword-face-pair
                  at-language-keywords))
        (language-builtins
         (mapcar 'at-make-builtin-face-pair
                  at-language-builtins))
        (at-mode-font-lock-keywords
          (append language-keywords language-builtins)))

    (setq font-lock-defaults
          `(,at-mode-font-lock-keywords ; keywords
            nil ; keywords-only
            t ; case-fold
            nil ; syntax-alist
            nil))) ;syntax-begin
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'at-indent-line)
  (run-mode-hooks 'at-mode-hook)
  (message (concat mode-name " loaded."))
)

(provide 'at-mode)
