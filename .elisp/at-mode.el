;-*-Lisp-*-
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
;; Goal:
;; --- braces
;; when: hoge becomes: {|tako|
;;     opera(tako);
;; }
;; --- one line keyword function
;; when: tako becomes: {|tako| niko} except: { fuga };
;; operation(tako);
;; -- nested def braces
;; def a() {
;;     fuga
;;     def o := object: {}
;;     def p := object: {
;;         def tako() {
;;             operation();
;;         }
;;     }
;; }
;; --- arguments and parameters
;; def func(hoge, fuga, tako,
;;          uma, niko) {
;;      opera(niko, pass, fuga,
;;            meko, tako);
;; }
;; --- nested keyword function braces
;; when: hoge becomes: {|tako|
;;     when: hoge becomes: {|tako|
;;         opera(tako);
;;         when: hoge becomes: {|tako|
;;             opera(tako);
;;         }
;;     }
;; }
;; --- if then else
;; if: {
;;     hoge
;; } else {
;;     tako
;; }
;; if: (takotako) then: {fugafuga}
;; operation();
;; --- tables
;; def t := [a, b, 
;;           c, d];
;; --- closure as parameter
;; operation({|arg|
;;     op1({|arg2|
;;          someop2(tako
;;                  );
;;          someop3();});
;; });
;; 
;; Algorithms
;; if the head cursor is '}'
;;   find matching '{' and the same indentation as the line
;; if the head cursor is ')'
;;   find matching '(' and the same indentation as the paran
;; search the first '(', '{', or '[' in the previous line from its end,
;; that doesn't match its pair.
;;   if there is no such bracket
;;     the same indentation as the previous line
;;   if the cursor is inside parens or brackets
;;     indentation will be the same as previous open paren
;;   if the cursor is inside braces
;;     indentation will be bigger by one-level from the previous line's one

(cons ?\( ?\})
(cons ?a ?\[)

(defun at-char-at (str pos)
  "Returns character at the position."
  (string-to-char (substring str pos (+ 1 pos))))

(at-find-the-first-open-brace "{}")

(defun at-find-the-first-open-brace (twochars)
  "Finds position of the most nearest open brace, return nil if no such brace."
  (save-excursion
    (let ((c 0)
          (p (point))
          (loop-do-flag t)
          (ret nil)
          (chr-open (at-char-at twochars 0))
          (chr-close (at-char-at twochars 1))
          ; regexp will be "[\(\)]"
          (regexp (concat "["
                          "\\"
                          (substring twochars 0 1)
                          "\\"
                          (substring twochars 1 2)
                          "]")))
      (while loop-do-flag
        (progn
          (setq p (re-search-backward regexp nil t))
          (cond
           ((eq nil p) ; reaches the beginning of buffer
            (progn (setq ret nil) (setq loop-do-flag nil)))
           ((char-equal chr-open (char-after)) 
            (if (= c 0) ; open paren
                (progn ; success
                  (setq loop-do-flag nil)
                  (setq ret (point)))
              (setq c (- 1 c))))
           ((char-equal chr-close (char-after)) ; close paren
            (setq c (+ 1 c))))))
      ret)))

(defun hoge ()
  (interactive)
  (let ((p (at-find-the-first-open-brace "{}")))
    (if (eq nil p)
        (message "cannot found...")
      (progn
        (message "found open brace")
        (goto-char p)))))

(defun at-indent-close-brace-line ()
  (message "indent-close-brace-line")
  (let ((p (at-find-the-first-open-brace "{}"))
        ci)
    (if (eq nil p)
        nil
      (progn
        (save-excursion (goto-char p)
                        (setq ci (current-indentation)))
        (indent-to ci)
        t))))

;; find the last open braces


(defun at-indent-close-paren-line ()
  (message "indent-close-brace-line"))

(defun at-open-bracket-in-prev-line ()
  ?\()

(defun at-indent-as-previous-paren ()
  (message "indent-as-previous-line"))

(defun at-indent-as-previous-brace-line ()
  (message "indent-as-previous-brace-line"))

(defun at-first-char-in-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (message (char-to-string (char-after)))
    (char-after)))

(defun at-in-comment-p ()
  nil)


(defun at-indent-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((fc (at-first-char-in-line)))
      (cond
       ((at-in-comment-p) nil)
       ((char-equal ?\} fc) (at-indent-close-brace-line))
       ((char-equal ?\) fc) (at-indent-close-paren-line))
       (t (let ((first-open-bracket (at-open-bracket-in-prev-line)))
            (cond
             ((eq nil first-open-bracket) (at-indent-as-previous-line))
             ((char-equal ?\( first-open-bracket) (at-indent-as-previous-paren))
             ((char-equal ?\{ first-open-bracket) (at-indent-add-previous-brace-line))
             (t (message "invalid char of first-open-bracket"))))))))
  )


(setq at-local-map (make-keymap))
(define-key at-local-map "h" 'backward-char)
(define-key at-local-map "\C-ci" 'at-indent-line)

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
  (use-local-map at-local-map)
  (run-mode-hooks 'at-mode-hook)
  (message (concat mode-name " loaded."))
)

(provide 'at-mode)




