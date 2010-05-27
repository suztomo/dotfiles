;-*-Lisp-*-
;; AmbientTalk-mode Version 0
;; http://beta-reduction.blogspot.com/2010/01/blog-post.html
;; For major mode coding conventions, see
;;   http://www.gnu.org/s/emacs/manual/html_node/elisp/Major-Mode-Conventions.html#Major-Mode-Conventions
(eval-when-compile (require 'cl))


(defcustom at-indent-level 4
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
  (list "deftype" "def " "import" "super"))

;; http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_367.html

(defvar at-language-builtins
  (list "if" "raise" "then" "else" "when" "becomes" "try" "catch" "raise"
        "foreach" "in" "whenever" "is" "taggedAs"))

(defun at-make-keyword-face-pair (name)
  (cons name font-lock-keyword-face))

(defun at-make-builtin-face-pair (name)
  (cons (concat name ":") font-lock-builtin-face))

(defvar at-mode-debug t)

(defun at-debug (msg)
  (if at-mode-debug
      (message msg)))


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

; 
; abc((()) and "()" => 3
;
(defun at-find-the-first-open-brace (twochars)
  "Position of the most nearest open brace, return nil if no such brace."
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
          (at-debug (number-to-string (point)))
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

(defun at-indent-to (num)
  (let (start end)
    (progn
      (save-excursion
        (progn
          (beginning-of-line)
          (setq start (point))
          (back-to-indentation)
          (setq end (point))))
      (delete-region start end)))
  (indent-to num))

; somebraces( object:{
;     def hoge()
;
(defun at-indent-close-brace-line ()
  "Indents add plus 1"
  (at-debug "indent-close-brace-line")
  (let ((p (at-find-the-first-open-brace "{}"))
        ci)
    (if (eq nil p)
        nil
      (progn
        (save-excursion (goto-char p)
                        (setq ci (current-indentation)))
        (at-indent-to ci)
        t))))

; somefunc(a
;          b);
(defun at-indent-close-paren-line ()
  "Finds the last open braces, and indent to it"
  (at-debug "indent-close-brace-line")
  (let ((p (at-find-the-first-open-brace "()"))
        lbp)
    (if (eq nil p)
        nil
      (progn
        (save-excursion (goto-char p)
                        (setq lbp (current-column)))
        (at-indent-to (+ lbp 1))
        t))))




(defun at-open-bracket-in-prev-line ()
  "Bracket of previous line"
  ; hoge() { () => ?\{
  (save-excursion
    (let ((p (at-point-of-open-bracket-in-prev-line)))
      (if (eq nil p) nil
        (progn (goto-char p) (char-after))))))


(defun at-point-of-open-bracket-in-prev-line ()
  "Point of the open bracket in previous line"
  ; a{}{() => 3
  (at-point-of-unclosed-bracket-in-prev-line ?\( ?\) ?\[ ?\] ?\{ ?\}))

(defun at-point-of-close-bracket-in-prev-line ()
  "Point of the close bracket in previous line"
  ; ai)(){ => 2
  (at-point-of-unclosed-bracket-in-prev-line ?\) ?\( ?\] ?\[ ?\} ?\{))

(defun at-point-of-unclosed-bracket-in-prev-line (a-op a-cl b-op b-cl c-op c-cl)
  (let ((loop-do-flag t)
        (bpl 0)
        (ret nil)
        (c 0)
        (c0 0) ; for (
        (c1 0) ; for [
        (c2 0) ; for {
        p)
    (progn
      (save-excursion
        (at-move-non-white-line-backward)
        (setq bpl (point))
        (forward-line 1)
        (while loop-do-flag
          ; ??? [\(\)\{\}\[\]] ???
          (setq p (re-search-backward "[\(\)\{\}]" bpl t))
          (setq c (+ c 1))
          (if (eq nil p)
              (setq loop-do-flag nil)
            (progn
              (cond
               ((> c 10) (progn (at-debug "baka") (setq loop-do-flag nil)))
               ((char-equal a-op (char-after))
                (if (= 0 c0) (progn (setq ret (point)) (setq loop-do-flag nil))
                  (setq c0 (- c0 1))))
               ((char-equal b-op (char-after))
                (if (= 0 c1) (progn (setq ret (point)) (setq loop-do-flag nil))
                  (setq c1 (- c1 1))))
               ((char-equal c-op (char-after)) 
                (if (= 0 c2) (progn (setq ret (point)) (setq loop-do-flag nil))
                  (setq c2 (- c2 1))))
               ((char-equal a-cl (char-after)) (setq c0 (+ c0 1)))
               ((char-equal b-cl (char-after)) (setq c1 (+ c1 1)))
               ((char-equal c-cl (char-after)) (setq c2 (+ c2 1)))
               (t (at-debug (concat "invalid regexp matching" (string (char-after))))))
              (goto-char p))))))
    ret))



(defun at-first-char-in-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (char-after)))

(defun at-in-comment-p ()
  nil)


(defun at-indent-as-prev-bracket ()
  "Indents as the same previous line"
  (at-debug "at-indent-as-prev-bracket")
  (let (i p)
    (progn
      (save-excursion
        (beginning-of-line)
        (setq p (at-find-the-first-open-brace "[]"))
        (if (eq nil p)
            (at-debug "cannot find bracket...")
          (progn (goto-char p)
                 (setq i (current-column)))))
      (at-indent-to (+ 1 i)))))

(defun at-search-backward-unclosed-semi ()
  (re-search-backward ";" nil t))


(defun at-in-white-char-line-p ()
  (save-excursion
    (back-to-indentation)
    (cond
     ((char-equal ?\t (char-after)) t)
     ((char-equal ?\  (char-after)) t)
     ((char-equal ?\n (char-after)) t)
     (t nil))))


(defun at-move-non-white-line-backward ()
  "moves the non-white line backward"
  (let ((loop-do-flag t) (c 0))
    (progn
      (while loop-do-flag
      (progn
        (forward-line -1)
        (setq c (+ c 1))
        (cond
         ((bobp)
          (progn (setq loop-do-flag nil)))
         ((> c 1000)
          (progn (setq loop-do-flag nil)
                 (at-debug "ba-ka")))
         ((at-in-white-char-line-p)
          (progn
            (at-debug "white line")
            (setq loop-do-flag t)))
         (t (progn
              (at-debug "success to seek next non-white char")
              (setq loop-do-flag nil)))))
      (beginning-of-line)))))

(defun at-line-head-p ()
  (let ((p (point)))
    (save-excursion
      (back-to-indentation)
      (eq p (point)))))

; aa(aa)() and (lambda () (char-equal ?a (char-after))) => move to the 1
; 
(defun at-search-backward-unclosed-cond (cond-p &optional limit)
  "Moves back to the chr that is not closed."
  (if (eq limit nil)
      (setq limit 0))
  (let ((c0 0) ; for (
        (c1 0) ; for {
        (c2 0) ; for [
        (loop-do-flag t)
        a b)
    (progn
      (while loop-do-flag
        (progn
          (backward-char)
          (cond
           ((< (point) limit)
            (progn (setq loop-do-flag nil)))
           ((and (<= c0 0)
                 (<= c1 0)
                 (<= c2 0)
                 (funcall cond-p))
            (progn (setq loop-do-flag nil)))
           ((char-equal (char-after) ?\))
            (progn (setq c0 (+ c0 1))))
           ((char-equal (char-after) ?\})
            (progn (setq c1 (+ c1 1))))
           ((char-equal (char-after) ?\])
            (progn (setq c2 (+ c2 1))))
           ((char-equal (char-after) ?\()
            (progn (setq c0 (- c0 1))))
           ((char-equal (char-after) ?\{)
            (progn (setq c1 (- c1 1))))
           ((char-equal (char-after) ?\[)
            (progn (setq c2 (- c2 1))))))))))


(defun at-search-backward-unclosed-line-head ()
  "Moves the line head that is not closed in brackets"
  (at-search-backward-unclosed-cond 'at-line-head-p))


(defun at-indent-as-prev-unclosed-line-head ()
  (at-debug "at-indent-as-prev-unclosed-line-head")
  (let (a)
    (save-excursion
      (at-search-backward-unclosed-line-head)
      (setq a (current-indentation)))
    (at-indent-to a)))

(defun at-indent-prev-unclosed-line-head-plus1 ()
  "Indents plus 1 level by the unclosed line head"
  (at-debug "at-indent-as-prev-unclosed-line-head")
  (let (a)
    (save-excursion
      (at-search-backward-unclosed-line-head)
      (setq a (current-indentation)))
    (at-indent-to (+ at-indent-level a))))

;
; def func (object: { <-
;    takotao
(defun at-indent-as-prev-unclosed-open-brace-line ()
  "Indents as prev unclosed open brace"
  (at-debug "at-indent-as-prev-unclosed-open-brace-line")
  (let (a)
    (save-excursion
      (at-search-backward-unclosed-cond
       '(lambda () (char-equal ?\{ (char-after))))
      (at-debug (number-to-string (point)))
      (at-search-backward-unclosed-line-head)
      (at-debug (number-to-string (point)))
      (setq a (current-indentation)))
    (at-indent-to a)))

(defun p ()
  (interactive)
  (at-debug (number-to-string (point))))


(defun at-indent-as-prev-paren ()
  "Indents as the same previous line"
  (at-debug "at-indent-as-prev-paren")
  (let (i p)
    (progn
      (save-excursion
        (beginning-of-line)
        (setq p (at-find-the-first-open-brace "()"))
        (if (eq nil p)
            (at-debug "cannot find brace...")
          (progn (goto-char p)
                 (setq i (current-column)))))
      (at-indent-to (+ 1 i)))))

(defun at-indent-line ()
  (interactive)
  (at-debug "at-indent-line")
  (if (at-in-comment-p) nil
      (save-excursion
        (beginning-of-line)
        (let ((fc (at-first-char-in-line)))
          (cond
           ((and (numberp fc) (or (char-equal ?\} fc) (char-equal ?\) fc)))
            (progn
              (at-debug "first char in line is bracket")
              (cond
               ((char-equal ?\} fc) (at-indent-as-prev-unclosed-open-brace-line))
               ((char-equal ?\) fc) (at-indent-close-paren-line)))))
           (t (let ((first-open-bracket (at-open-bracket-in-prev-line)))
                (cond
                 ((eq nil first-open-bracket)
                  (progn (at-debug "no open paren in prevline")
                         (at-indent-as-prev-unclosed-line-head)))
                 ((char-equal ?\( first-open-bracket) (at-indent-as-prev-paren))
                 ((char-equal ?\{ first-open-bracket) (at-indent-prev-unclosed-line-head-plus1))
                 ((char-equal ?\[ first-open-bracket) (at-indent-as-prev-bracket))
                 (t (at-debug "invalid char of first-open-bracket"))))))))
    (back-to-indentation)))



(setq at-local-map (make-keymap))
;(define-key at-local-map "\C-ci" 'at-indent-line)

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
  (at-debug (concat mode-name " loaded."))
)

(provide 'at-mode)

