;; -*-Lisp-*-
;; AmbientTalk-mode Version 0.1
;;
;; Copyright (c) 2010 Tomohiro Suzuki
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.


;; Usage
;; You can download the ambienttalk.el, put it in "loadpath" of your Emacs,
;; and add the following two lines to your .emacs file:
;;  (load "ambienttalk")
;;  (setq auto-mode-alist (cons '("??.at" . at-mode) auto-mode-alist))



;; For major mode coding conventions, see
;;   http://www.gnu.org/s/emacs/manual/html_node/elisp/Major-Mode-Conventions.html#Major-Mode-Conventions
(eval-when-compile (require 'cl))


;; TODO: switch spaces to tab?
(defcustom at-indent-level 4
  "*Indentation of AmbientTalk statements with respect to containing block."
  :type 'integer
  :group 'at-indentation-details)

;; keymap for at-mode
(defvar at-mode-map (make-keymap))

(define-key at-mode-map "}" 'at-electric-brace)


(defun at-electric-brace ()
  "Inserts curly brace and Indents the line"
  (interactive)
  (insert "}")
  (at-indent-line)
  (forward-char)
  )

;; Colors
(defface fudef-face '((t (:foreground "white" :background
                                      "black" :bold t :italic nil))) nil)
(defvar fundef-face 'title-face)

(defvar at-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\  " " st)
    ;;    (modify-syntax-entry ?^ "(^" st)
    ;;    (modify-syntax-entry ?$ ")$" st) *)
    ;;    (modify-syntax-entry ?/ ". 124b" st)
    ;;    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\^m "> b" st) ; Give CR the same syntax as newline, for selective-display
    st)
  "Syntax table used while in `at-mode'.")

(defvar at-language-keywords
  (list "deftype" "def " "import" "super"))

;; e.f. http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_367.html
(defvar at-language-builtins
  (list "if" "raise" "then" "else" "when" "becomes" "try" "catch" "raise"
        "foreach" "in" "whenever" "is" "taggedAs" "export" "as" "object"))

;; c.f. http://www.gnu.org/s/emacs/manual/html_node/emacs/Font-Lock.html#Font-Lock
(defun at-make-keyword-face-pair (name)
  (cons name font-lock-keyword-face))

(defun at-make-builtin-face-pair (name)
  (cons (concat name ":") font-lock-builtin-face))

(defun at-font-lock-add-function-name-by-regexp (regexp)
  (font-lock-add-keywords nil
                 (list `(,regexp 1 font-lock-function-name-face nil))))


(defun at-font-lock-add-keywords-by-regexps (lst)
  (if lst
      (progn (at-font-lock-add-keyword-by-regexp (car lst))
             (at-font-lock-add-keywords-by-regexps (cdr lst)))))


;; Level?
;; http://www.gnu.org/s/emacs/manual/html_node/elisp/Levels-of-Font-Lock.html#Levels-of-Font-Lock
(defun at-setup-font-lock-const () 
  (let* ((language-keywords
          (mapcar 'at-make-keyword-face-pair
                  at-language-keywords))
         (language-builtins
          (mapcar 'at-make-builtin-face-pair
                  at-language-builtins))
         (at-mode-font-lock-keywords
          (append language-keywords language-builtins)))

    (setq font-lock-defaults
          `(,language-keywords ; keywords
            nil                         ; keywords-only
            t                           ; case-fold
            nil                         ; syntax-alist
            nil)))                      ;syntax-begin
)


;; Syntax highlights
;; todo:
;;    def [future, resolver] := futureMaker(receiver);

(defun at-setup-font-lock ()
  (at-setup-font-lock-const)
  (font-lock-add-keywords nil
                 `(("deftype\s+\\<\\([a-z]+\\)" 1 font-lock-constant-face nil)))
  (font-lock-add-keywords nil
                 `(("def\s+\\<\\([a-z]+\\)\s+:=" 1 font-lock-variable-name-face nil)))
  (font-lock-add-keywords nil
                 `(("def\s+\\<\\([a-z]+\\)\s*(" 1 font-lock-function-name-face nil)))
; ??? doesn't work...
;  (font-lock-add-keywords nil
;                 `(("\\<\\(/[\.|a-z|A-Z]\\)" 1 font-lock-function-name-face nil)))
;  (font-lock-add-keywords nil
;                 `((":\s*\\<\\([a-z]+\\)" 1 font-lock-variable-name-face nil)))
  (at-font-lock-add-function-name-by-regexp "\\<\\([a-z]+\\):")
)




(setq at-mode-debug-flag nil)

(defun at-debug (msg)
  (if at-mode-debug-flag
      (message msg)))


;; Indent functions
;; Goal:
;; -- nested def braces
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

;; Known Bugs
;;      code
;; //  somecode
;;  somecode  //<TAB> never aligns indentation
;;
;; tako {
;; // hoge :)
;; }fuga //<TAB> never aligns indentation
;;
;;
;;   def up() { count := count + 1; }
;; moves the cursor to strange position
;;
;;
;;  def k () {
;;  <TAB> doesn not align; it actually puts spaces but goes to the first column
;;

(defun at-char-at (str pos)
  "Returns character at the position."
  (string-to-char (substring str pos (+ 1 pos))))

;; 
;; abc((()) and "()" => 3
;;
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
          (regexp 
           (if (string-equal twochars "[]") "[]\[]"
                    (concat "["
                            ""
                            (substring twochars 0 1)
                            ""
                            (substring twochars 1 2)
                            "]")
                    )
           ))
      (while loop-do-flag
        (progn
          (setq p (re-search-backward regexp nil t))
          (cond
           ((eq nil p)               ; reaches the beginning of buffer
            (progn (setq ret nil) (setq loop-do-flag nil)))
           ((char-equal chr-open (char-after))
            (if (= c 0)                 ; open paren
                (progn                  ; success
                  (setq loop-do-flag nil)
                  (setq ret (point)))
              (setq c (- 1 c))))
           ((char-equal chr-close (char-after)) ; close paren
            (setq c (+ 1 c))))))
      ret)))

(defun at-indent-to (num)
  (message "indenting to %d" num)
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

;; somebraces( object:{
;;    def hoge()
;;
(defun at-indent-close-brace-line ()
  "Indents add plus 1"
  (let ((p (at-find-the-first-open-brace "{}"))
        ci)
    (if (eq nil p)
        nil
      (progn
        (save-excursion (goto-char p)
                        (setq ci (current-indentation)))
        (at-indent-to ci)
        t))))

;; somefunc(a
;;          b);
(defun at-indent-close-paren-line ()
  "Finds the last open braces, and indent to it"
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
  ;; hoge() { () => ?\{
  (save-excursion
    (let ((p (at-point-of-open-bracket-in-prev-line)))
      (if (eq nil p) nil
        (progn (goto-char p) (char-after))))))


(defun at-point-of-open-bracket-in-prev-line ()
  "Point of the open bracket in previous line"
  ;; a{}{() => 3
  ;; nil if the line has no previous line
  (at-point-of-unclosed-bracket-in-prev-line ?\( ?\) ?\[ ?\] ?\{ ?\}))

(defun at-point-of-close-bracket-in-prev-line ()
  "Point of the close bracket in previous line"
  ;; ai)(){ => 2
  (at-point-of-unclosed-bracket-in-prev-line ?\) ?\( ?\] ?\[ ?\} ?\{))


(defun at-point-of-unclosed-bracket-in-prev-line (a-op a-cl b-op b-cl c-op c-cl)
  (let ((loop-do-flag t)
        (bpl 0)
        (ret nil)
        (c 0)
        (c0 0)  ;; for (
        (c1 0)  ;; for [
        (c2 0)  ;; for {
        p)
    (progn
      (save-excursion
        (at-move-non-white-line-backward)
        (setq bpl (point))
        (forward-line 1)
        (while loop-do-flag
          ;; ??? [\(\)\{\}\[\]] ???
          (setq p (re-search-backward "[]\[(){}]" bpl t))
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
  (let (a b (c (point)))
    (save-excursion
      (setq a (re-search-backward "/\\*" nil t))
      (goto-char c)
      (setq b (re-search-backward "*/" nil t))
      (and a (if b (> a b)
               t)))))

(defun hoge ()
  (interactive)
  (if (at-in-comment-p)
      (message "in comment")
    (message "out of comment")))

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
            (progn (setq loop-do-flag nil)))
           ((at-in-white-char-line-p)
            (progn
              (setq loop-do-flag t)))
           (t (progn
                (setq loop-do-flag nil)))))
        (beginning-of-line)))))

(defun at-line-head-p ()
  (let ((p (point)))
    (save-excursion
      (back-to-indentation)
      (eq p (point)))))

(defun at-search-backward-unclosed-cond (cond-p &optional limit cond-first)
  "Moves back to the chr that is not closed."
  (if (eq limit nil)
      (setq limit 0))
  (let ((c0 0)                          ; for (
        (c1 0)                          ; for {
        (c2 0)                          ; for [
        (loop-do-flag t)
        a b)
    (progn
      (while loop-do-flag
        (progn
          (if (bobp)
              (setq loop-do-flag nil)
            (backward-char))
                                        ;          (message (string (char-after)))
                                        ;          (message (format "%c(%d): %d %d %d" (char-after) (point) c0 c1 c2))
          (cond
           ((and cond-first
                 (<= c0 0)
                 (<= c1 0)
                 (<= c2 0)
                 (funcall cond-p))
            (progn 
                                        ;              (message "satisfied") 
              (setq loop-do-flag nil)))
                                        ;
                                        ; b m2() {
                                        ;     c: {}
                                        ;
           ((<= (point) limit)
            (progn (setq loop-do-flag nil)))
           ((char-equal (char-after) ?\()
            (progn (setq c0 (if (> c0 0) (- c0 1) 0))))
           ((char-equal (char-after) ?\{)
            (progn (setq c1 (if (> c1 0) (- c1 1) 0))))
           ((char-equal (char-after) ?\[)
            (progn (setq c2 (if (> c2 0) (- c2 1) 0))))
           ((char-equal (char-after) ?\))
            (progn (setq c0 (+ c0 1))))
           ((char-equal (char-after) ?\})
            (progn (setq c1 (+ c1 1))))
           ((char-equal (char-after) ?\])
            (progn (setq c2 (+ c2 1))))
           ((and (<= c0 0)
                 (<= c1 0)
                 (<= c2 0)
                 (funcall cond-p))
            (progn
              ;;              (message "satisfied")
              (setq loop-do-flag nil)))
           ))))))

(defun at-empty-line-p ()
  (and (eolp) (bolp)))

(defun at-search-backward-unclosed-line-head ()
  "Moves the line head that is not closed in brackets"
  (at-search-backward-unclosed-cond
   '(lambda () (and (at-line-head-p) (not (at-empty-line-p))))))


(defun at-indent-as-prev-unclosed-line-head ()
  (at-debug "at-indent-as-prev-unclosed-line-head")
  (let (a)
    (save-excursion
      (at-search-backward-unclosed-line-head)
      (setq a (current-indentation)))

    (at-indent-to a)))

(defun at-indent-prev-unclosed-line-head-plus1 ()
  "Indents plus 1 level by the unclosed line head"
  (at-debug "at-indent-as-prev-unclosed-line-head-plus1")
  (let (a)
    (save-excursion
      (at-search-backward-unclosed-line-head)
      (setq a (current-indentation)))
    (at-indent-to (+ at-indent-level a))))

;;
;; def func (object: { <-
;;    takotao
(defun at-indent-as-prev-unclosed-open-brace-line ()
  "Indents as prev unclosed open brace"
  (at-debug "at-indent-as-prev-unclosed-open-brace-line")
  (let (a)
    (save-excursion
      (at-search-backward-unclosed-cond
       '(lambda () (char-equal ?\{ (char-after))) nil t)
      (at-search-backward-unclosed-line-head)
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
      (if (bobp)
          (progn
            (message "first line"))
        (let ((fc (at-first-char-in-line)))
          (cond
           ((and (numberp fc) (or (char-equal ?\} fc) (char-equal ?\) fc)))
            (progn
              (at-debug "first char in line is bracket")
              (cond
               ((char-equal ?\} fc) (at-indent-as-prev-unclosed-open-brace-line))
               ((char-equal ?\) fc) (at-indent-close-paren-line)))))
           (t (let ((first-open-bracket (at-open-bracket-in-prev-line)))
                ;;              (message (string first-open-bracket))
                (cond
                 ((eq nil first-open-bracket)
                  (progn (at-debug "no open bracket in previous line")
                         (at-indent-as-prev-unclosed-line-head)))
                 ((char-equal ?\( first-open-bracket) (at-indent-as-prev-paren))
                 ((char-equal ?\{ first-open-bracket) (at-indent-prev-unclosed-line-head-plus1))
                 ((char-equal ?\[ first-open-bracket) (at-indent-as-prev-bracket))
                 (t (at-debug "invalid char of first-open-bracket"))))))))

      )
      (end-of-line)
      (back-to-indentation)))



;; (defun at-mode-indent-test-p (before after &optional comm)
;;   "Runs indentation, returns nil if no error or indented text if error"
;;   (if (eq nil comm) (setq comm 'at-indent-line))
;;   (let ((indent-func-backup 'indent-line-function)
;;         (buffer-backup (current-buffer))
;;         (sandbox-buffer (get-buffer-create "*at-test-sandbox*"))
;;         indented-text)
;;     (save-excursion
;;       (set-buffer sandbox-buffer)
;;       (at-mode)
;;       (insert before)
;;       (setq indent-line-function comm)
;;       (setq indent-region-function nil)
;;       (indent-region (point-min) (point-max))
;;       (setq indented-text (buffer-string))
;;       (kill-buffer (current-buffer))
;;       (set-buffer buffer-backup)
;;       (if (string-equal indented-text after) nil
;;         indented-text))))

(defun at-mode-indent-test-p (before after &optional comm)
  "Runs indentation, returns nil if no error or indented text if error"
  (if (eq nil comm) (setq comm 'at-indent-line))
  (at-mode-test-sandbox-cond-p
   before
   '(lambda ()
      (progn
        (setq indent-line-function 'at-indent-line)
        (setq indent-region-function nil)
        (indent-region (point-min) (point-max))))
   '(lambda ()
      (if (string-equal (buffer-string) after) nil
        (buffer-string)))))



(setq at-mode-indent-test-cases '(("
when: hoge becomes: {|tako|
opera(tako);
}
" "
when: hoge becomes: {|tako|
    opera(tako);
}
") ("
when: hoge becomes: {|tako|
op2};
op3();
" "
when: hoge becomes: {|tako|
    op2};
op3();
") ("
func1(arg1, arg2
arg3, arg4,
  arg5);
func2();
" "
func1(arg1, arg2
      arg3, arg4,
      arg5);
func2();
") ("
def o := object: {
         def func()
}
" "
def o := object: {
    def func()
}
") ("
def obj := object: {

def m1();}
def obj2
" "
def obj := object: {

    def m1();}
def obj2
") ("
op1({|arg2|
      someop2(tako
       );
    someop3();});
" "
op1({|arg2|
    someop2(tako
            );
    someop3();});
") ("
operation({|arg|
 op1({|arg2|
      someop2(tako
       );
    someop3();});
});
" "
operation({|arg|
    op1({|arg2|
        someop2(tako
                );
        someop3();});
});
") ("
if: {
  hoge
       } else {
     tako
}
if: (takotako) then: {fugafuga}
    operation();
" "
if: {
    hoge
} else {
    tako
}
if: (takotako) then: {fugafuga}
operation();
") ("
func1(arg1, object: {
def m1() {}
def m2() {
   object: {};
}
" "
func1(arg1, object: {
    def m1() {}
    def m2() {
        object: {};
    }
") ("
def a() {
  fuga
   def o := object: {}
def p := object: {
           def tako() {
              operation();
}}
}
" "
def a() {
    fuga
    def o := object: {}
    def p := object: {
        def tako() {
            operation();
        }}
}
") ("
def func(hoge, fuga, tako,
      uma, niko) {
    opera(niko, pass, fuga,
     meko, tako);
}
" "
def func(hoge, fuga, tako,
         uma, niko) {
    opera(niko, pass, fuga,
          meko, tako);
}
") ("
when: hoge becomes: {|tako|
when: hoge becomes: {|tako|
       opera(tako);
     when: hoge becomes: {|tako|
      opera(tako);
     }
  }
" "
when: hoge becomes: {|tako|
    when: hoge becomes: {|tako|
        opera(tako);
        when: hoge becomes: {|tako|
            opera(tako);
        }
    }
") ("
def t := [a, b, 
c, d];
" "
def t := [a, b, 
          c, d];
") ))


(defun at-mode-test-indent-run-iter (case-list count)
  (if case-list
      (let* ((c (car case-list))
             (before (car c))
             (after (car (cdr c)))
             (comm (car (cdr (cdr c))))
             msg ret)
        (progn
          (setq msg (format "  test %d: " count))
                                        ;          (condition-case ex
          (setq error (at-mode-indent-test-p before after comm))
                                        ;            (setq error (format "Caught exception: [%s]" ex)))
          (if (eq nil error) (setq msg (concat msg "passed."))
            (progn
              (setq msg (concat msg "FAILED.\n--- Before:" before "\n--- After:"
                                error))))
          (message msg)
          (at-mode-test-indent-run-iter (cdr case-list) (+ count 1))))))

(defun at-mode-test-indent-run ()
  "Runs indentation tests"
  (interactive)
  (message "AmbientTalk mode indentation tests")
  (at-mode-test-indent-run-iter at-mode-indent-test-cases 0))


(defun at-mode-test-sandbox (txt comm)
  "Runs the command in sandbox, returns the result"
  (let ((buffer-backup (current-buffer))
        (sandbox-buffer (get-buffer-create "*at-test-sandbox*"))
        modified-text ret)
    (save-excursion
      (set-buffer sandbox-buffer)
      (at-mode)
      (insert txt)
      (setq ret (funcall comm))
      (kill-buffer (current-buffer))
      (set-buffer buffer-backup)
      ret)))

(defun at-mode-test-sandbox-cond-p (txt comm1 cond-p)
  (at-mode-test-sandbox txt '(lambda () (progn (funcall comm1) (funcall cond-p)))))


(setq at-mode-sandbox-test-cases
      '((
         "abcd(
  efgc()
hijk"
         (lambda () (progn (beginning-of-line) (at-search-backward-unclosed-line-head)))
         (lambda () (char-equal ?e (char-after)))
         ) (
         "abcd(xyz(
  efgc)
hijk"
         (lambda () (progn (beginning-of-line) (at-search-backward-unclosed-line-head)))
         (lambda () (char-equal ?a (char-after)))
         ) (
         "op1({|arg2|
      abc(tako
       );
    someop3();});"
         (lambda () (progn (beginning-of-line) (at-search-backward-unclosed-line-head)))
         (lambda () (char-equal ?a (char-after)))
         ) (
         "
    a m1() {}
    b m2() {
        c: {};
        }"
         (lambda () (progn
                      (beginning-of-line)
                      (at-search-backward-unclosed-cond
                       '(lambda () (char-equal ?\{ (char-after))) nil t)))
         (lambda () (progn  (= 27 (point))))
         ) (
         "if: {
    hoge
} else {
        tako"
         (lambda () (progn
                      (beginning-of-line)
                      (at-search-backward-unclosed-line-head)))
         (lambda () (progn (= 1 (point))))
         ) (
         "def t := [a, b,
c, d]"
         (lambda () ())
         (lambda () (progn
                      (at-debug (number-to-string (point)))
                      (let ((chr (at-open-bracket-in-prev-line)))
                        (and chr (progn
                                   (char-equal ?\[ chr))))))
         ) (
"def func() {
    def tako {
    }

kome"
         (lambda () (progn
                      (end-of-buffer) (at-indent-line)))
         (lambda () (progn
                      (= (current-indentation) 4)
                      )
         ))  (
"f {
    d t{
    }

kome"
         (lambda () (progn
                      (end-of-buffer)
                      (beginning-of-line)
                      (at-search-backward-unclosed-line-head)))
         (lambda () (progn
                      (= (point) 9)
                      )
         ))
(
"a
iu
/*
comment
*/
hoge"
         (lambda () (progn
                      (goto-char 10)))
         (lambda () (progn
                      (at-in-comment-p)
                      )
         ))
(
""
(lambda () ())
(lambda () (progn
             (not (at-in-comment-p))
             )
))
(
"a
iu
/*

*/
hogehoge
/*
comment
*/
hoge"
(lambda () (progn
             (goto-char 16)))
(lambda () (progn
             (not (at-in-comment-p))
             )
))
(
"def f() {"
(lambda () (progn
             (at-indent-line)))
(lambda () (progn
             (message "point: %d" (point))
             (= (point) 1))
  ))
))


(defun at-mode-sandbox-test-run-iter (case-list count)
  (if case-list
      (let* ((c (car case-list))
             (txt (car c))
             (comm (car (cdr c)))
             (cond-p (car (cdr (cdr c))))
             msg ret)
        (progn
          (setq msg (format "  test %d: " count))
          (setq result (at-mode-test-sandbox-cond-p txt comm cond-p))
          (if result (setq msg (concat msg "passed."))
            (progn
              (setq msg (concat msg "FAILED.\n--- Text:\n" txt))))
          (message msg)
          (at-mode-sandbox-test-run-iter (cdr case-list) (+ count 1))))))

(defun at-mode-sandbox-test-run ()
  (interactive)
  (message "AmbientTalk mode sandbox tests")
  (at-mode-sandbox-test-run-iter at-mode-sandbox-test-cases 0))


(defun at-mode-test-run ()
  (interactive)
  (at-mode-test-indent-run)
  (at-mode-sandbox-test-run))

(defun at-mode ()
  "Major mode for editing AmbientTalk source files in Emacs.

This module provides syntax highlight and indentation.
There is no other functionaly

useful commands:
    TAB     - Indent the line.

"
  (interactive)
  (kill-all-local-variables)
  (setf major-mode 'at-mode
        mode-name "AmbientTalk")   ; used in minibuffer
  (use-local-map at-mode-map)
  (set-syntax-table at-mode-syntax-table)
  (at-setup-font-lock)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'at-indent-line)
  (run-mode-hooks 'at-mode-hook)
  (at-debug (concat mode-name " loaded."))
  )

(provide 'at-mode)

