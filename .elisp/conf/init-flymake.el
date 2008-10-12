;; Flymake
;;(require 'flymake)
(require 'flymake)

;; set-perl5lib
;; 開いたスクリプトのパスに応じて、PERL5LIBにlibを追加してくれる
;; http://svn.coderepos.org/share/lang/elisp/set-perl5lib/set-perl5lib.el
(require 'set-perl5lib)

;; エラー、ウォーニング時のフェイス
;; (custom-set-faces
;;   '(flymake-errline ((((class color)) (:background "Gray90"))))
;;   '(flymake-warnline ((((class color)) (:background "Gray90")))))

(set-face-background 'flymake-errline "red4")
(set-face-foreground 'flymake-errline "Gray90")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")


;; flymake for perl
;; http://unknownplace.org/memo/2007/12/21

(defvar flymake-perl-err-line-patterns '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))
(defconst flymake-allowed-perl-file-name-masks '(("\\.pl$" flymake-perl-init)
                                                ("\\.pm$" flymake-perl-init)
                                                ("\\.t$" flymake-perl-init)))



(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (set-perl5lib)
  (flymake-mode t))


;; ;; C++
;; ;; http://d.hatena.ne.jp/pyopyopyo/20070715/

;; (setq flymake-allowed-file-name-masks
;;       (cons '(".+\\.cpp$"
;;               flymake-simple-make-init
;;               flymake-simple-cleanup
;;               flymake-get-real-file-name)
;;             flymake-allowed-file-name-masks))


(require 'flymake)


(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (flymake-mode t)))



;; エラーをミニバッファに表示
;; http://d.hatena.ne.jp/xcezx/20080314/1205475020
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let** ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let** ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

(defadvice flymake-report-status (before flymake-quite-report-status (e-w &optional status))
  (if (not e-w)
      (progn
;        (flymake-mode t)
        (flymake-log 0 "switched OFF Flymake mode due to unknown fatal status (maybe tramp is running)"))))
(ad-activate 'flymake-report-status)
(add-hook 'cperl-mode-hook '(lambda () (flymake-perl-load)))
