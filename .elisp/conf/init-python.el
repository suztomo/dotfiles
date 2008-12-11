(add-hook 'python-mode-hook
              (lambda ()
;                (define-key python-mode-map "\"" 'electric-pair)
;                (define-key python-mode-map "\'" 'electric-pair)
;                (define-key python-mode-map "(" 'electric-pair)
;                (define-key python-mode-map "[" 'electric-pair)
;                (define-key python-mode-map "{" 'electric-pair)
                ))

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))


(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
       (local-file (file-relative-name
                    temp-file
                    (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
           '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook
          '(lambda ()
             (flymake-mode t)))