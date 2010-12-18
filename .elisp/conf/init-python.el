

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


; Sets environemntal variable PYTHONPATH used in pylint
; the pythonpath cannot be inherited from zsh. So you need to specify it.
(progn
  (setenv "PYTHONPATH" "/Applications/GoogleAppEngineLauncher.app/Contents/Resources/GoogleAppEngine-default.bundle/Contents/Resources/google_appengine/:")
  (setenv "PYTHONPATH" (concat "/Users/suztomo/Documents/gae/rss-twit/third_party:"
                               (getenv "PYTHONPATH")))
  (setenv "PYTHONPATH" (concat "/usr/local/google_appengine/:/usr/local/google_appengine/lib/yaml/lib/:/usr/local/google_appengine/lib/webob/:/usr/local/google_appengine/lib/django/django/:" (getenv "PYTHONPATH")))
  (setenv "PYTHONPATH" (concat "./:./third_party:" (getenv "PYTHONPATH")))
)


; PythonMode
; http://www.emacswiki.org/cgi-bin/wiki/PythonMode
; To enable this flymake configuration, you need to have epylint.py

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
       (local-file (file-relative-name
                    temp-file
                    (file-name-directory buffer-file-name))))
      (list "~/dotfiles/bin/epylint.py" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
           '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook
          '(lambda ()
             (flymake-mode nil)))

;(add-hook 'python-mode-hook
;          '(lambda () (eldoc-mode 1)) t)

(setq py-indent-offset 4)

;(add-hook 'python-mode-hook
;          (function (lambda ()
;                      (setq tab-width py-indent-offset)
;                      )))

;(add-hook 'python-mode-hook
;          (function (lambda ()
;                      (setq tab-width py-indent-offset)
;                      (setq indent-tabs-mode t))))

(add-hook 'python-mode-hook
      '(lambda()
;;         (setq indent-tabs-mode t)
         (setq indent-level 4)
         (setq python-indent 4)
         (setq tab-width 4)))
