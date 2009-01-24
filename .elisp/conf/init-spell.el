(add-hook 'yatex-mode-hook 'flyspell-mode)

(add-hook 'yatex-mode-hook (function (lambda () (setq ispell-parser 'tex))))

(add-hook 'yatex-mode-hook (function (lambda () (command-execute 'flyspell-buffer))))

(global-set-key "\M-i" 'ispell-word)

;(add-to-list 'ispell-local-dictionary-alist (expand-file-name "~/dotfiles/.elisp/ispell"))

;(custom-set-variables
; '(flyspell-default-dictionary (expand-file-name "~/dotfiles/.elisp/ispell")))

;(custom-set-variables
; '(flyspell-default-dictionary nil))

; flyspell source code are in following file specially for CarbonEmacs
;   /Applications/Emacs.app/Contents/Resources/lisp/textmodes/flyspell.el
; How to use aspell
; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?Aspell

(custom-set-variables
 '(flyspell-use-global-abbrev-table-p nil))

(custom-set-variables
 '(flyspell-auto-correct-binding [(control ?\t)]))
