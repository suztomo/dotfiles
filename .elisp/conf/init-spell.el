(add-hook 'yatex-mode-hook 'flyspell-mode)

(add-hook 'yatex-mode-hook (function (lambda () (setq ispell-parser 'tex))))

(add-hook 'yatex-mode-hook (function (lambda () (command-execute 'flyspell-buffer))))

(global-set-key "\M-i" 'ispell-word)

;(add-to-list 'ispell-local-dictionary-alist (expand-file-name "~/dotfiles/.elisp/ispell"))

;(custom-set-variables
; '(flyspell-default-dictionary (expand-file-name "~/dotfiles/.elisp/ispell")))

;(custom-set-variables
; '(flyspell-default-dictionary nil))

