;; skk setup
;; Add pahts to SKK and APEL

(defvar system-load-path load-path)
(setq my-load-path '("/usr/share/emacs/22.1/site-lisp/skk"
                     "/usr/share/emacs/22.1/site-lisp/apel"
                     "/usr/share/emacs/22.1/site-lisp/emu"))
(setq load-path (append my-load-path system-load-path))
(require 'skk-autoloads)

;; Off AquaSKK
(setq mac-pass-control-to-system nil)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)


(setq skk-large-jisyo "~/Library/AquaSKK/SKK-JISYO.L")



;; ラージ辞書ではなくskkservへ

(when (eq window-system 'mac)
  (add-hook 'window-setup-hook
    (lambda ()
      (progn
        (setq skk-large-jisyo nil)
        (setq skk-server-host "127.0.0.1")
        (setq skk-server-potnum 1178)
        (message "SKK serv setup")
        )

      )))

(setq skk-kutouten-type 'en)

;; 変換時，改行でも確定
(setq skk-egg-like-newline t)

;; メッセージは日本語で
(setq skk-japanese-message-and-error t)

;;"「"を入力したら"」"も自動で挿入
;(setq skk-auto-insert-paren nil)

;;漢字登録のミスをチェックする
(setq skk-check-okurigana-on-touroku t)

;; 変換候補をツールチップに表示
;; (setq skk-show-tooltip t)

;; 変換候補をインラインに表示
(setq skk-show-inline t)

;; isearch時にSKKをオフ
(setq skk-isearch-start-mode 'latin)

;; 10 分放置すると個人辞書が自動的に保存される設定
;; (defvar skk-auto-save-jisyo-interval 600)
;; (defun skk-auto-save-jisyo ()
;;   (skk-save-jisyo)
;;   (skk-bayesian-save-history)
;;   (skk-bayesian-corpus-save))
;; (run-with-idle-timer skk-auto-save-jisyo-interval
;;                      skk-auto-save-jisyo-interval
;;                      'skk-auto-save-jisyo)



