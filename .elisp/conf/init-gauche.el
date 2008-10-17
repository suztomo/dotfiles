(setq scheme-program-name "gosh")
(require 'cmuscheme)

;; ’¥¦’¥£’¥ó’¥É’¥¦’¤ò’£²’¤Ä’¤Ë’Ê¬’¤±’¤Æ’¡¢
;; ’°ì’Êý’¤Çgosh’¥¤’¥ó’¥¿’¥×’¥ê’¥¿’¤ò’¼Â’¹Ô’¤¹’¤ë’¥³’¥Þ’¥ó’¥É’¤ò’Äê’µÁ’¤·’¤Þ’¤¹’¡£
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
;; ’¤½’¤Î’¥³’¥Þ’¥ó’¥É’¤òCtrl-cS’¤Ç’¸Æ’¤Ó’½Ð’¤·’¤Þ’¤¹’¡£
(define-key global-map
  "\C-cS" 'scheme-other-window)

;; ’Ä¾’Á°/’Ä¾’¸å’¤Î’³ç’¸Ì’¤Ë’ÂÐ’±þ’¤¹’¤ë’³ç’¸Ì’¤ò’¸÷’¤é’¤»’¤Þ’¤¹’¡£
(show-paren-mode)


