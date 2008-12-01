;;; skk-autoloads.el --- autoload settings for SKK.

;; This file was generated automatically by SKK-MK at Wed Apr 21 21:02:38 2004.

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:


;;;### (autoloads (skk-cursor-off-1 skk-cursor-set-1 skk-cursor-current-color)
;;;;;;  "skk-cursor" "skk-cursor.el" (15876 54738))
;;; Generated autoloads from skk-cursor.el

(autoload (quote skk-cursor-current-color) "skk-cursor" nil nil nil)

(autoload (quote skk-cursor-set-1) "skk-cursor" nil nil nil)

(autoload (quote skk-cursor-off-1) "skk-cursor" nil nil nil)

;;;***

;;;### (autoloads (skk-viper-normalize-map) "skk-viper" "skk-viper.el"
;;;;;;  (15976 23100))
;;; Generated autoloads from skk-viper.el

(autoload (quote skk-viper-normalize-map) "skk-viper" nil nil nil)

;;;***

;;;### (autoloads (skk-lookup-search) "skk-lookup" "skk-lookup.el"
;;;;;;  (16006 21136))
;;; Generated autoloads from skk-lookup.el

(autoload (quote skk-lookup-search) "skk-lookup" nil nil nil)

;;;***

;;;### (autoloads (skk-jisx0213-henkan-list-filter) "skk-jisx0213"
;;;;;;  "skk-jisx0213.el" (15876 54738))
;;; Generated autoloads from skk-jisx0213.el

(autoload (quote skk-jisx0213-henkan-list-filter) "skk-jisx0213" nil nil nil)

;;;***

;;;### (autoloads (skk-e21-prepare-modeline-properties) "skk-e21"
;;;;;;  "skk-e21.el" (16006 20820))
;;; Generated autoloads from skk-e21.el

(autoload (quote skk-e21-prepare-modeline-properties) "skk-e21" nil nil nil)

;;;***

;;;### (autoloads (update-buffer-local-frame-params) "ccc" "ccc.el"
;;;;;;  (15876 54738))
;;; Generated autoloads from ccc.el

(put (quote ccc-defadvice) (quote lisp-indent-function) (quote defun))

(autoload (quote update-buffer-local-frame-params) "ccc" nil nil nil)

;;;***

;;;### (autoloads (skk-abbrev-search) "skk-abbrev" "skk-abbrev.el"
;;;;;;  (15876 54738))
;;; Generated autoloads from skk-abbrev.el

(autoload (quote skk-abbrev-search) "skk-abbrev" nil nil nil)

;;;***

;;;### (autoloads (skk-annotation-update-jisyo-format skk-annotation-quote
;;;;;;  skk-annotation-remove skk-annotation-add skk-annotation-show)
;;;;;;  "skk-annotation" "skk-annotation.el" (15876 54738))
;;; Generated autoloads from skk-annotation.el

(autoload (quote skk-annotation-show) "skk-annotation" nil nil nil)

(autoload (quote skk-annotation-add) "skk-annotation" "\
最後に確定した語に annotation を付ける。
既に付けられている annotation があればそれを編集バッファに出力する。
no-previous-annotation を指定すると (C-u M-x skk-annotation-add で指定可)
既に付けられている annotation を編集バッファに出力しない。" t nil)

(autoload (quote skk-annotation-remove) "skk-annotation" "\
最後に確定した語から annotation を取り去る。" t nil)

(autoload (quote skk-annotation-quote) "skk-annotation" "\
最後に確定した語に含まれる `;' を候補の一部として quote する。" t nil)

(autoload (quote skk-annotation-update-jisyo-format) "skk-annotation" nil t nil)

;;;***

;;;### (autoloads (skk-adjust-search-prog-list-for-auto-okuri skk-okuri-search-1)
;;;;;;  "skk-auto" "skk-auto.el" (15876 54738))
;;; Generated autoloads from skk-auto.el

(autoload (quote skk-okuri-search-1) "skk-auto" nil nil nil)

(autoload (quote skk-adjust-search-prog-list-for-auto-okuri) "skk-auto" nil nil nil)

;;;***

;;;### (autoloads (skk-comp-previous/next skk-comp-previous skk-comp-do
;;;;;;  skk-comp skk-comp-start-henkan) "skk-comp" "skk-comp.el"
;;;;;;  (16134 35637))
;;; Generated autoloads from skk-comp.el

(autoload (quote skk-comp-start-henkan) "skk-comp" "\
▽モードで読みの補完を行なった後、変換する。
それ以外のモードではオリジナルのキーマップに割り付けられたコマンドをエミュレー
トする。" t nil)

(autoload (quote skk-comp) "skk-comp" nil nil nil)

(autoload (quote skk-comp-do) "skk-comp" nil nil nil)

(autoload (quote skk-comp-previous) "skk-comp" nil nil nil)

(autoload (quote skk-comp-previous/next) "skk-comp" nil nil nil)

;;;***

;;;### (autoloads (skk-cus-setup) "skk-cus" "skk-cus.el" (15876 54738))
;;; Generated autoloads from skk-cus.el

(defvar skk-custom-file "~/.skk-cus")

(defvar skk-custom-alist nil)

(autoload (quote skk-cus-setup) "skk-cus" nil nil nil)

;;;***

;;;### (autoloads (skk-submit-bug-report) "skk-develop" "skk-develop.el"
;;;;;;  (15876 54738))
;;; Generated autoloads from skk-develop.el

(autoload (quote skk-submit-bug-report) "skk-develop" "\
SKK のバグレポートを書くメールバッファを用意する。
mail-user-agent を設定することにより好みのメールインターフェイスを使用すること
ができる。例えば、Wanderlust を使用したい場合は下記のように設定する。

    (setq mail-user-agent 'wl-user-agent) " t nil)

(eval-after-load "font-lock" (quote (setq lisp-font-lock-keywords-2 (nconc (list (list (concat "(\\(skk-def\\(" "\\(un-cond\\|subst-cond\\|advice\\)\\|" "\\(var\\|localvar\\)" "\\)\\)\\>" "[ 	'(]*" "\\(\\sw+\\)?") (quote (1 font-lock-keyword-face)) (quote (5 (cond ((match-beginning 3) font-lock-function-name-face) ((match-beginning 5) font-lock-variable-name-face)) nil t)))) lisp-font-lock-keywords-2))))

;;;***

;;;### (autoloads (skk-dic-setup-buffer skk-search-small-dic) "skk-dic"
;;;;;;  "skk-dic.el" (16172 51795))
;;; Generated autoloads from skk-dic.el

(autoload (quote skk-search-small-dic) "skk-dic" "\
`skk-henkan-key' について `skk-dic-buffer' を検索し、候補のリストを返す。
`skk-search-prog-list' の要素に `(skk-search-small-dic)' を指定して利用する。
SKK インストール時にインストーラが SKK-JISYO.S を見つけたら、skk-dic.el
ロード時に `skk-dic-buffer' は自動的に設定される。" nil nil)

(autoload (quote skk-dic-setup-buffer) "skk-dic" nil nil nil)

;;;***

;;;### (autoloads (skk-gadget-units-conversion skk-henkan-face-off-and-remove-itself
;;;;;;  skk-ignore-dic-word skk-times skk-minus skk-plus skk-calc
;;;;;;  skk-gengo-to-ad-1 skk-gengo-to-ad skk-ad-to-gengo-1 skk-ad-to-gengo
;;;;;;  skk-clock skk-today skk-default-current-date skk-current-date)
;;;;;;  "skk-gadget" "skk-gadget.el" (15876 54738))
;;; Generated autoloads from skk-gadget.el

(autoload (quote skk-current-date) "skk-gadget" "\
`current-time-string' の出力を加工し、現在の日時 (string)を返す。
オプショナル引数の PP-FUNCTION を指定すると、`skk-current-date-1'
の返り値、FORMAT と AND-TIME を引数にして `funcall' する。
PP-FUNCTION の指定がない場合は `skk-default-current-date-function' を
`funcall' する。
FORMAT は `format' の第一引数の様式 (string)による出力指定テンプレート。
AND-TIME (boolean) を指定すると時刻も返す。
`skk-today' と `skk-clock' のサブルーチン。" nil nil)

(autoload (quote skk-default-current-date) "skk-gadget" "\
日付情報の標準的な出力をする他、ユーザにある程度のカスタマイズ機能を提供する。
この関数の引数でカスタマイズできない出力を希望する場合は、
`skk-default-current-date-function' に自前の関数を指定する。

DATE-INFORMATION は `current-time-string' が返した文字列を

  (year month day day-of-week hour minute second)

の形式で変換したリスト (各要素は文字列)。
FORMAT は `format' の第一引数の様式による出力形態を指定する文字列。
  nil であれば \"%s年%s月%s日(%s)%s時%s分%s秒\" (もしくは
  \"%s年%s月%s日(%s)\" が使われる。
NUM-TYPE (number) は
  0 -> 無変換,
  1 -> 全角数字へ変換,
  2 -> 漢数字へ変換 (位取りなし),
  3 -> 漢数字へ変換 (位取りをする),
  4 -> その数字そのものをキーにして辞書を再検索,
  5 -> 漢数字 (手形などで使用する文字を使用)へ変換 (位取りをする),
  9 -> 将棋で使用する数字 (\"３四\" など) に変換
GENGO は元号表示するかどうか (boolean)。
GENGO-INDEX は `skk-gengo-alist' の各要素の cadr を 0 とする index
 (number)。nil であれば `current-time-string' の出力のまま無変換。
MONTH-ALIST-INDEX は `skk-month-alist' の各要素の cadr を 0 とする
 index (number)。nil であれば `current-time-string' の出力のまま無変換。
DAYOFWEEK-ALIST-INDEX は `skk-day-of-week-alist' の各要素の cadr を
 0 とする index (number)。nil であれば `current-time-string' の出力のま
ま無変換。
AND-TIME は時刻も表示するかどうか (boolean)。" nil nil)

(autoload (quote skk-today) "skk-gadget" "\
`current-time-string' の出力を加工し、現在の日時を表す文字列を作り、挿入する。
実質的に today エントリの呼出しなので、個人辞書の today エントリによりカスタマ
イズすることができる。" t nil)

(autoload (quote skk-clock) "skk-gadget" "\
デジタル時計をミニバッファに表示する。
quit するとその時点の日時を候補として挿入する。
quit したときに起動してからの経過時間をミニバッファに表示する。
interactive に起動する他、\"clock /(skk-clock)/\" などのエントリを SKK の辞書
に加え、\"/clock\"+ SPC で変換することによっても起動可。C-g で止まる。
実行変換で起動した場合は、C-g した時点の時点の日時を挿入する。
オプショナル引数の KAKUTEI-WHEN-QUIT が non-nil であれば C-g したときに確
定する。
オプショナル引数の TIME-SIGNAL が non-nil であれば、NTT の時報風に ding する。
それぞれ \"clock /(skk-clock nil t)/\" のようなエントリを辞書に挿入すれば良い。
skk-date-ad と skk-number-style によって表示方法のカスタマイズが可能。" t nil)

(autoload (quote skk-ad-to-gengo) "skk-gadget" nil nil nil)

(autoload (quote skk-ad-to-gengo-1) "skk-gadget" nil nil nil)

(autoload (quote skk-gengo-to-ad) "skk-gadget" nil nil nil)

(autoload (quote skk-gengo-to-ad-1) "skk-gadget" nil nil nil)

(autoload (quote skk-calc) "skk-gadget" nil nil nil)

(autoload (quote skk-plus) "skk-gadget" nil nil nil)

(autoload (quote skk-minus) "skk-gadget" nil nil nil)

(autoload (quote skk-times) "skk-gadget" nil nil nil)

(autoload (quote skk-ignore-dic-word) "skk-gadget" nil nil nil)

(autoload (quote skk-henkan-face-off-and-remove-itself) "skk-gadget" nil nil nil)

(autoload (quote skk-gadget-units-conversion) "skk-gadget" "\
`skk-units-alist'を参照し、換算を行なう。
NUMBER について UNIT-FROM から UNIT-TO への換算を行なう。" nil nil)

;;;***

;;;### (autoloads (skk-isearch-skk-mode skk-isearch-mode-cleanup
;;;;;;  skk-isearch-mode-setup skk-isearch-message) "skk-isearch"
;;;;;;  "skk-isearch.el" (15876 54738))
;;; Generated autoloads from skk-isearch.el

(autoload (quote skk-isearch-message) "skk-isearch" "\
Show isearch message." nil nil)

(autoload (quote skk-isearch-mode-setup) "skk-isearch" "\
hook function called when skk isearch begin." nil nil)

(autoload (quote skk-isearch-mode-cleanup) "skk-isearch" "\
Hook function called when skk isearch is done." nil nil)

(autoload (quote skk-isearch-skk-mode) "skk-isearch" nil t nil)

(defconst skk-isearch-really-early-advice (function (lambda nil (defadvice isearch-message-prefix (around skk-isearch-ad activate) (let ((current-input-method (unless (and (boundp (quote skk-isearch-switch)) skk-isearch-switch) current-input-method))) ad-do-it)) (defadvice isearch-toggle-input-method (around skk-isearch-ad activate) (cond ((string-match "^japanese-skk" (format "%s" default-input-method)) (let ((skk-isearch-initial-mode-when-skk-mode-disabled (quote latin))) (skk-isearch-mode-setup) (skk-isearch-skk-mode))) ((null default-input-method) ad-do-it (when (string-match "^japanese-skk" (format "%s" default-input-method)) (let ((skk-isearch-initial-mode-when-skk-mode-disabled (quote latin))) (skk-isearch-mode-setup)) (inactivate-input-method))) (t ad-do-it))))))

(unless (featurep (quote xemacs)) (define-key isearch-mode-map [(control \\)] (quote isearch-toggle-input-method)) (cond ((and (featurep (quote advice)) (assq (quote skk-isearch-ad) (assq (quote around) (ad-get-advice-info (quote isearch-toggle-input-method))))) nil) ((locate-library "advice") (funcall skk-isearch-really-early-advice)) (t (add-hook (quote before-init-hook) skk-isearch-really-early-advice))))

;;;***

;;;### (autoloads (skk-katakana-to-jisx0201-region skk-hiragana-to-jisx0201-region
;;;;;;  skk-toggle-katakana skk-jisx0201-mode) "skk-jisx0201" "skk-jisx0201.el"
;;;;;;  (16006 20820))
;;; Generated autoloads from skk-jisx0201.el

(autoload (quote skk-jisx0201-mode) "skk-jisx0201" "\
SKK のモードを JIS X 0201 モードに変更する。" t nil)

(autoload (quote skk-toggle-katakana) "skk-jisx0201" nil t nil)

(autoload (quote skk-hiragana-to-jisx0201-region) "skk-jisx0201" nil nil nil)

(autoload (quote skk-katakana-to-jisx0201-region) "skk-jisx0201" nil nil nil)

;;;***

;;;### (autoloads (skk-jisyo-edit-mode) "skk-jisyo-edit-mode" "skk-jisyo-edit-mode.el"
;;;;;;  (15976 23100))
;;; Generated autoloads from skk-jisyo-edit-mode.el

(autoload (quote skk-jisyo-edit-mode) "skk-jisyo-edit-mode" "\
Major mode for editing SKK JISYO." t nil)

(add-to-list (quote auto-mode-alist) (quote ("SKK-JISYO" . skk-jisyo-edit-mode)))

(add-to-list (quote auto-mode-alist) (quote ("\\.skk-jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$" . skk-jisyo-edit-mode)))

(add-to-list (quote auto-mode-alist) (quote ("\\..*skk/jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$" . skk-jisyo-edit-mode)))

;;;***

;;;### (autoloads (skk-romaji-message skk-romaji-region skk-hurigana-katakana-message
;;;;;;  skk-hurigana-katakana-region skk-hurigana-message skk-hurigana-region
;;;;;;  skk-gyakubiki-katakana-message skk-gyakubiki-katakana-region
;;;;;;  skk-gyakubiki-message skk-gyakubiki-region) "skk-kakasi"
;;;;;;  "skk-kakasi.el" (15876 54738))
;;; Generated autoloads from skk-kakasi.el

(autoload (quote skk-gyakubiki-region) "skk-kakasi" "\
領域の漢字、送り仮名を全てひらがなに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload (quote skk-gyakubiki-message) "skk-kakasi" "\
領域の漢字、送り仮名を全てひらがなに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload (quote skk-gyakubiki-katakana-region) "skk-kakasi" "\
領域の漢字、送り仮名を全てカタカナに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload (quote skk-gyakubiki-katakana-message) "skk-kakasi" "\
領域の漢字、送り仮名を全てカタカナに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload (quote skk-hurigana-region) "skk-kakasi" "\
領域の漢字に全てふりがなを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload (quote skk-hurigana-message) "skk-kakasi" "\
領域の漢字に全てふりがなを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload (quote skk-hurigana-katakana-region) "skk-kakasi" "\
領域の漢字に全てフリガナを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload (quote skk-hurigana-katakana-message) "skk-kakasi" "\
領域の漢字に全てフリガナを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload (quote skk-romaji-region) "skk-kakasi" "\
領域の漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換する。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

skk-romaji-*-by-hepburn が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。" t nil)

(autoload (quote skk-romaji-message) "skk-kakasi" "\
領域の漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換し、エコーする。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

skk-romaji-*-by-hepburn が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。" t nil)

;;;***

;;;### (autoloads (skk-display-code-for-char-at-point skk-input-by-code-or-menu)
;;;;;;  "skk-kcode" "skk-kcode.el" (15876 54738))
;;; Generated autoloads from skk-kcode.el

(autoload (quote skk-input-by-code-or-menu) "skk-kcode" "\
7bit もしくは 8bit もしくは 区点コードに対応する 2byte 文字を挿入する。" t nil)

(autoload (quote skk-display-code-for-char-at-point) "skk-kcode" "\
ポイントにある文字の EUC コードと JIS コードを表示する。" t nil)

;;;***

;;;### (autoloads (skk-auto-fill-inactivate skk-inactivate skk-auto-fill-activate
;;;;;;  skk-activate) "skk-leim" "skk-leim.el" (15876 54739))
;;; Generated autoloads from skk-leim.el

(autoload (quote skk-activate) "skk-leim" nil nil nil)

(autoload (quote skk-auto-fill-activate) "skk-leim" nil nil nil)

(autoload (quote skk-inactivate) "skk-leim" nil nil nil)

(autoload (quote skk-auto-fill-inactivate) "skk-leim" nil nil nil)

(register-input-method "japanese-skk" "Japanese" (quote skk-activate) "" "Simple Kana to Kanji conversion program")

(register-input-method "japanese-skk-auto-fill" "Japanese" (quote skk-auto-fill-activate) "" "Simple Kana to Kanji conversion program with auto-fill")

;;;***

;;;### (autoloads (skk-look-ispell skk-look-completion skk-look)
;;;;;;  "skk-look" "skk-look.el" (16153 6549))
;;; Generated autoloads from skk-look.el

(autoload (quote skk-look) "skk-look" nil nil nil)

(autoload (quote skk-look-completion) "skk-look" nil nil nil)

(autoload (quote skk-look-ispell) "skk-look" nil nil nil)

;;;***

;;;### (autoloads nil "skk-macs" "skk-macs.el" (16152 26631))
;;; Generated autoloads from skk-macs.el

(put (quote dolist) (quote lisp-indent-function) 1)

(put (quote dotimes) (quote lisp-indent-function) 1)

(put (quote skk-defadvice) (quote lisp-indent-function) (quote defun))

(put (quote skk-deflocalvar) (quote lisp-indent-function) (quote defun))

(put (quote skk-loop-for-buffers) (quote lisp-indent-function) 1)

;;;***

;;;### (autoloads (skk-num skk-num-update-jisyo skk-num-henkan-key
;;;;;;  skk-num-initialize skk-num-uniq skk-num-exp skk-num-multiple-convert
;;;;;;  skk-num-convert skk-num-compute-henkan-key) "skk-num" "skk-num.el"
;;;;;;  (15876 54739))
;;; Generated autoloads from skk-num.el

(autoload (quote skk-num-compute-henkan-key) "skk-num" "\
KEY の中の連続する数字を現わす文字列を \"#\" に置き換えた文字列を返す。
\"12\" や \"０９\" など連続する数字を 1 つの \"#\" に置き換えることに注意。
置き換えた数字を `skk-num-list' の中にリストの形で保存する。
例えば、KEY が \"へいせい7年12がつ\" であれば、\"へいせい#ねん#がつ\"
と変換し、`skk-num-list' に (\"7\" \"12\") というリストを代入する。
辞書の見出し語の検索に使用する。" nil nil)

(autoload (quote skk-num-convert) "skk-num" "\
INDEX が指す `skk-henkan-list' の要素を数値変換のために加工する。
`skk-henkan-list' の INDEX が指している候補 (数値変換キーの)を
  \"#2\" -> (\"#2\" .\"一\")
のように変換する。" nil nil)

(autoload (quote skk-num-multiple-convert) "skk-num" nil nil nil)

(autoload (quote skk-num-exp) "skk-num" "\
ascii 数字 (string) の NUM を TYPE に従い変換し、変換後の文字列を返す。
TYPE は下記の通り。
0 -> 無変換
1 -> 全角数字へ変換
2 -> 漢数字へ変換 (位取りなし)
3 -> 漢数字へ変換 (位取りをする)
4 -> その数字そのものをキーにして辞書を再検索
5 -> 漢数字 (手形などで使用する文字を使用) へ変換 (位取りをする)
9 -> 将棋で使用する数字 (\"３四\" など) に変換" nil nil)

(autoload (quote skk-num-uniq) "skk-num" nil nil nil)

(autoload (quote skk-num-initialize) "skk-num" "\
`skk-use-numeric-conversion' 関連の変数を初期化する。" nil nil)

(autoload (quote skk-num-henkan-key) "skk-num" "\
適切な変換キーを返す。
type4 の数値再変換が行なわれたときは、数値自身を返し、それ以外の数値変換
では、`skk-henkan-key' の数値を \"#\" で置き換えたキーを返す。" nil nil)

(autoload (quote skk-num-update-jisyo) "skk-num" "\
数字自身を見出し語として辞書のアップデートを行なう。" nil nil)

(autoload (quote skk-num) "skk-num" "\
数字を `skk-number-style' の値に従い変換する。
`skk-current-date' のサブルーチン。" nil nil)

;;;***

;;;### (autoloads (skk-obsolete-put-obsolete-mark skk-obsolete-check-all-files
;;;;;;  skk-obsolete-check) "skk-obsolete" "skk-obsolete.el" (15876
;;;;;;  54739))
;;; Generated autoloads from skk-obsolete.el

(autoload (quote skk-obsolete-check) "skk-obsolete" "\
FILE 内の obsolete 変数名と obsolete 関数名をチェックし、書換える。" t nil)

(autoload (quote skk-obsolete-check-all-files) "skk-obsolete" "\
関連ファイル全ての obsolete 変数名と obsolete 関数名をチェックし、書換える。
C-u M-x skk-obsolete-check-all-files のように起動したときは、ディフォルトディレ
クトリにある SKK プログラムファイルもチェックを行なう。" t nil)

(autoload (quote skk-obsolete-put-obsolete-mark) "skk-obsolete" nil nil nil)

;;;***

;;;### (autoloads (skk-adjust-search-prog-list-for-server-search
;;;;;;  skk-search-server-1 skk-server-version) "skk-server" "skk-server.el"
;;;;;;  (16134 35637))
;;; Generated autoloads from skk-server.el

(autoload (quote skk-server-version) "skk-server" "\
Return version information of SKK server.
When called interactively, print version information." t nil)

(autoload (quote skk-search-server-1) "skk-server" "\
skk-search-server のサブルーチン。" nil nil)

(autoload (quote skk-adjust-search-prog-list-for-server-search) "skk-server" "\
変数 `skk-search-prog-list' を調整する。
`skk-server-host' もしくは `skk-servers-list' が nil であれば、
`skk-search-prog-list' から `skk-search-server' を car に持つリストを消す。
non-nil であれば、加える。" nil nil)

;;;***

;;;### (autoloads (skk-tutorial) "skk-tut" "skk-tut.el" (16006 20820))
;;; Generated autoloads from skk-tut.el

(autoload (quote skk-tutorial) "skk-tut" "\
SKK チュートリアルを起動する。
\\[universal-argument] \\[skk-tutorial] すると、チュートリアルファイルの選択が可能。" t nil)

;;;***

;;;### (autoloads nil "skk-vars" "skk-vars.el" (16151 60717))
;;; Generated autoloads from skk-vars.el

(defvar skk-preload nil "\
Non-nil ならば、SKK を前もってロードする。
これによって初回起動が高速になる。")

(defvar skk-isearch-switch nil)

;;;***

;;;### (autoloads (skk-version) "skk-version" "skk-version.el" (16151
;;;;;;  60675))
;;; Generated autoloads from skk-version.el

(autoload (quote skk-version) "skk-version" "\
Return SKK version with its codename.
If WITHOUT-CODENAME is non-nil, simply return SKK version without
the codename." t nil)

;;;***

;;;### (autoloads (skk-preload skk-remove-duplicates skk-compile-rule-list
;;;;;;  skk-auto-fill-mode skk-mode) "skk" "skk.el" (16152 26630))
;;; Generated autoloads from skk.el

(autoload (quote skk-mode) "skk" "\
日本語入力モード。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
負の引数を与えると SKK モードから抜ける。

An input mode for Japanese, converting romanized phonetic strings to kanji.

A minor mode, it should not affect the use of any major mode or
orthogonal minor modes.

In the initial SKK mode, hiragana submode, the mode line indicator is
\"かな\".  Lowercase romaji inputs are automatically converted to
hiragana where possible.  The lowercase characters `q' and `l' change
submodes of SKK, and `x' is used as a prefix indicating a small kana.

`q' is used to toggle between hiragana and katakana (mode line
indicator \"カナ\") input submodes.

`l' is used to enter ASCII submode (mode line indicator \"SKK\").
Uppercase `L' enters JISX0208 latin (wide ASCII) submode (mode line
indicator \"全英\").  `
' returns to hiragana submode from either
ASCII submode.

Kanji conversion is complex, but the basic principle is that the user
signals the appropriate stem to be matched against dictionary keys by
the use of uppercase letters.  Because SKK does not use grammatical
information, both the beginning and the end of the stem must be marked.

For non-inflected words (eg, nouns) consisting entirely of kanji, the
simplest way to invoke conversion is to enter the reading of the kanji,
the first character only in uppercase.  A leading \"▽\" indicates that
kanji conversion is in progress.  After entering the reading, press
space.  This invokes dictionary lookup, and the hiragana reading will be
redisplayed in kanji as the first candidate.  Pressing space again gives
the next candidate.  Further presses of space produce further candidates,
as well as a list of the next few candidates in the minibuffer.  Eg,
\"Benri\" => \"▽べんり\", and pressing space produces \"▼便利\" (the
solid triangle indicates that conversion is in progress).  Backspace
steps through the candidate list in reverse.

A candidate can be accepted by pressing `
', or by entering a
self-inserting character.  (Unlike other common Japanese input methods,
RET not only accepts the current candidate, but also inserts a line
break.)

Inflected words (verbs and adjectives), like non-inflected words, begin
input with a capital letter.  However, for these words the end of the
kanji string is signaled by capitalizing the next mora.  Eg, \"TuyoI\"
=> \"▼強い\".  If no candidate is available at that point, the inflection
point will be indicated with an asterisk \"*\", and trailing characters
will be displayed until a candidate is recognized.  It will be
immediately displayed (pressing space is not necessary).  Space and
backspace are used to step forward and backward through the list of
candidates.

For more information, see the `skk' topic in Info.  (Japanese only.)

A tutorial is available in Japanese or English via \"M-x skk-tutorial\".
Use a prefix argument to choose the language.  The default is system-
dependent." t nil)

(autoload (quote skk-auto-fill-mode) "skk" "\
日本語入力モード。自動折り返し機能付き。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
正の引数を与えると、強制的に auto-fill-mode 及び SKK モードに入る。
負の引数を与えると auto-fill-mode 及び SKK モードから抜ける。" t nil)

(autoload (quote skk-compile-rule-list) "skk" "\
rule list を木の形にコンパイルする。" nil nil)

(autoload (quote skk-remove-duplicates) "skk" "\
LIST から重複をなくしたリストを返す。" nil nil)

(autoload (quote skk-preload) "skk" "\
変数 `skk-preload' が非 nil のとき、`after-init-hook' から呼ばれる。
あらかじめ SKK を呼んでおくことで、 SKK の初回起動を速くする。" nil nil)

(add-hook (quote after-init-hook) (function (lambda nil (when (symbol-value (quote init-file-user)) (when (and (boundp (quote skk-custom-file)) (load skk-custom-file t) (cdr (assq (quote skk-preload) skk-custom-alist))) (setq skk-preload t)) (when skk-preload (skk-preload))))))

;;;***

;;;### (autoloads (skk-study-read skk-study-copy-theme skk-study-remove-theme
;;;;;;  skk-study-switch-current-theme skk-study-save skk-study-update
;;;;;;  skk-study-search) "skk-study" "skk-study.el" (16153 13560))
;;; Generated autoloads from skk-study.el

(autoload (quote skk-study-search) "skk-study" "\
学習データを参照して ENTRY を加工し、関連性のある語の優先順位を上げて返す。" nil nil)

(autoload (quote skk-study-update) "skk-study" "\
MIDASI と WORD について `skk-study-data-ring' の最初の関連語を関連付けて学習する。" nil nil)

(autoload (quote skk-study-save) "skk-study" "\
`skk-study-file' に学習結果を保存する。
オプショナル引数の NOMSG が non-nil であれば、保存メッセージを出力しない。" t nil)

(autoload (quote skk-study-switch-current-theme) "skk-study" "\
skk-study のカレントバッファに対する学習テーマ THEME を設定する。
学習テーマには任意の文字列を設定できる。
カレントバッファの学習テーマが設定されないときは、学習テーマ
\"general\" に対する学習が行われる。" t nil)

(autoload (quote skk-study-remove-theme) "skk-study" "\
skk-study の学習テーマ THEME を削除する。" t nil)

(autoload (quote skk-study-copy-theme) "skk-study" "\
skk-study の学習テーマ FROM を TO にコピーする。
TO の既存データは破壊される。" t nil)

(autoload (quote skk-study-read) "skk-study" "\
`skk-study-file' から学習結果を読み込む。
オプショナル引数の FORCE が non-nil であれば、破棄の確認をしない。" t nil)

;;;***
(provide 'skk-autoloads)
;;; skk-autoloads.el ends here
