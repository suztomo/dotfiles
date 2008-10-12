;;; -*- coding: utf-8; mode:emacs-lisp -*-
;;; set-perl5lib.el --- set path into PERL5LIB if its file path includes 'lib' directory

;; Copyright (C) 2008 Taiyoh Tanaka
;; Author: Taiyoh Tanaka <sun.basix@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; * set-perl5lib について

;;; ファイルのパスに"lib"というディレクトリが含まれていたら、
;;; そこまでのパスをPERL5LIBに登録します。
;;;
;;; また、.emacsにて、
;;; (require 'set-perl5lib)
;;; のあとで、Flymakeのflymake-perl-load関数をオーバーライドして
;;; (set-perl5lib)
;;; を関数内に追加すれば、自動的にパスを登録できます。
;;;
;;; SeeAlso: http://d.hatena.ne.jp/sun-basix/20080117/1200528765

(defun perllib-check-path (lst lib-path)
  (let ((dir (car lst))
        (set-lib-path (concat lib-path "/lib")))
    (if (setf stock-lst (cdr lst))
        (cond ((string= dir "lib") set-lib-path)
              ((and (string= dir "t")
                    (file-readable-p set-lib-path)) set-lib-path)
              (t (perllib-check-path stock-lst (concat lib-path "/" dir)))))))

(defun set-perl5lib ()
  "Set path into PERL5LIB if its file path includes 'lib' directory"
  (interactive)
  (let* ((path-list (cdr (split-string
                          (if (string-match "^.:" buffer-file-name)
                              (concat (cygwin-mount-get-cygdrive-prefix)
                                      (mapconcat 'identity (split-string buffer-file-name ":") ""))
                            (buffer-file-name)
                            )
                          "/")))
         (lib-path (perllib-check-path path-list ""))
         (current-perl5lib (getenv "PERL5LIB")))
    (when (or (and lib-path current-perl5lib
               (not (string-match lib-path current-perl5lib)))
              (not current-perl5lib))
      (setenv "PERL5LIB" (concat lib-path ":" current-perl5lib))
      (message "Added %s into PERL5LIB" lib-path))))

(provide 'set-perl5lib)
