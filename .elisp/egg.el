;;; egg -- Emacs Got Git
;;; A magit fork

;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Marius Vollmer
;;
;; Egg is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Egg is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary
;;;    This is my fork of Marius's excellent magit. his work is at:
;;;    http://zagadka.vm.bytemark.co.uk/magit
;;;

(require 'cl)
(require 'electric)
(require 'ediff)
(require 'ffap)

(defgroup egg nil
  "Controlling Git from Emacs."
  :prefix "egg-"
  :group 'tools)

(defgroup egg-faces nil
  "Colourful Faces for Egg."
  :group 'egg)

(defface egg-header
  '((t :weight bold :inherit variable-pitch :height 1.1))
  "Face for generic headers.

Many Egg faces inherit from this one by default."
  :group 'egg-faces)

(defface egg-text-base
  '((((class color) (background light))
     :foreground "navy" :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "SteelBlue" :inherit variable-pitch)
    (t))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-1
  '((t :inherit egg-text-base))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-help
  '((t :inherit egg-text-base :height 0.8))
  "Face for help text."
  :group 'egg-faces)

(defface egg-help-header-1
  '((t :inherit egg-text-base :weight bold))
  "Face for help text."
  :group 'egg-faces)

(defface egg-help-header-2
  '((((class color) (background light))
     :foreground "Black" :inherit egg-text-1 :height 0.9)
    (((class color) (background dark))
     :foreground "LightSteelBlue" :inherit egg-text-1 :height 0.9)
    (t :inherit egg-text-1))
  "Face for help text."
  :group 'egg-faces)

(defface egg-text-2
  '((t :inherit egg-text-base :height 1.1))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-3
  '((t :inherit egg-text-base :height 1.2))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-4
  '((t :inherit egg-text-base :height 1.4))
  "Face for description text."
  :group 'egg-faces)

(defface egg-electrict-choice
  '((((class color) (background light))
     :foreground "Blue" :inherit egg-text-1 :weight bold)
    (((class color) (background dark))
     :foreground "Cyan" :inherit egg-text-1 :weight bold)
    (t))
  "Face for description text."
  :group 'egg-faces)

(defface egg-section-title
  '((((class color) (background light))
     :foreground "DarkGoldenrod" :inherit egg-header :height 1.1)
    (((class color) (background dark))
     :foreground "PaleGreen" :inherit egg-header :height 1.1)
    (t :weight bold))
  "Face for generic header lines.

Many Egg faces inherit from this one by default."
  :group 'egg-faces)

(defface egg-branch
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit egg-header :height 1.1)
    (((class color) (background dark))
     :foreground "Yellow" :inherit egg-header :height 1.1)
    (t :weight bold))
  "Face for the current branch."
  :group 'egg-faces)

(defface egg-log-buffer-mark
  '((((class color) (background light))
     :foreground "black" :inherit bold)
    (((class color) (background dark))
     :foreground "orchid1" :inherit bold)
    (t :weight bold))
  "Face to mark commit line in log-buffer."
  :group 'egg-faces)

(defface egg-branch-mono
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit bold)
    (((class color) (background dark))
     :foreground "Yellow" :inherit bold)
    (t :weight bold))
  "Face for a branch."
  :group 'egg-faces)

(defface egg-tag-mono
  '((((class color) (background light))
     :foreground "GoldenRod" :inherit bold)
    (((class color) (background dark))
     :foreground "SkyBlue" :inherit bold)
    (t :weight bold))
  "Face for a tag."
  :group 'egg-faces)

(defface egg-an-tag-mono
  '((((class color) (background light))
     :foreground "DarkGoldenRod" :inherit bold)
    (((class color) (background dark))
     :foreground "LightGreen" :inherit bold)
    (t :weight bold))
  "Face for an annotated branch."
  :group 'egg-faces)

(defface egg-remote-mono
  '((((class color) (background light))
     :foreground "Orchid" :inherit bold)
    (((class color) (background dark))
     :foreground "DarkSalmon" :inherit bold)
    (t :weight bold))
  "Face for a remote."
  :group 'egg-faces)

(defface egg-term
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit bold)
    (((class color) (background dark))
     :foreground "Yellow" :inherit bold)
    (t :weight bold))
  "Face for an important term."
  :group 'egg-faces)

(defface egg-help-key 
  '((t :inherit 'egg-term :height 0.9))
  "Hilight Face in help text."
  :group 'egg-faces)

(defface egg-warning
  '((((class color) (background light))
     :foreground "Red" :inherit bold)
    (((class color) (background dark))
     :foreground "Orange" :inherit bold)
    (t :weight bold))
  "Face for a warning."
  :group 'egg-faces)

(defface egg-diff-file-header
  '((((class color) (background light))
     :foreground "SlateBlue" :inherit egg-header)
    (((class color) (background dark))
     :foreground "LightSlateBlue" :inherit egg-header)
    (t :weight bold))
  "Face for diff file headers."
  :group 'egg-faces)

(defface egg-unmerged-diff-file-header
  '((((class color) (background light))
     :foreground "Red" :inherit egg-diff-file-header)
    (((class color) (background dark))
     :foreground "Orange" :inherit egg-diff-file-header)
    (t :weight bold))
  "Face for unmerged diff file headers."
  :group 'egg-faces)

(defface egg-diff-hunk-header
  '((((class color) (background light))
     :background "grey85")
    (((class color) (background dark))
     :background "grey45"))
  "Face for diff hunk headers."
  :group 'egg-faces)

(defface egg-diff-add
  '((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "white"))
  "Face for lines in a diff that have been added."
  :group 'egg-faces)

(defface egg-diff-none
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'egg-faces)

(defface egg-diff-del
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "OrangeRed"))
  "Face for lines in a diff that have been deleted."
  :group 'egg-faces)

(defface egg-diff-conflict
  '((((class color) (background light))
     :foreground "Blue")
    (((class color) (background dark))
     :foreground "Orange"))
  "Face for lines in a diff that have been deleted."
  :group 'egg-faces)

(defface egg-graph
  '((((class color) (background light))
     :foreground "grey90")
    (((class color) (background dark))
     :foreground "grey30"))
  "Face for graph."
  :group 'egg-faces)

(defface egg-blame
  '((((class color) (background light))
     :background: "grey85" :foreground "black")
    (((class color) (background dark))
     :background "grey15" :foreground "white")
    (t :inherit region))
  "Face for blame header."
  :group 'egg-faces)

(defface egg-blame-culprit
  '((((class color) (background light))
     :inherit egg-text-2 :background "grey85" :foreground "grey35")
    (((class color) (background dark))
     :inherit egg-text-2 :background "grey15" :foreground "grey60")
    (t :inherit egg-blame))
  "Face for blame culprit."
  :group 'egg-faces)

(defface egg-blame-subject
  '((((class color) (background light))
     :inherit egg-blame-culprit :foreground "black")
    (((class color) (background dark))
     :inherit egg-blame-culprit :foreground "white")
    (t :inherit egg-blame))
  "Face for blame tag line."
  :group 'egg-faces)

(defface egg-log-HEAD
  '((t (:inherit region)))
    "Face to highlight HEAD in the log buffer."
    :group 'egg-faces)

(defcustom egg-buffer-hide-sub-blocks-on-start nil
  "Initially hide all sub-blocks."
  :group 'egg
  :type '(set (const :tag "Status Buffer"   egg-status-buffer-mode)
	      (const :tag "Log Buffer"	    egg-log-buffer-mode)
	      (const :tag "File Log Buffer" egg-file-log-buffer-mode)
	      (const :tag "RefLog Buffer"   egg-reflog-buffer-mode)
	      (const :tag "Diff Buffer"     egg-diff-buffer-mode)
	      (const :tag "Commit Buffer"   egg-commit-buffer-mode)))

(defcustom egg-buffer-hide-help-on-start nil
  "Initially hide keybindings help."
  :group 'egg
  :type '(set (const :tag "Status Buffer"   egg-status-buffer-mode)
	      (const :tag "Log Buffer"	    egg-log-buffer-mode)
	      (const :tag "File Log Buffer" egg-file-log-buffer-mode)
	      (const :tag "RefLog Buffer"   egg-reflog-buffer-mode)
	      (const :tag "Diff Buffer"     egg-diff-buffer-mode)
	      (const :tag "Commit Buffer"   egg-commit-buffer-mode)))

(defcustom egg-log-HEAD-max-len 1000
  "Maximum number of entries when showing the history of HEAD."
  :group 'egg
  :type 'integer)

(defcustom egg-log-all-max-len 10000
  "Maximum number of entries when showing the history of HEAD."
  :group 'egg
  :type 'integer)

(defcustom egg-confirm-next-action t
  "Always prompt for confirmation while guessing the next logical action ."
  :group 'egg
  :type 'boolean)

(defcustom egg-status-buffer-sections '(repo unstaged staged untracked)
  "Sections to be listed in the status buffer and their order."
  :group 'egg
  :type '(repeat (choice (const :tag "Repository Info" repo)
			 (const :tag "Unstaged Changes Section" unstaged)
			 (const :tag "Staged Changes Section" staged)
			 (const :tag "Untracked/Uignored Files" untracked))))


(defcustom egg-commit-buffer-sections '(staged unstaged untracked)
  "Sections to be listed in the status buffer and their order."
  :group 'egg
  :type '(repeat (choice (const :tag "Unstaged Changes Section" unstaged)
			 (const :tag "Staged Changes Section" staged)
			 (const :tag "Untracked/Uignored Files" untracked))))


(defcustom egg-refresh-index-in-backround nil
  "Whether to refresh the index in the background when emacs is idle."
  :group 'egg
  :type 'boolean)

(defcustom egg-git-rebase-subdir ".dotest-merge"
  "Name of the rebase's workdir.
Different versions of git have different names for this subdir."
  :group 'egg
  :type '(choice (const ".dotest-merge")
		 (const "rebase-merge")
		 string))

(defcustom egg-show-key-help-in-buffers '(:log :status :diff :file-log :reflog)
  "Display keybinding help in egg special buffers."
  :group 'egg
  :type '(set (const :tag "Status Buffer"   :status)
	      (const :tag "Log Buffer"	    :log)
	      (const :tag "File Log Buffer" :file-log)
	      (const :tag "RefLog Buffer"   :reflog)
	      (const :tag "Diff Buffer"     :diff)
	      (const :tag "Commit Buffer"   :commit)))


(defcustom egg-dummy-option nil
  "Foo bar"
  :group 'egg
  :type '(set (const :bold) (const :italic)))

;;;========================================================
;;; simple routines
;;;========================================================
(defmacro egg-text (text face)
  "Format TEXT with face FACE at compile-time or run-time."
  (if (stringp text)
      (propertize text 'face (if (symbolp face) face
			       (nth 1 face))) 
    `(propertize ,text 'face ,face)))

(defmacro egg-prop (text &rest prop)
  "Propertize TEXT with properties list PROP at compile-time or run-time."
  (if (stringp text)
      (apply 'propertize text
	     (mapcar (lambda (sym)
		       (if (consp sym)
			   (nth 1 sym)
			 sym))
		     prop))
    `(propertize ,text ,@prop)))

(defalias 'egg-string-at-point 'ffap-string-at-point)
(defalias 'egg-find-file-at-point 'find-file-at-point)

(defsubst egg-prepend (str prefix &rest other-properties)
  "Make STR appear to have prefix PREFIX.
If OTHER-PROPERTIES was non-nil, apply it to STR."
  (setq prefix (concat prefix (substring str 0 1)))
  (setq str (apply 'propertize str other-properties))
  (put-text-property 0 1 'display prefix str)
  str)

(defsubst egg-commit-contents (rev)
  "Retrieve the raw-contents of the commit REV."
  (with-temp-buffer
    (call-process "git" nil t nil "cat-file" "commit" rev)
    (buffer-string)))

(defsubst egg-commit-message (rev)
  "Retrieve the commit message of REV."
  (with-temp-buffer
    (call-process "git" nil t nil "cat-file" "commit" rev)
    (goto-char (point-min))
    (re-search-forward "^\n")
    (buffer-substring-no-properties (match-end 0) (point-max))))


(defsubst egg-cmd-to-string-1 (program args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (with-temp-buffer
    (if (= (apply 'call-process program nil t nil args) 0)
	(buffer-substring-no-properties
	 (point-min) (if (> (point-max) (point-min)) 
			 (1- (point-max)) (point-max))))))

(defsubst egg-cmd-to-string (program &rest args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (egg-cmd-to-string-1 program args))


(defsubst egg-git-to-string (&rest args)
  "run GIT wih ARGS and return the output as a string."
  (egg-cmd-to-string-1 "git" args))

(defsubst egg-cmd-ok (program buffer &rest args)
  "run PROGRAM with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process program nil buffer nil args) 0))

(defsubst egg-git-ok (buffer &rest args)
  "run GIT with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process "git" nil buffer nil args) 0))

(defsubst egg-git-region-ok (start end &rest args)
  "run GIT with ARGS and insert output into current buffer at point.
return the t if the exit-code was 0. The text between START and END
is used as input to GIT."
  (= (apply 'call-process-region start end "git" t t nil args) 0))

(defsubst egg-wdir-clean () (egg-git-ok nil "diff" "--quiet"))
(defsubst egg-file-updated (file) 
  (egg-git-ok nil "diff" "--quiet" "--" file))
(defsubst egg-file-committed (file) 
  (egg-git-ok nil "diff" "--quiet" "HEAD" "--" file))
(defsubst egg-index-empty () (egg-git-ok nil "diff" "--cached" "--quiet"))


(defsubst egg-git-to-lines (&rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (save-match-data
    (split-string (or (egg-cmd-to-string-1 "git" args) "") "[\n]+" t)))

(defun egg-git-lines-matching (re idx &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (= (apply 'call-process "git" nil t nil args) 0)
      (let (lines)
	(save-match-data
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (setq lines (cons (match-string-no-properties idx) lines)))
	  lines)))))

(defun egg-git-lines-matching-multi (re indices &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (= (apply 'call-process "git" nil t nil args) 0)
      (let (lines matches)
	(save-match-data
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (setq matches nil)
	    (dolist (idx indices)
	      (when (match-beginning idx)
		(setq matches 
		      (cons (cons idx (match-string-no-properties idx))
			    matches))))
	    (setq lines (cons matches lines)))
	  lines)))))

(defsubst egg-file-git-name (file)
  "return the repo-relative name of FILE."
  (egg-git-to-string "ls-files" "--full-name" "--" file))

(defsubst egg-buf-git-name (&optional buf)
  "return the repo-relative name of the file visited by BUF.
if BUF was nil then use current-buffer"
  (egg-file-git-name (buffer-file-name buf)))

(defsubst egg-files-git-name (files)
  "return the repo-relative name for each file in the list of files FILES."
  (delete-duplicates 
   (apply 'egg-git-to-lines "ls-files" "--full-name" "--" files)
   :test 'string-equal))

(defsubst egg-unmerged-files ()
  "return a list of repo-relative names for each unmerged files."
  (delete-duplicates 
   (mapcar 'car 
	   (mapcar 'last
		   (mapcar
		    'split-string
		    (egg-git-to-lines "ls-files" "--full-name" "-u"))))
   :test 'string-equal))

(defsubst egg-local-branches ()
  "Get a list of local branches. E.g. (\"master\", \"wip1\")."
  (egg-git-to-lines "rev-parse" "--symbolic" "--branches"))

(defsubst egg-local-refs ()
  "Get a list of local refs. E.g. (\"master\", \"wip1\")."
  (egg-git-to-lines "rev-parse" "--symbolic" "--branches" "--tags"))

(defun egg-remote-branches (&optional raw)
  "Get a list of remote branches. E.g. (\"origin/master\", \"joe/fork1\")."
  (let ((lst (egg-git-to-lines "rev-parse" "--symbolic" "--remotes")))
    (if raw lst
      (mapcar (lambda (full-name)
		(let ((tmp (save-match-data (split-string full-name "/"))))
		  (cons (cadr tmp) (car tmp))))
	      lst))))

(defsubst egg-rbranch-to-remote (rbranch)
  "Return the remote name in the remote-branch RBRANCH.
E.g: `foo' in `foo/bar'"
  (and (stringp rbranch)
       (> (length rbranch) 0)
       (directory-file-name (file-name-directory rbranch))))

(defsubst egg-rbranch-name (rbranch)
  "Return the ref name in the remote-branch RBRANCH.
E.g: `bar' in `foo/bar'"
  (and (stringp rbranch) 
       (> (length rbranch) 0)
       (file-name-nondirectory rbranch)))

(defsubst egg-short-ref (full-ref)
  "Return the short ref name of the full ref name FULL-REF.
like `my_tag' in `refs/tags/my_tag'."
  (and (stringp full-ref) 
       (> (length full-ref) 0)
       (file-name-nondirectory full-ref)))

(defsubst egg-file-as-string-raw (file-name)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (buffer-string)))

(defsubst egg-file-as-string (file-name)
  "return the contents of file FILE-NAME as a string."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (buffer-substring-no-properties
     (point-min) (if (> (point-max) (point-min)) 
		     (1- (point-max)) (point-max)))))


(defun egg-pick-file-contents (file-name regexp &rest indices)
  "Pick a string out of the contents of the file FILE-NAME.
This function searches for and return the 1st match of REGEXP on the
contents of the file. If indices was not nil, then return the first
successful submatch in the order in INDICES."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (if (null indices)
	  (match-string-no-properties 0)
	(dolist (idx indices)
	  (if (match-beginning idx)
	      (return (match-string-no-properties idx))))))))

(defun egg-pick-file-records (file-name start-re end-re)
  "Return a list of strings from the contents of the file FILE-NAME.
START-RE is the regexp to match the beginning of a record.
END-RE is the regexp to match the end of a record."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (goto-char (point-min))
    (let ((beg (point-min)) 
	  (end (point-max))
	  lst)
      (save-match-data
	(while (and (> end beg)
		    (not (eobp))
		    (re-search-forward start-re nil t))
	  (setq beg (match-beginning 0))
	  (when (re-search-forward end-re nil t)
	    (setq end (match-beginning 0))
	    (if (> end beg)
		(setq lst (cons (buffer-substring-no-properties 
				 beg (match-beginning 0))
				lst)))
	    (goto-char end))))
      lst)))

(defsubst egg-is-in-git ()
  "is the default-directory in a git repo."
  (= (call-process "git" nil nil nil "rev-parse" "--git-dir") 0))

(defsubst egg-is-dir-in-git (dir)
  "is DIR in a git repo."
  (let ((default-directory dir)) (egg-is-in-git)))

(defsubst egg-name-rev (rev)
  "get the symbolic name of REV."
  (egg-git-to-string "name-rev" "--always" "--name-only" rev))

(defsubst egg-describe-rev (rev)
  "get the long symbolic name of REV."
  (egg-git-to-string "describe" "--always" "--tags" rev))

(defsubst egg-sha1 (rev)
  "get the SHA1 of REV."
  (egg-git-to-string "rev-parse" (concat rev "~0")))

(defun egg-read-git-dir ()
  "call GIT to read the git directory of default-directory."
  (let ((dir (egg-git-to-string "rev-parse" "--git-dir")))
    (if (stringp dir) 
	(expand-file-name dir))))

(defsubst egg-read-dir-git-dir (dir)
  "call GIT to read the git directory of DIR."
  (let ((default-directory dir)) (egg-read-git-dir)))

(defvar egg-git-dir nil)
(defsubst egg-git-dir (&optional error-if-not-git)
  "return the (pre-read) git-dir of default-directory"
  (if (local-variable-p 'egg-git-dir)
      egg-git-dir
    (set (make-local-variable 'egg-git-dir) 
	 (or (egg-read-git-dir)
	     (and error-if-not-git
		  (or (kill-local-variable 'egg-git-dir) t)
		  (error "Not in a git repository: %s" default-directory))))
    ;; first time, no status yet.
    ;; this directory's specific var will be updated by
    ;; egg-set-mode-info
    (set (intern (concat "egg-" egg-git-dir "-HEAD")) " Egg")
    egg-git-dir))

(defsubst egg-buf-git-dir (buffer)
  "return the (pre-read) git-dir of BUFFER."
  (with-current-buffer buffer
    (egg-git-dir)))

(defun egg-HEAD ()
  "return HEAD. Either a symbolic ref or a sha1."
  (let* ((git-dir (egg-git-dir))) 
    (if git-dir
	(egg-pick-file-contents (concat git-dir "/HEAD")
				 "^ref: refs/heads/\\(.+\\)\\|^\\([0-9a-f]+\\)" 1 2))))

(defun egg-all-refs ()
  "Get a list of all refs."
  (append (egg-git-to-lines "rev-parse" "--symbolic"
			    "--branches" "--tags" "--remotes")
	  (delq nil
		(mapcar 
		 (lambda (head)
		   (if (file-exists-p (concat (egg-git-dir) "/" head))
		       head))
		 '("HEAD" "ORIG_HEAD" "MERGE_HEAD" "FETCH_HEAD")))))

;; (defun egg-sha1-ref-alist ()
;;   (mapcar (lambda (line)
;; 	    (when (string-match "\\`\\(\\S-+\\) refs/\\(heads\\|tags\\|remotes\\)/\\(.+\\)\\'" line)
;; 	      (list (match-string-no-properties 1 line)
;; 		    (match-string-no-properties 3 line)
;; 		    (match-string-no-properties 2 line))))
;; 	  (egg-git-to-lines "show-ref")))

(defun egg-ref-type-alist ()
  "Build an alist of (REF-NAME . :type) cells."
  (mapcar (lambda (ref-desc)
	    (cons (cdr (assq 5 ref-desc))
		  (cond ((assq 2 ref-desc) :head)
			((assq 3 ref-desc) :tag)
			((assq 4 ref-desc) :remote))))
	  (egg-git-lines-matching-multi 
	   "^.+ \\(refs/\\(?:\\(heads\\)\\|\\(tags\\)\\|\\(remotes\\)\\)/\\(\\([^/\n]+/\\)?[^/\n]+\\)\\)$"
	   ;; 1: full-name
	   ;; 2: head
	   ;; 3: tag
	   ;; 4: remote
	   ;; 5: name
	   ;; 6: remote-host
	   '(1 2 3 4 5 6) "show-ref")))

(defun egg-full-ref-decorated-alist (head-face head-keymap
					       tag-face an-tag-face tag-keymap
					       remote-site-face
					       remote-rname-face 
					       remote-keymap &optional
					       remote-site-keymap)
  "Build an alist of (REF . :type) cells.
A REF string of a head will be formatted with HEAD-FACE and
HEAD-KEYMAP.  A REF string of a tag will be formatted with
TAG-FACE (or AN-TAG-FACE if it was an annotated tag) and
TAG-KEYMAP.  A REF string of a remote will be formatted with
REMOTE-SITE-FACE/REMOTE-RNAME-FACE and
RETMOTE-KEYMAP/REMOTE-SITE-KEYMAP."
  (let ((refs-desc-list
	 (egg-git-lines-matching-multi 
	  "^.+ \\(refs/\\(?:\\(heads\\)\\|\\(tags\\)\\|\\(remotes\\)\\)/\\(\\([^/\n]+/\\)?[^/\n{}]+\\)\\)\\(\\^{}\\)?$"
	  ;; 1: full-name
	  ;; 2: head
	  ;; 3: tag
	  ;; 4: remote
	  ;; 5: name
	  ;; 6: remote-host
	  ;; 7: is annotated tag 
	  '(1 2 3 4 5 6 7) "show-ref" "-d"))
	;; if null remote-site-map then use remote-keymap for the site
	(remote-site-keymap (or remote-site-keymap remote-keymap))
	annotated-tags)
    ;; remove the annotated tags from the list
    (setq refs-desc-list
	  (delq nil 
		(mapcar (lambda (desc)
			  (if (not (assq 7 desc))
			      desc ;; not an annotated tag
			    (setq annotated-tags 
				  (cons (cdr (assq 1 desc)) 
					annotated-tags))
			    nil))
			refs-desc-list)))
    ;; decorate the ref alist
    (mapcar (lambda (desc)
	      (let ((full-name (cdr (assq 1 desc)))
		    (name (cdr (assq 5 desc)))
		    (remote (cdr (assq 6 desc))))
		(cond ((assq 2 desc) 
		       ;; head
		       (cons full-name
			     (propertize name 
					 'face head-face 
					 'keymap head-keymap
					 :ref (cons name :head))))
		      ((assq 3 desc) 
		       ;; tag
		       (cons full-name
			     (propertize name 
					 'face 
					 (if (member full-name
						     annotated-tags)
					     an-tag-face
					   tag-face)
					 'keymap tag-keymap
					 :ref (cons name :tag))))
		      ((assq 4 desc)
		       ;; remote
		       (cons full-name
			     (concat
			      (propertize remote
					  'face remote-site-face
					  'keymap remote-site-keymap
					  :ref (cons name :remote))
			      (propertize (substring name (length remote)) 
					  'face remote-rname-face
					  'keymap remote-keymap
					  :ref (cons name :remote))))))))
	    refs-desc-list)))


;; (defsubst egg-full-ref-alist ()
;;   (mapcar (lambda (desc)
;; 	    (cons (cdr (assq 1 desc))
;; 		  (cdr (assq 2 desc))))
;; 	  (egg-git-lines-matching-multi 
;; 	   "^.+ \\(refs/\\(?:heads\\|tags\\|remotes\\)/\\(.+\\)\\)$"
;; 	   '(1 2) "show-ref")))

(defun egg-complete-rev (string &optional ignored all)
  "Do revision completion"
  (save-match-data
    (cond ((string-match "\\`:[0-3]*" string) ;; stages
	   (funcall (if all 'all-completions 'try-completion)
		    string '(":0" ":1" ":2" ":3")))

	  ;; rev^, rev~10 etc.
	  ((string-match "[\\^~][\\^~0-9]*\\'" string)
	   ;; check with rev-parse
	   (if (egg-git-ok nil "rev-parse" string) 
	       ;; rev-parse ok
	       (if all 
		   ;; fixme: how to do a full expansion?
		   (list string)
		 ;; match
		 string)))

	  ;; normal rev name
	  (t (let ((matches 
		    ;; match all types of refs
		    (egg-git-to-lines "for-each-ref" "--format=%(refname)"
				      (concat "refs/*/" string "*")
				      (concat "refs/*/" string "*/*")))
		   prefix)
	       ;; get the short name
	       ;; with 1.6.x: for-each-ref" "--format=%(refname=short)
	       (setq matches
		     (mapcar (lambda (long)
			       (string-match 
				"\\`refs/\\(?:heads\\|tags\\|remotes\\)/\\(.+\\)\\'"
				long)
			       (match-string-no-properties 1 long))
			     matches))
	       ;; do the completion
	       (setq prefix 
		     (funcall (if all 'all-completions 'try-completion)
			      string 
			      (nconc (directory-files (egg-git-dir)
						      nil "HEAD")
				     matches)))
	       (cond (all prefix)
		     ((stringp prefix) prefix)
		     ((null prefix) nil)
		     (t string)))))))

;; (defun egg-decorate-ref (full-name)
;;   (save-match-data
;;     (let (name type)
;;       (if (not (string-match
;; 	      "\\`refs/\\(heads\\|tags\\|remotes\\)/\\(.+\\)\\'"
;; 	      full-name))
;; 	full-name
;; 	(setq name (match-string-no-properties 2 full-name)
;; 	      type (match-string-no-properties 1 full-name))
;; 	(cond ((string= type "heads")
;; 	       (propertize name 'face 'egg-branch-mono
;; 			   :ref (cons name :head)))
;; 	      ((string= type "tags")
;; 	       (propertize name 'face 'egg-tag-mono
;; 			   :ref (cons name :tag)))
;; 	      ((string= type "remotes")
;; 	       (propertize
;; 		(concat (egg-text (egg-rbranch-name name)
;; 				  'egg-remote-mono)
;; 			(egg-text (egg-rbranch-name name)
;; 				  'egg-branch-mono))
;; 		:ref (cons name :remote))))))))

(defsubst egg-get-symbolic-HEAD (&optional file)
  ;; get the symbolic name of HEAD
  (setq file (or file (concat (egg-git-dir) "/HEAD")))
  (egg-pick-file-contents file
			  "^ref: refs/heads/\\(.+\\)"
			  1))

(defsubst egg-get-full-symbolic-HEAD (&optional file)
  ;; get the symbolic full name of HEAD
  (setq file (or file (concat (egg-git-dir) "/HEAD")))
  (egg-pick-file-contents file
			  "^ref: \\(refs/heads/.+\\)"
			  1))

(defsubst egg-get-current-sha1 ()
  (egg-git-to-string "rev-parse" "--verify" "-q" "HEAD"))

(defsubst egg-set-mode-info (state)
  "Set the mode-line string for buffers visiting files in the current repo.
The string is built based on the current state STATE."
  (set (intern (concat "egg-" egg-git-dir "-HEAD"))
       (format " Git:%s" (cond ((plist-get state :rebase-dir)
				"(rebasing)")
			       ((plist-get state :merge-heads)
				"(merging)")
			       ((plist-get state :branch)
				(plist-get state :branch))
			       (t "(detached)")))))

(defsubst egg-get-rebase-merge-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -m variant."
  (list :rebase-dir rebase-dir
	:rebase-head 
	(egg-name-rev (egg-file-as-string (concat rebase-dir "head-name")))
	:rebase-upstream
	(egg-describe-rev (egg-file-as-string (concat rebase-dir "onto_name")))
	:rebase-step			;; string-to-number?
	(egg-file-as-string (concat rebase-dir "msgnum"))
	:rebase-num			;; string-to-number?
	(egg-file-as-string (concat rebase-dir "end"))))

(defsubst egg-get-rebase-interactive-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -i variant."
  (list :rebase-dir rebase-dir
	:rebase-head 
	(egg-name-rev (egg-file-as-string (concat rebase-dir "head-name")))
	:rebase-upstream
	(egg-describe-rev (egg-file-as-string (concat rebase-dir "onto")))
	:rebase-num
	(length
	 (egg-pick-file-records (concat rebase-dir "git-rebase-todo.backup")
				"^[pes]" "$")) 
	:rebase-step
	(if (file-exists-p (concat rebase-dir "done")) 
	    (length (egg-pick-file-records (concat rebase-dir "done")
					       "^[pes]" "$")) 
	  0)
	:rebase-cherry
	(if (file-exists-p (concat rebase-dir "done")) 
	    (car (egg-pick-file-records 
		  (concat rebase-dir "done")
		  "^[pes]" "$")))))

(defsubst egg-git-rebase-dir (&optional git-dir)
  (concat (or git-dir (egg-git-dir)) "/" egg-git-rebase-subdir "/"))

(defsubst egg-rebase-author-info (rebase-dir)
  "Retrieve an alist of commit environment variables of the current
cherry in REBASE-DIR."
  (mapcar (lambda (lst) 
	    ;; chop the ' '
	    (setcar (cdr lst) (substring (cadr lst) 1 -1))
	    lst)
   (mapcar (lambda (line)
	     ;; name-value split
	     (save-match-data (split-string line "=" t)))
	   ;; grab the GIT_xxx=yyy
	   (egg-pick-file-records (concat rebase-dir "author-script")
				  "^GIT_\\(.+\\)" "$"))))

(defsubst egg-interactive-rebase-in-progress ()
  "Is an interactive rebase in progress in the current repo?"
  (file-exists-p (concat (egg-git-dir) "/" egg-git-rebase-subdir 
			 "/interactive") ))

(defvar egg-internal-current-state nil)
(defun egg-get-repo-state (&optional extras)
  "Retrieve current repo's state as a plist.
The properties:
:gitdir :head :branch :sha1 :merge-heads :rebase-dir :rebase-head
:rebase-upstream :rebase-steop :rebase-num :rebase-cherry

EXTRAS contains the extra properties to retrieve: :staged :unstaged

if EXTRAS contains :error-if-not-git then error-out if not a git repo.
"
  (let* ((git-dir (egg-git-dir (memq :error-if-not-git extras)))
	 (head-file (concat git-dir "/HEAD"))
	 (merge-file (concat git-dir "/MERGE_HEAD"))
	 (branch (egg-get-symbolic-HEAD head-file))
	 (branch-full-name (egg-get-full-symbolic-HEAD head-file))
	 (sha1 (egg-get-current-sha1))
	 (merge-heads
	  (mapcar 'egg-name-rev 
		  (if (file-readable-p merge-file)
		      (egg-pick-file-records merge-file "^" "$"))))
	 (rebase-dir 
	  (if (file-directory-p (concat git-dir "/" egg-git-rebase-subdir))
	      (concat git-dir "/" egg-git-rebase-subdir "/")))
	 (is-rebase-interactive
	  (file-exists-p (concat rebase-dir "interactive")))
	 (rebase-state
	  (when rebase-dir
	    (if is-rebase-interactive
		(egg-get-rebase-interactive-state rebase-dir)
	      (egg-get-rebase-merge-state rebase-dir))))
	 (state (nconc (list :gitdir git-dir
			     :head branch-full-name
			     :branch branch 
			     :sha1 sha1 
			     :merge-heads merge-heads)
		       rebase-state)))
    (dolist (req extras)
      (cond ((eq req :unstaged)
	     (setq state 
		   (nconc (list :unstaged 
				(egg-git-to-lines "diff" "--name-only"))
			  state)))
	    ((eq req :staged)
	     (setq state 
		   (nconc (list :staged 
				(egg-git-to-lines "diff" "--cached"
						  "--name-only"))
				state)))))
    ;; update mode-line
    (egg-set-mode-info state)
    state))

(defsubst egg-repo-state (&rest args)
  "return the cached repo state or re-read it.
if ARGS contained :force then ignore the cached state."
  (or (unless (memq :force args) egg-internal-current-state)
      (egg-get-repo-state args)))

(defsubst egg-repo-clean (&optional state)
  "Whether the current repos is clean base on the current repo state.
use STATE as repo state if it was not nil. Otherwise re-read the repo state."
  (unless state 
    (setq state (egg-repo-state :staged :unstaged)))
  (and 
   (null (plist-get state :rebase-num))
   (null (plist-get state :merge-heads))
   (not (if (memq :unstaged state)
	    (plist-get state :unstaged)
	  (egg-wdir-clean)))
   (not (if (memq :staged state)
	    (plist-get state :staged)
	  (egg-index-empty)))))

(defsubst egg-current-branch (&optional state)
  "The current symbolic value of HEAD. i.e. name of a branch. if STATE
was not nil then use it as repo state instead of re-read from disc."
  (plist-get (or state (egg-repo-state)) :branch))

(defsubst egg-current-sha1 (&optional state)
  "The immutable sha1 of HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (plist-get (or state (egg-repo-state)) :sha1))

(defsubst egg-head (&optional state)
  "a cons cell (branch . sha1) of HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (if (egg-git-dir)
      (let ((state (or state (egg-repo-state))))
	(cons (egg-current-sha1 state) 
	      (egg-current-branch state)))))

(defun egg-pretty-head-string (&optional state)
  "Pretty description of HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (let* ((state (or state (egg-repo-state)))
	 (branch (plist-get state :branch))
	 (merge-heads (plist-get state :merge-heads))
	 (rebase-head (plist-get state :rebase-head))
	 (rebase-upstream (plist-get state :rebase-upstream))
	 (sha1 (plist-get state :sha1)))
    (cond ((and branch merge-heads)
	   (concat "Merging to " branch " from: "
		   (mapconcat 'identity merge-heads ",")))
	  (merge-heads 
	   (concat "Merging to " (egg-name-rev sha1) " from: "
		   (mapconcat 'identity merge-heads ",")))
	  ((and rebase-head rebase-upstream)
	   (format "Rebasing %s onto %s" rebase-head rebase-upstream))
	  (branch branch)
	  (t (concat "Detached HEAD: " (egg-describe-rev sha1))))))

(defsubst egg-pretty-head-name (&optional state)
  "Pretty name for HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (let* ((state (or state (egg-repo-state)))
	 (branch (plist-get state :branch)))
    (or branch (egg-describe-rev (plist-get state :sha1)))))


(defsubst egg-config-section-raw (type &optional name)
  (egg-pick-file-contents (concat (egg-git-dir) "/config")
			  (concat "^"
				  (if name (format "\\[%s \"%s\"\\]" type name)
				    (format "\\[%s\\]" type))
				  "\n"
				  "\\(\\(?:\t.+\n\\)+\\)")
			  1))

(defsubst egg-config-section (type &optional name)
  (save-match-data
    (mapcar 
     (lambda (line) 
       (split-string line "[ =]+" t))
     (split-string (or (egg-config-section-raw type name) "")
		   "[\t\n]+" t))))

(defun egg-config-get-all (file type)
  (interactive "fFilename: ")
  (save-match-data
    (mapcar (lambda (rec)
	      (let ((key (car rec))
		    (infos (cdr rec)))
		(cons (progn (string-match "\"\\(.+\\)\"" key)
			     (match-string-no-properties 1 key))
		      (mapcar (lambda (attr)
				(split-string attr "[ =]+" t))
			      infos))))
	    (mapcar (lambda (str)
		      (split-string str "[\t\n]+" t))
		    (egg-pick-file-records file
					   (concat "^\\[" type " \"")
					   "^\\[\\|\\'")))))

(defsubst egg-config-get-all-branches ()
  (egg-config-get-all (concat (egg-git-dir) "/config") "branch"))

(defsubst egg-config-get-all-remotes ()
  (egg-config-get-all (concat (egg-git-dir) "/config") "remote"))

(defsubst egg-config-get-all-remote-names ()
  (mapcar 'car (egg-config-get-all-remotes)))

(defsubst egg-config-get (type attr &optional name)
  (and (egg-git-dir)
       (cadr (assoc attr (egg-config-section type name)))))

(defun egg-tracking-target (branch &optional mode)
  (let ((remote (egg-config-get "branch" "remote" branch))
	(rbranch (egg-config-get "branch" "merge" branch)))
    (when (stringp rbranch)
      (setq rbranch (egg-rbranch-name rbranch))
      (cond ((null mode) (concat remote "/" rbranch))
	    ((eq :name-only mode) rbranch)
	    (t (cons rbranch remote))))))


(defsubst egg-read-rev (prompt &optional default)
  "Query user for a revision using PROMPT. DEFAULT is the default value."
  (completing-read prompt 'egg-complete-rev nil nil default))

(defsubst egg-read-remote (prompt &optional default)
  "Query user for a remote using PROMPT. DEFAULT is the default value."
  (completing-read prompt (egg-config-get-all-remote-names) nil t default))

;;;========================================================
;;; Async Git process
;;;========================================================

(defsubst egg-async-process ()
  (let* ((buffer (get-buffer-create "*egg-process*"))
	 (proc (get-buffer-process buffer)))
    (if (and (processp proc) 		;; is a process
	     (not (eq (process-status proc) 'exit)) ;; not finised
	     (= (process-exit-status proc) 0))      ;; still running
	proc)))

(defun egg-async-do (exit-code func-args args)
  "Run GIT asynchronously with ARGS.
if EXIT code is an exit-code from GIT other than zero but considered
success."
  (let ((dir (file-name-directory (egg-git-dir)))
	(buf (get-buffer-create "*egg-process*"))
	(inhibit-read-only inhibit-read-only)
	(accepted-msg (and (integerp exit-code)
			   (format "exited abnormally with code %d"
				   exit-code)))
	proc)
    (setq proc (get-buffer-process buf))
    (when (and (processp proc) 		;; is a process
	       (not (eq (process-status proc) 'exit)) ;; not finised
	       (= (process-exit-status proc) 0))      ;; still running
      (error "EGG: %s is already running!" (process-command proc)))
    (with-current-buffer buf
      (setq inhibit-read-only t)
      (setq default-directory dir)
      ;;(erase-buffer)
      (widen)
      (goto-char (point-max))
      (insert "EGG-GIT-CMD:\n")
      (insert (format "%S\n" args))
      (insert "EGG-GIT-OUTPUT:\n")
      (setq proc (apply 'start-process "egg-git" buf "git" args))
      (setq mode-line-process " git")
      (when (and (consp func-args) (functionp (car func-args)))
	(process-put proc :callback-func (car func-args))
	(process-put proc :callback-args (cdr func-args)))
      (when (stringp accepted-msg)
	(process-put proc :accepted-msg accepted-msg)
	(process-put proc :accepted-code exit-code))
      (process-put proc :cmds (cons "git" args))
      (set-process-sentinel proc #'egg-process-sentinel))
    proc))

(defsubst egg-async-0 (func-args &rest args)
  (egg-async-do nil func-args args))

(defsubst egg-async-1 (func-args &rest args)
  (egg-async-do 1 func-args args))

(defvar egg-async-process nil)
(defvar egg-async-cmds nil)
(defvar egg-async-exit-msg nil)

(defun egg-process-sentinel (proc msg)
  (let ((exit-code (process-get proc :accepted-code))
	(accepted-msg (process-get proc :accepted-msg))
	(callback-func (process-get proc :callback-func))
	(callback-args (process-get proc :callback-args))
	(cmds (process-get proc :cmds)))
    (cond ((string= msg "finished\n")
	   (message "EGG: git finished."))
	  ((string= msg "killed\n")
	   (message "EGG: git was killed."))
	  ((and accepted-msg (string-match accepted-msg msg))
	   (message "EGG: git exited with code: %d." exit-code))
	  ((string-match "exited abnormally" msg)
	   (message "EGG: git failed."))
	  (t (message "EGG: git is weird!")))
    (with-current-buffer (process-buffer proc)
      (setq mode-line-process nil)
      (widen)
      (goto-char (point-max))
      (re-search-backward "^EGG-GIT-CMD:" nil t)
      ;; Narrow to the last command
      (narrow-to-region (point) (point-max))
      (if (functionp callback-func)
	  (let ((egg-async-process proc)
		(egg-async-cmds cmds)
		(egg-async-exit-msg msg)) 
	    (apply callback-func callback-args))))))

;;;========================================================
;;; Blame utils
;;;========================================================
(defun egg-parse-git-blame (target-buf blame-buf &optional ov-attributes)
  "Parse blame-info in buffer BLAME-BUF and decorate TARGET-BUF buffer.
OV-ATTRIBUTES are the extra decorations for each blame chunk."
  (save-match-data
    (let ((blank (egg-text " " 'egg-blame))
	  (nl (egg-text "\n" 'egg-blame))
	  (commit-hash (make-hash-table :test 'equal :size 577))
	  commit commit-info old-line new-line num old-file subject author
	  info ov beg end blame)
      (with-current-buffer blame-buf
	(goto-char (point-min))
	;; search for a ful commit info
	(while (re-search-forward "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$" nil t)
	  (setq commit (match-string-no-properties 1)
		old-line (string-to-number
			  (match-string-no-properties 2))
		new-line (string-to-number
			  (match-string-no-properties 3))
		num (string-to-number
		     (match-string-no-properties 4)))
	  ;; was this commit already seen (and stored in the hash)?
	  (setq commit-info (gethash commit commit-hash))
	  ;; Nope, this is the 1st time, the full commit-info follow.
	  (unless commit-info
	    (re-search-forward "^author \\(.+\\)$")
	    (setq author (match-string-no-properties 1))
	    (re-search-forward "^filename \\(.+\\)$")
	    (setq old-file (match-string-no-properties 1))
	    (re-search-forward "^summary \\(.+\\)$")
	    (setq subject (match-string-no-properties 1))
	    (setq commit-info (nconc
			       (list :sha1 commit :author author 
				     :subject subject :file old-file)
			       ov-attributes))
	    ;; save it in the hash
	    (puthash commit commit-info commit-hash))
	  ;; add the current blame-block into the list INFO.
	  (setq info (cons (list old-line new-line num commit-info)
			   info))))
      ;; now do from beginning
      (setq info (nreverse info))
      (with-current-buffer target-buf
	;; for every blame chunk
	(dolist (chunk info)
	  (setq commit-info (nth 3 chunk)
		old-line (nth 0 chunk)
		new-line (nth 1 chunk)
		num (nth 2 chunk)
		commit (plist-get commit-info :sha1)
		author (plist-get commit-info :author) 
		subject (plist-get commit-info :subject))
	  
	  (goto-line new-line)
	  (setq beg (line-beginning-position)
		end (save-excursion
		      (forward-line num)
		      (line-beginning-position)))
	  ;; mark the blame chunk
	  (put-text-property beg end :blame chunk)

	  ;; make an overlay with blame info as 'before-string
	  ;; on the current chunk.
	  (setq ov (make-overlay beg end))
	  (overlay-put ov :blame chunk)
	  (setq blame (concat 
		       (egg-text (substring-no-properties commit 0 8)
				 'egg-blame)
		       blank
		       (egg-text (format "%-20s" author)
				 'egg-blame-culprit)
		       blank
		       (egg-text subject 'egg-blame-subject)
		       blank nl))
	  (overlay-put ov 'before-string blame))))))

(defsubst egg-file-buffer-blame-off (buffer)
  (save-excursion
    (save-restriction
      (with-current-buffer buffer
	(widen)
	(mapc (lambda (ov)
		(if (overlay-get ov :blame)
		    (delete-overlay ov)))
	      (overlays-in (point-min) (point-max)))))))

(defun egg-file-buffer-blame-on (buffer &rest ov-attributes)
  (egg-file-buffer-blame-off buffer)
  (save-excursion
    (with-current-buffer buffer
      (save-restriction
	(with-temp-buffer
	  (when (egg-git-ok t "blame" "--porcelain" "--" 
			    (file-name-nondirectory 
			     (buffer-file-name buffer)))
	    (egg-parse-git-blame buffer (current-buffer)
				 ov-attributes)))))))

;;;========================================================
;;; Diff/Hunk
;;;========================================================

(defconst egg-hide-show-map 
  (let ((map (make-sparse-keymap "Egg:HideShow")))
    (define-key map (kbd "h") 'egg-section-cmd-toggle-hide-show)
    (define-key map (kbd "H") 'egg-section-cmd-toggle-hide-show-children)
    map)
  "Keymap for a section than can be hidden/shown.\\{egg-hide-show-map}")

(defconst egg-section-map 
  (let ((map (make-sparse-keymap "Egg:Section")))
    (set-keymap-parent map egg-hide-show-map)
    (define-key map (kbd "n") 'egg-buffer-cmd-navigate-next)
    (define-key map (kbd "p") 'egg-buffer-cmd-navigate-prev)
    map)
  "Keymap for a section in sequence that can be navigated back and forth.
\\{egg-section-map}")

(defconst egg-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:Diff")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-diff-section-cmd-visit-file-other-window)
    (define-key map (kbd "f") 'egg-diff-section-cmd-visit-file)
    (define-key map (kbd "=") 'egg-diff-section-cmd-ediff)
    map)
  "Keymap for a diff section in sequence of deltas.
\\{egg-diff-section-map}")

(defconst egg-staged-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:StagedDiff")))
    (set-keymap-parent map egg-diff-section-map)
    (define-key map (kbd "=") 'egg-staged-section-cmd-ediff3)
    (define-key map (kbd "s") 'egg-diff-section-cmd-unstage)
    map)
  "Keymap for a diff section in sequence of staged deltas.
\\{egg-staged-diff-section-map}")

(defconst egg-wdir-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:WdirDiff")))
    (set-keymap-parent map egg-diff-section-map)
    (define-key map (kbd "u") 'egg-diff-section-cmd-undo)
    map)
  "Keymap for a diff section in sequence of deltas between the workdir and
the index. \\{egg-wdir-diff-section-map}")

(defconst egg-unstaged-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:UnstagedDiff")))
    (set-keymap-parent map egg-wdir-diff-section-map)
    (define-key map (kbd "=") 'egg-unstaged-section-cmd-ediff)
    (define-key map (kbd "s") 'egg-diff-section-cmd-stage)
    map)
  "Keymap for a diff section in sequence of unstaged deltas.
\\{egg-unstaged-diff-section-map}")

(defconst egg-unmerged-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:UnmergedDiff")))
    (set-keymap-parent map egg-unstaged-diff-section-map)
    (define-key map (kbd "=") 'egg-unmerged-section-cmd-ediff3)
    map)
  "Keymap for a diff section in sequence of unmerged deltas.
\\{egg-unmerged-diff-section-map}")

(defconst egg-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:Hunk")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-hunk-section-cmd-visit-file-other-window)
    (define-key map (kbd "=") 'egg-diff-section-cmd-ediff)
    (define-key map (kbd "f") 'egg-hunk-section-cmd-visit-file)
    map)
  "Keymap for a hunk in a diff section. \\{egg-hunk-section-map}")

(defconst egg-staged-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:StagedHunk")))
    (set-keymap-parent map egg-hunk-section-map)
    (define-key map (kbd "=") 'egg-staged-section-cmd-ediff3)
    (define-key map (kbd "s") 'egg-hunk-section-cmd-unstage)
    map)
  "Keymap for a hunk in a staged diff section.
\\{egg-staged-hunk-section-map}")

(defconst egg-wdir-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:WdirHunk")))
    (set-keymap-parent map egg-hunk-section-map)
    (define-key map (kbd "u") 'egg-hunk-section-cmd-undo)
    map)
  "Keymap for a hunk in a diff section between the workdir and the index.
\\{egg-wdir-hunk-section-map}")

(defconst egg-unstaged-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:UnstagedHunk")))
    (set-keymap-parent map egg-wdir-hunk-section-map)
    (define-key map (kbd "=") 'egg-unstaged-section-cmd-ediff)
    (define-key map (kbd "s") 'egg-hunk-section-cmd-stage)
    map)
  "Keymap for a hunk in a unstaged diff section.
\\{egg-unstaged-hunk-section-map}")

(defconst egg-unmerged-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:UnmergedHunk")))
    ;; no hunking staging in unmerged file
    (set-keymap-parent map egg-wdir-hunk-section-map)
    (define-key map (kbd "=") 'egg-unmerged-section-cmd-ediff3)
    map)
  "Keymap for a hunk in a unmerged diff section.
\\{egg-unmerged-hunk-section-map}")

(defun list-tp ()
  (interactive)
  (message "tp: %S" (text-properties-at (point))))

(defun list-nav ()
  (interactive)
  (message "nav: %c:%s-%c:%s" 
	   (preceding-char)
	   (get-text-property (1- (point)) :navigation)
	   (following-char)
	   (get-text-property (point) :navigation)))

(defsubst egg-safe-search (re limit &optional no)
  (save-excursion
    (save-match-data
      (and (re-search-forward re limit t)
	   (match-beginning (or no 0))))))

(defsubst egg-safe-search-pickup (re &optional limit no)
  (save-excursion
    (save-match-data
      (and (re-search-forward re limit t)
	   (match-string-no-properties (or no 0))))))

(defsubst egg-decorate-diff-header (beg end line-beg line-end)
  (put-text-property line-beg (1+ beg)
		     'display 
		     (egg-text
		      (concat "\n"
			      (buffer-substring-no-properties beg
							      (1+ beg)))
		      'egg-diff-file-header))
  (put-text-property (1+ beg) end 'face 'egg-diff-file-header))

(defsubst egg-decorate-cc-diff-header (beg end line-beg line-end)
  (put-text-property line-beg (1+ beg)
		     'display 
		     (egg-text
		      (concat "\n"
			      (buffer-substring-no-properties beg
							      (1+ beg)))
		      'egg-unmerged-diff-file-header))
  (put-text-property (1+ beg) end 'face 'egg-unmerged-diff-file-header))

(defsubst egg-decorate-diff-index-line (beg end line-beg line-end)
  (put-text-property (1- line-beg) beg 'display "    -- ")
  (put-text-property beg end 'face 'egg-diff-none))

(defsubst egg-decorate-hunk-header (beg end line-beg line-end)
  (put-text-property beg end 'face 'egg-diff-hunk-header)
  (put-text-property end line-end 'face 'egg-diff-none))

(defvar egg-internal-buffer-obarray nil)

(defsubst egg-make-navigation (parent child)
  "Make a symbolic and unique navigation id.
return a symbol PARENT-CHILD from an internal obarray."
  (unless (vectorp egg-internal-buffer-obarray)
    (error "Arrg! egg-internal-buffer-obarray is not an obarray!"))
  (intern (format "%s-%s" parent child) egg-internal-buffer-obarray))

(defsubst egg-do-compute-navigation (section pos)
  "Come up with a symbolic and unique navigation id for
section SECTION at position POS."
  (egg-make-navigation (get-text-property pos :navigation)
		       (if (consp section)
			   (car section)
			 section)))

(defun egg-compute-navigation (ignored-1 section pos ignored-2)
  "Come up with a symbolic and unique navigation id for
section SECTION at position POS."
  (egg-do-compute-navigation section pos))

(defun egg-delimit-section (sect-type section beg end 
					  &optional inv-beg
					  keymap navigation)
  "Mark section for navigation and add local/context keymap.
SECT-TYPE is the type of the section (usually a :symbol).
SECTION is the name of the section (usually a string).  BEG and
END are limits of the section.  INV-BEG is the position after the
position that would remain visible when the section is hidden.
KEYMAP is the local/context keymap for the section.
NAVIGATION is the navigation id of the section. NAVIGATION can also
a function to call to compute the navigation id of the section."
  (let ((nav (cond ((functionp navigation)
		    (funcall navigation sect-type section beg end))
		   ((null navigation) beg)
		   (t navigation))))
    (put-text-property beg end :sect-type sect-type)
    (put-text-property beg end sect-type section)
    (put-text-property beg end :navigation nav)
    (when (keymapp keymap)
      (put-text-property beg end 'keymap keymap))
    (when (integer-or-marker-p inv-beg) 
      (let ((current-inv (get-text-property inv-beg 'invisible)))
	(add-to-list 'current-inv nav t)
	(put-text-property inv-beg (1- end) 'invisible current-inv)))))

(defsubst egg-make-hunk-info (name beg end diff)
  "Build a hunk info NAME from BEG to END based on DIFF.
Hunk info contains name and posistions of the hunk. Positions are offsets
from DIFF because it can the whole diff can be pushed around inside
the buffer."
  (let ((b (nth 1 diff)))
    (list name (- beg b) (- end b))))

(defsubst egg-make-diff-info (name beg end head-end)
  "Build a diff info NAME from BEG to END. HEAD-END is the end position
of the diff header.

Diff info contains name and posistions of the diff. The beginning position
is stored as a marker and the others are offset from the beginning posistion
 because the whole diff can be pushed around inside the buffer."  
  (let ((b (make-marker)))
    (set-marker b beg)
    ;; no insertion indo the diff
    (set-marker-insertion-type b t)
    ;; all other posistions are offsets from B.
    (list name b (- end beg) (- head-end beg))))

(defun egg-decorate-diff-sequence (args)
  "Decorate a sequence of deltas. ARGS is a plist containing the
positions of the sequence as well as the decorations.

:begin :end :diff-map :hunk-map :cc-diff-map :cc-hunk-map 
:conflict-map :src-prefix :dst-prefix
"
  (let* ((beg		(plist-get args	:begin))
	 (end		(plist-get args	:end))
	 (diff-map 	(plist-get args	:diff-map))
	 (hunk-map 	(plist-get args	:hunk-map))
	 (cc-diff-map 	(plist-get args	:cc-diff-map))
	 (cc-hunk-map 	(plist-get args	:cc-hunk-map))
	 (conflict-map 	(plist-get args	:conflict-map))
	 (a 		(plist-get args	:src-prefix))
	 (b 		(plist-get args	:dst-prefix))

	 ;; the sub match id of the regexp below
	 (diff-no	1)
	 (cc-diff-no	2)
	 (hunk-no	3)
	 (cc-hunk-no	4)
	 (src-no 	5)
	 (dst-no 	6)
	 (index-no	7)
	 (conf-beg-no	8)
	 (conf-div-no	9)
	 (conf-end-no	10)
	 (del-no 	11)
	 (add-no 	12)
	 (none-no	13)

	 (regexp
	  (concat "^\\(?:"
		  "diff --git " a ".+" b "\\(.+\\)\\|"	;1 diff header
		  "diff --cc \\(.+\\)\\|"		;2 cc-diff header
		  "\\(@@ .+@@\\).*\\|"			;3 hunk
		  "\\(@@@ .+@@@\\).*\\|"		;4 cc-hunk
		  "--- " a "\\(.+\\)\\|"		;5 src
		  "\\+\\+\\+ " b "\\(.+\\)\\|"		;6 dst
		  "index \\(.+\\)\\|"			;7 index
		  "\\+\\+<<<<<<< \\(.+\\):.+\\|"		;8 conflict start
		  "\\(\\+\\+=======\\)\\|"			;9 conflict div
		  "\\+\\+>>>>>>> \\(.+\\):.+\\|"		;10 conflict end
		  "\\(-.*\\)\\|"			;11 del
		  "\\(\\+.*\\)\\|"			;12 add
		  "\\( .*\\)"				;13 none
		  "\\)$"))

	 ;; where the hunk end?
	 (hunk-end-re "^\\(?:diff\\|@@\\)")
	 ;; where the diff end?
	 (diff-end-re "^diff ")
	 
	 sub-beg sub-end head-end m-b-0 m-e-0 m-b-x m-e-x 
	 last-diff last-cc)

    (save-match-data
      (save-excursion
	(goto-char beg)
	(while (re-search-forward regexp end t)
	  (setq sub-beg (match-beginning 0)
		m-b-0 sub-beg
		m-e-0 (match-end 0)) 
	  (cond ((match-beginning del-no) ;; del
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-del))

		((match-beginning add-no) ;; add
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-add))

		((match-beginning none-no) ;; unchanged
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-none))

		((match-beginning dst-no) ;; +++ b/file
		 (setq m-b-x (match-beginning dst-no)
		       m-e-x (match-end dst-no))
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-add)
		 (put-text-property m-b-x m-e-x 'face 'egg-diff-none))

		((match-beginning src-no) ;; --- a/file
		 (setq m-b-x (match-beginning src-no)
		       m-e-x (match-end src-no))
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-del)
		 (put-text-property m-b-x m-e-x 'face 'egg-diff-none))

		((match-beginning conf-beg-no) ;;++<<<<<<<
		 (setq m-b-x (match-beginning conf-beg-no)
		       m-e-x (match-end conf-beg-no))
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-conflict)
		 (put-text-property m-b-x m-e-x 'face 'egg-branch-mono)
		 (put-text-property m-e-x m-e-0 'face 'egg-diff-none)
		 ;; mark the whole conflict section
		 (setq sub-end (egg-safe-search "^++>>>>>>>.+$" end))
		 (put-text-property m-b-0 sub-end 'keymap
				    conflict-map))

		((match-beginning conf-end-no)
		 (setq m-b-x (match-beginning conf-end-no)
		       m-e-x (match-end conf-end-no))
		 ;; just decorate, no mark.
		 ;; the section was already mark when the conf-beg-no
		 ;; matched.
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-conflict)
		 (put-text-property m-b-x m-e-x 'face 'egg-branch-mono)
		 (put-text-property m-e-x m-e-0 'face 'egg-diff-none))

		((match-beginning conf-div-no) ;;++=======
		 ;; just decorate, no mark.
		 ;; the section was already mark when the conf-beg-no
		 ;; matched.
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-conflict))

		((match-beginning hunk-no) ;; hunk @@
		 (setq m-b-x (match-beginning hunk-no)
		       m-e-x (match-end hunk-no)
		       ;; find the end of the hunk section
		       sub-end (or (egg-safe-search hunk-end-re end)
				   end))
		 ;; decorate the header
		 (egg-decorate-hunk-header m-b-x m-e-x m-b-0 m-e-0)
		 ;; mark the whole hunk based on the last diff header
		 (egg-delimit-section 
		  :hunk (egg-make-hunk-info 
			 (match-string-no-properties hunk-no)
			 sub-beg sub-end last-diff)
		  sub-beg sub-end m-e-0 hunk-map 
		  'egg-compute-navigation))

		((match-beginning cc-hunk-no) ;; cc-hunk
		 (setq m-b-x (match-beginning cc-hunk-no)
		       m-e-x (match-end cc-hunk-no)
		       ;; find the end of the hunk section
		       sub-end (or (egg-safe-search hunk-end-re end)
				   end))
		 ;; decorate the header
		 (egg-decorate-hunk-header m-b-x m-e-x m-b-0 m-e-0)
		 ;; mark the whole hunk based on the last cc-diff header
		 (egg-delimit-section 
		  :hunk (egg-make-hunk-info 
			 (match-string-no-properties cc-hunk-no)
			 sub-beg sub-end last-cc)
		  sub-beg sub-end m-e-0 cc-hunk-map 
		  'egg-compute-navigation))

		((match-beginning diff-no) ;; diff
		 (setq m-b-x (match-beginning diff-no)
		       m-e-x (match-end diff-no)
		       sub-end (or (egg-safe-search diff-end-re end) end)
		       ;; find the end of the header
		       head-end (or (egg-safe-search "^@@" end) end))
		 ;; decorate the header
		 (egg-decorate-diff-header m-b-x m-e-x m-b-0 m-e-0)
		 ;; mark the whole diff
		 (egg-delimit-section
		  :diff (setq last-diff
			      (egg-make-diff-info
			       (match-string-no-properties diff-no)
			       sub-beg sub-end head-end))
		  sub-beg sub-end m-e-0 diff-map 'egg-compute-navigation))

		((match-beginning cc-diff-no) ;; cc-diff
		 (setq m-b-x (match-beginning cc-diff-no)
		       m-e-x (match-end cc-diff-no)
		       sub-end (or (egg-safe-search diff-end-re end) end)
		       ;; find the end of the header
		       head-end (or (egg-safe-search "^@@@" end) end))
		 ;; decorate the header
		 (egg-decorate-cc-diff-header m-b-x m-e-x m-b-0 m-e-0)
		 ;; mark the whole diff
		 (egg-delimit-section
		  :diff (setq last-cc
			      (egg-make-diff-info
			       (match-string-no-properties cc-diff-no)
			       sub-beg sub-end head-end))
		  sub-beg sub-end m-e-0 cc-diff-map
		  'egg-compute-navigation))

		((match-beginning index-no) ;; index
		 (setq m-b-x (match-beginning index-no)
		       m-e-x (match-end index-no))
		 (egg-decorate-diff-index-line m-b-x m-e-x m-b-0 m-b-0))
		) ;; cond
	  ) ;; while
	) ;; save-excursion
      ) ;;; save -match-data

    nil))

(defun egg-decorate-diff-section (&rest args)
  "Decorate a section containing a sequence of diffs.
See `egg-decorate-diff-sequence'."
  (let ((beg (plist-get args	 :begin))
	(end (plist-get args	 :end))
	(a   (or (plist-get args :src-prefix) "a/"))
	(b   (or (plist-get args :dst-prefix) "b/"))
	(a-rev (plist-get args 	 :src-revision))
	(b-rev (plist-get args 	 :dst-revision)))
    (when (stringp a-rev)
      (put-text-property beg end :src-revision a-rev))
    (when (stringp b-rev)
      (put-text-property beg end :dst-revision b-rev))
    (egg-decorate-diff-sequence 
     (nconc (list :src-prefix a :dst-prefix b) args))))
  
(defun egg-diff-section-cmd-visit-file (file)
  "Visit file FILE."
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file file))

(defun egg-diff-section-cmd-visit-file-other-window (file)
  "Visit file FILE in other window."
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file-other-window file))

(defun egg-unmerged-section-cmd-ediff3 (file)
  "Run ediff3 to resolve merge conflicts in FILE."
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file file)
  (egg-resolve-merge-with-ediff))

(defun egg-unstaged-section-cmd-ediff (file)
  "Compare FILE and its staged copy using ediff."
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file file)
  (egg-file-do-ediff ":0" "INDEX"))

(defun egg-staged-section-cmd-ediff3 (file)
  "Compare the staged copy of FILE and the version in HEAD using ediff."
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file file)
  (egg-file-do-ediff ":0" "INDEX" "HEAD"))

(defvar egg-diff-buffer-info nil)

(defun egg-diff-section-cmd-ediff (file pos)
  "Ediff src and dest versions of FILE based on the diff at POS."
  (interactive (list (car (get-text-property (point) :diff))
		     (point)))
  (let ((commit (get-text-property pos :commit))
	(diff-info egg-diff-buffer-info)
	src src-name dst commit)
    (find-file file)
    (cond (commit
	   (setq src (egg-describe-rev (concat commit "^"))
		 dst (egg-describe-rev commit)))
	  ((setq src (plist-get diff-info :src-revision))
	   (setq src (egg-describe-rev src)))
	  ((setq src (and diff-info ":0"))
	   (setq src-name "INDEX")))
    (unless src (error "Ooops!"))
    (egg-file-do-ediff src src-name dst nil 'ediff2)))

(defun egg-hunk-compute-line-no (hunk-header hunk-beg)
  "Calaculate the effective line number in the original file based
on the position of point in a hunk. HUNK-HEADER is the header and
HUNK-BEG is the starting position of the current hunk."
  (let ((limit (line-end-position))
	(line (string-to-number 
	       (nth 2 (save-match-data
			(split-string hunk-header "[ @,\+,-]+" t)))))
	(adjust 0))
    (save-excursion
      (goto-char hunk-beg)
      (forward-line 1)
      (end-of-line)
      (while (re-search-forward "^\\(?:\\+\\| \\).*" limit t)
	(setq adjust (1+ adjust))))
    (+ line adjust)))

(defsubst egg-hunk-info-at (pos)
  "Rebuild the hunk info at POS.
Hunk info are relative offsets. This function compute the
physical offsets."
  (let* ((diff-info (get-text-property pos :diff))
	 (head-beg (nth 1 diff-info))
	 (hunk-info (get-text-property pos :hunk))
	 (hunk-beg (+ (nth 1 hunk-info) head-beg))
	 (hunk-end (+ (nth 2 hunk-info) head-beg)))
    (list (car diff-info) (car hunk-info) hunk-beg hunk-end)))

(defun egg-hunk-section-cmd-visit-file (file hunk-header hunk-beg
					     &rest ignored)
  "Visit FILE and goto the current line of the hunk."
  (interactive (egg-hunk-info-at (point)))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (find-file file)
    (goto-line line)))

(defun egg-hunk-section-cmd-visit-file-other-window (file hunk-header hunk-beg
							  &rest ignored)
  "Visit FILE in other-window and goto the current line of the hunk."
  (interactive (egg-hunk-info-at (point)))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (find-file file)
    (goto-line line)))

(defun egg-section-cmd-toggle-hide-show (nav)
  "Toggle the hidden state of the current navigation section of type NAV."
  (interactive (list (get-text-property (point) :navigation)))
  (if (assoc nav buffer-invisibility-spec)
      (remove-from-invisibility-spec (cons nav t))
    (add-to-invisibility-spec (cons nav t)))
  (force-window-update (current-buffer)))

(defun egg-section-cmd-toggle-hide-show-children (pos sect-type)
  "Toggle the hidden state of the subsections of the current navigation section at POS."
  (interactive (list (previous-single-property-change (1+ (point))
						      :navigation)
		     (get-text-property (point) :sect-type)))
  (unless pos
    (setq pos (point)))
  (let ((end (next-single-property-change pos sect-type nil (point-max)))
	child-pos child-nav
	currently-hidden)
    ;; guess the current state
    (setq child-pos (next-single-property-change pos :navigation nil end))
    (when child-pos
      (setq child-nav (get-text-property child-pos :navigation))
      (setq currently-hidden (and child-nav
				  (assoc child-nav
					 buffer-invisibility-spec))))
    (setq child-pos pos)
    ;; toggle every child
    (while (< (setq child-pos (next-single-property-change child-pos :navigation nil end))
	      end)
      (setq child-nav (get-text-property child-pos :navigation))
      (if currently-hidden
	  (remove-from-invisibility-spec (cons child-nav  t))
	(add-to-invisibility-spec (cons child-nav t))))
    (force-window-update (current-buffer))))

(defun egg-diff-section-patch-string (&optional pos)
  "Build a file patch based on the diff section at POS."
  (let* ((diff-info (get-text-property (or pos (point)) :diff))
	 (beg (nth 1 diff-info))
	 (end (+ (nth 2 diff-info) beg)))
    (buffer-substring-no-properties beg end)))

(defun egg-hunk-section-patch-string (&optional pos)
  "Build a single hunk patch based on the delta hunk at POS."
  (let* ((diff-info (get-text-property (or pos (point)) :diff))
	 (head-beg (nth 1 diff-info))
	 (head-end (+ (nth 3 diff-info) head-beg))
	 (hunk-info (get-text-property (or pos (point)) :hunk))
	 (hunk-beg (+ (nth 1 hunk-info) head-beg))
	 (hunk-end (+ (nth 2 hunk-info) head-beg)))
    ;; append hunk to diff header
    (concat (buffer-substring-no-properties head-beg head-end)
	    (buffer-substring-no-properties hunk-beg hunk-end))))

;;;========================================================
;;; Buffer
;;;========================================================
(defvar egg-buffer-refresh-func nil)
(defvar egg-buffer-async-cmd-refresh-func nil)
(defvar egg-internal-update-index-timer nil)

(defsubst egg-buffer-async-do (accepted-code &rest args)
  "Run git asynchronously and refresh the current buffer on exit.
exit code ACCEPTED-CODE is considered a success."
  (egg-async-do accepted-code 
		(cons (or egg-buffer-async-cmd-refresh-func
			  egg-buffer-refresh-func) 
		      (list (current-buffer)))
		args))

(defsubst egg-run-buffers-update-hook (&optional newly-read-state)
  "Update all egg special buffers."
  (let ((egg-internal-current-state 
	 (or newly-read-state (egg-get-repo-state))))
    (run-hooks 'egg-buffers-refresh-hook)))

(defun egg-buffer-cmd-refresh ()
  "Refresh the current egg special buffer."
  (interactive)
  (when (and (egg-git-dir)
	     (functionp egg-buffer-refresh-func))
    (funcall egg-buffer-refresh-func (current-buffer))
    (recenter)))

(defun egg-buffer-cmd-navigate-next ()
  "Move to the next section."
  (interactive)
  (goto-char (or (next-single-property-change (point) :navigation)
		 (point))))

(defun egg-buffer-cmd-navigate-prev ()
  "Move to the previous section."
  (interactive)
  (goto-char (previous-single-property-change (point) :navigation
					      nil (point-min))))

(defconst egg-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:Buffer")))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'egg-buffer-cmd-refresh)
    (define-key map (kbd "n") 'egg-buffer-cmd-navigate-next)
    (define-key map (kbd "p") 'egg-buffer-cmd-navigate-prev)
    map)
  "Common map for an egg special buffer.\\{egg-buffer-mode-map}" )

(defun egg-get-buffer (fmt create)
  "Get a special egg buffer. If buffer doesn't exist and CREATE was not nil then
creat the buffer. FMT is used to construct the buffer name. The name is built as:
(format FMT current-dir-name git-dir-full-path)."
  (let* ((git-dir (egg-git-dir))
	 (dir (file-name-directory git-dir))
	 (dir-name (file-name-nondirectory
		    (directory-file-name dir)))
	 (buf-name (format fmt dir-name git-dir))
	 (default-directory dir)
	 (buf (get-buffer buf-name)))
    (unless (or (bufferp buf) (not create))
      (setq buf (get-buffer-create buf-name)))
    buf))

(defmacro define-egg-buffer (type name-fmt &rest body)
  "Define an egg-special-file type."
  (let* ((type-name (symbol-name type))
	 (get-buffer-sym (intern (concat "egg-get-" type-name "-buffer")))
	 (buffer-mode-sym (intern (concat "egg-" type-name "-buffer-mode")))
	 (buffer-mode-hook-sym (intern (concat "egg-" type-name "-buffer-mode-hook")))
	 (buffer-mode-map-sym (intern (concat "egg-" type-name "-buffer-mode-map")))
	 (update-buffer-no-create-sym (intern (concat "egg-update-" type-name "-buffer-no-create"))))
    `(progn
       (defun ,buffer-mode-sym ()
	 ,@body
	 (set (make-local-variable 'egg-internal-buffer-obarray)
	      (make-vector 67 0)))

       (defun ,get-buffer-sym (&optional create)
	 (let ((buf (egg-get-buffer ,name-fmt create)))
	   (when (bufferp buf)
	     (with-current-buffer buf
	       (unless (eq major-mode ',buffer-mode-sym)
		 (,buffer-mode-sym))))
	   buf))
       ,(unless (string-match ":" type-name)
	  `(progn
	     (defun ,update-buffer-no-create-sym ()
	       (let ((buf (,get-buffer-sym)))
		 (when (bufferp buf)
		   (with-current-buffer buf
		     (when (functionp egg-buffer-refresh-func)
		       (funcall egg-buffer-refresh-func buf))))))
	     (add-hook 'egg-buffers-refresh-hook ',update-buffer-no-create-sym))))))


;; (cl-macroexpand '(define-egg-buffer diff "*diff-%s@egg:%s*"))
;; (cl-macroexpand ' (define-egg-buffer diff (buf) "*diff-%s@egg:%s*" (show-diff buf) ))



;;;========================================================
;;; Status Buffer
;;;========================================================
(defconst egg-status-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:StatusBuffer")))
    (set-keymap-parent map egg-buffer-mode-map)
    (define-key map (kbd "c") 'egg-commit-log-edit)
    (define-key map (kbd "o") 'egg-checkout-ref)
    (define-key map (kbd "l") 'egg-log)
    (define-key map (kbd "L") 'egg-reflog)
    (define-key map (kbd "S") 'egg-stage-all-files)
    map)
  "Keymap for the status buffer.\\{egg-status-buffer-mode-map}")

(defconst egg-status-buffer-rebase-map 
  (let ((map (make-sparse-keymap "Egg:StatusBufferRebase")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "x") 'egg-buffer-rebase-abort)
    (define-key map (kbd "u") 'egg-buffer-rebase-skip)
    (define-key map (kbd "RET") 'egg-buffer-selective-rebase-continue)
    map)
  "Context keymap for the repo section of the status buffer when
  rebase is in progress.\\{egg-status-buffer-rebase-map}")

(defun egg-buffer-do-rebase (upstream-or-action 
			     &optional old-base prompt)
  "Perform rebase action from an egg special buffer.
See `egg-do-rebase-head'."
  (let ((git-dir (egg-git-dir))
	modified-files res)
    (if (stringp upstream-or-action)
	(unless (egg-repo-clean)
	  (egg-status)
	  (error "Repo %s is not clean" git-dir))
      (unless (file-directory-p (concat git-dir "/" egg-git-rebase-subdir))
	(error "No rebase in progress in directory %s"
	       (file-name-directory git-dir))))
    (setq res (egg-do-rebase-head upstream-or-action old-base prompt))
    (setq modified-files (plist-get res :files))
    (if modified-files
	(egg-revert-visited-files modified-files))
    (message "GIT-REBASE> %s" (plist-get res :message))
    (plist-get res :success)))

(defun egg-buffer-rebase-continue ()
  "Continue the current rebase session."
  (interactive)
  (message "continue with current rebase")
  (unless (egg-buffer-do-rebase :continue)
    (egg-status)))

(defsubst egg-do-async-rebase-continue (callback closure &optional
						 action
						 exit-code)
  "Continue the current rebase session asynchronously."
  (let ((process-environment process-environment)
	(action (or action "--continue"))
	(buffer (current-buffer))
	proc)
    (setenv "EDITOR" "\nplease commit in egg")
    (setq proc (egg-async-1 (list callback closure) "rebase" action))
    (process-put proc :orig-buffer buffer)
    proc))

(defun egg-buffer-selective-rebase-continue ()
  "Continue the current rebase session.
The mode, sync or async, will depend on the nature of the current
rebase session."
  (interactive)
  (if (not (egg-interactive-rebase-in-progress))
      (egg-buffer-rebase-continue)
    (message "continue with interactive rebase")
    (egg-do-async-rebase-continue
     #'egg-handle-rebase-interactive-exit
     (egg-pick-file-contents (concat (egg-git-rebase-dir) "head") "^.+$"))))

(defun egg-buffer-rebase-skip ()
  (interactive)
  (message "skip rebase's current commit")
  (unless (egg-buffer-do-rebase :skip)
    (egg-status)))

(defun egg-buffer-rebase-abort ()
  (interactive)
  (message "abort current rebase")
  (egg-buffer-do-rebase :abort)
  (egg-status))


(defsubst egg-pretty-help-text (&rest strings)
  "Perform key bindings substitutions and highlighting in STRINGS."
  (let* ((map (current-local-map)) last-found)
    (with-temp-buffer
      (use-local-map map)
      (save-match-data
	;; key substitutions
	(insert (substitute-command-keys
		 (mapconcat 'identity strings "")))
	(goto-char (point-min))
	;; key highlighting
	(while (re-search-forward "\\(\\<[^\n \t:]+\\|[/+.~-]\\):" nil t)
	  (put-text-property (match-beginning 1) (match-end 1)'face 'egg-help-key)
	  (if last-found
	      (put-text-property last-found (1- (match-beginning 0))
				 'face 'egg-text-help))
	  (setq last-found (point)))
	(if last-found
	    (put-text-property last-found (line-end-position) 'face 'egg-text-help))
	;; return the total
	(buffer-string)))))

(defconst egg-status-buffer-common-help-text
  (concat
   (egg-text "Common Key Bindings:" 'egg-help-header-2)
   (egg-pretty-help-text
    "\\<egg-status-buffer-mode-map>\n"
    "\\[egg-buffer-cmd-navigate-prev]:previous block  "
    "\\[egg-buffer-cmd-navigate-next]:next block  " 
    "\\[egg-commit-log-edit]:commit staged modifications  "
    "\\[egg-log]:show repo's history\n" 
    "\\[egg-stage-all-files]:stage all modifications  " 
    "\\<egg-hide-show-map>"
    "\\[egg-section-cmd-toggle-hide-show]:hide/show block  " 
    "\\[egg-section-cmd-toggle-hide-show-children]:hide sub-blocks  "
    "\\<egg-buffer-mode-map>"
    "\\[egg-buffer-cmd-refresh]:redisplay  "
    "\\[quit-window]:quit\n")))

(defconst egg-status-buffer-rebase-help-text
  (concat 
   (egg-text "Key Bindings for Rebase Operations:" 'egg-help-header-2)
   (egg-pretty-help-text
    "\\<egg-status-buffer-rebase-map>\n"
    "\\[egg-buffer-selective-rebase-continue]:resume rebase  "
    "\\[egg-buffer-rebase-skip]:skip this rebase step  "
    "\\[egg-buffer-rebase-abort]:abort current rebase session\n")))

(defconst egg-status-buffer-diff-help-text
  (concat
   (egg-text "Extra Key Bindings for the Diff Sections:" 
	     'egg-help-header-2)
   (egg-pretty-help-text
    "\\<egg-unstaged-diff-section-map>\n"
    "\\[egg-diff-section-cmd-visit-file-other-window]:visit file/line  "
    "\\[egg-diff-section-cmd-stage]:stage/unstage file/hunk  "
    "\\[egg-diff-section-cmd-undo]:undo file/hunk's modificatons\n")))
  
(defun egg-sb-insert-repo-section ()
  "Insert the repo section into the status buffer."
  (let* ((state (egg-repo-state))
	 (sha1 (plist-get state :sha1))
	 (beg (point))
	 (map egg-section-map)
	 (rebase-step (plist-get state :rebase-step))
	 (rebase-num (plist-get state :rebase-num))
	 inv-beg help-beg help-inv-beg rebase-beg)
    ;; head, sha1 and git-dir
    (insert (egg-text (egg-pretty-head-string state) 'egg-branch) "\n"
	    (egg-text sha1 'font-lock-string-face) "\n"
	    (egg-text (plist-get state :gitdir) 'font-lock-constant-face)
	    "\n")
    ;; invisibility start at the newline
    (setq inv-beg (1- (point)))
    (when rebase-step
      ;; Rebase info and keybindings
      (insert (format "Rebase: commit %s of %s\n" rebase-step rebase-num))
      (setq map egg-status-buffer-rebase-map))
    (when (memq :status egg-show-key-help-in-buffers)
      ;; Help
      (insert "\n")
      (setq help-beg (point))
      (insert (egg-text "Help" 'egg-help-header-1) "\n")
      (setq help-inv-beg (1- (point)))
      (insert egg-status-buffer-common-help-text)
      (when (eq egg-status-buffer-rebase-map map)
	(insert egg-status-buffer-rebase-help-text))
      (insert egg-status-buffer-diff-help-text))
    ;; Mark the repo section
    (egg-delimit-section :section 'repo beg (point) inv-beg map 'repo)
    (when help-beg
      ;; Mark the help sub-section so it can be hidden
      (egg-delimit-section :help 'help help-beg (point) help-inv-beg map 
			 'egg-compute-navigation))))

(defun egg-ignore-pattern-from-string-at-point ()
  (interactive)
  (let ((string (egg-string-at-point))
	(file (ffap-file-at-point))
	dir pattern gitignore)
    (setq pattern (read-string "ignore pattern: "
			       (if (string-match "\\.[^.]+\\'" string)
				   (match-string-no-properties 0 string)
				 string)))
    (when (equal pattern "")
      (error "Can't ignore empty string!"))
    (setq dir (if (stringp file)
		  (file-name-directory (expand-file-name file))
		default-directory))
    (setq gitignore 
	  (read-file-name (format "add pattern `%s' to: " pattern)
			  dir nil nil ".gitignore"))
    (save-excursion
      (with-current-buffer (find-file-noselect gitignore t t)
	(goto-char (point-max))
	(insert pattern "\n")
	(save-buffer)
	(kill-buffer (current-buffer))))
    (egg-buffer-cmd-refresh)))

(defconst egg-untracked-file-map
  (let ((map (make-sparse-keymap "Egg:UntrackedFile")))
    (define-key map (kbd "RET") 'egg-find-file-at-point)
    (define-key map (kbd "DEL") 'egg-ignore-pattern-from-string-at-point)
    map))

(defun egg-sb-insert-untracked-section ()
  "Insert the untracked files section into the status buffer."
  (let ((beg (point)) inv-beg end)
    (insert (egg-prepend "Untracked Files:" "\n\n" 
			 'face 'egg-section-title)
	    "\n")
    (setq inv-beg (1- (point)))
    (call-process "git" nil t nil "ls-files" "--others"  
		  "--exclude-standard")
    (setq end (point))
    (egg-delimit-section :section 'untracked beg end 
			  inv-beg egg-section-map 'untracked)
    (put-text-property beg end 'keymap egg-untracked-file-map)))

(defun egg-sb-insert-unstaged-section (title &rest extra-diff-options)
  "Insert the unstaged changes section into the status buffer."
  (let ((beg (point)) inv-beg diff-beg)
    (insert (egg-prepend title "\n\n" 'face 'egg-section-title)
	    "\n")
    (setq diff-beg (point))
    (setq inv-beg (1- (point)))
    (apply 'call-process "git" nil t nil "diff" "--no-color"  "-p"
	   "--src-prefix=INDEX:/" "--dst-prefix=WORKDIR:/"
	   extra-diff-options)
    (egg-delimit-section :section 'unstaged beg (point)
			  inv-beg egg-section-map 'unstaged)
    ;; this section might contains merge conflicts, thus cc-diff
    (egg-decorate-diff-section :begin diff-beg 
			       :end (point) 
			       :src-prefix "INDEX:/"
			       :dst-prefix "WORKDIR:/"
			       :diff-map egg-unstaged-diff-section-map
			       :hunk-map egg-unstaged-hunk-section-map
			       :cc-diff-map egg-unmerged-diff-section-map
			       :cc-hunk-map egg-unmerged-hunk-section-map
			       :conflict-map egg-unmerged-hunk-section-map
			       )))

(defun egg-sb-insert-staged-section (title &rest extra-diff-options)
  "Insert the staged changes section into the status buffer."
  (let ((beg (point)) inv-beg diff-beg)
    (insert (egg-prepend title "\n\n"
			  'face 'egg-section-title)
	    "\n")
    (setq diff-beg (point)
	  inv-beg (1- diff-beg))
    (apply 'call-process "git" nil t nil "diff" "--no-color" "--cached" "-p"
	   "--src-prefix=HEAD:/" "--dst-prefix=INDEX:/"
	   extra-diff-options)
    (egg-delimit-section :section 'staged beg (point)
			  inv-beg egg-section-map 'staged)
    ;; this section never contains merge conflicts, thus no cc-diff
    (egg-decorate-diff-section :begin diff-beg 
			       :end (point) 
			       :src-prefix "HEAD:/"
			       :dst-prefix "INDEX:/"
			       :diff-map egg-staged-diff-section-map
			       :hunk-map egg-staged-hunk-section-map)))

(defun egg-checkout-ref (&optional default)
  "Prompt a revision to checkout. Default is DEFAULT."
  (interactive (list (car (get-text-property (point) :ref))))
  (egg-do-checkout (completing-read "checkout: " (egg-all-refs)
				    nil nil (or default "HEAD"))))

(defsubst egg-buffer-hide-all ()
  "Hide all sections in current special egg buffer."
  (let ((pos (point-min)) nav)
    (while (setq pos (next-single-property-change (1+ pos) :navigation))
      (setq nav (get-text-property pos :navigation))
      (add-to-invisibility-spec (cons nav t)))))

(defsubst egg-buffer-maybe-hide-all ()
  "If requsted, hide all sections in current special egg buffer.
See `egg-buffer-hide-sub-blocks-on-start'."
  (if (memq major-mode egg-buffer-hide-sub-blocks-on-start)
      (egg-buffer-hide-all)))

(defsubst egg-buffer-maybe-hide-help (help-nav &optional top-nav)
  "If requested, hide the help section in the current special buffer.
See `egg-buffer-hide-help-on-start'."
  (if (memq major-mode egg-buffer-hide-help-on-start)
      (add-to-invisibility-spec 
       (cons (if (symbolp help-nav) help-nav
	       (egg-make-navigation top-nav help-nav))
	     t))))

(defun egg-status-buffer-redisplay (buf &optional init)
  "(Re)Display the contents of the status buffer in BUF.
If INIT was not nil, then perform 1st-time initializations as well."
  (with-current-buffer buf
    (let ((inhibit-read-only t)
	  (orig-pos (point)))
      (erase-buffer)

      (dolist (sect egg-status-buffer-sections)
	(cond ((eq sect 'repo) (egg-sb-insert-repo-section))
	      ((eq sect 'unstaged) (egg-sb-insert-unstaged-section "Unstaged Changes:"))
	      ((eq sect 'staged) (egg-sb-insert-staged-section "Staged Changes:"))
	      ((eq sect 'untracked) (egg-sb-insert-untracked-section))))
      (if init (egg-buffer-maybe-hide-all))
      (if init (egg-buffer-maybe-hide-help "help" 'repo))
      (goto-char orig-pos))))

(defun egg-internal-background (proc msg)
  "Background job sentinel."
  (let ((name (process-name proc)))
    (cond ((string= msg "finished\n")
	   (message "EGG BACKGROUND: %s finished." name))
	  ((string= msg "killed\n")
	   (message "EGG BACKGROUND: %s was killed." name))
	  ((string-match "exited abnormally" msg)
	   (message "EGG BACKGROUND: %s failed." name))
	  (t (message "EGG BACKGROUND: %s is weird!" name)))))

(defun egg-internal-background-refresh-index (buffer-name)
  (let ((buffer (get-buffer buffer-name))
	proc)
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
	(setq proc (start-process (format "refresh index in %s"
					  default-directory)
				  nil
				  "git" "update-index"
				  "-q" "--really-refresh" "--unmerged"))
	  (set-process-sentinel proc #'egg-internal-background)))))

(defvar egg-internal-status-buffer-names-list nil)
(defvar egg-internal-background-jobs-timer nil)

(defun egg-status-buffer-background-job ()
  (when egg-refresh-index-in-backround
    (mapcar #'egg-internal-background-refresh-index
	    egg-internal-status-buffer-names-list)))

(defsubst egg-internal-background-jobs-restart ()
  (cancel-function-timers #'egg-status-buffer-background-job)
  (setq egg-internal-background-jobs-timer
	(run-with-idle-timer egg-background-idle-period t
			     #'egg-status-buffer-background-job)))

(defun egg-set-background-idle-period (var val)
  (custom-set-default var val)
  (egg-internal-background-jobs-restart))

(defcustom egg-background-idle-period 30
  "How long emacs has been idle before we trigger background jobs."
  :group 'egg
  :set 'egg-set-background-idle-period
  :type 'integer)

(egg-internal-background-jobs-restart)

(define-egg-buffer status "*%s-status@%s*"
  "Major mode to display the egg status buffer."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-status-buffer-mode
	mode-name  "Egg-Status"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-status-buffer-mode-map)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-status-buffer-redisplay)
  (setq buffer-invisibility-spec nil)
  (add-to-list 'egg-internal-status-buffer-names-list (buffer-name))
  (run-mode-hooks 'egg-status-buffer-mode-hook))

(defun egg-status (&optional no-update caller)
  (interactive "P")
  (let* ((egg-internal-current-state 
	  (egg-repo-state (if (interactive-p) :error-if-not-git)))
	 (buf (egg-get-status-buffer 'create)))
    (unless no-update
      (with-current-buffer buf
	(egg-status-buffer-redisplay buf 'init)))
    (cond ((interactive-p) (display-buffer buf t))
	  ((eq caller :sentinel) (pop-to-buffer buf))
	  (t (pop-to-buffer buf t)))))

;;;========================================================
;;; action
;;;========================================================

(defun egg-revert-visited-files (file-or-files)
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 (files (if (consp file-or-files) 
		   file-or-files
		 (list file-or-files))))
    (mapcar (lambda (file)
	      (let ((buf (get-file-buffer file)))
		(when (bufferp buf)
		  (with-current-buffer buf
		    (when (equal (egg-git-dir) git-dir)
		      (revert-buffer t t t))))))
	    files)))

(defun egg-revert-all-visited-files ()
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 bufs files)
    (setq files
	  (delq nil (mapcar (lambda (buf)
			      (with-current-buffer buf
				(when (and (buffer-file-name buf)
					   (equal (egg-git-dir) git-dir))
				  (buffer-file-name buf))))
			    (buffer-list))))
    (when (consp files)
      (setq files (mapcar 'expand-file-name
			  (apply 'egg-git-to-lines "ls-files" files)))
      (when (consp files)
	(egg-revert-visited-files files)))))

(defun egg-cmd-log-buffer ()
  (or (get-buffer (concat " *egg-cmd-logs@" (egg-git-dir) "*"))
      (let ((git-dir (egg-git-dir))
	    (default-directory default-directory)
	    dir)
	(unless git-dir
	  (error "Can't find git dir in %s" default-directory))
	(setq dir (file-name-nondirectory git-dir))
	(setq default-directory dir)
	(get-buffer-create (concat " *egg-cmd-logs@" git-dir "*")))))

(defsubst egg-cmd-log (&rest strings)
  (with-current-buffer (egg-cmd-log-buffer)
    (goto-char (point-max))
    (cons (current-buffer)
	  (prog1 (point)
	    (apply 'insert "LOG/" strings)))))

(defun egg-sync-handle-exit-code (ret accepted-codes logger)
  (let (output)
    (with-current-buffer (car logger)
      (save-excursion
	(goto-char (cdr logger))
	(forward-line 1)
	(setq output (buffer-substring-no-properties 
		      (point) (point-max)))))
    (egg-cmd-log (format "RET:%d\n" ret))
    (if (listp accepted-codes)
	(setq accepted-codes (cons 0 accepted-codes))
      (setq accepted-codes (list 0 accepted-codes)))
    (if (null (memq ret accepted-codes))
	(with-current-buffer (car logger)
	  (widen)
	  (narrow-to-region (cdr logger) (point-max))
	  (display-buffer (current-buffer) t)
	  nil)
      (egg-run-buffers-update-hook)
      output)))

(defun egg-sync-do (program stdin accepted-codes args)
  (let (logger ret)
    (setq logger (egg-cmd-log "RUN:" program " " (mapconcat 'identity args " ")
			      (if stdin " <REGION\n" "\n")))
    (setq ret 
	  (cond ((stringp stdin)
		 (with-temp-buffer
		   (insert stdin)
		   (apply 'call-process-region (point-min) (point-max)
				program nil (car logger) nil args)))
		((consp stdin)
		 (apply 'call-process-region (car stdin) (cdr stdin)
			program nil (car logger) nil args))
		((null stdin)
		 (apply 'call-process program nil (car logger) nil args)))) 
    (egg-sync-handle-exit-code ret accepted-codes logger)))

(defsubst egg-sync-do-region-0 (program beg end args)
  (egg-sync-do program (cons beg end) nil args))

(defsubst egg-sync-0 (&rest args)
  (egg-sync-do "git" nil nil args))

(defsubst egg-sync-do-region (program beg end &rest args)
  (egg-sync-do program (cons beg end) nil args))

(defsubst egg-sync-git-region (beg end &rest args)
  (egg-sync-do "git" (cons beg end) nil args))

(defun egg-sync-do-file (file program stdin accepted-codes args)
  (let ((default-directory (file-name-directory (egg-git-dir)))
	output)
    (setq file (expand-file-name file))
    (setq args (mapcar (lambda (word)
			 (if (string= word file) file word))
		       args))
    (when (setq output (egg-sync-do program stdin accepted-codes args))
      (cons file output))))

(defun egg-hunk-section-patch-cmd (pos program &rest args)
  (let ((patch (egg-hunk-section-patch-string pos))
	(file (car (get-text-property pos :diff))))
    (unless (stringp file)
      (error "No diff with file-name here!"))
    (egg-sync-do-file file program patch nil args)))

(defun egg-show-git-output (output line-no &optional prefix)
  (unless (stringp prefix) (setq prefix "GIT"))
  (if (consp output) (setq output (cdr output)))
  (when (and (stringp output) (> (length output) 1))
    (when (numberp line-no)
      (when (setq output (save-match-data (split-string output "\n" t)))
	(cond ((< line-no 0)
	       (setq line-no (1+ line-no))
	       (setq output (nth line-no (nreverse output))))
	      ((> line-no 0)
	       (setq line-no (1- line-no))
	       (setq output (nth line-no output)))
	      (t (setq output nil)))))
    (when (stringp output)
      (message "%s> %s" prefix output)
      t)))

(defun egg-hunk-section-cmd-stage (pos)
  (interactive (list (point)))
  (egg-show-git-output 
   (egg-hunk-section-patch-cmd pos "git" "apply" "--cached")
   -1 "GIT-APPLY"))

(defun egg-hunk-section-cmd-unstage (pos)
  (interactive (list (point)))
  (egg-show-git-output 
   (egg-hunk-section-patch-cmd pos "git" "apply" "--cached" "--reverse")
   -1 "GIT-APPLY"))

(defun egg-hunk-section-cmd-undo (pos)
  (interactive (list (point)))
  (let ((file (egg-hunk-section-patch-cmd pos "patch"
					  "-p1" "--quiet" "--reverse")))
    (if (consp file) (setq file (car file)))
    (when (stringp file)
      (egg-revert-visited-files file))))

(defun egg-diff-section-patch-cmd (pos accepted-codes &rest args)
  (let ((file (car (get-text-property pos :diff))))
    (unless (stringp file)
      (error "No diff with file-name here!"))
    (egg-sync-do-file file "git" nil accepted-codes
		      (append args (list file)))))

(defun egg-diff-section-cmd-stage (pos)
  (interactive (list (point)))
  (egg-diff-section-patch-cmd pos nil "add"))

(defun egg-diff-section-cmd-unstage (pos)
  (interactive (list (point)))
  (egg-show-git-output 
   (egg-diff-section-patch-cmd pos 1 "reset" "HEAD" "--")
   1  "GIT-RESET"))

(defun egg-diff-section-cmd-undo-old-no-revsion-check (pos)
  (interactive (list (point)))
  (let ((file (egg-diff-section-patch-cmd pos nil "checkout" "--")))
    (if (consp file) (setq file (car file)))
    (when (stringp file)
      (egg-revert-visited-files file))))

(defun egg-diff-section-cmd-undo (pos)
  (interactive (list (point)))
  (let ((file (car (or (get-text-property pos :diff)
		       (error "No diff with file-name here!"))))
	(src-rev (get-text-property pos :src-revision))
	args)
    (setq args
	  (if (stringp src-rev)
	      (list "checkout" src-rev "--" file)
	    (list "checkout" "--" file)))
    (when (setq file (egg-sync-do-file file "git" nil nil args))
      (if (consp file) (setq file (car file)))
      (when (stringp file)
	(egg-revert-visited-files file)))))

(defun egg-file-stage-current-file ()
  (interactive)
  (let ((git-dir (egg-git-dir))
	(file (buffer-file-name)))
    (when (egg-sync-do-file file "git" nil nil (list "add" "--" file))
	(message "staged %s modifications" file))))

(defun egg-stage-all-files ()
  (interactive)
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir)))
    (when (egg-sync-do "git" nil nil (list "add" "-u"))
	(message "staged all tracked files's modifications"))))


(defun egg-do-checkout (rev)
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir)))
    (if (egg-sync-do "git" nil nil (list "checkout" rev))
	(egg-revert-all-visited-files))))

(defun egg-do-tag (&optional rev prompt force)
  (let ((all-refs (egg-all-refs))
	(name (read-string (or prompt "new tag name: ")))
	(rev (or rev "HEAD")))
    (when (and (not force) (member name all-refs))
      (error "referene %s already existed!" name))
    (if force
	(egg-git-ok nil "tag" "-f" name rev)
      (egg-git-ok nil "tag" name rev))))

(defun egg-do-create-branch (&optional rev checkout prompt force)
  (let ((all-refs (egg-all-refs))
	(name (read-string (or prompt "create new branch: ")))
	(rev (or rev "HEAD")))
    (when (and (not force) (member name all-refs))
      (error "referene %s already existed!" name))
    (if (null checkout)
	(if force 
	    (egg-git-ok nil "branch" "-f" name rev)
	  (egg-git-ok nil "branch" name rev))
      (if force
	  (egg-sync-0 "checkout" "-b" "-f" name rev)
	(egg-sync-0 "checkout" "-b" name rev)))))

(defun egg-do-move-head (rev &optional update-wdir update-index)
  (when (egg-show-git-output
	 (cond (update-wdir (egg-sync-0 "reset" "--hard" rev))
	       (update-index (egg-sync-0 "reset" rev))
	       (t (egg-sync-0 "reset" "--soft" rev)))
	 -1 "GIT-RESET")
    (if update-wdir (egg-revert-all-visited-files))))

(defun egg-do-merge-to-head (rev &optional no-commit)
  (let ((msg (concat "merging in " rev))
	(commit-flag (if no-commit "--no-commit" "--commit"))
	(pre-merge (egg-get-current-sha1))
	merge-cmd-res modified-files res feed-back)
    (with-temp-buffer
      (setq merge-cmd-res (egg-git-ok (current-buffer)
		      "merge" "--log" commit-flag "-m" msg rev))
      (goto-char (point-min))
      (setq modified-files 
	    (egg-git-to-lines "diff" "--name-only" pre-merge))
      (setq feed-back
	    (save-match-data
	      (car (nreverse (split-string (buffer-string)
					   "[\n]+" t)))))
      (egg-run-buffers-update-hook)
      (list :success merge-cmd-res
	    :files modified-files
	    :message feed-back))))

(defun egg-do-rebase-head (upstream-or-action 
			   &optional old-base prompt)
  (let ((pre-merge (egg-get-current-sha1))
	cmd-res modified-files feed-back old-choices)
;;;     (with-temp-buffer
    (with-current-buffer (get-buffer-create "*egg-debug*")
      (erase-buffer)
      (when (and (stringp upstream-or-action) ;; start a rebase
		 (eq old-base t))	      ;; ask for old-base
	(unless (egg-git-ok (current-buffer) "rev-list"
			    "--topo-order" "--reverse"
			    (concat upstream-or-action "..HEAD^"))
	  (error "Failed to find rev between %s and HEAD^: %s"
		 upstream-or-action (buffer-string)))
	(unless (egg-git-region-ok (point-min) (point-max)
				   "name-rev" "--stdin")
	  (error "Failed to translate revisions: %s" (buffer-string)))
	(save-match-data 
	  (goto-char (point-min))
	  (while (re-search-forward "^.+(\\(.+\\))$" nil t)
	    (setq old-choices (cons (match-string-no-properties 1)
				    old-choices))))
	(setq old-base
	      (completing-read (or prompt "old base: ") old-choices))
	(erase-buffer))
      
      (setq cmd-res 
	    (cond ((and (stringp old-base) (stringp upstream-or-action))
		   (egg-git-ok (current-buffer) "rebase" "-m" "--onto"
			       upstream-or-action old-base))
		  ((eq upstream-or-action :abort)
		   (egg-git-ok (current-buffer) "rebase" "--abort"))
		  ((eq upstream-or-action :skip)
		   (egg-git-ok (current-buffer) "rebase" "--skip"))
		  ((eq upstream-or-action :continue)
		   (egg-git-ok (current-buffer) "rebase" "--continue"))
		  ((stringp upstream-or-action)
		   (egg-git-ok (current-buffer) "rebase" "-m" 
			       upstream-or-action))))
      (goto-char (point-min))
      (setq feed-back
	    (egg-safe-search-pickup 
	     "^\\(?:CONFLICT\\|All done\\|HEAD is now at\\|Fast-forwarded\\|You must edit all merge conflicts\\).+$")) 
      (setq modified-files 
	    (egg-git-to-lines "diff" "--name-only" pre-merge))
      (egg-run-buffers-update-hook)
      (list :success cmd-res
	    :message feed-back
	    :files modified-files))))

(defun egg-rm-ref (&optional force name prompt default)
  (let* ((refs-alist (egg-ref-type-alist))
	 (name (or name (completing-read (or prompt "remove ref: ")
					 refs-alist nil t
					 default)))
	 (type (cdr (assoc name refs-alist))))
    (unless (and name type)
      (error "Cannot find reference %s!" name))
    (egg-show-git-output
     (cond ((eq :tag type)
	    (egg-sync-0 "tag" "-vd" name))
	   ((eq :head type)
	    (egg-sync-0 "branch" (if force "-vD" "-vd") name))
	   ((eq :remote type)
	    (egg-sync-0 "branch" (if force "-vrD" "-vrd") name)))
     -1)))

;;;========================================================
;;; log message
;;;========================================================

(require 'derived)
(require 'ring)

(defvar egg-log-msg-ring (make-ring 32))
(defvar egg-log-msg-ring-idx nil)
(defvar egg-log-msg-action nil)
(defvar egg-log-msg-text-beg nil)
(defvar egg-log-msg-text-end nil)
(defvar egg-log-msg-diff-beg nil)
(define-derived-mode egg-log-msg-mode text-mode "Egg-LogMsg"
  "Major mode for editing Git log message.\n\n
\{egg-log-msg-mode-map}."
  (setq default-directory (file-name-directory (egg-git-dir)))
  (make-local-variable 'egg-log-msg-action)
  (set (make-local-variable 'egg-log-msg-ring-idx) nil)
  (set (make-local-variable 'egg-log-msg-text-beg) nil)
  (set (make-local-variable 'egg-log-msg-text-end) nil)
  (set (make-local-variable 'egg-log-msg-diff-beg) nil))

(define-key egg-log-msg-mode-map (kbd "C-c C-c") 'egg-log-msg-done)
(define-key egg-log-msg-mode-map (kbd "M-p") 'egg-log-msg-older-text)
(define-key egg-log-msg-mode-map (kbd "M-n") 'egg-log-msg-newer-text)
(define-key egg-log-msg-mode-map (kbd "C-l") 'egg-buffer-cmd-refresh)

(defun egg-log-msg-commit ()
  (let (output)
    (setq output 
	  (egg-sync-git-region egg-log-msg-text-beg egg-log-msg-text-end 
			       "commit" "-F" "-"))
    (when output
      (egg-show-git-output output -1 "GIT-COMMIT")
      (egg-run-buffers-update-hook))))

(defun egg-log-msg-amend-commit ()
  (let (output)
    (setq output 
	  (egg-sync-git-region egg-log-msg-text-beg egg-log-msg-text-end 
			       "commit" "--amend" "-F" "-"))
    (when output
      (egg-show-git-output output -1 "GIT-COMMIT-AMEND")
      (egg-run-buffers-update-hook))))

(defun egg-log-msg-done ()
  (interactive)
  (widen)
  (goto-char egg-log-msg-text-beg)
  (if (save-excursion (re-search-forward "\\sw\\|\\-" 
					 egg-log-msg-text-end t))
      (when (functionp egg-log-msg-action)
	(ring-insert egg-log-msg-ring 
		     (buffer-substring-no-properties egg-log-msg-text-beg
						     egg-log-msg-text-end))
	(funcall egg-log-msg-action)
	(let ((inhibit-read-only t)
	      (win (get-buffer-window (current-buffer))))
	  (erase-buffer)
	  (if (windowp win) (quit-window nil win))))
    (message "Please enter a log message!")
    (ding)))

(defun egg-log-msg-hist-cycle (&optional forward)
  "Cycle through message log history."
  (let ((len (ring-length egg-log-msg-ring)))
    (cond ((<= len 0) 
	   ;; no history
	   (message "No previous log message.")
	   (ding))
	  ;; don't accidentally throw away unsaved text
	  ((and  (null egg-log-msg-ring-idx)
		 (> egg-log-msg-text-end egg-log-msg-text-beg)
		 (not (y-or-n-p "throw away current text? "))))
	  ;; do it
	  (t (delete-region egg-log-msg-text-beg egg-log-msg-text-end)
	     (setq egg-log-msg-ring-idx
		   (if (null egg-log-msg-ring-idx)
		       (if forward 
			   ;; 1st-time + fwd = oldest
			   (ring-minus1 0 len)
			 ;; 1st-time + bwd = newest
		       0)
		     (if forward 
			 ;; newer
			 (ring-minus1 egg-log-msg-ring-idx len)
		       ;; older
		       (ring-plus1 egg-log-msg-ring-idx len)))) 
	     (goto-char egg-log-msg-text-beg)
	     (insert (ring-ref egg-log-msg-ring egg-log-msg-ring-idx))))))

(defun egg-log-msg-older-text ()
  "Cycle backward through comment history."
  (interactive)
  (egg-log-msg-hist-cycle))

(defun egg-log-msg-newer-text ()
  "Cycle forward through comment history."
  (interactive)
  (egg-log-msg-hist-cycle t))

(defun egg-commit-log-buffer-show-diffs (buf &optional init)
  (with-current-buffer buf
    (let ((inhibit-read-only t) beg)
      (goto-char egg-log-msg-diff-beg)
      (delete-region (point) (point-max))
      (setq beg (point))

      (dolist (sect egg-commit-buffer-sections)
	(cond ((eq sect 'staged)
	       (egg-sb-insert-staged-section "Changes to Commit:" "--stat"))
	      ((eq sect 'unstaged)
	       (egg-sb-insert-unstaged-section "Deferred Changes:"))
	      ((eq sect 'untracked)
	       (egg-sb-insert-untracked-section))))

      (put-text-property beg (point) 'read-only t)
      (put-text-property beg (point) 'front-sticky nil)
      (if init (egg-buffer-maybe-hide-all))
      (force-window-update buf))))

(define-egg-buffer commit "*%s-commit@%s*"
  (egg-log-msg-mode)
  (setq major-mode 'egg-commit-buffer-mode
	mode-name "Egg-Commit"
	mode-line-process "")
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-commit-log-buffer-show-diffs)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-commit-buffer-mode-hook))

(defun egg-commit-log-edit (title-function
			    action-function
			    insert-init-text-function)
  (interactive (if current-prefix-arg
		   (list (concat 
			  (egg-text "Amending  " 'egg-text-3)
			  (egg-text (egg-pretty-head-name) 'egg-branch)) 
			 #'egg-log-msg-amend-commit
			 (egg-commit-message "HEAD"))
		 (list (concat 
			(egg-text "Committing into  " 'egg-text-3)
			(egg-text (egg-pretty-head-name) 'egg-branch))
		       #'egg-log-msg-commit
		       nil)))
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 (buf (egg-get-commit-buffer 'create))
	 (state (egg-repo-state))
	 (head-info (egg-head))
	 (head (or (cdr head-info) 
		   (format "Detached HEAD! (%s)" (car head-info))))
	 (inhibit-read-only inhibit-read-only))
    (with-current-buffer buf
      (setq inhibit-read-only t)
      (erase-buffer)
      (set (make-local-variable 'egg-log-msg-action)
	   action-function)
      (insert (cond ((functionp title-function) 
		     (funcall title-function state))
		    ((stringp title-function) title-function)
		    (t "Shit happens!"))
	      "\n"
	      "Repository: " (egg-text git-dir 'font-lock-constant-face) "\n"
	      (egg-text "--------------- Commit Message (type C-c C-c when done) ---------------"
			'font-lock-comment-face))
      (put-text-property (point-min) (point) 'read-only t)
      (put-text-property (point-min) (point) 'rear-sticky nil)
      (insert "\n")
      (set (make-local-variable 'egg-log-msg-text-beg) (point-marker))
      (set-marker-insertion-type egg-log-msg-text-beg nil)
      (put-text-property (1- egg-log-msg-text-beg) egg-log-msg-text-beg 
			 :navigation 'commit-log-text)
      (insert (egg-prop "\n------------------------ End of Commit Message ------------------------" 
			'read-only t 'front-sticky nil
			'face 'font-lock-comment-face))
      (set (make-local-variable 'egg-log-msg-diff-beg) (point-marker))
      (set-marker-insertion-type egg-log-msg-diff-beg nil)
      (egg-commit-log-buffer-show-diffs buf 'init)
      (goto-char egg-log-msg-text-beg)
      (cond ((functionp insert-init-text-function)
	     (funcall insert-init-text-function))
	    ((stringp insert-init-text-function)
	     (insert insert-init-text-function)))
      (set (make-local-variable 'egg-log-msg-text-end) (point-marker))
      (set-marker-insertion-type egg-log-msg-text-end t))
    (pop-to-buffer buf t)))

;;;========================================================
;;; diff-mode
;;;========================================================
(defconst egg-diff-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:DiffBuffer")))
    (set-keymap-parent map egg-buffer-mode-map)
    map))

(defun egg-diff-buffer-insert-diffs (buffer)
  (with-current-buffer buffer
    (let ((args (plist-get egg-diff-buffer-info :args))
	  (title (plist-get egg-diff-buffer-info :title))
	  (prologue (plist-get egg-diff-buffer-info :prologue))
	  (src-prefix (plist-get egg-diff-buffer-info :src))
	  (dst-prefix (plist-get egg-diff-buffer-info :dst))
	  (help (plist-get egg-diff-buffer-info :help))
	  (inhibit-read-only t)
	  pos beg inv-beg help-beg help-end help-inv-beg)
      (erase-buffer)
      (insert (egg-text title 'egg-section-title) "\n")
      (insert prologue "\n")
      (setq inv-beg (1- (point)))
      (when help
	(insert "\n")
	(setq help-beg (point))
	(insert (egg-text "Help" 'egg-help-header-1) "\n")
	(setq help-inv-beg (1- (point)))
	(insert help)
	(setq help-end (point)))
      (setq pos (point))
      (setq beg (point))
      (apply 'call-process "git" nil t nil "diff" args)
      (unless (> (point) beg)
	(insert (egg-text "No difference!\n" 'egg-text-4)))
      (egg-delimit-section :section 'file (point-min) (point) inv-beg
			   egg-section-map 'file)
      (egg-delimit-section :help 'help help-beg help-end help-inv-beg
			   egg-section-map 'egg-compute-navigation)
      (apply 'egg-decorate-diff-section
	     :begin (point-min)
	     :end (point)
	     :src-prefix src-prefix
	     :dst-prefix dst-prefix
	     egg-diff-buffer-info)
      (goto-char pos))))

(define-egg-buffer diff "*%s-diff@%s*"
  "Major mode to display the git diff output."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-diff-buffer-mode
	mode-name  "Egg-Diff"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-diff-buffer-mode-map)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-diff-buffer-insert-diffs)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-diff-buffer-mode-hook))

(defconst egg-diff-buffer-common-help-text
  (concat
   (egg-text "Common Key Bindings:" 'egg-help-header-2)
   (egg-pretty-help-text
    "\\<egg-buffer-mode-map>\n"
    "\\[egg-buffer-cmd-navigate-prev]:previous block  "
    "\\[egg-buffer-cmd-navigate-next]:next block  " 
    "\\[egg-buffer-cmd-refresh]:redisplay  "
    "\\[quit-window]:quit\n")))

(defconst egg-diff-buffer-diff-help-heading
  (egg-text "Extra Bindings for Diff blocks:" 'egg-help-header-2))

(defconst egg-unstaged-diff-help-text
  (egg-pretty-help-text
   "\\<egg-unstaged-diff-section-map>\n"
   "\\[egg-diff-section-cmd-stage]:stage file/hunk  "
   "\\[egg-diff-section-cmd-undo]:undo file/hunk  "
   "\\[egg-diff-section-cmd-visit-file-other-window]:visit file/line\n"))

(defconst egg-staged-diff-help-text
  (egg-pretty-help-text
   "\\<egg-staged-diff-section-map>\n"
   "\\[egg-diff-section-cmd-stage]:unstage file/hunk  "
   "\\[egg-diff-section-cmd-visit-file-other-window]:visit file/line\n"))

(defconst egg-plain-diff-help-text
  (egg-pretty-help-text
   "\\<egg-diff-section-map>\n"
   "\\[egg-diff-section-cmd-visit-file-other-window]:visit file/line\n"))

(defconst egg-wdir-diff-help-text
  (egg-pretty-help-text
   "\\<egg-wdir-diff-section-map>\n"
   "\\[egg-diff-section-cmd-undo]:undo file/hunk  "
   "\\[egg-diff-section-cmd-visit-file-other-window]:visit file/line\n"))

(defun egg-diff-info-add-help (info)
  (let ((map (plist-get info :diff-map)) help)
    (setq help
	  (concat egg-diff-buffer-common-help-text
		  egg-diff-buffer-diff-help-heading
		  (cond ((eq map egg-unstaged-diff-section-map)
			 egg-unstaged-diff-help-text)
			((eq map egg-staged-diff-section-map)
			 egg-staged-diff-help-text)
			((eq map egg-diff-section-map)
			 egg-plain-diff-help-text)
			((eq map egg-wdir-diff-section-map)
			 egg-wdir-diff-help-text))))
    (plist-put info :help help)))

(defun egg-do-diff (diff-info)
  (let* ((git-dir (egg-git-dir))
	 (dir (file-name-directory git-dir))
	 (buf (egg-get-diff-buffer 'create)))
    (with-current-buffer buf
      (set (make-local-variable 'egg-diff-buffer-info) diff-info)
      (egg-diff-buffer-insert-diffs buf))
    buf))

(defun egg-build-diff-info (src dst &optional file)
  (let* ((git-dir (egg-git-dir))
	 (dir (file-name-directory git-dir))
	 info tmp)
    (setq info
	  (cond ((and (null src) (null dst))
		 (list :args (list "--no-color" "-p"
				   "--src-prefix=INDEX/"
				   "--dst-prefix=WORKDIR/")
		       :title (format "from INDEX to %s" dir)
		       :prologue "hunks can be removed or added into INDEX."
		       :src "INDEX/" :dst "WORKDIR/"
		       :diff-map egg-unstaged-diff-section-map
		       :hunk-map egg-unstaged-hunk-section-map))
		((and (equal src "HEAD") (equal dst "INDEX"))
		 (list :args (list "--no-color" "--cached" "-p"
				   "--src-prefix=INDEX/"
				   "--dst-prefix=WORKDIR/")
		       :title "from HEAD to INDEX" 
		       :prologue "hunks can be removed from INDEX."
		       :src "HEAD/" :dst "INDEX/"
		       :diff-map egg-staged-diff-section-map
		       :hunk-map egg-staged-hunk-section-map))
		((and (stringp src) (stringp dst))
		 (list :args (list "--no-color" "-p"
				   (concat src ".." dst))
		       :title (format "from %s to %s" src dst) 
		       :prologue (format "a: %s\nb: %s" src dst)
		       :src-revision src
		       :dst-revision dst
		       :diff-map egg-diff-section-map
		       :hunk-map egg-hunk-section-map))
		((and (stringp src) (null dst))
		 (list :args (list "--no-color" "-p" src)
		       :title (format "from %s to %s" src dir) 
		       :prologue (concat (format "a: %s\nb: %s\n" src dir)
					 "hunks can be removed???")
		       :src-revision src
		       :diff-map egg-wdir-diff-section-map
		       :hunk-map egg-wdir-hunk-section-map))))
    (if (memq :diff egg-show-key-help-in-buffers)
	(egg-diff-info-add-help info))
    (when (stringp file)
      (setq file (list file)))
    (when (consp file) 
      (setq tmp (plist-get info :prologue))
      (setq tmp (concat (egg-text (mapconcat 'identity file "\n")
				  'egg-text-3)
			"\n"
			(egg-text tmp 'egg-text-1)))
      (plist-put info :prologue tmp)
      (setq tmp (plist-get info :args))
      (setq tmp (append tmp (cons "--" file)))
      (plist-put info :args tmp))
    info))



;;;========================================================
;;; log browsing
;;;========================================================
(defvar egg-log-buffer-comment-column nil)
(defvar egg-internal-log-buffer-closure nil)

(defun egg-run-git-log-HEAD ()
  (egg-git-ok t "log" (format "--max-count=%d" egg-log-HEAD-max-len) 
	      "--graph" "--topo-order"
		"--pretty=oneline" "--decorate"))

(defun egg-run-git-log-all ()
  (egg-git-ok t "log" (format "--max-count=%d" egg-log-all-max-len)
	      "--graph" "--topo-order"
		"--pretty=oneline" "--decorate" "--all"))

(defun egg-run-git-log-pickaxe (string)
  (egg-git-ok t "log" "--pretty=oneline" "--decorate"
	      (concat "-S" string)))

(defconst egg-log-commit-base-map
  (let ((map (make-sparse-keymap "Egg:LogCommitBase")))
    (set-keymap-parent map egg-hide-show-map)
    (define-key map (kbd "SPC") 'egg-log-buffer-insert-commit)
    (define-key map (kbd "B") 'egg-log-buffer-create-new-branch)
    (define-key map (kbd "b") 'egg-log-buffer-start-new-branch)
    (define-key map (kbd "o") 'egg-log-buffer-checkout-commit)
    (define-key map (kbd "t") 'egg-log-buffer-tag-commit)
    (define-key map (kbd "T") 'egg-log-buffer-atag-commit)
    (define-key map (kbd "a") 'egg-log-buffer-attach-head)
    (define-key map (kbd "m") 'egg-log-buffer-merge)
    (define-key map (kbd "r") 'egg-log-buffer-rebase)
    map))

(defconst egg-log-commit-map 
  (let ((map (make-sparse-keymap "Egg:LogCommit")))
    (set-keymap-parent map egg-log-commit-base-map)
    (define-key map (kbd "+") 'egg-log-buffer-mark-pick)
    (define-key map (kbd ".") 'egg-log-buffer-mark-squash)
    (define-key map (kbd "~") 'egg-log-buffer-mark-edit)
    (define-key map (kbd "-") 'egg-log-buffer-unmark)
    map))

(defconst egg-log-ref-map 
  (let ((map (make-sparse-keymap "Egg:LogRef")))
    (set-keymap-parent map egg-log-commit-map)
    (define-key map (kbd "L") 'egg-log-buffer-reflog-ref)
    (define-key map (kbd "x") 'egg-log-buffer-rm-ref)
    (define-key map (kbd "u") 'egg-log-buffer-push-to-local)
    map))

(defconst egg-log-local-ref-map 
  (let ((map (make-sparse-keymap "Egg:LogLocalRef")))
    (set-keymap-parent map egg-log-ref-map)
    (define-key map (kbd "U") 'egg-log-buffer-push-to-remote)
    (define-key map (kbd "d") 'egg-log-buffer-push-head-to-local)
    map))

(defconst egg-log-remote-ref-map 
  (let ((map (make-sparse-keymap "Egg:LogRemoteRef")))
    (set-keymap-parent map egg-log-ref-map)
    (define-key map (kbd "d") 'egg-log-buffer-fetch-remote-ref)
    (define-key map (kbd "d") 'egg-log-buffer-fetch-remote-ref)
    map))

(defconst egg-log-remote-site-map 
  (let ((map (make-sparse-keymap "Egg:LogRef")))
    (set-keymap-parent map egg-log-commit-map)
    (define-key map (kbd "d") 'egg-log-buffer-fetch)
    (define-key map (kbd "U") 'egg-log-buffer-push)
    map))

(defconst egg-log-diff-map 
  (let ((map (make-sparse-keymap "Egg:LogDiff")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-log-diff-cmd-visit-file-other-window)
    (define-key map (kbd "f") 'egg-log-diff-cmd-visit-file)
    (define-key map (kbd "=") 'egg-diff-section-cmd-ediff)
    map))

(defconst egg-log-hunk-map 
  (let ((map (make-sparse-keymap "Egg:LogHunk")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-log-hunk-cmd-visit-file-other-window)
    (define-key map (kbd "f") 'egg-log-hunk-cmd-visit-file)
    (define-key map (kbd "=") 'egg-diff-section-cmd-ediff)
    map))

(defconst egg-log-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:LogBuffer")))
    (set-keymap-parent map egg-buffer-mode-map)
    (define-key map "n" 'egg-log-buffer-next-ref)
    (define-key map "s" 'egg-status)
    (define-key map "p" 'egg-log-buffer-prev-ref)
    (define-key map "L" 'egg-reflog)
    (define-key map "/" 'egg-search-changes)
    map))


(defun egg-decorate-log (&optional line-map head-map tag-map remote-map remote-site-map)
  (let ((start (point))
	(head-sha1 (egg-get-current-sha1)) 
	(ov (make-overlay (point-min) (point-min) nil t))
	(dec-ref-alist (egg-full-ref-decorated-alist
			'egg-branch-mono head-map 
			'egg-tag-mono 'egg-an-tag-mono tag-map
			'egg-remote-mono 'egg-branch-mono remote-map
			remote-site-map))
	(ref-string-len 0) 
	(dashes-len 0)
	(min-dashes-len 300)
	separator ref-string refs full-refs sha1
	line-props graph-len beg end sha-beg sha-end subject-beg
	refs-start refs-end ref-alist
	head-line)
    (setq ref-alist (mapcar (lambda (pair)
			      (cons (car pair)
				    (substring-no-properties (cdr pair))))
			    dec-ref-alist))
    (save-excursion
      (while (re-search-forward "\\([0-9a-f]\\{40\\}\\) .+$" nil t)
	(setq sha-beg (match-beginning 1) 
	      sha-end (match-end 1)
	      subject-beg (1+ sha-end)
	      beg (line-beginning-position)
	      end (match-end 0) 
	      refs-start nil)
	(setq graph-len (if (= beg sha-beg) 0 (- sha-beg beg 1))
	      sha1 (buffer-substring-no-properties sha-beg sha-end)
	      subject-beg (if (/= (char-after subject-beg) ?\()
			      subject-beg
			    (setq refs-start (1+ subject-beg))
			    (goto-char subject-beg)
			    (skip-chars-forward "^)")
			    (setq refs-end (point))
			    (+ (point) 2)))
	(setq full-refs (when refs-start
			  (save-match-data
			    (mapcar (lambda (lref)
				      (if (string-equal (substring lref 0 5) "tag: ")
					  (substring lref 5)
					lref))
				    (split-string 
				     (buffer-substring-no-properties (+ sha-end 2)
								     refs-end) 
				     ", +" t)))))
	(setq refs (mapcar (lambda (full-ref-name) 
			     (cdr (assoc full-ref-name ref-alist)))
			   full-refs))

	;; common line decorations
	(setq line-props (list :navigation sha1 :commit sha1))

	(if line-map
	    (setq line-props (nconc (list 'keymap line-map)
				    line-props)))
	(when refs
	  (setq line-props (nconc (list :references refs)
				  line-props)))

	
	(setq separator (apply 'propertize " " line-props))
	(setq ref-string
	      (if full-refs
		  (propertize
		   (mapconcat (lambda (full-ref-name)
				(cdr (assoc full-ref-name 
					    dec-ref-alist)))
			      full-refs separator)
		   :navigation sha1 :commit sha1
		   :references refs)))
	(setq ref-string-len (if ref-string (length ref-string)))

	;; entire line
	(add-text-properties beg (1+ end) line-props)

	;; comment
	(put-text-property subject-beg end 'face 'egg-text-2)
	;; delete refs list (they're already parsed)
	(if refs-start 
	  (delete-region (1- refs-start) (+ refs-end 2)))

	;; shorten sha
 	(delete-region (+ sha-beg 8) sha-end)
	(put-text-property sha-beg (+ sha-beg 8) 
			   'face 'font-lock-constant-face)
	
	(setq dashes-len (- 300 graph-len 1 
			    (if refs (1+ ref-string-len) 0)))
	(setq min-dashes-len (min min-dashes-len dashes-len))

	(put-text-property sha-beg (1+ sha-beg)
			   :dash-refs
			   (apply 'concat 
				  (apply 'propertize 
					 (make-string dashes-len ?-)
					 (nconc (list 'face 'egg-graph)
						line-props))
				  separator
				  (if refs
				      (list ref-string separator))))
;;; 	(when (string= sha1 head-sha1)
;;; 	  (overlay-put ov 'face 'egg-log-HEAD)
;;; 	  (overlay-put ov 'evaporate t)
;;; 	  (move-overlay ov beg (1+ (line-end-position))))
	(when (string= sha1 head-sha1)
	  (setq head-line (point-marker)))
	
	(goto-char (line-end-position)))
      
      (if (= min-dashes-len 300)
	  (insert (egg-text "nothing found!" 'egg-warning))
	

	;; compute how many dashes can be deleted while
	;; leaving at least 1 dash
	(setq min-dashes-len (1- min-dashes-len))

	;; before cut
	;; type a: 300 = graph spc dashes
	;; type b: 300 = graph spc dashes spc ref-string
	;;
	;; after cut
	;; type a: 300 - min-dashes-len = graph spc dashes
	;; type b: 300 - min-dashes-len = graph spc dashes spc ref-string
	;; 
	;; a: comment-column = graph spc dashes spc sha1-8 spc
	;; b: comment-column = graph spc dashes spc ref-string spc sha1-8 spc
	;; need to remove the 1st spc if graph-len = 0
	(set (make-local-variable 'egg-log-buffer-comment-column)
	     (+ (- 300 min-dashes-len (if (> graph-len 0) 0 1)) 1 8 1))

	(when (and (> min-dashes-len 0))
	  (goto-char (1- start))
	  (while (setq start (next-single-property-change (point) 
							  :dash-refs))
	    (goto-char start)
	    (insert (substring (get-text-property start :dash-refs)
			       min-dashes-len))
	    (forward-char 2)))

	(when head-line
	  (goto-char head-line)
	  (overlay-put ov 'face 'egg-log-HEAD)
	  (overlay-put ov 'evaporate t)
	  (move-overlay ov (line-beginning-position)
			(1+ (line-end-position))))
	head-line))))

(defsubst egg-log-buffer-insert-n-decorate-logs (log-insert-func)
  (let ((beg (point)))
    (funcall log-insert-func)
    (goto-char beg)
    (egg-decorate-log egg-log-commit-map 
		      egg-log-local-ref-map
		      egg-log-local-ref-map
		      egg-log-remote-ref-map
		      egg-log-remote-site-map)))


(defun egg-log-diff-cmd-visit-file (file sha1)
  (interactive (list (car (get-text-property (point) :diff))
		     (get-text-property (point) :commit)))
  (pop-to-buffer (egg-file-get-other-version file sha1 nil t)))

(defun egg-log-diff-cmd-visit-file-other-window (file sha1)
  (interactive (list (car (get-text-property (point) :diff))
		     (get-text-property (point) :commit)))
  (pop-to-buffer (egg-file-get-other-version file sha1 nil t) t))

(defun egg-log-hunk-cmd-visit-file (sha1 file hunk-header hunk-beg &rest ignored)
  (interactive (cons (get-text-property (point) :commit)
		     (egg-hunk-info-at (point))))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (pop-to-buffer (egg-file-get-other-version file sha1 nil t))
    (goto-line line)))

(defun egg-log-hunk-cmd-visit-file-other-window (sha1 file hunk-header hunk-beg &rest ignored)
  (interactive (cons (get-text-property (point) :commit)
		     (egg-hunk-info-at (point))))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (pop-to-buffer (egg-file-get-other-version file sha1 nil t) t)
    (goto-line line)))

(defun egg-log-buffer-get-rev-at (pos &rest options)
  (let* ((commit (get-text-property pos :commit))
	 (refs (get-text-property pos :references))
	 (first-head (if (stringp refs) refs (car (last refs))))
	 (ref-at-point (car (get-text-property pos :ref)))
	 (head-sha1 (egg-get-current-sha1)))
    (if (and (not (memq :no-HEAD options)) (string= head-sha1 commit)) 
	"HEAD"
      (or ref-at-point first-head 
	  (if (memq :symbolic options) 
	      (egg-describe-rev commit)
	    commit)))))

(defun egg-log-buffer-do-mark (pos char &optional unmark)
  (let ((commit (get-text-property pos :commit))
	(inhibit-read-only t)
	(col (- egg-log-buffer-comment-column 10))
	(step (if unmark -1 1)))
    (when commit 
      (move-to-column col)
      (funcall (if unmark
		       #'remove-text-properties
		     #'add-text-properties)
		   (point) (1+ (point))
		   (list :mark char
			 'display 
			 (and char 
			      (egg-text (char-to-string char)
					'egg-log-buffer-mark))))
      (forward-line step)
      (while (not (or (get-text-property pos :commit)
		      (eobp) (bobp)))
	(forward-line step))
      (move-to-column col))))

(defun egg-log-buffer-mark-pick (pos)
  (interactive "d")
  (egg-log-buffer-do-mark pos ?+))

(defun egg-log-buffer-mark-squash (pos)
  (interactive "d")
  (egg-log-buffer-do-mark pos ?.))

(defun egg-log-buffer-mark-edit (pos)
  (interactive "d")
  (egg-log-buffer-do-mark pos ?~))

(defun egg-log-buffer-do-unmark-all ()
  (interactive)
  (let ((pos (point-min))
	(inhibit-read-only t))
      (while (setq pos (next-single-property-change (1+ pos) :mark))
	(remove-text-properties pos (1+ pos)
				(list :mark nil 'display nil)))))

(defun egg-log-buffer-unmark (pos &optional all)
  (interactive "d\nP")
  (if all
      (egg-log-buffer-do-unmark-all)
    (egg-log-buffer-do-mark pos nil t)))

(defun egg-log-buffer-get-marked-alist ()
  (let ((pos (point-min))
	marker subject alist)
    (save-excursion
      (while (setq pos (next-single-property-change (1+ pos) :mark))
	(goto-char pos)
	(setq marker (point-marker))
	(move-to-column egg-log-buffer-comment-column)
	(setq subject (buffer-substring-no-properties 
		       (point) (line-end-position)))
	(setq alist (cons (list (get-text-property pos :commit)
				(get-text-property pos :mark)
				subject marker)
			  alist))))
    alist))


(defun egg-setup-rebase-interactive (rebase-dir upstream onto repo-state commit-alist)
  (let ((process-environment process-environment)
	(repo-state (or repo-state (egg-repo-state :staged :unstaged)))
	(orig-buffer (current-buffer))
	orig-head-sha1)
    (setq orig-head-sha1 (plist-get repo-state :sha1))
    (unless (egg-repo-clean repo-state) (error "Repo not clean"))
    (unless onto (setq onto upstream))

    (with-temp-buffer
      (make-directory rebase-dir t)

      (write-region (point-min) (point-min) 
		    (concat rebase-dir "interactive"))
      (insert (plist-get repo-state :head) "\n")
      (write-region (point-min) (point-max) 
		    (concat rebase-dir "head-name"))
      (erase-buffer)
      (insert (plist-get repo-state :sha1) "\n")
      (write-region (point-min) (point-max) 
		    (concat rebase-dir "head"))
      (erase-buffer)
      (insert upstream "\n")
      (write-region (point-min) (point-max) 
		    (concat rebase-dir "upstream"))
      (erase-buffer)
      (insert onto "\n")
      (write-region (point-min) (point-max) 
		    (concat rebase-dir "onto"))
      (erase-buffer)
      (insert "# Rebase " upstream ".." orig-head-sha1 " onto " onto "\n")
      (dolist (rev-info commit-alist)
	(insert (cond ((eq (nth 1 rev-info) ?+) "pick")
		      ((eq (nth 1 rev-info) ?.) "squash")
		      ((eq (nth 1 rev-info) ?~) "edit"))
		" " (nth 0 rev-info) " " (nth 2 rev-info) "\n"))
      (write-region (point-min) (point-max) 
		    (concat rebase-dir "git-rebase-todo"))
      (write-region (point-min) (point-max) 
		    (concat rebase-dir "git-rebase-todo.backup")))

    (setenv "GIT_REFLOG_ACTION" (format "rebase -i (%s)" onto))
    (with-current-buffer (get-buffer-create "*egg-debug*")
      (egg-git-ok nil "update-ref" "ORIG_HEAD" orig-head-sha1)
      (egg-git-ok nil "checkout" onto)
      (egg-do-async-rebase-continue #'egg-handle-rebase-interactive-exit
				    orig-head-sha1))))

(defun egg-handle-rebase-interactive-exit (&optional orig-sha1)
  (let ((exit-msg egg-async-exit-msg)
	(proc egg-async-process)
	state buffer res msg rebase-dir)
    (goto-char (point-min))
    (save-match-data
      (re-search-forward 
       (eval-when-compile 
	 (concat "^\\(?:"
		 "\\(please commit in egg.+$\\)"             "\\|"
		 "\\(Successfully rebased and updated.+$\\)" "\\|"
		 "\\(You can amend the commit now\\)" 	     "\\|"
		 "\\(Automatic cherry-pick failed\\)"	     "\\|"
		 "\\(\\(?:Cannot\\|Could not\\).+\\)" "\\)")) nil t)
      (setq msg (match-string-no-properties 0))
      (setq res (cond ((match-beginning 1) :rebase-commit)
		      ((match-beginning 2) :rebase-done)
		      ((match-beginning 3) :rebase-edit)
		      ((match-beginning 4) :rebase-conflict)
		      ((match-beginning 5) :rebase-fail))))
    (setq buffer (process-get proc :orig-buffer))
    (with-current-buffer buffer
      (egg-run-buffers-update-hook)
      (egg-revert-all-visited-files) ;; too heavy ???
      (setq state (egg-repo-state :force))
      (setq rebase-dir (plist-get state :rebase-dir))
      (cond ((eq res :rebase-done)
	     (message "GIT-REBASE-INTERACTIVE: %s" msg))

	    ((eq res :rebase-commit)
	     (egg-commit-log-edit 
	      (let* ((cherry (plist-get state :rebase-cherry))
		     (cherry-op (save-match-data 
				  (car (split-string cherry)))))
		(concat 
		 (egg-text "Rebasing " 'egg-text-3)
		 (egg-text (plist-get state :rebase-head) 'egg-branch)
		 ": "
		 (egg-text (concat "Commit " cherry-op "ed cherry")
			   'egg-text-3))) 
	      `(lambda ()
		 (let ((process-environment process-environment))
		   (mapcar (lambda (env-lst)
			     (setenv (car env-lst) (cadr env-lst)))
			   (egg-rebase-author-info ,rebase-dir))
		   (egg-log-msg-commit))
		 (with-current-buffer ,buffer
		   (egg-do-async-rebase-continue
		    #'egg-handle-rebase-interactive-exit
		    ,orig-sha1)))
	      (egg-file-as-string (concat rebase-dir "message"))))


	    ((eq res :rebase-edit)
	     (egg-commit-log-edit 
	      (concat (egg-text "Rebasing " 'egg-text-3)
		      (egg-text (plist-get state :rebase-head) 
				'egg-branch) ": "
				(egg-text "Re-edit cherry's commit log" 
					  'egg-text-3))
	      `(lambda ()
		 (let ((process-environment process-environment))
		   (mapcar (lambda (env-lst)
			     (setenv (car env-lst) (cadr env-lst)))
			   (egg-rebase-author-info ,rebase-dir))
		   (egg-log-msg-amend-commit))
		 (with-current-buffer ,buffer
		   (egg-do-async-rebase-continue
		    #'egg-handle-rebase-interactive-exit
		    ,orig-sha1)))
	      (egg-commit-message "HEAD")))

	    ((eq res :rebase-conflict)
	     (egg-status nil :sentinel)
	     (ding)
	     (message "automatic rebase stopped! please resolve conflict(s)"))
	    ((eq res :rebase-fail)
	     (egg-status nil :sentinel)
	     (ding)
	     (message "Automatic rebase failed!"))))))

(defun egg-log-buffer-merge (pos &optional no-commit)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos :symbolic :no-HEAD))
	res modified-files buf)
    (unless (egg-repo-clean)
      (egg-status) 
      (error "Repo is not clean!"))
    (if  (null (y-or-n-p (format "merge %s to HEAD? " rev)))
	(message "cancel merge from %s to HEAD!" rev)
      (setq res (egg-do-merge-to-head rev no-commit))
      (setq modified-files (plist-get res :files))
      (if modified-files
	  (egg-revert-visited-files modified-files)) 
      (message "GIT-MERGE> %s" (plist-get res :message))
      (unless (and (plist-get res :success) (null no-commit))
	(egg-status)))))

(defun egg-log-buffer-rebase (pos &optional move)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos :symbolic :no-HEAD))
	res modified-files buf)
    (if  (null (y-or-n-p (format "rebase HEAD to %s? " rev)))
	(message "cancel rebase HEAD to %s!" rev)
      (unless (egg-buffer-do-rebase 
	       rev (if move t)
	       (if move
		   (format "starting point to rebase HEAD onto %s: "
			   rev)))
	(egg-status)))))

(defun egg-log-buffer-rebase-interactive (pos &optional move)
  (interactive "d\nP")
  (let* ((state (egg-repo-state :staged :unstaged))
	 (rebase-dir (concat (plist-get state :gitdir) "/"
			     egg-git-rebase-subdir "/"))
	 (todo-alist (egg-log-buffer-get-marked-alist))
	 (commits (mapcar 'car todo-alist))
	 (upstream (egg-log-buffer-get-rev-at pos :symbolic :no-HEAD))
	 (all (egg-git-to-lines "rev-list" "--reverse" "--cherry-pick"
				(concat upstream "..HEAD"))))
    (unless (egg-repo-clean state)
      (error "repo %s is not clean" (plist-get state :gitdir)))
    (mapc (lambda (commit)
	    (unless (member commit all)
	      (error "commit %s is not between HEAD and upstream %s"
		     commit upstream)))
	  commits)
    
    (egg-setup-rebase-interactive rebase-dir upstream nil
				  state todo-alist)
    (egg-status)))

(defun egg-log-buffer-rewrite ()
  (interactive)
  (let* ((state (egg-repo-state :staged :unstaged))
	 (rebase-dir (concat (plist-get state :gitdir) "/"
			     egg-git-rebase-subdir "/"))
	 (todo-alist (egg-log-buffer-get-marked-alist))
	 (commits (mapcar 'car todo-alist))
	 (oldest (car commits))
	 (upstream (egg-sha1 (concat oldest "^")))
	 (all (egg-git-to-lines "rev-list" "--reverse" "--cherry-pick"
				(concat upstream "..HEAD"))))
    (unless (egg-repo-clean state)
      (error "repo %s is not clean" (plist-get state :gitdir)))
    (mapc (lambda (commit)
	    (unless (member commit all)
	      (error "commit %s is not between HEAD and upstream %s"
		     commit upstream)))
	  commits)
    
    (egg-setup-rebase-interactive rebase-dir upstream nil
				  state todo-alist)
    (egg-status)))

(defun egg-log-buffer-checkout-commit (pos)
  (interactive "d")
  (egg-do-checkout 
   (completing-read "checkout: " (egg-all-refs) nil nil 
		    (egg-log-buffer-get-rev-at pos :symbolic :no-HEAD))))

(defun egg-log-buffer-tag-commit (pos &optional force)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos)))
    (when (egg-do-tag rev (format "tag %s with name: " rev) force)
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-atag-commit (pos &optional force)
  (interactive "d\nP")
  (let ((commit (get-text-property pos :commit)))
    (egg-create-annotated-tag 
     (read-string (format "create annotated tag on %s with name: "
			  (egg-describe-rev commit)))
     commit)))

(defun egg-log-buffer-create-new-branch (pos &optional force)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos)))
    (when (egg-do-create-branch
	   rev nil
	   (format "create new branch at %s with name: " rev) 
	   force)
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-start-new-branch (pos &optional force)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos)))
    (when (egg-do-create-branch
	   rev 'checkout
	   (format "start new branch at %s with name: " rev)
	   force))))

(defun egg-log-buffer-attach-head (pos &optional strict-level)
  (interactive "d\np")
  (let* ((rev (egg-log-buffer-get-rev-at pos :symbolic :no-HEAD))
	 (branch (egg-current-branch))
	 (update-index (> strict-level 3))
	 (update-wdir (> strict-level 15))
	 (prompt (format "%s to %s%s? " 
			 (if branch 
			     (concat "move " branch)
			   "attach HEAD")
			 rev
			 (cond (update-wdir " (and update workdir)")
			       (update-index " (and update index)")
			       (t "")))))
    (if (y-or-n-p prompt)
	(egg-do-move-head rev update-wdir update-index))))


(defun egg-log-buffer-rm-ref (pos &optional force)
  (interactive "d\nP")
  (let ((refs (get-text-property pos :references))
	(ref-at-point (car (get-text-property pos :ref)))
	victim)
    (unless ref-at-point
      (setq ref-at-point (last refs)))
    (setq victim (completing-read "remove reference: " refs
				  nil nil ref-at-point))
    (when (egg-rm-ref force victim)
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-fetch-remote-ref (pos)
  (interactive "d")
  (let* ((ref-at-point (get-text-property pos :ref))
	 (ref (car ref-at-point))
	 (type (cdr ref-at-point))
	 name remote)
    (unless (eq type :remote)
      (error "Nothing to fetch from here!"))
    (setq name (file-name-nondirectory ref)
	  remote (egg-rbranch-to-remote ref))
    (when (and remote name)
      (message "GIT> fetching %s from %s..." ref remote)
      (egg-buffer-async-do nil "fetch" remote 
			   (format "refs/heads/%s:refs/remotes/%s"
				   name ref)))))

(defun egg-log-buffer-fetch (pos)
  (interactive "d")
  (let* ((ref-at-point (get-text-property pos :ref))
	 (ref (car ref-at-point))
	 (type (cdr ref-at-point))
	 site name def remote)
    (unless (eq type :remote)
      (error "No site here to fetch from!"))
    (setq remote (egg-rbranch-to-remote ref))
    (when remote
      (setq name (read-string (format "fetch from %s (default all): "
				      remote) nil nil "--all"))
      (if (equal name "--all")
	  (progn
	    (message "GIT> fetching everything from %s..." remote)
	    (egg-buffer-async-do nil "fetch" remote))
	(message "GIT> fetching %s from %s..." name remote)
	(egg-buffer-async-do nil "fetch" remote 
			     (format "refs/heads/%s:refs/remotes/%s/%s"
				     name remote name))))))

(defun egg-log-buffer-push-to-local (pos &optional non-ff)
  (interactive "d\nP")
  (let* ((src (car (get-text-property pos :ref)))
	 dst)
    (unless src
      (error "Nothing to push here!"))
    (setq dst (completing-read (format "use %s to update: " src) 
			       (egg-local-refs) nil t))
    (when (egg-show-git-output
	   (egg-sync-0 "push" "." (if non-ff "-vf" "-v")
		       (concat src ":" dst))
	   -1 "GIT-PUSH")
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-push-head-to-local (pos &optional non-ff)
  (interactive "d\nP")
  (let* ((dst (car (get-text-property pos :ref))))
    (unless dst
      (error "Nothing here to push to!"))
    (if (y-or-n-p (format "update %s with HEAD? " dst))
    (when (egg-show-git-output
	   (egg-sync-0 "push" "." (if non-ff "-vf" "-v")
		       (concat "HEAD:" dst))
	   -1 "GIT-PUSH")
      (funcall egg-buffer-refresh-func (current-buffer))))))

(defun egg-log-buffer-push-to-remote (pos &optional non-ff)
  (interactive "d\nP")
  (let* ((ref-at-point (get-text-property pos :ref))
	 (lref (car ref-at-point))
	 (type (cdr ref-at-point))
	 rref tracking remote spec)
    (unless ref-at-point
      (error "Nothing to push here!"))
    (cond ((eq type :remote)		;; delete a remote head
	   (setq rref (file-name-nondirectory lref))
	   (setq remote (directory-file-name
			 (file-name-directory lref)))
	   (setq lref ""))
	  ((eq type :head)
	   (setq tracking (egg-tracking-target lref :remote))
	   (if (consp tracking)
	       (setq rref (car tracking) remote (cdr tracking))
	     (setq remote (egg-read-remote
			   (format "push branch %s to remote: " lref)))
	     (setq rref (read-string 
			 (format "push branch %s to %s as: " lref remote)
				     lref))))
	  ((eq type :tag)
	   (setq remote (egg-read-remote "push to remote: "))
	   (setq rref (read-string 
		       (format "remote tag to push at (on %s): " remote)
		       lref))
	   (setq lref (read-string 
		       (format "local tag to push at %s/%s (empty mean delete the remote tag) : "
			       remote rref)
		       rref))))
    (unless (> (length lref) 0)
      (unless (y-or-n-p (format "delete %s/%s? " remote rref))
	(message "cancel removal of %s/%s" remote rref)
	(setq remote nil rref nil lref nil)))
    (when (and remote rref lref)
      (setq spec (concat lref ":" rref))
      (message "GIT> pushing %s to %s on %s..." lref rref remote)
      (egg-buffer-async-do nil "push" (if non-ff "-vf" "-v") 
			   remote spec))))

(defun egg-log-buffer-push (pos)
  (interactive "d")
  (let* ((ref-at-point (get-text-property pos :ref))
	 (ref (car ref-at-point))
	 (type (cdr ref-at-point))
	 site name def remote)
    (unless (eq type :remote)
      (error "No site here to push to!"))
    (setq remote (egg-rbranch-to-remote ref))
    (when remote
      (setq name 
	    (completing-read (format "push to %s (default all heads): "
				     remote)
			     (egg-local-refs) nil nil nil nil "--all"))
      (message "GIT> pushing %s to %s..."
	       (if (equal name "--all") "everything" name) remote) 
      (egg-buffer-async-do nil "push" remote name))))

(defun egg-log-buffer-goto-pos (pos)
  (goto-char pos)
  (goto-char (line-beginning-position))
  (let ((sha1 (get-text-property (point) :commit)))
    (when (stringp sha1)
      (setq sha1 (substring sha1 0 6))
      (save-match-data
	(if (looking-at (concat "^.* \\(" sha1 "\\)"))
	    (goto-char (match-beginning 1)))))))

(defun egg-log-buffer-next-ref (pos)
  (interactive "d")
  (let ((current-ref (get-text-property pos :references))
	(n-pos (next-single-property-change pos :references))
	n-ref)
    (when n-pos
      (setq n-ref (get-text-property n-pos :references))
      (if n-ref
	  (egg-log-buffer-goto-pos n-pos)
	(if (setq n-pos (next-single-property-change n-pos :references))
	    (egg-log-buffer-goto-pos n-pos))))))

(defun egg-log-buffer-prev-ref (pos)
  (interactive "d")
  (let ((current-ref (get-text-property pos :references))
	(p-pos (previous-single-property-change pos :references))
	p-ref)
    (when p-pos
      (setq p-pos (1- p-pos))
      (setq p-ref (get-text-property p-pos :references))
      (if (and p-ref (not (equal p-ref current-ref)))
	  (egg-log-buffer-goto-pos p-pos)
	(when (setq p-pos (previous-single-property-change p-pos :references))
	  (setq p-pos (1- p-pos))
	  (egg-log-buffer-goto-pos p-pos))))))

(defun egg-log-buffer-do-insert-commit (pos)
  (save-excursion
    (let ((sha1 (get-text-property pos :commit))
	  (ref (get-text-property pos :references))
	  (nav (get-text-property pos :navigation))
	  (inhibit-read-only t)
	  (indent-column egg-log-buffer-comment-column)
	  (indent-spaces (make-string egg-log-buffer-comment-column ? ))
	  beg end)
      (goto-char pos)
      (goto-char (1+ (line-end-position)))
      (setq beg (point))
      (unless (egg-git-ok t "log" "--max-count=1" "-p"
			  (concat
			     "--pretty=format:"
			     indent-spaces "%ai%n"
			     indent-spaces "%an%n%n"
			     "%b%n" 
			     ) 
			  sha1)
	(error "error calling git log %s!" ref))
      (setq end (point))
      (egg-delimit-section :commit sha1 beg end (1- beg) nil nav)
      (put-text-property beg end 'keymap egg-section-map)
      (egg-decorate-diff-section :begin beg
				 :end end
				 :diff-map egg-log-diff-map
				 :hunk-map egg-log-hunk-map)
      (goto-char beg)
      (setq end (next-single-property-change beg :diff))
      (put-text-property beg (+ indent-column beg) 'face 'egg-diff-none)
      (put-text-property (+  indent-column beg) (line-end-position)
			 'face 'egg-text-2)
      (forward-line 1)
      (put-text-property (point) (+ indent-column (point)) 'face 'egg-diff-none)
      (put-text-property (+ indent-column (point)) end 'face 'egg-text-2)
      (set-buffer-modified-p nil))))

(defun egg-log-buffer-insert-commit (pos)
  (interactive "d")
  (let* ((next (next-single-property-change pos :diff))
	 (sha1 (and next (get-text-property next :commit))))
    (unless (equal (get-text-property pos :commit) sha1)
      (egg-log-buffer-do-insert-commit pos))))

(defun egg-generic-display-logs (data &optional init)
  (buffer-disable-undo)
  (and init (setq buffer-invisibility-spec nil))
  (let ((title (plist-get data :title))
	(subtitle (plist-get data :subtitle))
	(git-dir (egg-git-dir))
	(desc (plist-get egg-internal-log-buffer-closure :description))
	(closure (plist-get egg-internal-log-buffer-closure :closure))
	(help (plist-get egg-internal-log-buffer-closure :help))
	(inhibit-read-only t)
	inv-beg beg help-beg)
      (erase-buffer)
      (insert title
	      (if subtitle (concat "\n" subtitle "\n") "\n")
	      (egg-text "repo: " 'egg-text-2)
	      (egg-text (egg-git-dir) 'font-lock-constant-face)
	      (if desc (concat "\n" desc "\n") "\n")
	      "\n")
      (setq inv-beg (- (point) 2))
      (when (stringp help) 
	(setq help-beg (point))
	(insert (egg-text "Help" 'egg-help-header-1) "\n")
	(setq inv-beg (1- (point)))
	(insert help "\n"))
      (setq beg (point))
      (when help-beg
	(egg-delimit-section :section :help help-beg (point)
			     inv-beg egg-section-map :help))
      (if init (egg-buffer-maybe-hide-help :help))
      (goto-char (or (funcall closure) beg))))

(defun egg-log-buffer-redisplay (buffer &optional init)
  (with-current-buffer buffer
    (let* ((state (egg-repo-state))
	   (sha1 (plist-get state :sha1)))
      (plist-put egg-internal-log-buffer-closure :title
		 (egg-text (egg-pretty-head-string state) 'egg-branch))
      (plist-put egg-internal-log-buffer-closure :subtitle
		 (egg-text sha1 'font-lock-string-face))
      (egg-generic-display-logs egg-internal-log-buffer-closure init))))

(defun egg-log-buffer-redisplay-from-command (buffer)
  ;; in process buffer
  (when (and (processp egg-async-process)
	     (equal (current-buffer) (process-buffer egg-async-process)))
    (or 
     (save-excursion
       (let ((cmd "GIT>") cmd-sexp cmd-string pos done)
	 (goto-char (point-min))
	 (skip-chars-forward "^(")
	 (setq cmd-sexp (read (current-buffer)))
	 (when (consp cmd-sexp)
	   (setq cmd-string (car cmd-sexp)
		 cmd (cond ((string-equal "fetch" cmd-string) "GIT-FETCH>")
			   ((string-equal "push" cmd-string) "GIT-PUSH>"))))
	 (goto-char (point-min))
	 (save-match-data
	   (cond ((re-search-forward "^\\(?:From\\|To\\) \\(.+\\)\n\\(.+\\)$" nil t)
		  (message "%s %s: %s" cmd
			   (match-string-no-properties 1)
			   (match-string-no-properties 2))
		  (setq done t))))
	 (unless done
	   (goto-char (point-min))
	   (save-match-data
	     (cond ((re-search-forward "^fatal: \\(.+\\)$" nil t)
		    (message "%s fatal: %s" cmd 
			     (match-string-no-properties 1))
		    (setq done t)))))))))
  ;; update log buffer
  (egg-log-buffer-redisplay buffer))

(define-egg-buffer log "*%s-log@%s*"
  "Major mode to display the output of git log.\\<egg-log-buffer-mode-map>
Each line with a shorten sha1 representing a commit in the repo's history.
\\[egg-log-buffer-next-ref] move the cursor to the next commit with a ref
\\[egg-log-buffer-prev-ref] move the cursor to the previous commit line with a ref.
\\[egg-buffer-cmd-refresh] refresh the display of the log buffer
\\[egg-status] shows the repo's current status.

\\{egg-log-buffer-mode-map}

Each line representing a commit has extra keybindings:\\<egg-log-commit-map>
\\[egg-log-buffer-insert-commit] fetch and show the commit's details.
\\[egg-section-cmd-toggle-hide-show] hide/show the current commit's details
\\[egg-section-cmd-toggle-hide-show-children] hide all the sub-blocks of the current commit's details.
\\[egg-log-buffer-create-new-branch] create a new branch starting from the current commit.
\\[egg-log-buffer-start-new-branch] start in a new branch from the current commit.
\\[egg-log-buffer-checkout-commit] checkout the current commit.
\\[egg-log-buffer-tag-commit] create a new lightweight tag pointing at the current commit.
\\[egg-log-buffer-attach-head] move HEAD (and maybe the current branch tip) to the 
current commit (the underlying git command is `reset --soft'.
C-u \\[egg-log-buffer-attach-head] move HEAD (and maybe the current branch tip) as well as
the index to the current commit (the underlying git command
is `reset --mixed'.)
C-u C-u \\[egg-log-buffer-attach-head] move HEAD (and maybe the current branch tip) and
the index to the current commit, the work dir will also be
updated (the underlying git command is `reset --hard').
\\[egg-log-buffer-merge] will merge the current commit into HEAD.
C-u \\[egg-log-buffer-merge] will merge the current commit into HEAD but will not
auto-commit if the merge was successful.

\\{egg-log-commit-map}

Each ref on the commit line has extra extra keybindings:\\<egg-log-ref-map>
\\[egg-log-buffer-rm-ref] delete the ref under the cursor.
\\[egg-log-buffer-push-to-local] update another local ref using the ref under the cursor.

Each local ref on the commit line has extra extra extra keybindings:\\<egg-log-local-ref-map>
\\[egg-log-buffer-push-to-remote] upload to a remote the ref under the cursor.
  for a remote-tracking local branch this would updating the tracking target.
  for other local refs this  means uploading (or deleting) the local value
   of the ref to the remote repository.
\\[egg-log-buffer-push-head-to-local] update the local ref under the cursor with the current HEAD.

Each remote ref on the commit line has extra extra extra keybindings:\\<egg-log-remote-ref-map>
\\[egg-log-buffer-fetch-remote-ref] download the new value of the ref from the remote repo.
."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-log-buffer-mode
	mode-name  "Egg-Log"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-log-buffer-mode-map)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-log-buffer-redisplay)
  (set (make-local-variable 'egg-buffer-async-cmd-refresh-func)
       'egg-log-buffer-redisplay-from-command)
  (set (make-local-variable 'egg-log-buffer-comment-column) 0)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-log-buffer-mode-hook))

(defconst egg-log-buffer-help-text
  (concat
   (egg-text "Common Key Bindings:" 'egg-help-header-2)
   "\n"
   (egg-pretty-help-text
    "\\<egg-log-buffer-mode-map>"
    "\\[egg-log-buffer-next-ref]:next thing  "
    "\\[egg-log-buffer-prev-ref]:previous thing  " 
    "\\[egg-search-changes]:search history  " 
    "\\[egg-status]:show repo's status  "
    "\\[egg-buffer-cmd-refresh]:redisplay  " 
    "\\[quit-window]:quit\n")
   (egg-text "Extra Key Bindings for a Commit line:" 'egg-help-header-2)
   "\n"
   (egg-pretty-help-text
    "\\<egg-log-commit-map>"
    "\\[egg-log-buffer-insert-commit]:load details  " 
    "\\[egg-section-cmd-toggle-hide-show]:hide/show details "
    "\\[egg-section-cmd-toggle-hide-show-children]:hide sub-blocks  "
    "\\[egg-log-buffer-checkout-commit]:checkout  " 
    "\\[egg-log-buffer-start-new-branch]:start new branch\n" 
    "\\[egg-log-buffer-attach-head]:anchor HEAD  " 
    "\\[egg-log-buffer-tag-commit]:new tag  " 
    "\\[egg-log-buffer-atag-commit]:new annotated tag  " 
    "\\[egg-log-buffer-merge]:merge to HEAD  " 
    "\\[egg-log-buffer-rebase]:rebase HEAD  " 
    "\\[egg-log-buffer-create-new-branch]:create branch\n")
   (egg-text "Extra Key Bindings to prepare a (interactive) rebase:" 'egg-help-header-2)
   "\n"
   (egg-pretty-help-text
    "\\<egg-log-commit-map>"
    "\\[egg-log-buffer-mark-pick]:mark as picked  " 
    "\\[egg-log-buffer-mark-squash]:mark as squashed  " 
    "\\[egg-log-buffer-mark-edit]:mark as edited  " 
    "\\[egg-log-buffer-unmark]:unmark\n")
   (egg-text "Extra Extra Key Bindings for a Ref:" 'egg-help-header-2) 
   "\n"
   (egg-pretty-help-text
    "\\<egg-log-local-ref-map>"
    "\\[egg-log-buffer-rm-ref]:delete ref  "
    "\\[egg-log-buffer-push-to-local]:update another local ref  "
    "\\[egg-log-buffer-push-to-remote]:upload to remote  "
    "\\[egg-log-buffer-push-head-to-local]:download\n")
   (egg-text "Extra Key Bindings for a Diff Block:" 'egg-help-header-2)
   "\n"
   (egg-pretty-help-text
    "\\<egg-log-diff-map>"
    "\\[egg-log-diff-cmd-visit-file-other-window]:visit version/line\n")
   (egg-text "References:" 'egg-help-header-2) "\n"
   (egg-text "local-branch" 'egg-branch-mono) " "
   (egg-text "lightweight-tag" 'egg-tag-mono) " "
   (egg-text "annotated-tag" 'egg-an-tag-mono) " "
   (egg-text "remote/" 'egg-remote-mono)
   (egg-text "branch" 'egg-branch-mono)
   "\n"))

(defun egg-log (&optional all)
  (interactive "P")
  (let* ((egg-internal-current-state 
	  (egg-repo-state (if (interactive-p) :error-if-not-git)))
	 (git-dir (egg-git-dir (interactive-p)))
	 (default-directory (file-name-directory git-dir))
	 (buf (egg-get-log-buffer 'create))
	 help)
    (with-current-buffer buf
      (when (memq :log egg-show-key-help-in-buffers)
	(setq help egg-log-buffer-help-text))
      (set 
       (make-local-variable 'egg-internal-log-buffer-closure)
       (if all
	   (list :description (concat 
			       (egg-text "history scope: " 'egg-text-2)
			       (egg-text "all refs" 'egg-term))
		 :closure (lambda ()
			    (egg-log-buffer-insert-n-decorate-logs
			     'egg-run-git-log-all)))
	 (list :description (concat 
			     (egg-text "history scope: " 'egg-text-2)
			     (egg-text "HEAD" 'egg-term))
	       :closure (lambda ()
			  (egg-log-buffer-insert-n-decorate-logs
			   'egg-run-git-log-HEAD)))))
      (if help (plist-put egg-internal-log-buffer-closure :help help))
      (egg-log-buffer-redisplay buf 'init))
    (pop-to-buffer buf t)))
;;;========================================================
;;; file history
;;;========================================================
(define-egg-buffer file-log "*%s-file-log@%s*"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-file-log-buffer-mode
	mode-name  "Egg-FileHistory"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-log-buffer-mode-map)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-log-buffer-simple-redisplay)
  (set (make-local-variable 'egg-log-buffer-comment-column) 0)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-file-log-buffer-mode-hook))

(defsubst egg-run-git-file-log-HEAD (file)
  (egg-git-ok t "log" (format "--max-count=%d" egg-log-HEAD-max-len) 
	      "--graph" "--topo-order"
		"--pretty=oneline" "--decorate" "--" file))

(defsubst egg-run-git-file-log-all (file)
  (egg-git-ok t "log" (format "--max-count=%d" egg-log-all-max-len)
	      "--graph" "--topo-order"
		"--pretty=oneline" "--decorate" "--all" "--" file))

(defconst egg-log-commit-simple-map 
  (let ((map (make-sparse-keymap "Egg:FileLogCommit")))
    (set-keymap-parent map egg-log-commit-base-map)
    (define-key map (kbd "RET") 'egg-log-locate-commit)
    (define-key map (kbd "C-c C-c") 'egg-log-locate-commit)
    map))

(defsubst egg-log-buffer-decorate-logs-simple (log-insert-func arg)
  (let ((beg (point)))
    (funcall log-insert-func arg)
    (unless (= (char-before (point-max)) ?\n)
      (goto-char (point-max))
      (insert ?\n))
    (goto-char beg)
    (egg-decorate-log egg-log-commit-simple-map
		      egg-log-commit-simple-map
		      egg-log-commit-simple-map
		      egg-log-commit-simple-map)))

(defun egg-log-buffer-simple-redisplay (buffer &optional init)
  (with-current-buffer buffer
    (egg-generic-display-logs egg-internal-log-buffer-closure init)))

(defconst egg-file-log-help-text
  (concat
   (egg-text "Common Key Bindings:" 'egg-help-header-2) "\n"
   (egg-pretty-help-text
    "\\<egg-log-buffer-mode-map>"
    "\\[egg-log-buffer-next-ref]:next thing  "
    "\\[egg-log-buffer-prev-ref]:previous thing  " 
    "\\[egg-status]:show repo's status  "
    "\\[egg-buffer-cmd-refresh]:redisplay  " 
    "\\[quit-window]:quit\n" )
   (egg-text "Extra Key Bindings for a Commit line:" 'egg-help-header-2) "\n"
   (egg-pretty-help-text
    "\\<egg-log-commit-simple-map>"
    "\\[egg-log-locate-commit]:locate commit in history  " 
    "\\[egg-log-buffer-insert-commit]:load details  " 
    "\\[egg-section-cmd-toggle-hide-show]:hide/show details  "
    "\\[egg-section-cmd-toggle-hide-show-children]:hide sub-blocks\n"
    "\\[egg-log-buffer-attach-head]:anchor HEAD  " 
    "\\[egg-log-buffer-checkout-commit]:checkout  " 
    "\\[egg-log-buffer-start-new-branch]:start new branch  " 
    "\\[egg-log-buffer-create-new-branch]:create branch\n" 
    "\\[egg-log-buffer-tag-commit]:new tag  " 
    "\\[egg-log-buffer-atag-commit]:new annotated tag  " 
    "\\[egg-log-buffer-merge]:merge to HEAD  " 
    "\\[egg-log-buffer-rebase]:rebase HEAD\n" 
    )
   (egg-text "Extra Key Bindings for a Diff Block:" 'egg-help-header-2) "\n"
   (egg-pretty-help-text
    "\\<egg-log-diff-map>"
    "\\[egg-log-diff-cmd-visit-file-other-window]:visit version/line\n")
   ))

(defun egg-file-log (file-name &optional all)
  (interactive (list (buffer-file-name) current-prefix-arg))
  (unless (and file-name (file-exists-p file-name))
    (error "File does not exist: %s" file-name))
  (let ((buffer (egg-get-file-log-buffer 'create))
	(title (concat (egg-text "history of " 'egg-text-2)
		       (egg-text file-name 'egg-term)))
	help)
    (with-current-buffer buffer
      (set 
       (make-local-variable 'egg-internal-log-buffer-closure)
       (if all
	   (list :title title
		 :description (concat (egg-text "scope: " 'egg-text-2)
				      (egg-text "all refs" 'egg-branch-mono)) 
		 :closure `(lambda () 
			     (egg-log-buffer-decorate-logs-simple
			      #'egg-run-git-file-log-all ,file-name)))
	 (list :title title
	       :description (concat (egg-text "scope: " 'egg-text-2)
				    (egg-text "HEAD" 'egg-branch-mono))
	       :closure `(lambda ()
			   (egg-log-buffer-decorate-logs-simple
			    #'egg-run-git-file-log-HEAD ,file-name)))))
      (when (memq :file-log egg-show-key-help-in-buffers)
	(setq help egg-file-log-help-text))
      (if help (plist-put egg-internal-log-buffer-closure :help help)))
    (egg-log-buffer-simple-redisplay buffer 'init)
    (pop-to-buffer buffer t)))

;;;========================================================
;;; commit search
;;;========================================================
(defconst egg-query:commit-commit-map 
  (let ((map (make-sparse-keymap "Egg:LogQueryCommit")))
    (set-keymap-parent map egg-hide-show-map)
    (define-key map (kbd "SPC") 'egg-log-buffer-insert-commit)
    (define-key map (kbd "o") 'egg-log-buffer-checkout-commit)
    (define-key map (kbd "a") 'egg-log-buffer-attach-head)
    (define-key map (kbd "RET") 'egg-log-locate-commit)
    (define-key map (kbd "C-c C-c") 'egg-log-locate-commit)
    map))

(defun egg-log-locate-commit (pos)
  (interactive "d")
  (let ((sha1 (get-text-property pos :commit))
	(buf (egg-get-log-buffer 'create)))
    (with-current-buffer buf
      (set (make-local-variable 'egg-internal-log-buffer-closure)
	   (list :description 
		 (concat (egg-text "history scope: " 'egg-text-2)
			 (egg-text "HEAD" 'egg-term)
			 (egg-text " and " 'egg-text-2)
			 (egg-text sha1 'egg-term))
		 :closure 
		 (lambda ()
		   (egg-log-buffer-insert-n-decorate-logs
		    `(lambda ()
		       (egg-git-ok t "log" "--max-count=10000" "--graph"
				   "--topo-order" "--pretty=oneline"
				   "--decorate" "HEAD" sha1)))))) 
      (egg-log-buffer-redisplay buf)
      (setq pos (point-min))
      (while (and pos
		  (not (equal (get-text-property pos :commit) sha1)))
	(setq pos (next-single-property-change pos :commit))))
    (pop-to-buffer buf t)
    (egg-log-buffer-goto-pos pos)
    (recenter)))

(defun egg-query:commit-buffer-rerun (buffer &optional init)
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (plist-put egg-internal-log-buffer-closure :title
	       (egg-text "History Search" 'egg-branch))
    (egg-generic-display-logs egg-internal-log-buffer-closure init)))

(define-egg-buffer query:commit "*%s-query:commit@%s*"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-query:commit-buffer-mode
	mode-name  "Egg-Query:Commit"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-buffer-mode-map)
  (set (make-local-variable 'egg-internal-log-buffer-closure) nil)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-query:commit-buffer-rerun)
  ;; re use log-buffer redrawing
  (set (make-local-variable 'egg-log-buffer-comment-column) 0)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-query:commit-buffer-mode-hook))

(defun egg-insert-n-decorate-pickaxed-logs (string)
  (let ((beg (point)))
    (egg-git-ok t "log" "--pretty=oneline" "--decorate" 
		(concat "-S" string))
    (goto-char beg)
    (egg-decorate-log egg-query:commit-commit-map
		      egg-query:commit-commit-map
		      egg-query:commit-commit-map
		      egg-query:commit-commit-map)))

(defun egg-search-changes (string)
  (interactive "ssearch history for changes containing: ")
  (let* ((git-dir (egg-git-dir (interactive-p)))
	 (default-directory (file-name-directory git-dir))
	 (buf (egg-get-query:commit-buffer 'create))
	 (desc (concat (egg-text "Commits containing: " 'egg-text-2)
		       (egg-text string 'egg-term)))
	 (func `(lambda ()
		  (egg-insert-n-decorate-pickaxed-logs ,string))))
    (with-current-buffer buf
      (set (make-local-variable 'egg-internal-log-buffer-closure)
	 (list :description desc :closure func))
      (egg-query:commit-buffer-rerun buf 'init))
    (pop-to-buffer buf t)))
;;;========================================================
;;; reflog
;;;========================================================
(defsubst egg-run-reflog-branch (branch)
  (egg-git-ok t "log" "-g" "--pretty=oneline" "--decorate"
	      (format "--max-count=%d" egg-log-HEAD-max-len) 
	      branch))

(define-egg-buffer reflog "*%s-reflog@%s*"
  (egg-file-log-buffer-mode)
  (setq major-mode 'egg-reflog-buffer-mode
	mode-name  "Egg-RefLog")
  (run-mode-hooks 'egg-reflog-buffer-mode-hook))

(defconst egg-reflog-help-text
  (concat
   (egg-text "Common Key Bindings:" 'egg-help-header-2) "\n"
   (egg-pretty-help-text
    "\\<egg-log-buffer-mode-map>"
    "\\[egg-log-buffer-next-ref]:next thing  "
    "\\[egg-log-buffer-prev-ref]:previous thing  " 
    "\\[egg-status]:show repo's status  "
    "\\[egg-buffer-cmd-refresh]:redisplay  " 
    "\\[quit-window]:quit\n" )
   (egg-text "Extra Key Bindings for a Commit line:" 'egg-help-header-2) "\n"
   (egg-pretty-help-text
    "\\<egg-log-commit-simple-map>"
    "\\[egg-log-locate-commit]:locate commit in history  " 
    "\\[egg-log-buffer-insert-commit]:load details  " 
    "\\[egg-section-cmd-toggle-hide-show]:hide/show details  "
    "\\[egg-section-cmd-toggle-hide-show-children]:hide sub-blocks\n"
    "\\[egg-log-buffer-attach-head]:anchor HEAD  " 
    "\\[egg-log-buffer-checkout-commit]:checkout  " 
    "\\[egg-log-buffer-start-new-branch]:start new branch  " 
    "\\[egg-log-buffer-create-new-branch]:create branch\n" 
    "\\[egg-log-buffer-tag-commit]:new tag  " 
    "\\[egg-log-buffer-atag-commit]:new annotated tag  " 
    "\\[egg-log-buffer-merge]:merge to HEAD  " 
    "\\[egg-log-buffer-rebase]:rebase HEAD\n" 
    )
   (egg-text "Extra Key Bindings for a Diff Block:" egg-help-header-2) "\n"
   (egg-pretty-help-text
    "\\<egg-log-diff-map>"
    "\\[egg-log-diff-cmd-visit-file-other-window]:visit version/line\n")
   "\n"
   ))

(defun egg-reflog (branch)
  (interactive (list (if current-prefix-arg
			 (egg-read-rev "show history of ref: " "HEAD")
		       "HEAD")))
  (unless branch (setq branch "HEAD"))
  (let ((egg-internal-current-state (egg-repo-state :error-if-not-git))
	(buffer (egg-get-reflog-buffer 'create))
	(title (concat (egg-text "history of " 'egg-text-2)
		       (egg-text branch 'egg-branch)))
	help)
    (with-current-buffer buffer
      (set 
       (make-local-variable 'egg-internal-log-buffer-closure)
       (list :title title
	     :closure `(lambda ()
			 (egg-log-buffer-decorate-logs-simple
			  #'egg-run-reflog-branch ,branch))))
      (when (memq :reflog egg-show-key-help-in-buffers)
	(setq help egg-reflog-help-text))
      (if help (plist-put egg-internal-log-buffer-closure :help help)))
    (egg-log-buffer-simple-redisplay buffer 'init)
    (pop-to-buffer buffer t)))

(defun egg-log-buffer-reflog-ref (pos)
  (interactive "d")
  (egg-reflog (car (get-text-property pos :ref))))

;;;========================================================
;;; annotated tag
;;;========================================================
(defvar egg-internal-annotated-tag-name nil)
(defvar egg-internal-annotated-tag-target nil)

(defun egg-tag-msg-create-tag ()
  (let (output)
    (setq output 
	  (egg-sync-git-region egg-log-msg-text-beg egg-log-msg-text-end 
			       "tag" "-a" "-F" "-"
			       egg-internal-annotated-tag-name
			       egg-internal-annotated-tag-target))
    (when output
      (egg-show-git-output output -1 "GIT-ANNOTATED-TAG")
      (egg-run-buffers-update-hook))))

(define-egg-buffer tag:msg "*%s-tag:msg@%s*"
  (egg-log-msg-mode)
  (setq major-mode 'egg-tag:msg-buffer-mode
	mode-name "Egg-Tag:Msg"
	mode-line-process "")
  (make-local-variable 'egg-internal-annotated-tag-name) 
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-tag:msg-mode-hook))


(defun egg-create-annotated-tag (name commit-1)
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 (buf (egg-get-tag:msg-buffer 'create))
	 (commit (egg-git-to-string "rev-parse" "--verify" commit-1))
	 (pretty (egg-describe-rev commit))
	 (inhibit-read-only inhibit-read-only))
    (or commit (error "Bad commit: %s" commit-1))
    (pop-to-buffer buf t)
    (setq inhibit-read-only t)
    (erase-buffer)
    (set (make-local-variable 'egg-log-msg-action)
	 'egg-tag-msg-create-tag)
    (set (make-local-variable 'egg-internal-annotated-tag-name) name)
    (set (make-local-variable 'egg-internal-annotated-tag-target) commit)
    (insert (egg-text "Create Annotated Tag" 'egg-text-2) " "
	    (egg-text name 'egg-branch) "\n\n"
	    (egg-text "on commit:" 'egg-text-2) " "
	    (egg-text commit 'font-lock-string-face) "\n"
	    (egg-text "aka:" 'egg-text-2) " "
	    (egg-text pretty 'font-lock-string-face) "\n"
	    (egg-text "Repository: " 'egg-text-2)
	    (egg-text git-dir 'font-lock-constant-face) "\n"
	    (egg-text "----------------- Tag Message (type C-c C-c when done) ---------------"
			'font-lock-comment-face))
    (put-text-property (point-min) (point) 'read-only t)
    (put-text-property (point-min) (point) 'rear-sticky nil)
    (insert "\n")
    (set (make-local-variable 'egg-log-msg-text-beg) (point-marker))
    (set-marker-insertion-type egg-log-msg-text-beg nil)
    (set (make-local-variable 'egg-log-msg-text-end) (point-marker))
    (set-marker-insertion-type egg-log-msg-text-end t)))
;;;========================================================
;;; minor-mode
;;;========================================================
(defun egg-file-toggle-blame-mode (save)
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (when (and (buffer-modified-p)
	     (or save 
		 (y-or-n-p (format "save %s first? " (buffer-file-name)))))
    (save-buffer))
  (let (blame-was-on buffer-was-readonly)
    (mapc (lambda (ov)
	    (when (overlay-get ov :blame)
	      (setq buffer-was-readonly (plist-get (overlay-get ov :blame)
						   :buffer-read-only))
	      (setq blame-was-on t)))
	  (overlays-at (point)))
    (if blame-was-on
	(progn (egg-file-buffer-blame-off (current-buffer))
	       (set-buffer-modified-p nil)
	       (setq buffer-read-only buffer-was-readonly))
      (egg-file-buffer-blame-on (current-buffer)
				:buffer-read-only buffer-read-only)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))))

(defun egg-file-diff (&optional ask)
  "Diff the current file in another window."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let ((git-file (egg-buf-git-name))
	(src-rev (and ask (egg-read-rev "diff against: " "HEAD")))
	buf)
    (setq buf (egg-do-diff (egg-build-diff-info src-rev nil git-file))) 
    (pop-to-buffer buf t)))

(defun egg-file-checkout-other-version (&optional no-confirm)
  "Checkout HEAD's version of the current file.
if CONFIRM-P was not null, then ask for confirmation if the
current file contains unstaged changes."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file (buffer-file-name))
	 (file-modified (not (egg-file-committed (buffer-file-name))))
	 rev)
    (when file-modified
      (unless (y-or-n-p (format "ignored uncommitted changes in %s? " file))
	(error "File %s contains uncommitted changes!" file)))
    (setq rev (egg-read-rev (format "checkout %s version: " file) "HEAD"))
    (when (egg-sync-do-file file "git" nil nil (list "checkout" rev "--" file))
      (revert-buffer t t t))))

(defun egg-file-cancel-modifications (&optional no-confirm)
  "Checkout INDEX's version of the current file.
if CONFIRM-P was not null, then ask for confirmation if the
current file contains unstaged changes."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file (buffer-file-name))
	 (file-modified (not (egg-file-updated (buffer-file-name))))
	 rev)
    (when (and file-modified (not no-confirm))
      (unless (y-or-n-p (format "ignored unstaged changes in %s? " file))
	(error "File %s contains unstaged changes!" file)))
    (when (egg-sync-do-file file "git" nil nil (list "checkout" "--" file))
      (revert-buffer t t t))))

(defun egg-start-new-branch ()
  (interactive)
  (egg-do-create-branch nil 'checkout "start new branch with name: "))

(defun egg-file-get-other-version (file &optional rev prompt same-mode name)
  (let* ((mode (assoc-default file auto-mode-alist 'string-match))
	 (git-dir (egg-git-dir))
	 (lbranch (egg-current-branch))
	 (rbranch (and git-dir (or (egg-tracking-target lbranch)
				   rev ":0")))
	 (prompt (or prompt (format "%s's version: " file)))
	 (rev (or rev (egg-read-rev prompt rbranch)))
	 (canon-name (egg-file-git-name file))
	 (git-name (concat rev ":" canon-name))
	 (buf (get-buffer-create (concat "*" (or name git-name) "*"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(unless (= (call-process "git" nil buf nil "show" git-name)
		   0)
	  (error "Failed to get %s's version: %s" file rev))
	(when (and (functionp mode) same-mode)
	  (funcall mode))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)))
    buf))

(defun egg-file-version-other-window (&optional ask)
  "Show other version of the current file in another window."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let ((buf (egg-file-get-other-version
	      (buffer-file-name) (if ask nil ":0")
	      (format "show %s's version:" (buffer-file-name))
	      t)))
    (unless (bufferp buf)
      (error "Oops! can't get %s older version" (buffer-file-name)))
    (pop-to-buffer buf t)))

(defun egg-file-ediff (&optional ask-for-dst)
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file buffer-file-name)
	 (dst-buf (if ask-for-dst
		      (egg-file-get-other-version
		       (buffer-file-name) nil
		       (format "(ediff) %s's newer version: " file)
		       t)
		    (current-buffer)))
	 (src-buf (egg-file-get-other-version 
		   (buffer-file-name)
		   nil
		   (format "(ediff) %s's older version: " file)
		   t)))
    (unless (and (bufferp dst-buf) (bufferp src-buf))
      (error "Ooops!"))
    (ediff-buffers dst-buf src-buf)))

(defun egg-resolve-merge-with-ediff (&optional what)
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file buffer-file-name)
	 (short-file (file-name-nondirectory file))
	 (ours (egg-file-get-other-version file ":2" nil t (concat "our:" short-file)))
	 (theirs (egg-file-get-other-version file ":3" nil t (concat "their:" short-file))))
    (unless (and (bufferp ours) (bufferp theirs))
      (error "Ooops!"))
    (ediff-buffers3 theirs ours (current-buffer))))

(defun egg-file-do-ediff (closer-rev closer-rev-name &optional further-rev further-rev-name ediff2)
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file buffer-file-name)
	 (short-file (file-name-nondirectory file)) 
	 (closer-name (concat (or closer-rev-name closer-rev)
			      ":" short-file))
	 (this (egg-file-get-other-version file closer-rev nil t closer-name))
	 (further-name (and further-rev
			    (concat (or further-rev-name further-rev)
				    ":" short-file)))
	 (that (and further-rev
		    (egg-file-get-other-version file further-rev nil t further-name))))
    (unless (bufferp this) (error "Ooops!"))
    (unless (or (null further-rev) (bufferp that)) (error "Ooops!"))
    (if (bufferp that)
	(if ediff2
	    (ediff-buffers that this)
	  (ediff-buffers3 that this (current-buffer)))
      (ediff-buffers this (current-buffer)))))

(defconst egg-key-action-alist 
  '((?m :merge-file "[m]erge current file" "Resolve merge conflict(s) in current file.")
    (?f :stage-file "stage current [f]ile" "Stage current file's changes")
    (?s :status "show repo's [s]tatus" "Browse the current status of the repo" )
    (?a :stage-all "stage [a]ll files" "Stage all current changes inside this repo.")
    (?r :rebase-continue "continue [r]rebase" "Continue with the current rebase session.")
    (?d :diff-file "[d]iff current file" "Compare the current file against the staged snapshot.")
    (?c :commit "[c]ommit staged changes" "Proceed to commit current staged changes onto HEAD.")
    (?y :sync "s[y]nc" "Synchronize branches and repos (push/fetch).")
    (?? :more-options "[?] more options" nil)
    (?b :new-branch "start new [b]ranch" "Create and switch to a new branch starting from HEAD.")
    (?q :quit "[q] quit" nil)))

(defconst egg-action-function-alist
  '((:merge-file	. egg-resolve-merge-with-ediff)
    (:stage-file	. egg-file-stage-current-file)
    (:status		. egg-status)
    (:stage-all		. egg-stage-all-files)
    (:rebase-continue	. egg-buffer-rebase-continue)
    (:diff-file		. egg-file-diff)
    (:commit		. egg-commit-log-edit)
    (:sync		. egg-log)
    (:new-branch	. egg-start-new-branch)
    (:quit		. (lambda () (interactive) (message "do nothing now! later.") (ding) nil))))

(defconst egg-electrict-select-action-buffer 
  (get-buffer-create "*Egg:Select Action*"))

(defun egg-select-action-run ()
  (interactive)
  (let (action)
    (save-excursion
      (with-current-buffer egg-electrict-select-action-buffer
	(beginning-of-line)
	(when (boundp 'egg-electric-in-progress-p)
	  (setq action (get-text-property (point) :action))
	  (if action
	      (throw 'egg-select-action action)
	    (ding)))))))

(defun egg-select-action-quit ()
  (interactive)
  (let (action)
    (save-excursion
      (with-current-buffer egg-electrict-select-action-buffer 
	(beginning-of-line)
	(when (boundp 'egg-electric-in-progress-p)
	  (throw 'egg-select-action nil))))))

(defconst egg-electric-select-action-map 
  (let ((map (make-sparse-keymap "Egg:SelectAction")))
    (define-key map "q" 'egg-select-action-quit)
    (define-key map (kbd "RET") 'egg-select-action-run)
    (define-key map (kbd "SPC") 'egg-select-action-run)
    (define-key map (kbd "C-l") 'recenter)
    map))

(defun egg-electric-select-action (default banner &optional alternatives)
  (let ((egg-electric-in-progress-p t)
	(old-buffer (current-buffer))
	(buf egg-electrict-select-action-buffer)
	(action-alist
	 (delq nil (mapcar (lambda (entry)
			     (if (and (cadddr entry) 
				      (or (null alternatives)
					  (memq (cadr entry) alternatives)))
				 (cons (cadr entry)
				       (cadddr entry))))
			   egg-key-action-alist)))
	action default-entry beg)
    (setq default-entry (assq default action-alist))
    (setq action-alist
	  (cons default-entry (remq default-entry action-alist)))
    (unwind-protect
	(setq action
	      (catch 'egg-select-action
		(save-window-excursion
		  (with-current-buffer buf
		    (let ((inhibit-read-only t))
		      (erase-buffer)
		      (insert (egg-text "Select Action\n" 
					'egg-section-title))
		      (insert (egg-text banner 'egg-text-1) "\n\n")
		      (insert (egg-text "select an action:" 'egg-text-1)
			      "\n\n")
		      (put-text-property (point-min) (point)
					 'intangible t)
		      (setq beg (point))
		      (insert 
		       (mapconcat
			(lambda (entry)
			  (egg-prop (concat "- " (cdr entry))
				    :action (car entry)
				    'face 'egg-electrict-choice))
			action-alist
			"\n")
		       "\n")
		      (goto-char beg)
		      (set-buffer-modified-p nil) 
		      (setq buffer-read-only t))
		    (setq major-mode 'egg-select-action)
		    (setq mode-name "Egg-Select")
		    (use-local-map egg-electric-select-action-map)) 
		  (Electric-pop-up-window egg-electrict-select-action-buffer)
		  (goto-char beg)
		  (Electric-command-loop 'egg-select-action
					 "select next action> "))))
      (bury-buffer buf))
    (when (and action (symbolp action))
      action)))

(defsubst egg-guess-next-action (desc)
  (cond ((memq :file-has-merged-conflict desc) :merge-file)
	((memq :file-is-modified desc) 	       :stage-file)
	((memq :wdir-has-merged-conflict desc) :status)
	((memq :wdir-is-modified desc)	       :stage-all)
	((memq :rebase-in-progress desc)       :rebase-continue)
	((memq :has-staged-changes desc)       :commit)
	(t     	    			       :sync)))

(defun egg-limit-alternative-actions (desc)
  (let ((alternatives (mapcar 'car egg-action-function-alist)))
    (unless (memq :file-has-merged-conflict desc)
      (setq alternatives (delq :merge-file alternatives)))
    (unless (memq :file-is-modified desc)
      (setq alternatives (delq :diff-file (delq :stage-file alternatives)))) 
    (when (or (not (memq :wdir-is-modified desc))
	      (memq :wdir-has-merged-conflict desc)) 
      (setq alternatives (delq :stage-all alternatives)))
    (when (or (not (memq :rebase-in-progress desc))
	      (memq :wdir-is-modified desc))
      (setq alternatives (delq :rebase-continue alternatives)))
    (when (or (memq :wdir-is-modified desc)
	      (memq :rebase-in-progress desc)
	      (not (memq :has-staged-changes desc)))
      (setq alternatives (delq :commit alternatives)))
    (when (or (memq :wdir-has-merged-conflict desc)
	      (memq :rebase-in-progress desc))
      (setq alternatives (delq :new-branch alternatives)))
    (when (or (memq :wdir-is-modified desc)
	      (memq :has-staged-changes desc)
	      (memq :rebase-in-progress desc))
      (setq alternatives (delq :sync alternatives)))
    alternatives))



(defun egg-describe-state (state)
  (let* ((git-dir (plist-get state :gitdir))
	 (current-file (buffer-file-name))
	 (default-directory (file-name-directory git-dir))
	 (file-git-name (and current-file (egg-file-git-name current-file)))
	 (unstaged-files (plist-get state :unstaged))
	 (staged-files (plist-get state :staged))
	 desc dummy)
    (when unstaged-files 
      (setq desc (cons :wdir-is-modified desc)))

    (when staged-files
      (setq desc (cons :has-staged-changes desc)))

    (when (plist-get state :rebase-step)
      (setq desc (cons :rebase-in-progress desc)))
    
    (when unstaged-files
      (with-temp-buffer
	(egg-git-ok t "diff")
	(setq dummy (buffer-string))
	(save-match-data 
	  (goto-char (point-min))
	  (if (search-forward "\n++<<<<<<<" nil t)
	      (setq desc (cons :wdir-has-merged-conflict desc)))))

      (when (and file-git-name (member file-git-name unstaged-files))
	(setq desc (cons :file-is-modified desc))
	(with-temp-buffer
	  (egg-git-ok t "diff" file-git-name)
	  (setq dummy (buffer-string))
	  (save-match-data 
	  (goto-char (point-min))
	    (if (search-forward "\n++<<<<<<<" nil t)
		(setq desc (cons :file-has-merged-conflict desc)))))))
    desc))

(defsubst egg-build-key-prompt (prefix default alternatives)
  (let ((action-desc-alist (mapcar 'cdr egg-key-action-alist)))
    (concat prefix " default: "
	    (nth 1 (assq default action-desc-alist))
	  ". alternatives:  "
	  (mapconcat 'identity 
		     (mapcar (lambda (action)
			       (nth 1 (assq action action-desc-alist)))
			     (remq default alternatives)) ", "))))

(defun egg-prompt-next-action (described-state)
  (let ((default (egg-guess-next-action described-state))
	(limited-alternatives (egg-limit-alternative-actions described-state))
	banner key action alternatives)
    (setq alternatives (list default :status :more-options))
    (while (null action)
      (setq key (read-key-sequence 
		 (egg-build-key-prompt "next action?"
				       default alternatives)))
      (setq key (string-to-char key))
      (setq action 
	    (if  (memq key '(?\r ?\n ?\ ))
		default 
	      (cadr (assq key egg-key-action-alist))))
      (when (eq action :more-options)
	(setq banner
	      (format "%s %s\n%s %s\nINDEX %s"
		      (buffer-file-name)
		      (cond ((memq :file-has-merged-conflict described-state)
			     "contains conflicting merge-changes")
			    ((memq :file-is-modified described-state)
			     "contains unstaged changes")
			    (t "is not modified"))
		      (file-name-directory (egg-git-dir))
		      (cond ((memq :wdir-has-merged-conflict described-state)
			     "has files with conflicting merge changes")
			    ((memq :wdir-is-modified described-state)
			     "has files with unstaged changes")
			    (t "is clean"))
		      (cond ((memq :rebase-in-progress described-state)
			     "has unfinished rebase session")
			    ((memq :has-staged-changes described-state)
			     "contains staged changes ready to commit")
			    (t "is empty"))))
	(setq action (egg-electric-select-action default banner limited-alternatives)))
      (when (null action)
	(ding)))
    action))

(defun egg-next-action (&optional ask)
  (interactive "P")
  (save-some-buffers nil 'egg-is-in-git)
  (let* ((state (egg-repo-state :unstaged :staged :error-if-not-git))
	 (desc (egg-describe-state state))
	 action default)
    (setq action (if (or ask egg-confirm-next-action)
		     (egg-prompt-next-action desc)
		   (egg-guess-next-action desc)))
     
     (call-interactively (cdr (assq action egg-action-function-alist)))))

(defvar egg-minor-mode nil)
(defvar egg-minor-mode-map (make-sparse-keymap "Egg"))
(defvar egg-file-cmd-map (make-sparse-keymap "Egg:File"))

(defun egg-mode-key-prefix-set (var val)
  (define-key egg-minor-mode-map (read-kbd-macro val) egg-file-cmd-map)
  (custom-set-default var val))

(defun egg-file-log-pickaxe (string)
  (interactive (list (read-string "search history for: "
				  (egg-string-at-point))))
  (egg-search-changes string))

(let ((map egg-file-cmd-map))
  (define-key map (kbd "a") 'egg-file-toggle-blame-mode)
  (define-key map (kbd "b") 'egg-start-new-branch)
  (define-key map (kbd "d") 'egg-status)
  (define-key map (kbd "c") 'egg-commit-log-edit)
  (define-key map (kbd "e") 'egg-file-ediff)
  (define-key map (kbd "g") 'egg-grep)
  (define-key map (kbd "i") 'egg-file-stage-current-file)
  (define-key map (kbd "l") 'egg-log)
  (define-key map (kbd "L") 'egg-reflog)
  (define-key map (kbd "h") 'egg-file-log)
  (define-key map (kbd "o") 'egg-file-checkout-other-version)
  (define-key map (kbd "s") 'egg-status)
  (define-key map (kbd "u") 'egg-file-cancel-modifications)
  (define-key map (kbd "v") 'egg-next-action)
  (define-key map (kbd "w") 'egg-commit-log-edit)
  (define-key map (kbd "/") 'egg-file-log-pickaxe)
  (define-key map (kbd "=") 'egg-file-diff)
  (define-key map (kbd "~") 'egg-file-version-other-window))

(defcustom egg-mode-key-prefix "C-x v"
  "Prefix keystrokes for egg minor-mode commands."
  :group 'egg
  :type 'string
  :set 'egg-mode-key-prefix-set)

(defvar egg-minor-mode-name " Git")

;;;###autoload
(defun egg-minor-mode (&optional arg)
  "Turn-on egg-minor-mode which would enable key bindings for
egg in current buffer.\\<egg-minor-mode-map>
\\[egg-start-new-branch] start a new branch from the current HEAD.
\\[egg-status] shows the repo's current status
\\[egg-commit-log-edit] start editing the commit message for the current staged changes.
\\[egg-file-stage-current-file] stage new changes of the current file 
\\[egg-log] shows repo's history 
\\[egg-file-checkout-other-version] checkout another version of the current file 
\\[egg-file-cancel-modifications] delete unstaged modifications in the current file
\\[egg-next-action] perform the next logical action
\\[egg-file-diff] compare file with index or other commits
\\[egg-file-version-other-window] show other version of the current file.

\\{egg-minor-mode-map}
"
  (interactive "p")
  (setq egg-minor-mode (if (null arg)
			     (not egg-minor-mode)
			   (> arg 0)))
  (when egg-minor-mode
    (if (boundp 'vc-mode)
	(set 'vc-mode nil))
    (make-local-variable 'egg-minor-mode-name) 
    (setq egg-minor-mode-name 
	  (intern (concat "egg-" (egg-git-dir) "-HEAD")))))

;;;###autoload
(defun egg-minor-mode-find-file-hook ()
  (when (egg-is-in-git)
    (make-local-variable 'egg-minor-mode)
    (egg-minor-mode 1)))

(or (assq 'egg-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(egg-minor-mode egg-minor-mode-name) minor-mode-alist)))

(setcdr (or (assq 'egg-minor-mode minor-mode-map-alist)
	    (car (setq minor-mode-map-alist
		       (cons (list 'egg-minor-mode)
			     minor-mode-map-alist))))
	egg-minor-mode-map)

(if (and (boundp 'vc-handled-backends)
	 (listp (symbol-value 'vc-handled-backends)))
    (set 'vc-handled-backends
	 (delq 'Git (symbol-value 'vc-handled-backends))))


(add-hook 'find-file-hook 'egg-git-dir)
(add-hook 'find-file-hook 'egg-minor-mode-find-file-hook)

(provide 'egg)
