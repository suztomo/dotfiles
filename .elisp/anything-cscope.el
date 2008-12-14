c(require 'anything)
(require 'xcscope)

(defvar anything-cscope-db-directory nil)

(defvar anything-c-source-cscope-c-cymbol
      '((name . "cscope : c-cymbol")
        
        (candidates . (lambda ()
                        (anything-cscope-candidates "-0")))
        (action . anything-c-source-cscope-action)
        (requires-pattern . 3)
        (candidate-number-limit . 10)
        (delayed)))

(defvar anything-c-source-cscope-global-definition
      '((name . "cscope : global-definition")
        
        (candidates . (lambda ()
                        (anything-cscope-candidates "-1")))
        
        (action . anything-c-source-cscope-action)
        (requires-pattern . 3)
        (candidate-number-limit . 10)
        (delayed)))

(defvar anything-c-source-cscope-called-function
      '((name . "cscope : called-function")
        
        (candidates . (lambda ()
                        (anything-cscope-candidates "-2")))
        
        (action . anything-c-source-cscope-action)
        (requires-pattern . 3)
        (delayed)))

(defvar anything-c-source-cscope-calling-this-function
      '((name . "cscope : calling-this-function")
        
        (candidates . (lambda ()
                        (anything-cscope-candidates "-3")))
        
        (action . anything-c-source-cscope-action)
        (requires-pattern . 3)
        (delayed)))

(defvar anything-c-source-cscope-text-string
      '((name . "cscope : text-string")
        
        (candidates . (lambda ()
                        (anything-cscope-candidates "-4")))
        
        (action . anything-c-source-cscope-action)
        (requires-pattern . 3)
        (delayed)))

(defun anything-cscope-candidates (search-type-arg)
  (let ((cscp-dir nil)
        (base-database-file-name)
        (next-item)
        (options)
        (cscope-directory)
        (database-file)
        (done)
        )
    (save-excursion
      (with-current-buffer anything-current-buffer
        (setq cscp-dir (cscope-canonicalize-directory
                        (or cscope-initial-directory default-directory)))
        )
      (setq default-directory cscp-dir
            cscope-search-list (cscope-find-info cscp-dir) 
            cscope-searched-dirs nil
            cscope-command-args (list search-type-arg (concat anything-pattern ".*"))
            cscope-first-match nil
            cscope-first-match-point nil
            cscope-stop-at-first-match-dir-meta (memq t cscope-search-list)
            )
      
      (catch 'finished
        (setq options '("-L"))
        (while (and (not done) cscope-search-list)
          (setq next-item (car cscope-search-list)
                cscope-search-list (cdr cscope-search-list)
                base-database-file-name cscope-database-file
                )
          (if (listp next-item)
              (progn
                (setq cscope-directory (car next-item))
                (if (not (stringp cscope-directory))
                    (setq cscope-directory
                          (cscope-search-directory-hierarchy
                           default-directory)))
                (if (file-regular-p cscope-directory)
                    (progn
                      ;; Handle the case where `cscope-directory' is really
                      ;; a full path name to a cscope database.
                      (setq base-database-file-name
                            (file-name-nondirectory cscope-directory)
                            cscope-directory
                            (file-name-directory cscope-directory))
                      ))
                (setq cscope-directory 
                      (file-name-as-directory cscope-directory))
                (if (not (member cscope-directory cscope-searched-dirs))
                    (progn
                      (setq cscope-searched-dirs (cons cscope-directory
                                                       cscope-searched-dirs)
                            done t)
                      ))
                )
            (progn
              (if (and cscope-first-match
                       cscope-stop-at-first-match-dir
                       cscope-stop-at-first-match-dir-meta)
                  (throw 'finished nil))
              ))
          )
        (if (not done)
            (throw 'finished nil))
        (if (car (cdr next-item))
            (let (newopts)
              (setq newopts (car (cdr next-item)))
              (if (not (listp newopts))
                  (error (format "Cscope options must be a list: %s" newopts)))
              (setq options (append options newopts))
              ))
        (if cscope-command-args
            (setq options (append options cscope-command-args)))
        (setq database-file (concat cscope-directory base-database-file-name)
              cscope-searched-dirs (cons cscope-directory
                                         cscope-searched-dirs)
              )

        ;; The database file and the directory containing the database file
        ;; must both be writable.
        (if (or (not (file-writable-p database-file))
                (not (file-writable-p (file-name-directory database-file)))
                cscope-do-not-update-database)
            (setq options (cons "-d" options)))

        (setq options (cons base-database-file-name options))
        (setq options (cons "-f" options))
        (setq default-directory cscope-directory)
        (setq anything-cscope-db-directory cscope-directory)
        (setq options (cons cscope-program options))
        (apply 'start-process (concat "anything-cscope" search-type-arg) nil
               options)
        ))))

(defun anything-c-source-cscope-action (line)
  (let* (
         (lines (split-string line " "))
         (file-name (car lines))
         (line-number (string-to-number (car (cdr (cdr lines)))))
         )
    (find-file (concat anything-cscope-db-directory file-name))
    (goto-line line-number)))

(defun anything-cscope-select ()
  (interactive)
  (anything '(anything-c-source-cscope-c-cymbol
              anything-c-source-gtags-select
              anything-c-source-cscope-called-function
              anything-c-source-cscope-calling-this-function
;              anything-c-source-cscope-text-string
              )
            (cscope-extract-symbol-at-cursor nil) ;; initial pattern
            ))

(provide 'anything-cscope)
