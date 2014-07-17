(require 'linum)

(defvar pycov2-data nil "Coverage data for the buffer")
(defvar pycov2-mode-text " pycov(I)")
;; Need to figure out how to use these without errors
(defvar pycov2-color-not-run "#ef2929")
(defvar pycov2-color-no-data "#fce94f")
(defvar pycov2-color-no-cov2emacs-binary "#5c3566")
(defvar pycov2-color-covered "")
(defvar pycov2-cov2emacs-cmd "PYTHONPATH=~/.emacs.d/el-get/pycoverage/cov2emacs ~/.emacs.d/el-get/pycoverage/cov2emacs/bin/cov2emacs")
(defvar pycov2-binary-installed nil)
(defvar pycov2-debug-message nil)
(defvar pycov2-linum-format-string "%4d")

(make-variable-buffer-local 'pycov2-mode-text)
(make-variable-buffer-local 'pycov2-data)
(make-variable-buffer-local 'pycov2-cov-file)
(make-variable-buffer-local 'pycov2-binary-installed)
(make-variable-buffer-local 'pycov2-debug-message)
(make-variable-frame-local 'pycov2-linum-format-string)

(define-minor-mode pycov2-mode
  "Allow annotating the file with coverage information"
  :lighter pycov2-mode-text
  (if pycov2-mode
      (if (or
           (equal "tests" (file-name-base buffer-file-name))
           (equal "test_"
                  (substring (file-name-nondirectory buffer-file-name) 0 5))
           (member "tests" (split-string buffer-file-name "/"))) ; inside a unit tests directory
          (progn
            (message "pycov2: not activating, %S looks like a test case"
                     (file-name-nondirectory buffer-file-name))
            (setq pycov2-mode nil))
           
        (add-hook 'after-save-hook 'pycov2-on-change nil t)
        (add-hook 'linum-before-numbering-hook 'pycov2-linum-get-format-string nil t)
        (setq pycov2-binary-installed (pycov2-exe-found pycov2-cov2emacs-cmd))
        (linum-mode t)
        (make-local-variable 'linum-format)
        (setf linum-format 'pycov2-line-format)
        (pycov2-on-change-force))

    (kill-local-variable 'linum-format)
    (remove-hook 'after-save-hook 'pycov2-on-change t)
    (remove-hook 'linum-before-numbering-hook 'pycov2-linum-get-format-string t)))

(defun pycov2-exe-found (path)
  ;; spliting and taking last item in order to support something like this:
  ;; "PYTHONPATH=~/.emacs.d/el-get/pycoverage/cov2emacs ~/.emacs.d/el-get/pycoverage/cov2emacs/bin/cov2emacs"
  (pycov2-message (format "looking for %s" path))
  (executable-find (car (last (split-string path " ")))))

(defun pycov2-message (txt)
  (if pycov2-debug-message
      (message txt)))

(defun pycov2-on-change-force (&optional beg end len)
  (pycov2-on-change beg end len t))

(defun pycov2-on-change (&optional beg end len force)
  (progn
    (pycov2-message "Rerunning PyCov")
    (pycov2-get-data (buffer-file-name))))

(defun pycov2-refresh ()
  "reload data for buffer"
  (interactive )
  (pycov2-get-data (buffer-file-name) pycov2-cov-file))

(defun pycov2-rerun (cov_file)
  "reload data for buffer using specified coverage file"
  (interactive "FCoverage file:")
  (setq pycov2-cov-file cov_file)
  (pycov2-get-data (buffer-file-name) cov_file))

(defun pycov2-get-data (filename &optional cov_file )
  (let* ((result (pycov2-run-better filename cov_file))
         (lines (split-string result "[\n]+")))
    (setq pycov2-data nil)
    (setq pycov2-binary-installed (pycov2-exe-found pycov2-cov2emacs-cmd))
    (if result
        (progn
          ;; take status from first line
          (pycov2-process-status (car lines))
          (mapcar (lambda (line)
                    (if (not (equal line ""))
                        (pycov2-process-script-line line)))
                  (cdr lines))))))

(defun pycov2-process-status (line)
  ;; status like looks like this: SUCCESS:23
  ;; where 23 is percent of coverage
  (let* ((data (split-string line ":"))
         (stat (first data)))
    (progn
      ;; set mode-line to error, others will overwrite
      (setq pycov2-mode-text " pycov(ERR)")
      (force-mode-line-update))
    (when (not pycov2-binary-installed)
      (setq pycov2-mode-text (format " pycov:EXE?" (second data))))
    (when (equal stat "SUCCESS")
      (progn
        ;; update mode-line
        (setq pycov2-mode-text (format " pycov:%s" (second data)))
        (force-mode-line-update)))
    (when (equal stat "OLD")
      (progn
        ;; update mode-line
        (setq pycov2-mode-text " pycov(OLD)")
        (force-mode-line-update)))
    (when (equal stat "NO COVERAGE FILE")
      (progn
        ;; update mode-line
        (setq pycov2-mode-text " pycov(?)")
        (force-mode-line-update)))))

(defun pycov2-process-script-line (line)
  ;; line looks like this filepath:103:MISSING
  (let* ((data (split-string line ":"))
         (path (first data))
         (number (string-to-number (second data)))
         (status (third data)))
    (when (equal status "MISSING")
      ;; add linenum to pycov2-data
      (add-to-list 'pycov2-data number))))

(defun pycov2-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (fmt (concat "%" (number-to-string width) "d")))
    (setq pycov2-linum-format-string fmt)))

(defun pycov2-line-format (line-number)
  (let ((content (format pycov2-linum-format-string line-number)))
    (cond
     ((not pycov2-binary-installed)
      (progn
        (pycov2-message "Missing cov2emacs in PATH")
        (propertize content 'face '(:background "#5c3566" ;; :foreground "#5c3566"
                                                )) )
      )
     ((member line-number pycov2-data)
      (propertize content 'face '(:background "#ef2929" ;; :foreground "#ef2929"
                                              ))
      )
     (pycov2-data
      ;; covered data
      (propertize content 'face 'linum))
     (t
      ;; some other issue (old data)
      (progn
        (pycov2-message "Coverage missing or old")
        (propertize content 'face '(:background "#FCAF3E" :foreground "black")))
      ))))

(defun pycov2-run-better (filename &optional cov_file)
  (let* ((command (if cov_file
                      (format "%s --compile-mode --python-file %s --coverage-file %s" pycov2-cov2emacs-cmd filename cov_file)
                    (format "%s --compile-mode --python-file %s" pycov2-cov2emacs-cmd filename))))
    (pycov2-message command)
    (shell-command-to-string command)))

(provide 'pycov2)
