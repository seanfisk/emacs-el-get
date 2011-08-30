;;; Flymake Shell mode

(require 'flymake)

(defcustom flymake-shell-arguments
  (list "-n")
  "Shell arguments to invoke syntax checking.")

(defconst flymake-allowed-shell-file-name-masks
  ;; just do it on all files, any extension can be a script and sh-mode is smart enough to detect it
  '((".*" flymake-shell-init))
  "Filename extensions that switch on flymake-shell mode syntax checks.")

(defcustom flymake-shell-err-line-pattern-re
  '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))
  "Regexp matching shell error messages.")

(defun flymake-shell-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list sh-shell-file (append flymake-shell-arguments (list local-file)))))

(defun flymake-shell-load ()
  (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-allowed-shell-file-name-masks)
  (set (make-local-variable 'flymake-err-line-patterns) flymake-shell-err-line-pattern-re)
  (flymake-mode t))

(provide 'flymake-shell)

;;; flymake-shell.el ends here