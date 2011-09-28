;;; Flymake Shell mode

(defconst flymake-allowed-shell-file-name-masks
  ;; just do it on all files, any extension can be a script and sh-mode is smart enough to detect it
  '((".*" flymake-shell-init))
  "Filename extensions that switch on flymake-shell mode syntax checks.")

(defun flymake-shell-syntax-checker ()
  (if (symbolp sh-shell)
      (symbol-name sh-shell)
    "/bin/bash"))

(defcustom flymake-shell-arguments
  (list "-n")
  "Shell arguments to invoke syntax checking.")

(defcustom flymake-shell-err-line-pattern-re
  '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))
  "Regexp matching shell error messages.")

(defun flymake-shell-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-shell")))

(defun flymake-shell-init ()
  (list (executable-find (flymake-shell-syntax-checker)) (list (flymake-init-create-temp-buffer-copy
								'flymake-shell-create-temp-in-system-tempdir))))
(defun flymake-shell-load ()
  (interactive)
  (require 'flymake)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-allowed-shell-file-name-masks)
  (if (executable-find (flymake-shell-syntax-checker))
      (flymake-mode t)
    (message (concat "Not enabling flymake: " flymake-shell-syntax-checker " command not found"))))

(add-hook 'sh-mode-hook 'flymake-shell-load)

(provide 'flymake-shell)

;;; flymake-shell.el ends here