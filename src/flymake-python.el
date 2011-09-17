(defconst flymake-allowed-python-file-name-masks '(("\\.py\\'" flymake-python-init)))

;; syntax checker for Python - examples include `pep8', `pyflakes', `flake8' (combination of pep8, pyflakes, and McCabe), `pychecker', or a custom shell script
(defcustom flymake-python-syntax-checker "flake8"
  "Syntax checker for Flymake Python."
  :type 'string)

(defun flymake-python-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-python")))

(defun flymake-python-init ()
  (list flymake-python-syntax-checker (list (flymake-init-create-temp-buffer-copy
					     'flymake-python-create-temp-in-system-tempdir))))

(defun flymake-python-load ()
  (interactive)
  (require 'flymake)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-allowed-python-file-name-masks)
  (if (executable-find flymake-python-syntax-checker)
      (flymake-mode t)
    (message (concat "Not enabling flymake: " flymake-python-syntax-checker " command not found"))))

(add-hook 'python-mode-hook 'flymake-python-load)

(provide 'flymake-python)