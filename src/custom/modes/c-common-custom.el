(defun c-mode-common-custom ()
  (setq c-default-style "bsd")		; BSD (Allman) style indentation
  (setq c-basic-offset 2)		; 2-space tab size
  (setq indent-tabs-mode t)		; use tabs
  (setq tab-width 2))			; small tabs

(add-hook 'c-mode-common-hook 'c-mode-common-custom)

(provide 'c-common-custom)