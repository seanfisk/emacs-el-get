(defun sh-custom ()
  (setq sh-basic-offset 2)		; 2-space tab size
  (setq indent-tabs-mode t)             ; use tabs
  (setq tab-width 2))                   ; small tabs

(add-hook 'sh-mode-hook 'sh-custom)

(provide 'sh-custom)
