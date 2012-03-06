;; sass / scss additions
(when (executable-find "sass")
  (add-to-list 'my:el-get-packages 'scss-mode)
  
  ;; hook for scss mode
  (defun scss-custom ()
    "scss-mode-hook"
    (setq scss-compile-at-save nil)) ; don't do this by default, set to t to compile on save
  
  (add-hook 'scss-mode-hook 'scss-custom))

(provide 'sass-custom)