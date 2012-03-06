;; ruby additions
(when (executable-find "ruby") ; only if we have ruby
  ;; rinari - rails ide
  (when (and (executable-find "rails") (el-get-executable-find "rake")) ; if we have rails and rake (needed for compiling rinari, error if we don't have it)
    (add-to-list 'my:el-get-packages 'rinari))
  
  ;; rvm integration
  (when (executable-find "rvm")
    (add-to-list 'my:el-get-packages 'rvm))
  
  ;; some more ruby niceties
  (add-to-list 'my:el-get-packages 'ruby-electric) ; ruby control structure matching
  ;; flymake for ruby
  (push
   '(:name flymake-ruby
	   :after (lambda ()
		    (push '("Buildfile$" flymake-ruby-init) flymake-ruby-allowed-file-name-masks)))
   el-get-sources)
  
  ;; hook for ruby-mode
  (defun ruby-custom ()
    "ruby-mode-hook"
    (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
    ;; Rsense + Autocomplete
    (add-to-list 'ac-sources 'ac-source-rsense-method)
    (add-to-list 'ac-sources 'ac-source-rsense-constant)
    ;; superceded in this mode by ruby-electric
    (setq autopair-dont-activate t))
  
  (add-hook 'ruby-mode-hook 'ruby-custom))

(provide 'ruby-custom)