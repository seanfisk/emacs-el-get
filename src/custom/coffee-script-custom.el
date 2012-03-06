;; coffee-script additions
(when (executable-find "coffee")
  (setq el-get-sources
	(append 
	 '((:name coffee-mode                   ; major mode for coffee-script
		  :depends (js2-mode autopair)
		  :after (lambda ()
			   ;; the recipe sets to javascript-mode - so reset to default `js2mode' because we have it
			   (setq coffee-js-mode 'js2-mode)))
	   (:name flymake-coffee           ; flymake support for coffee-script
		  :type git
		  :url "https://github.com/purcell/flymake-coffee.git"
		  :features flymake-coffee))
	 el-get-sources))
  ;; coffee-mode
  (defun coffee-custom ()
    "coffee-mode-hook"
    (set (make-local-variable 'tab-width) 2)
    (coffee-cos-mode nil) ; don't compile on save by default, set this to t to do compile-on-save
    (flymake-coffee-load)) ; flymake for coffee script
  
  (add-hook 'coffee-mode-hook 'coffee-custom))

(provide 'coffee-script-custom)