(when (executable-find "python")
  ;; sources of good information on Python in Emacs
  ;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/ (also see references)
  ;; http://janteichmanndevu.ipage.com/wordpress/2010/12/emacs-is-also-a-great-python-editor/
  ;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
  ;; https://github.com/renatoGarcia/auto-complete-rope/blob/master/auto-complete-rope.el
  (setq el-get-sources
        (append '((:name pymacs		; Python-EmacsLisp interface
			 :features pymacs
			 :depends auto-complete ; for the stuff below
			 :after (lambda ()
				  ;; for this to work, you must have `rope', `ropemacs', and `ropemode' installed through pip
				  (defun python-ropemacs-custom ()
				    (ropemacs-mode t))
				  
				  (add-hook 'python-mode-hook 'python-ropemacs-custom)
				  
                                  ;; set up auto-complete for ropemacs
				  ;; this does all the necessary rope setup as well
                                  (ac-ropemacs-initialize)
				  ))
		  (:name pythonbrew-mini ; Emacs interface to pythonbrew
			 :type git
			 :url "git://github.com/franckcuny/pythonbrew-mini.el.git"
			 :features pythonbrew-mini))
		el-get-sources))
  
  ;; flymake support
  (require 'flymake-python))

(provide 'python-custom)