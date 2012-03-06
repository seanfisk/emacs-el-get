;; Work-around for Emacs 23.1 to compile auto-indent-mode
;; Please see <http://paste.lisp.org/display/115598>
		    
;; If we have a version of called-interactively-p that doesn't accept            
;; arguments, redefine it so that it does take arguments. This                   
;; retains compatibility with packages that pass arguments to
;; called-interactively-p.
		    
(condition-case nil (called-interactively-p 'interactive)
  (wrong-number-of-arguments
   ;; Save reference to called-interactively-p in
   ;; substitute-called-interactively-p                                    
   (fset 'substitute-called-interactively-p
	 (symbol-function 'called-interactively-p))
   ;; Define called-interactively-p so that it discards
   ;; its arguments and calls substitute-called-interactively-p            
   (fset 'called-interactively-p
	 (lambda (&rest args)
	   (substitute-called-interactively-p)))))

(provide 'auto-indent-mode-workaround)