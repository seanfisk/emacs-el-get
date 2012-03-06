;; ack - grep replacement
(when (or (executable-find "ack") (executable-find "ack-grep"))
  (push
   '(:name full-ack
	   :after (lambda ()
		    (let ((ack-grep-executable (executable-find "ack-grep")))
		      (when ack-grep-executable
			(setq ack-executable ack-grep-executable)))
		    (global-set-key (kbd "C-x a") 'ack)
		    (global-set-key (kbd "C-x C-a") 'ack-find-file)))
   el-get-sources))

(provide 'ack-custom)