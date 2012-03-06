(defun server-custom ()
  (when (current-local-map)
    (use-local-map (copy-keymap (current-local-map))))
  (when server-buffer-clients
    (local-set-key (kbd "C-x j") 'server-edit))) ; use the same shortcut as kill this buffer

(add-hook 'server-switch-hook 'server-custom)

(server-start) ; boot the emacs server for use with emacsclient

(provide 'server-custom)