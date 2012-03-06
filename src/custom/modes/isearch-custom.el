;;; isearch other end - see <http://www.emacswiki.org/emacs/IsearchOtherEnd>
(defun isearch-goto-match-beginning ()
  (when isearch-forward (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'isearch-goto-match-beginning)

(provide 'isearch-custom)