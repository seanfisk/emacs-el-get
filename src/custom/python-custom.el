;;; python-custom.el --- Customizations for Python
;; 
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: languages, local, tools
;; Compatibility: GNU Emacs: 23.x, Aquamacs: 2.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-custom.el ends here
