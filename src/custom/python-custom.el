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
  
  ;; NOTE TO SELF: Do not let RVM add paths which will cause Emacs to use the
  ;; wrong version of Python!
  ;;
  ;; Example: Using the system Ruby, RVM will add `/usr/bin' to `exec-path',
  ;; which is used to find executables in Emacs. This will cause Emacs to find
  ;; the system Python!
  
  (setq el-get-sources
        (append '(
		  ;; pythonbrew-mini helps greatly in correctly loading Pymacs
		  ;; it sets PATH and exec-path correctly to make the correct
		  ;; python load
		  ;; by default it will load the latest python in pythonbrew
		  (:name pythonbrew-mini
			 :description "Emacs interface to pythonbrew."
			 :website "https://github.com/franckcuny/pythonbrew-mini.el"
			 :type github
			 :pkgname "franckcuny/pythonbrew-mini.el"
			 :features pythonbrew-mini
			 :after (progn
				  (pythonbrew-mini-use "Python-2.7.2")))
		  ;; Python-EmacsLisp interface
		  (:name pymacs
			 ;; latest tagged release - 0.24-beta2
			 ;; ensure that you `make' with this repo for version consistency!
			 :checkout "016b0bc9c870e1ad564e5885c7df2b7855c0948c"
			 :depends (pythonbrew-mini auto-complete)
			 :after (progn
				  ;; for this to work, you must have `rope', `ropemacs', and `ropemode' installed through pip
				  
				  ;; fix for Pymacs
				  ;; (defvaralias 'python-mode-map 'py-mode-map)
				  
				  ;; start up ropemacs-mode
				  (autoload 'pymacs-apply "pymacs")
				  (autoload 'pymacs-call "pymacs")
				  (autoload 'pymacs-eval "pymacs" nil t)
				  (autoload 'pymacs-exec "pymacs" nil t)
				  (autoload 'pymacs-load "pymacs" nil t)
				  
				  (defvar ropemacs-loaded nil)
				  (defun python-ropemacs-custom ()
				      (with-no-warnings
					(unless ac-ropemacs-loaded
					  (pymacs-load "ropemacs" "rope-")
					  (if (boundp 'ropemacs-enable-autoimport)
					      (setq ropemacs-enable-autoimport t))
					  (setq ropemacs-loaded t)))
				      (ropemacs-mode t))
				  (add-hook 'python-mode-hook 'python-ropemacs-custom)
				  
				  ;; (defun ac-ropemacs-setup ()
				  ;;   (ac-ropemacs-require)
				  ;;   (add-to-list 'ac-sources 'ac-source-ropemacs)
				  ;;   )

				  ;; (ac-ropemacs-initialize)
				  )))
		el-get-sources))
  
  ;; flymake support
  (require 'flymake-python))

(provide 'python-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-custom.el ends here
