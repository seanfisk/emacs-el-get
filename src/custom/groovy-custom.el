;;; groovy-custom.el --- Customizations for Groovy and Grails
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

(setq el-get-sources
      (append '(
		;; major mode for editing Groovy
		(:name groovy-mode
		       :website "http://groovy.codehaus.org/Emacs+Groovy+Mode"
		       :description "This is the (properly, an) Emacs major mode for the Groovy programming language."
		       :type http-tar
		       :options ("xzf")
		       :url "https://launchpad.net/groovy-emacs-mode/trunk/current-release/+download/emacs-groovy-mode_2011-06-29.tgz"
		       :load-path (".")
		       :features (groovy-mode inf-groovy)
		       :after (progn
				;; groovy-mode
				(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
				(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
				
				;; add electric and interpreter
				(defun groovy-custom ()
				  ;; disable autopair
				  (setq autopair-dont-activate t)
				  ;; turn on electric
				  (require 'groovy-electric)
				  (groovy-electric-mode)
				  ;; groovy interpreter
				  (inf-groovy-keys)
				  ;; spaces instead of tabs
				  (setq indent-tabs-mode nil)
				  ;; four-space indent
				  (setq tab-width 4))
				(add-hook 'groovy-mode-hook 'groovy-custom)))

		;; Groovy eval mode
		(:name groovy-eval-mode
		       :website "http://code.google.com/p/emacs-groovy-eval-mode/"
		       :description "Minor mode for evaluating Groovy within GNU Emacs"
		       :type svn
		       :url "http://emacs-groovy-eval-mode.googlecode.com/svn/trunk/"
		       :features groovy-eval
		       :after (progn
				(add-hook 'groovy-mode-hook 'groovy-eval)))
		;; minor mode for Grails
		(:name grails-mode
		       :website "https://code.google.com/p/emacs-grails-mode/"
		       :description "Simplifies working with Grails apps"
		       :type svn
		       :url "http://emacs-grails-mode.googlecode.com/svn/trunk/"
		       :features grails-mode
		       :depends project-mode
		       :after (progn
				(setq grails-mode t))))
	      el-get-sources))

(provide 'groovy-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; groovy-custom.el ends here
