;;; coffee-script-custom.el --- Customizations for Coffee Script
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; coffee-script-custom.el ends here
