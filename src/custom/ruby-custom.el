;;; ruby-custom.el --- Customizations for Ruby
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

;; rinari - rails ide
;; if we have rails and rake (needed for compiling rinari, error if we have rails but no rake)
;; (when (and (executable-find "rails") (el-get-executable-find "rake"))
;;   (push
;;    '(:name rinari
;;      :type elpa)
;;    el-get-sources))

;; rvm integration
(push
 '(:name rvm
	 :type elpa)
 el-get-sources)

;; some more ruby niceties
;; ruby control structure matching
(push '(:name ruby-electric) el-get-sources)
;; flymake for ruby
(push
 '(:name flymake-ruby
	 :type elpa)
 el-get-sources)

;; hook for ruby-mode
(defun ruby-custom ()
  "ruby-mode-hook"
  ;; add Buildfile to flymake
  (if (boundp 'flymake-ruby-allowed-file-name-masks)
      (push '("Buildfile$" flymake-ruby-init) flymake-ruby-allowed-file-name-masks))
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  ;; Rsense + Autocomplete
  (add-to-list 'ac-sources 'ac-source-rsense-method)
  (add-to-list 'ac-sources 'ac-source-rsense-constant)
  ;; superceded in this mode by ruby-electric
  (setq autopair-dont-activate t))

(add-hook 'ruby-mode-hook 'ruby-custom)

(provide 'ruby-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-custom.el ends here
