;;; ack-custom.el --- Customizations for ack (better than grep)
;; 
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: convenience, local, processes, tools, unix
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

;; ack - grep replacement
(when (or (executable-find "ack") (executable-find "ack-grep"))
  (push
   '(:name full-ack
	   :type elpa
	   :after (progn
		    (let ((ack-grep-executable (executable-find "ack-grep")))
		      (when ack-grep-executable
			(setq ack-executable ack-grep-executable)))
		    (global-set-key (kbd "C-x a") 'ack)
		    (global-set-key (kbd "C-x C-a") 'ack-find-file)))
   el-get-sources))

(provide 'ack-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ack-custom.el ends here
