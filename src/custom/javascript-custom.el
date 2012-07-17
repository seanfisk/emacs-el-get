;;; javascript-custom.el --- Customizations for JavaScript
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

;; javascript ide
(setq el-get-sources
      (append
       '((:name js2-mode
               :after (progn
                        (defun javascript-custom ()
                          (setq indent-tabs-mode nil))
                        (add-hook 'js2-mode-hook 'javascript-custom))))
      el-get-sources))

(provide 'javascript-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javascript-custom.el ends here
