;;; auto-indent-mode-workaround.el --- Work-around for Emacs 23.1 to compile auto-indent-mode
;; 
;; Author: inglorion
;; URL: http://paste.lisp.org/display/115598
;; Keywords: extensions, internal, lisp, local
;; Compatibility: GNU Emacs: 23.x, Aquamacs: 2.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; Work-around for Emacs 23.1 to compile auto-indent-mode
;; Please see <http://paste.lisp.org/display/115598>
;;    
;; If we have a version of called-interactively-p that doesn't accept            
;; arguments, redefine it so that it does take arguments. This                   
;; retains compatibility with packages that pass arguments to
;; called-interactively-p.
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

(condition-case nil (called-interactively-p 'interactive)
  (wrong-number-of-arguments
   ;; Save reference to called-interactively-p in
   ;; substitute-called-interactively-p                                    
   (fset 'substitute-called-interactively-p
	 (symbol-function 'called-interactively-p))
   ;; Define called-interactively-p so that it discards
   ;; its arguments and calls substitute-called-interactively-p            
   (fset 'called-interactively-p
	 (lambda (&rest args)
	   (substitute-called-interactively-p)))))

(provide 'auto-indent-mode-workaround)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-indent-mode-workaround.el ends here
