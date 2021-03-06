;;; init.el --- Emacs config for Sean Fisk
;; 
;; Author: Sean Fisk <sean@seanfisk.com>
;; Maintainer: Sean Fisk <sean@seanfisk.com>
;; Keywords: local
;; Compatibility: GNU Emacs: 23.x, Aquamacs: 2.x
;; URL: https://github.com/seanfisk/emacs
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (C) 2011 by Sean Fisk <sean@seanfisk.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;
;; Emacs kicker --- kick start emacs setup
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: https://github.com/dimitri/emacs-kicker
;; Created: 2011-04-15
;; Keywords: emacs setup el-get kick-start starter-kit
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; common lisp goodies, loop
(require 'cl)

;; local packages in `src/lib' directory
(add-to-list 'load-path (concat user-emacs-directory "src/lib"))
(require 'open-next-line)
(require 'flymake-shell)

;; bindings and hooks for modes
(add-to-list 'load-path (concat user-emacs-directory "src/custom/modes"))
(require 'c-common-custom)
(require 'server-custom)
(require 'elisp-custom)
(require 'isearch-custom)
(require 'sh-custom)
(require 'term-custom)
(require 'rst-custom)

;; load el-get package manager
;; el-get defaults to storing packages in `~/.emacs.d/el-get', we want to change
;; that (for Aquamacs)
;; However, el-get with elpa flips out if there are spaces in the path - I should look into fixing this
;; (setq el-get-dir (file-name-as-directory (concat user-emacs-directory "el-get")))
(setq el-get-dir (file-name-as-directory "~/.emacs.d/el-get"))
(add-to-list 'load-path (concat el-get-dir "el-get"))
(unless (require 'el-get nil t)
  (with-current-buffer
   (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
   (let (el-get-master-branch)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(add-to-list 'load-path (concat user-emacs-directory "src/custom"))
(require 'el-get-packages-custom)

;; other customizations
(require 'visual-custom)
(require 'misc-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
