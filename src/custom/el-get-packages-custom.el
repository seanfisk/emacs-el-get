;;; el-get-packages-custom.el --- Custom el-get package list
;; 
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: local
;; Compatibility: GNU Emacs: 23.x, Aquamacs 2.x
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

;; set local recipes
(setq
 el-get-sources
 '((:name package24
	  :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el")
   (:name buffer-move			; have to add your own keys
	  :after (lambda ()
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
   (:name smex                          ; a better (ido like) M-x
	  :after (lambda ()
		   (setq smex-save-file (concat user-emacs-directory ".smex-items"))
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
		   (global-set-key (kbd "C-x C-m") 'smex)))
   (:name goto-last-change              ; move pointer back to last change
	  :after (lambda ()
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name color-theme			; base for all color themes
	  :after (lambda ()
		   (global-set-key (kbd "C-c t") 'color-theme-select)))
   
   ;; This note is here to remind me of the issue with solarized.
   ;; Basically, it creates a border around the mode line in the bottom of the screen
   ;; This border pushes up the mode line and causes certain things (like less in terminal)
   ;; to cut off a line at the top since the bottom line isn't displayed completely.
   ;; I will ask the author if he can fix this.
   (:name color-theme-solarized		; dark and light Solarized color themes
	  :url "git://github.com/sellout/emacs-color-theme-solarized.git") ; https clone takes a long time for some reason - hopefully speed up clone time
   (:name color-theme-chocolate-rain
	  :load "color-theme-chocolate-rain.el")
   (:name color-theme-mac-classic     ; mac classic theme
          :features color-theme-mac-classic)
   (:name color-theme-railscasts
	  :features color-theme-railscasts ; this automatically activates the theme
	  :after (lambda ()
		   (setq frame-background-mode 'dark)))
   (:name whole-line-or-region		; use whole line when no region is present
	  :features whole-line-or-region
          :after (lambda ()
		   (whole-line-or-region-mode t)))
   ;; (:name auto-indent-mode		; auto-indentation, should be loaded before autopair and yasnippet
   ;; 	  :type emacswiki
   ;; 	  :features auto-indent-mode
   ;; 	  :before (lambda ()
   ;; 		    (require 'auto-indent-mode-workaround))
   ;; 	  :after (lambda ()
   ;; 	   	   (auto-indent-global-mode t)
   ;;                 (setq auto-indent-disabled-modes-list (append '(coffee-mode shell-mode term-mode markdown-mode python-mode) auto-indent-disabled-modes-list))))
   (:name autopair			; automatically complete everything that comes in pairs, load auto-indent-mode first
	  ;; :depends auto-indent-mode
	  :after (lambda ()
		   (autopair-global-mode t)))
   (:name auto-pair+			; smart quoting of regions for autopair
	  :features auto-pair+
	  :depends autopair
	  :type emacswiki)
   (:name auto-complete			; the best auto-complete extension for emacs!
	  :after (lambda ()
		   (ac-set-trigger-key "TAB")
		   ;; add to this list when more auto-completion is needed
		   (setq ac-modes (append '(scss-mode) ac-modes))
		   
		   (defun auto-complete-custom ()
		     "auto-complete-mode-hook"
		     ;; re-assign `dabbrev' to this
		     (local-set-key (kbd "M-/") 'auto-complete)
		     ;; add a tags auto-complete source when we have a tags file
		     ;; otherwise we get prompted for a tags file all the time, even when we don't have one
		     (when (or tags-file-name tags-table-list)
		       (add-to-list 'ac-sources 'ac-source-etags)))
		   
		   (add-hook 'auto-complete-mode-hook 'auto-complete-custom)))
   (:name auto-complete-etags		; auto-complete source for tags
	  :features auto-complete-etags
	  :depends auto-complete)
   (:name undo-tree	  		; undo history in a tree like vim, try C-x u
	  :features undo-tree
	  :after (lambda ()
		   (global-undo-tree-mode t)))
   (:name textmate			; textmate key emulation, try Command-T or Alt-T for goto file
	  :after (lambda ()
		   (textmate-mode t)))
   ;; (:name anything			; "Quicksilver for Emacs"
   ;; 	  :before (lambda ()
   ;; 		    (defvar org-directory ""))) ; Hack around free variable org-directory issue
   ;; (:name anything-etags+		; etags history for anything
   ;; 	  :type emacswiki
   ;; 	  :features anything-etags+
   ;; 	  :depends anything)
   (:name flymake-cursor		; show the syntax error for the line under the cursor in the minibuffer
	  :type emacswiki
	  :features flymake-cursor)
   ;; IMPORTANT : magit requires the `makeinfo' executable from the `texinfo' package to compile!!!
   (:name magit                         ; git meet emacs, and a binding
	  :build/darwin `(,(concat "make " (shell-quote-argument (concat "EMACS=" invocation-directory invocation-name)) " all"))
          :after (lambda ()
                   (global-set-key (kbd "C-x C-z") 'magit-status)))
   ;; (:name dtrt-indent		  ; foreign indentation detection mode
   ;; 	  :post-init (lambda ()))
   (:name ruby-mode			; major mode for ruby
	  :depends autopair) ; try not to cause problems with turning off autopair-mode later, in case ruby mode hook is activated
   ;; (:name multi-term	     ; better version of term
   ;; 	  :after (lambda ()
   ;; 		   ;; don't mess with my terminal keys
   ;; 		   (setq term-bind-key-alist nil)
   ;; 		   (setq term-unbind-key-list nil)
   
   ;; 		   (setq multi-term-program "zsh") ; use zsh
   
   ;; 		   ;; name the terminal immediately - my first real Emacs Lisp function - woohoo!
   ;;                 (defun multi-term-with-name()
   ;;                   (interactive)
   ;;                   (multi-term)
   ;;                   (call-interactively 'rename-buffer)
   ;; 		     (rename-buffer (concat (buffer-name) "-term")))
   ;; 		   ;;; give it a shortcut
   ;; 		   (global-set-key (kbd "C-x t") 'multi-term-with-name)))
   (:name misc-cmds                ; Drew Adams' miscellaneous commands
	  :type emacswiki
	  :features misc-cmds
	  :after (lambda ()
		   (substitute-key-definition 'move-beginning-of-line 'beginning-or-indentation global-map)
                   (substitute-key-definition 'move-end-of-line 'end-of-line+ global-map)))
   ;; (:name elscreen			; screen for emacs
   ;; 	  :features elscreen)
   (:name header2
          :description "Support for creation and update of file headers."
          :type emacswiki
          :features "header2")
   (:name fillcode
	  :website "http://snarfed.org/fillcode"
	  :description "Fillcode is an Emacs minor mode that fills, or wraps, some parts of source code."
	  :type http-tar
	  :options ("xzf")
	  :url "http://snarfed.org/fillcode.tar.gz"
	  :load "fillcode.el"
	  :features fillcode
	  :after (lambda ()
		   (add-hook 'c-mode-common-hook 'fillcode-mode)
		   (add-hook 'perl-mode-hook 'fillcode-mode)
		   (add-hook 'python-mode-hook 'fillcode-mode)
		   (add-hook 'shell-script-mode-hook 'fillcode-mode)
		   (add-hook 'sql-mode-hook 'fillcode-mode)))
   (:name fill-column-indicator
	  :after (lambda ()
		   ;; always use fci-mode
		   (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
		   (global-fci-mode 1)
		   ))
   (:name edit-server              ; for editing through Google Chrome
	  :features edit-server
	  :after (lambda ()
		   (edit-server-start)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   switch-window			; numbered windows for easy switching, takes over C-x o
   dired+				; many extensions to dired directory browser
   zencoding-mode			; http://www.emacswiki.org/emacs/ZenCoding
   nxhtml				; awesome html editing mode
   php-mode-improved			; better major mode for php
   haml-mode				; major mode for haml
   textile-mode				; major mode for textile markup
   markdown-mode			; major mode for markdown markup
   cmake-mode				; major mode for editing CMake config files
   flymake-fringe-icons			; show error icons at side
   ))

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;

(when (el-get-executable-find "cvs")
  ;;(add-to-list 'my:el-get-packages 'emacs-goodies-el) ; the debian addons for emacs
  (add-to-list 'my:el-get-packages 'ecb))             ; Emacs Code Browser

(when (el-get-executable-find "svn")
  ;; subversion plugin for emacs
  (add-to-list 'my:el-get-packages 'psvn)
  ;; javascript ide
  (add-to-list 'my:el-get-packages 'js2-mode))

;; other customizations for specific languages and tools
(require 'ack-custom)
(require 'coffee-script-custom)
(require 'python-custom)
(require 'ruby-custom)
(require 'sass-custom)

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

(provide 'el-get-packages-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; el-get-packages-custom.el ends here
