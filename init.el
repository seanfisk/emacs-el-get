;; Emacs config for Sean Fisk
;;
;; Author: Sean Fisk <sean@seanfisk.com>
;; URL: https://github.com/seanfisk/emacs
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

(setq user-emacs-directory (expand-file-name "~/.emacs.d"))

(require 'cl)				; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move			; have to add your own keys
	  :after (lambda ()
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
  (:name smex                          ; a better (ido like) M-x
         :after (lambda ()
                  (setq smex-save-file "~/.emacs.d/.smex-items")
                  (global-set-key (kbd "M-x") 'smex)
                  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                  (global-set-key (kbd "C-x C-m") 'smex)))
   (:name goto-last-change              ; move pointer back to last change
	  :after (lambda ()
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name color-theme-solarized		; awesome color theme
	  :depends color-theme
	  :url "git://github.com/sellout/emacs-color-theme-solarized.git" ; https clone takes a long time for some reason - hopefully speed up clone time
	  :after (lambda ()
		   (color-theme-solarized-dark)))
   (:name auto-indent-mode		; auto-indentation, should be loaded before autopair and yasnippet
	  :type emacswiki
	  :features auto-indent-mode
	  :after (lambda ()
		   (auto-indent-global-mode t)))
   (:name autopair			; automatically complete everything that comes in pairs, load auto-indent-mode first
	  :depends auto-indent-mode
	  :after (lambda ()
		   (autopair-global-mode t)))
   (:name auto-pair+			; add Textmate-like pairing to autopair
	  :depends autopair
	  :type emacswiki)
   (:name auto-complete			; the best auto-complete extension for emacs!
	  :after (lambda ()
		   (ac-set-trigger-key "TAB")
		   (add-to-list 'ac-modes 'scss-mode)))
   (:name auto-complete-etags		; auto-complete source for tags
	  :features auto-complete-etags
	  :depends auto-complete)
   (:name undo-tree
	  :type git
	  :url "http://www.dr-qubit.org/git/undo-tree.git"
	  :load "undo-tree.el"
          :features undo-tree
	  :after (lambda ()
		   (global-undo-tree-mode t)))
   (:name textmate
    	  :type git
    	  :url "git://github.com/defunkt/textmate.el.git"
    	  :load "textmate.el"
    	  :features textmate
    	  :after (lambda ()
		   (textmate-mode t)))
   (:name anything			; "Quicksilver for Emacs"
	  :before (lambda ()
		    (defvar org-directory ""))) ; Hack around free variable org-directory issue
   (:name anything-etags+		; even more etags goodness for anything
	  :type emacswiki
	  :features anything-etags+
	  :depends anything)
   (:name full-ack			; grep replacement
	  :after (lambda ()
		   (let ((ack-grep-executable (executable-find "ack-grep")))
		     (when ack-grep-executable
		       (setq ack-executable ack-grep-executable)))
		   (global-set-key (kbd "C-x C-a") 'ack)
		   (global-set-key (kbd "C-x a") 'ack-find-file)))
   (:name flymake-cursor
	  :type emacswiki
	  :features flymake-cursor)
   (:name rsense			; ruby type completion
	  :type git
	  :url "git://github.com/m2ym/rsense.git"
	  :load-path "etc"
	  :features rsense
	  :after (lambda()
		   (setq rsense-home (expand-file-name "."))))
   (:name edit-server			; for editing through Google Chrome
	  :features edit-server
	  :after (lambda ()
		   (edit-server-start)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   escreen            			; screen for emacs, C-\ C-h
   switch-window			; takes over C-x o
   color-theme		                ; nice looking emacs
   dtrt-indent				; foreign indentation detection mode
   dired+				; many extensions to dired directory browser
   undo-tree		  		; undo history in a tree like vim, try C-x u
   zencoding-mode			; http://www.emacswiki.org/emacs/ZenCoding
   ruby-mode				; major mode for ruby
   php-mode-improved			; if you're into php...
   haml-mode				; major mode for haml
   scss-mode				; major mode for scss
   js2-mode				; major mode for javascript
   coffee-mode				; major mode for coffee-script
   textile-mode				; major mode for textile markup
   markdown-mode			; major mode for markdown markup
   rvm					; rvm compatibility
   ruby-electric			; ruby control structure matching
   flymake-ruby				; flymake for ruby
   flymake-fringe-icons			; show error icons at side
   ))

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
;(when (el-get-executable-find "cvs")
;  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

(when (el-get-executable-find "svn")
  
  (add-to-list 'my:el-get-packages 'psvn)
  (add-to-list 'el-get-sources
	       '(:name yasnippet
		       :depends auto-indent-mode))) ; powerful snippedt mode, load auto-indent-mode first

(when (executable-find "rake")
  (add-to-list 'my:el-get-packages 'rinari)); rails ide

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)		; no splash screen, thanks

;; disable scrollbars
(scroll-bar-mode -1)

;; choose your own fonts, in a system dependant way
(if (window-system)
    (if (string-match "apple-darwin" system-configuration)
	(set-face-font 'default "Monaco-13")
      (set-face-font 'default "Monospace-10")))

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode t)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)

;(setq ido-show-dot-for-dired t)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(server-start) ; boot the emacs server for use with emacsclient
(desktop-save-mode t) ; save my files for next time

;; general bindings
(global-set-key (kbd "C-x j") 'kill-this-buffer) ; for ease
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; auto-saves
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/auto-saves"))))

;; bindings for modes

;;; auto-complete-mode
(defun auto-complete-custom ()
  "auto-complete-mode-hook"
  (local-set-key (kbd "M-/") 'auto-complete)
  (add-to-list 'ac-sources 'ac-source-etags))

(add-hook 'auto-complete-mode-hook 'auto-complete-custom)

;;; ruby-mode
(defun ruby-custom ()
  "ruby-mode-hook"
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  ; Rsense + Autocomplete
  (add-to-list 'ac-sources 'ac-source-rsense-method)
  (add-to-list 'ac-sources 'ac-source-rsense-constant)
  ; superceded in this mode by ruby-electric
  (setq autopair-dont-activate t))

(add-hook 'ruby-mode-hook
	  '(lambda () (ruby-custom)))

;;; coffee-mode
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2)
 (coffee-cos-mode t)) ; compile on save

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;;; scss-mode
(defun scss-custom ()
  "scss-mode-hook"
  (setq scss-compile-at-save nil)) ; don't do this by default

(add-hook 'scss-mode-hook
	  '(lambda () (scss-custom)))
