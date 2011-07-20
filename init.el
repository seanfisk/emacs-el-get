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

;; Add more package archives
(setq package-archives
      '(("original"    . "http://tromey.com/elpa/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))

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
   (:name goto-last-change		; move pointer back to last change
	  :after (lambda ()
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name color-theme-solarized
	  :after (lambda ()
		   (color-theme-solarized-dark)))
   (:name auto-indent-mode
	  :type elpa
	  :after (lambda ()
		   (auto-indent-global-mode)))
   (:name autopair
	  :after (lambda ()
		   (autopair-mode)))
   (:name autopair+
	  :type emacswiki)
   (:name auto-complete
	  :after (lambda ()
		   (auto-complete-mode)))
   (:name icicles
	  :type git
	  :url "git://github.com/emacsmirror/icicles.git"
	  :load "icicles.el"
	  :features icicles)
   ;; (:name textmate
   ;; 	  :type git
   ;; 	  :url "git://github.com/defunkt/textmate.el.git"
   ;; 	  :load "textmate.el"
   ;; 	  :features textmate
   ;; 	  :after (lambda ()
   ;; 		   (textmate-mode)))
   (:name rsense
	  :type git
	  :url "git://github.com/m2ym/rsense.git"
	  :load-path "etc"
	  :features rsense
	  :after (lambda()
		   (setq rsense-home (expand-file-name "."))))
   (:name edit-server
	  :features edit-server
	  :after (lambda ()
		   (edit-server-start)))))


;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   escreen            			; screen for emacs, C-\ C-h
   php-mode-improved			; if you're into php...
   switch-window			; takes over C-x o
   zencoding-mode			; http://www.emacswiki.org/emacs/ZenCoding
   color-theme		                ; nice looking emacs
;   color-theme-tango	                ; check out color-theme-solarized
   auto-complete-extension		; extensions for auto-complete
   dtrt-indent				; guess indent of foreign files
   undo-tree				; undo history in a tree
   ruby-mode				; major mode for ruby
;   ruby-compilation			; compile parts of ruby code
   ruby-electric			; ruby control structure matching
   flymake-ruby				; flymake for ruby
;   flymake-point			; show error under cursor
   flymake-fringe-icons			; show error icons at side
   rvm					; rvm compatibility
   rinari				; rails ide
   haml-mode				; major mode for haml
   scss-mode				; major mode for scss
   js2-mode				; major mode for javascript
   coffee-mode))			; major mode for coffee-script

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
(when (el-get-executable-find "cvs")
  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

(when (el-get-executable-find "svn")

  (loop for p in '(psvn    		; M-x svn-status
		   yasnippet		; powerful snippet mode
		   )
	do (add-to-list 'my:el-get-packages p)))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)		; no splash screen, thanks
;(line-number-mode 1)			; have line numbers and
;(column-number-mode 1)			; column numbers in the mode line

;; disable scrollbars
(scroll-bar-mode -1)

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))

;(global-hl-line-mode)			; highlight current line
;(global-linum-mode 1)			; add line numbers on the left

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

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

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
;(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
;(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
;(global-set-key (kbd "C-x B") 'ibuffer)

;; icicles
(icy-mode)
(setq icicle-max-candidates 20)
(setq icicle-incremental-completion-flag t)
(setq icicle-top-level-when-sole-completion-flag t)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; other bindings and such
(server-start) ; boot the emacs server for use with emacsclient
(desktop-save-mode 1) ; save my files for next time

;; auto-indent
;(global-set-key (kbd "RET") 'newline-and-indent) ; auto-indent everything

;; ruby-mode
(defun ruby-custom ()
  "ruby-mode-hook"
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
;  (autopair-mode)
  ; Rsense + Autocomplete
  (add-to-list 'ac-sources 'ac-source-rsense-method)
  (add-to-list 'ac-sources 'ac-source-rsense-constant))

(add-hook 'ruby-mode-hook '(lambda ()
                             (ruby-custom)))

;; coffee-mode
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2)
 (coffee-cos-mode t)) ; compile on save
; (auto-complete-mode)
; (autopair-mode)) 

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; auto-saves
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/auto-saves"))))
