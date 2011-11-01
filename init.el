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

(require 'cl)				; common lisp goodies, loop

;; local packages in `src' directory
(add-to-list 'load-path (concat user-emacs-directory "src"))
(require 'open-next-line)
(require 'flymake-shell)

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

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
   (:name color-theme-solarized		; dark and light Solarized color themes
	  :depends color-theme
	  :url "git://github.com/sellout/emacs-color-theme-solarized.git" ; https clone takes a long time for some reason - hopefully speed up clone time
	  :after (lambda ()
		   (when (window-system)
		     (color-theme-solarized-light))))
   (:name color-theme-chocolate-rain
	  :load "color-theme-chocolate-rain.el")
   (:name color-theme-mac-classic     ; mac classic theme
          :features color-theme-mac-classic)
   (:name color-theme-railscasts
	  :load "color-theme-railscasts.el")
   (:name whole-line-or-region		; use whole line when no region is present
	  :features whole-line-or-region
          :after (lambda ()
		   (whole-line-or-region-mode t)))
   (:name auto-indent-mode		; auto-indentation, should be loaded before autopair and yasnippet
	  :type emacswiki
	  :features auto-indent-mode
 	  :before (lambda ()
		    ;; Work-around for Emacs 23.1 to compile auto-indent-mode
		    ;; Please see <http://paste.lisp.org/display/115598>
		    
		    ;; If we have a version of called-interactively-p that doesn't accept            
		    ;; arguments, redefine it so that it does take arguments. This                   
		    ;; retains compatibility with packages that pass arguments to
		    ;; called-interactively-p.
		    
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
                               (substitute-called-interactively-p))))))
	  :after (lambda ()
	   	   (auto-indent-global-mode t)
                   (setq auto-indent-disabled-modes-list (append '(coffee-mode shell-mode term-mode markdown-mode) auto-indent-disabled-modes-list))))
   (:name autopair			; automatically complete everything that comes in pairs, load auto-indent-mode first
	  :depends auto-indent-mode
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
		   ))
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
   (:name anything			; "Quicksilver for Emacs"
	  :before (lambda ()
		    (defvar org-directory ""))) ; Hack around free variable org-directory issue
   (:name anything-etags+		; etags history for anything
	  :type emacswiki
	  :features anything-etags+
	  :depends anything)
   (:name flymake-cursor		; show the syntax error for the line under the cursor in the minibuffer
	  :type emacswiki
	  :features flymake-cursor)
   ;; IMPORTANT : magit requires the `makeinfo' executable from the `texinfo' package to compile!!!
   (:name magit                         ; git meet emacs, and a binding
          :after (lambda ()
                   (global-set-key (kbd "C-x C-z") 'magit-status)))
   (:name dtrt-indent		  ; foreign indentation detection mode
	  :post-init (lambda ()))
   (:name ruby-mode			; major mode for ruby
	  :depends autopair) ; try not to cause problems with turning off autopair-mode later, in case ruby mode hook is activated
   (:name multi-term	     ; better version of term
   	  :after (lambda ()
   		   ;; don't mess with my terminal keys
   		   (setq term-bind-key-alist nil)
		   (setq term-unbind-key-list nil)
		   
		   (setq multi-term-program "zsh") ; use zsh
		   
		   ;; name the terminal immediately - my first real Emacs Lisp function - woohoo!
                   (defun multi-term-with-name()
                     (interactive)
                     (multi-term)
                     (call-interactively 'rename-buffer)
		     (rename-buffer (concat (buffer-name) "-term")))
		   ;;; give it a shortcut
   		   (global-set-key (kbd "C-x t") 'multi-term-with-name)))
   (:name misc-cmds                ; Drew Adams' miscellaneous commands
	  :type emacswiki
	  :features misc-cmds
	  :after (lambda ()
		   (substitute-key-definition 'move-beginning-of-line 'beginning-or-indentation global-map)
                   (substitute-key-definition 'move-end-of-line 'end-of-line+ global-map)))
   (:name edit-server		   ; for editing through Google Chrome
	  :features edit-server
	  :after (lambda ()
		   (edit-server-start)))))
 
;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   escreen            			; screen for emacs
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
  (add-to-list 'my:el-get-packages 'js2-mode)
  ;; powerful snippet mode, load auto-indent-mode first
  (push 
   '(:name yasnippet
	   :depends auto-indent-mode)
   el-get-sources))

;; python additions
;; I use pythonbrew <https://github.com/utahta/pythonbrew> for development,
;; so this is my indicator to bring in python development packages
(when (executable-find "pythonbrew")
  ;; sources of good information on Python in Emacs
  ;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/ (also see references)
  ;; http://janteichmanndevu.ipage.com/wordpress/2010/12/emacs-is-also-a-great-python-editor/
  ;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
  ;; https://github.com/renatoGarcia/auto-complete-rope/blob/master/auto-complete-rope.el
  (setq el-get-sources
        (append '((:name pymacs		; Python-EmacsLisp interface
			 :features pymacs
			 :depends auto-complete ; for the stuff below
			 :after (lambda ()
				  ;; for this to work, you must have `rope', `ropemacs', and `ropemode' installed through pip
                                  ;; redefine this since the `ac-omni-completion-sources' is deprecated
                                  (defun ac-ropemacs-setup ()
                                    (ac-ropemacs-require)
                                    (setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
                                    ;;(setq ac-omni-completion-sources '(("\\." ac-source-ropemacs)))
                                    )
                                  
                                  ;; set up auto-complete for ropemacs
				  ;; this does all the necessary rope setup as well
                                  (ac-ropemacs-initialize)))
		  (:name pythonbrew-mini ; Emacs interface to pythonbrew
			 :type git
			 :url "git://github.com/franckcuny/pythonbrew-mini.el.git"
			 :features pythonbrew-mini))
		el-get-sources))
  
  ;; flymake support
  (require 'flymake-python)
  )

;; ruby additions
(when (executable-find "ruby") ; only if we have ruby
  ;; rinari - rails ide
  (when (and (executable-find "rails") (el-get-executable-find "rake")) ; if we have rails and rake (needed for compiling rinari, error if we don't have it)
    (add-to-list 'my:el-get-packages 'rinari))
  
  ;; rvm integration
  (when (executable-find "rvm")
    (add-to-list 'my:el-get-packages 'rvm))
  
  ;; some more ruby niceties
  (add-to-list 'my:el-get-packages 'ruby-electric) ; ruby control structure matching
  ;; flymake for ruby
  (push
   '(:name flymake-ruby
	   :after (lambda ()
		    (push '("Buildfile$" flymake-ruby-init) flymake-ruby-allowed-file-name-masks)))
   el-get-sources)
  
  ;; hook for ruby-mode
  (defun ruby-custom ()
    "ruby-mode-hook"
    (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
    ;; Rsense + Autocomplete
    (add-to-list 'ac-sources 'ac-source-rsense-method)
    (add-to-list 'ac-sources 'ac-source-rsense-constant)
    ;; superceded in this mode by ruby-electric
    (setq autopair-dont-activate t))
  
  (add-hook 'ruby-mode-hook 'ruby-custom))

;; sass / scss additions
(when (executable-find "sass")
  (add-to-list 'my:el-get-packages 'scss-mode)
  
  ;; hook for scss mode
  (defun scss-custom ()
    "scss-mode-hook"
    (setq scss-compile-at-save nil)) ; don't do this by default, set to t to compile on save
  
  (add-hook 'scss-mode-hook 'scss-custom))

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


;; ack - grep replacement
(when (or (executable-find "ack") (executable-find "ack-grep"))
  (push
   '(:name full-ack
	   :after (lambda ()
		    (let ((ack-grep-executable (executable-find "ack-grep")))
		      (when ack-grep-executable
			(setq ack-executable ack-grep-executable)))
		    (global-set-key (kbd "C-x a") 'ack)
		    (global-set-key (kbd "C-x C-a") 'ack-find-file)))
   el-get-sources))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)		; no splash screen, thanks
(line-number-mode 1)                    ; have line numbers and
(column-number-mode 1)                  ; column numbers in the mode line

(tool-bar-mode -1)                      ; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars

;; easy-on-the-eyes flymake
(require 'flymake)
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

;; choose your own fonts, in a system dependent way
(if (window-system)
    (condition-case nil
        (set-face-font 'default "Inconsolata-14") ; use Inconsolata if we have it
      (error
       (if (string-match "apple-darwin" system-configuration)
	   (set-face-font 'default "Monaco-13") ; use Monaco on Mac
	 (set-face-font 'default "Monospace-12"))))) ; otherwise default to Monospace

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; winner-mode provides C-<left> to get back to previous window layout
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
(setq ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)

;; (setq ido-show-dot-for-dired t)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; now that we've clobbered `kill-emacs', give a shortcut back
(global-set-key (kbd "C-x q") 'kill-emacs)

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

;; cursor
(blink-cursor-mode -1) ; no blinking cursor
(setq-default x-stretch-cursor t) ; use a block cursor
(setq-default cursor-type 'box)

;; paren matching
(show-paren-mode t) ; show matching parentheses

;; auto-saves
(setq backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "auto-saves")))))

;; general bindings
(global-set-key (kbd "C-x j") 'kill-this-buffer) ; an easy shortcut is needed for this common task
(global-set-key (kbd "C-c r") 'rename-buffer)	 ; another common task, mostly used for terminals

;;; switch buffers like other applications
(global-set-key (kbd "<C-tab>") 'bury-buffer)
;;;; override textmate-mode
(define-key *textmate-mode-map* (kbd "<C-tab>") 'bury-buffer)

;; bindings and hooks for modes

;;; isearch other end - see <http://www.emacswiki.org/emacs/IsearchOtherEnd>
(defun isearch-goto-match-beginning ()
  (when isearch-forward (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'isearch-goto-match-beginning)

;;; auto-complete-mode
(defun auto-complete-custom ()
  "auto-complete-mode-hook"
					; re-assign `dabbrev' to this
  (local-set-key (kbd "M-/") 'auto-complete)
					; add a tags auto-complete source when we have a tags file
					; otherwise we get prompted for a tags file all the time, even when we don't have one
  (when (or tags-file-name tags-table-list)
    (add-to-list 'ac-sources 'ac-source-etags)))

(add-hook 'auto-complete-mode-hook 'auto-complete-custom)

;;; real auto-complete global mode

;;;; dirty fix for having AC everywhere
;;;; see "Questions and Feedback" on <http://www.emacswiki.org/emacs/AutoComplete>
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                            (auto-complete-mode 1))
;;                        ))

;; (real-global-auto-complete-mode t)

;;; edit-server mode
(defun server-custom ()
  (when (current-local-map)
    (use-local-map (copy-keymap (current-local-map))))
  (when server-buffer-clients
    (local-set-key (kbd "C-x j") 'server-edit))) ; use the same shortcut as kill this buffer

(add-hook 'server-switch-hook 'server-custom)

;;; emacs lisp mode
(defun emacs-lisp-custom ()
  (eldoc-mode t))			; nice minibuffer documentation

;;; term mode
(defun term-custom ()
  (setq autopair-dont-activate t))

(add-hook 'term-mode-hook 'term-custom)

;;; sh-mode
(defun sh-custom ()
  (setq sh-basic-offset 2)		; 2-space tab size
  (setq indent-tabs-mode t)             ; use tabs
  (setq tab-width 2))                   ; small tabs

(add-hook 'sh-mode-hook 'sh-custom)

;;; c-like modes
(defun c-mode-common-custom ()
  (setq c-default-style "bsd")		; BSD (Allman) style indentation
  (setq c-basic-offset 2)		; 2-space tab size
  (setq indent-tabs-mode t)		; use tabs
  (setq tab-width 2))			; small tabs

(add-hook 'c-mode-common-hook 'c-mode-common-custom)
