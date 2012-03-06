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

;; cursor
(blink-cursor-mode -1) ; no blinking cursor
(setq-default x-stretch-cursor t) ; use a block cursor
(setq-default cursor-type 'box)

(provide 'visual-custom)