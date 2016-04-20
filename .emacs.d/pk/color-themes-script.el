;; This is a script I got from the emacs wiki (and fixed a bit) that, when run, lets you rotate through the color themes in color-themes by pressing f12. Note that this only includes themes in the color-themes package, so it's missing, for example, my old standby zenburn.

(defun my-theme-set-default () ; Set the first row
      (interactive)
      (setq theme-current color-themes)
      (funcall (car (car theme-current))))
     
(defun my-describe-theme () ; Show the current theme
  (interactive)
  (message "%s" (car theme-current)))
     
; Set the next theme (fixed by Chris Webber - thanks)
(defun my-theme-cycle ()		
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current color-themes))
  (funcall (car (car theme-current)))
  (message "%S" (car theme-current)))

(setq theme-current color-themes)
(setq color-theme-is-global nil) ; Initialization
(my-theme-set-default)
(global-set-key [f12] 'my-theme-cycle)

;; Color themes from color-themes I like:
;; Bharadwaj Slate
;; Fischmeister
;; Standard Emacs 20
;; Subtle Hacker
;; Taming Mr. Arneson
;; Wheat
