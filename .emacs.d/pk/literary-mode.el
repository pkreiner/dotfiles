;; Mode for reading and writing non-code: emails, essays, etc.

;; Someday: Write a command that lets you work with parts of sentences
;; delimited by punctuation

(provide 'literary-mode)
(define-minor-mode literary-mode
  "Mode for reading and writing non-code: emails, essays, etc."
  :keymap (make-sparse-keymap))

(defvar literary-mode-body-width 70
  "Width in characters of the text.")

;; All of the actual effects live here. At present they are
;; irreversible, i.e. disabling literary mode doesn't actual revert
;; the margin width, etc. to their old values.
(add-hook
 'literary-mode-hook
 (lambda ()
   ;; Only run if literary-mode is being enabled, not if it's being
   ;; disabled.
   (if literary-mode
       (progn
	 (linum-mode -1)
	 (visual-line-mode 1)
	 (fringes 0)
	 ;; (set-window-fringes (get-buffer-window) 0 0)
	 (set-body-width literary-mode-body-width)
	 (setq line-spacing 0.2)))))


(defun window-margin-list (&optional window)
  "Returns a list '(l r), where l and r are the sizes of the left
and right margins, with 0 (rather than nil) representing the
absence of a margin."
  (unless window (setq window (get-buffer-window)))
  (let ((zeroify (lambda (x) (if (eq nil x) 0 x)))
	(margin-pair (window-margins window)))
    (let ((l (zeroify (car margin-pair)))
	  (r (zeroify (cdr margin-pair))))
      (list l r))))

(defun window-margins-numeric (&optional window)
  "Like window-margins, returns the pair (l . r) of right and
left margin widths. The difference is that unlike window-margins,
empty margins will be represented by 0 instead of nil."
  (unless window (setq window (get-buffer-window)))
  (let* ((margins (window-margins window))
	 (l (car margins))
	 (r (cdr margins)))
    (let ((newL (if (null l) 0 l))
	  (newR (if (null r) 0 r)))
      (cons newL newR))))

(defun fringes (size)
  (setq left-fringe-width size)
  (setq right-fringe-width size)
  (set-window-buffer (get-buffer-window) (buffer-name)))

(defun margins (size)
  (interactive "nEnter margin width: ")
  (setq left-margin-width size)
  (setq right-margin-width size)
  (set-window-buffer (get-buffer-window) (buffer-name)))

(defun set-body-width (width)
  "Set the margins symmetrically to make the window's text body
the desired width, which is given in characters. Takes the widths
of the other elements that have width (fringes, the scroll bar,
and the vertical divider) into account, indirectly."
  (let* ((new-bw width)
	 (bw (window-body-width))
	 (margins (window-margins-numeric))
	 (mw (+ (car margins) (cdr margins)))
	 (new-mw (- (+ mw bw) new-bw))
	 (new-half-mw (/ new-mw 2)))
    (margins new-half-mw)))
    ;; (set-window-margins (get-buffer-window)
    ;; 			new-half-mw
    ;; 			new-half-mw)))
	 
