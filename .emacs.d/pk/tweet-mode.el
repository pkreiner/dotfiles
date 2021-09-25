;; Mode for composing tweets

(provide 'tweet-mode)
(define-minor-mode tweet-mode
  "Mode for composing tweets")

(setq tweet-mode-hook nil)
(add-hook
 'tweet-mode-hook
 (lambda ()
   (if tweet-mode
       (progn
	 (add-hook 'post-command-hook 'tweet-mode-update)
	 (setq-local mode-line-format
		     (append mode-line-format (list 'tweet-mode-string)))
	 )
     (progn
       (remove-hook 'post-command-hook 'tweet-mode-update)
       (setq-local mode-line-format
	       (butlast mode-line-format))))))


(defun tweet-mode-update ()
  ;; (message "updating")
  (setq tweet-mode-string (create-tweet-mode-string))
  (force-mode-line-update))

(defun create-tweet-mode-string ()
  (let* ((end (end-of-paragraph))
	 (beg (beginning-of-paragraph))
	 (pos (point))
	 (par-length (- end beg))
	 (pos-in-par (- pos beg)))
    (concat " " (number-to-string par-length) "/280"
	    " (" (number-to-string pos-in-par) ")"
	    )))

;; Assumes two newlines before paragraph,
;; may be 1-2 chars off otherwise.
(defun beginning-of-paragraph ()
  (save-excursion
    (search-backward "\n\n" nil "no-error")
    (+ (point) 2)))

;; Assumes two newlines after paragraph,
;; may be 1-2 chars off otherwise
(defun end-of-paragraph ()
  (save-excursion
    (search-forward "\n\n" nil "no-error")
    (- (point) 2)))
