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


;; for debugging:
;; (add-hook 'post-command-hook 'tweet-mode-update)
;; (remove-hook 'post-command-hook 'tweet-mode-update)


(defun tweet-mode-update ()
  (setq tweet-mode-char-count (pk/count-chars-current-paragraph))
  (setq tweet-mode-string (concat "    " (number-to-string tweet-mode-char-count) "/280")))


;; count number of chars in paragraph
(defun pk/count-chars-current-paragraph ()
  "Count the number of characters in the paragraph that the mark is in."
  (interactive)
  (- (end-of-paragraph) (beginning-of-paragraph)))
  
;; Assumes two newlines before paragraph,
;; may be 1-2 chars off otherwise.
(defun beginning-of-paragraph ()
  (interactive)
  (save-excursion
    (search-backward "\n\n" nil "no-error")
    (+ (point) 2)))

;; Assumes two newlines after paragraph,
;; may be 1-2 chars off otherwise
(defun end-of-paragraph ()
  (interactive)
  (save-excursion
    (search-forward "\n\n" nil "no-error")
    (- (point) 2)))

