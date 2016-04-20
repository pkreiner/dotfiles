;; Commands for moving and killing by elements that are either words or units of punctuation

(provide 'pk-word-punct)

(defun pk/punctuation-info ()
  "For each character on which the regexp [:punct:] and membership in the syntax class ?. do not agree, prints the character, along with those two values, and the syntax class."
  (let ((chars (number-sequence 0 127))
	(char-results nil))
    (insert "\n")
    (dolist (char chars char-results)
      (let ((str (char-to-string char)))
	(let ((matches-regexp
	       (if (string-match "[[:punct:]]" str) t nil))
	      (matches-syntax
	       (eq (char-syntax char) ?.)))
	  (let ((result (list char str matches-regexp matches-syntax (char-to-string (char-syntax char)))))
	    (if (not (eq matches-regexp matches-syntax))
		(progn (insert "\n")
		       (insert (format "%s" result))))
	    (setq char-results (cons result char-results))))))))


(defconst pk/punctuation-chars "[,;:.!?()_"
  "Contains punctuation characters except for ] and -, which need
to come at the beginning and end of the regexp respectively and
so will be inserted separately.")
(defconst pk/punct-regexp
  (concat "[]" pk/punctuation-chars "-]"))
(defconst pk/word-or-punct-regexp
  (concat "[]" pk/punctuation-chars "[:word:]" "-]"))

(defun pk/backward-word-punct ()
  "Move back to (before) the beginning of the current or previous
word or piece of punctuation, or the beginning of the buffer if
there isn't one."
  (interactive)
  ;; Go back to the last char of a word or piece of punctuation.
  (if (search-backward-regexp pk/word-or-punct-regexp nil t)
      (let ((char (char-after)))
	(if (eq (char-syntax char) ?w)
	    ;; if it's a word, go to the beginning of the
	    ;; word (or beginning of the buffer)
	    (if (search-backward-regexp "[^[:word:]]" nil t)
		(forward-char)
	      (beginning-of-buffer))
	  ;; if it's punctuation, we're done
	  ))
    (beginning-of-buffer))
  (point))

(defun pk/forward-word-punct ()
  "Move forward to (after) the end of the current or next word or
piece of punctuation, or the end of the buffer if there isn't
one."
  (interactive)
  (if (search-forward-regexp pk/word-or-punct-regexp nil t)
      (let ((char (char-before)))
 	(if (eq (char-syntax char) ?w)
	    (if (search-forward-regexp "[^[:word:]]" nil t)
		(backward-char)
	      (end-of-buffer))))
    (end-of-buffer))
  (point))

(defun pk/backward-kill-word-punct ()
  (interactive)
  (let ((end (point))
	(beg (pk/backward-word-punct)))
    (kill-region beg end)))

(defun pk/kill-word-punct ()
  (interactive)
  (let ((beg (point))
	(end (pk/forward-word-punct)))
    (kill-region beg end)))

