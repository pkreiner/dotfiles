(provide 'pk-helm-functions)

(defun pk/helm-occur-or-org-buffer-headings (arg)
  "A little wrapper function that runs helm-occur, or, with C-u
prefix, helm-org-in-buffer-headings."
  (interactive "P")
  (if arg
      (helm-org-in-buffer-headings)
    (helm-occur)))
