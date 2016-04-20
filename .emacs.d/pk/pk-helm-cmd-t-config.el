(provide 'pk-helm-cmd-t-config)

(setq elisp-source (helm-cmd-t-get-create-source-dir "~/.emacs.d"))
(defun helm-cmd-t-elisp ()
  (interactive)
  (helm :sources (list elisp-source)))

;; Suggested settings for helm-cmd-t, 
;; except I shortened the delays to 0
(setq helm-ff-lynx-style-map nil
      helm-input-idle-delay 0
      helm-idle-delay 0)
