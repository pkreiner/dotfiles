(require 'package)
(setq package-enable-at-startup 'nil)
;; (package-refresh-contents)
;; Note: not including marmalade because it sucks.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
