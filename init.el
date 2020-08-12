;;; init.el --- My configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;
;;; Code:
;; Loads private settings
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
;;; init.el ends here
