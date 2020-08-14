;;; init.el --- My configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;
;;; Code:
(org-babel-load-file "~/.emacs.d/config.org")
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(uncrustify-mode use-package-ensure-system-package tuareg toc-org smart-tabs-mode rainbow-delimiters projectile offlineimap nord-theme mood-line merlin-eldoc ivy-prescient irony-eldoc git-gutter-fringe general gcmh flycheck-ocaml flycheck-irony evil-magit evil-collection esup elfeed el-patch doom-themes dashboard counsel company-prescient company-irony-c-headers company-irony cmake-font-lock avy auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(flycheck-error ((t (:underline "#BF616A"))))
 '(flycheck-info ((t (:underline "#A3BE8C"))))
 '(flycheck-warning ((t (:underline "#EBCB8B"))))
 '(variable-pitch ((t nil))))
