;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval local-set-key (kbd "C-c C-v t") 'my/tangle-all-and-sync)
     (eval save-excursion (org-babel-goto-named-src-block "define-tangler")
           (org-babel-execute-src-block))
     (eval add-hook 'org-babel-tangle-finished-hook #'my/desktop-post-tangle nil
           t)
     (eval defun my/desktop-post-tangle nil
           (save-excursion
             (org-babel-goto-named-src-block "post-tangle-execution")
             (org-babel-execute-src-block))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:foreground "#e0af68" :weight bold))))
 '(diff-hl-change ((t (:foreground "#e0af68" :background unspecified))))
 '(diff-hl-delete ((t (:foreground "#f7768e" :background unspecified))))
 '(diff-hl-insert ((t (:foreground "#9ece6a" :background unspecified))))
 '(italic ((t (:foreground "#bb9af7" :slant italic))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7aa2f7"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#e0af68"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#9ece6a"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#1abc9c"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#bb9af7"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#9d7cd8"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#ff9e64"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#f7768e"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#7aa2f7")))))
