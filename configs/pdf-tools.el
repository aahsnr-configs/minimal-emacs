(use-package pdf-view
  :defer t
  :straight pdf-tools
  :defines pdf-annot-activate-created-annotations
  :functions pdf-tools-install
  :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
         (pdf-tools-enabled . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-annot-activate-created-annotations t
        ;; Tokyo Night theme colors
        pdf-view-midnight-colors '("#c0caf5" . "#1a1b26"))
  :config
  (pdf-tools-install t nil t nil)

  ;; Tokyo Night theme custom faces
  (custom-set-faces
   '(pdf-view-rectangle-face ((t (:background "#7aa2f7" :foreground "#1a1b26"))))
   '(pdf-isearch-match ((t (:background "#e0af68" :foreground "#1a1b26"))))
   '(pdf-isearch-lazy ((t (:background "#414868" :foreground "#c0caf5"))))
   '(pdf-isearch-batch ((t (:background "#414868" :foreground "#c0caf5"))))
   '(pdf-view-region ((t (:background "#364a82"))))
   '(pdf-links-read-link ((t (:background "#7aa2f7" :foreground "#1a1b26"))))
   '(pdf-occur-document-face ((t (:foreground "#7aa2f7"))))
   '(pdf-occur-page-face ((t (:foreground "#9ece6a"))))
   '(pdf-annot-list-annotation-face ((t (:foreground "#bb9af7"))))
   '(pdf-annot-list-location-face ((t (:foreground "#565f89"))))
   '(pdf-history-debug-message-face ((t (:foreground "#565f89"))))
   '(pdf-sync-forward-matching-page-label ((t (:foreground "#e0af68"))))
   '(pdf-sync-backward-matching-page-label ((t (:foreground "#e0af68"))))))
