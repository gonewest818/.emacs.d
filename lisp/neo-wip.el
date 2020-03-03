;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT

(use-package deferred
  :ensure t)

(use-package request-deferred
  :ensure t)

(use-package geolocation
  :load-path "~/.emacs.d/dev/geolocation"
  :commands (geolocation-update-position
             geolocation-get-position
             geolocation-scan-wifi))

(use-package mincal
  :load-path "~/.emacs.d/dev/mincal.el"
  :commands (mincal-display mincal-retrieve)
  :bind ("C-c qm" . mincal-display))

