;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT

(use-package deferred
  :ensure t)

(use-package request-deferred
  :ensure t)

(defun neo-open-map-at-location ()
  "Open map at coordinates found in `geolocation-location'."
  (interactive)
  (when geolocation-location
    (let ((display-buffer-alist
           (cons (cons "\\*Async Shell Command\\*.*"
                       (cons #'display-buffer-no-window nil))
                 display-buffer-alist)))
      (message (format-time-string
                "open map: %D %r"
                (seconds-to-time (alist-get 'timestamp geolocation-location))))
      (async-shell-command
       (format "open \"https://maps.apple.com/?q=%f,%f\""
               (alist-get 'latitude geolocation-location)
               (alist-get 'longitude geolocation-location))))))

(use-package geolocation
  :load-path "~/.emacs.d/dev/geolocation"
  :bind (("C-c ql" . neo-open-map-at-location))
  :commands (geolocation-update-position
             geolocation-get-position
             geolocation-scan-wifi))

(use-package mincal
  :load-path "~/.emacs.d/dev/mincal.el"
  :commands (mincal-display mincal-retrieve)
  :bind ("C-c qm" . mincal-display))

