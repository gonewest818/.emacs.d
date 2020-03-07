;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT

(use-package deferred
  :ensure t)

(use-package request-deferred
  :ensure t)

(defun tablist--patch-wisent-total-conflicts (fn &rest args)
  "Monkey patch `wisent-total-conflicts' to work around errors in 27.0.90.
Supply a fictitious `load-file-name' to satisfy the code, and
then call FN with ARGS.  This patch can be removed when the bug
is resolved: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39911"
  (if (wisent-source)
      (apply fn args)
    (let ((load-file-name "tablist-fictitious-file.el"))
      (apply fn args))))

(unless (version< "27" emacs-version)
  (advice-add 'wisent-total-conflicts
              :around #'tablist--patch-wisent-total-conflicts))

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
             geolocation-scan-wifi)
  :config
  (add-hook 'geolocation-update-hook #'geolocation-update-calendar)
  (geolocation-update-position))

(use-package sf511
  :load-path "~/.emacs.d/dev/sf511.el"
  :bind (("C-c qs" . sf511-operators)))

(use-package mincal
  :load-path "~/.emacs.d/dev/mincal.el"
  :commands (mincal-display mincal-retrieve)
  :bind ("C-c qm" . mincal-display))

