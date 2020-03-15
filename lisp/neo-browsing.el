;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BROWSING

(defun neo-elfeed-search-tag-saved ()
  "Tag article at point with `saved'."
  (interactive)
  (save-excursion
    (push-mark (point) t t)
    (elfeed-search-tag-all 'saved)
    (pop-mark)))

(defun neo-elfeed-search-untag-saved ()
  "Tag article at point with `saved'."
  (interactive)
  (save-excursion
    (push-mark (point) t t)
    (elfeed-search-untag-all 'saved)
    (pop-mark)))

(defun neo-elfeed-search-tag-unread-to-point (arg)
  "Tag articles above the point with `unread'.
If prefix ARG is present, tag articles below the point."
  (interactive "P")
  (save-excursion
    (push-mark (point) t t)
    (if arg
        (goto-char (point-max))
      (goto-char (point-min)))
    (elfeed-search-tag-all-unread)
    (pop-mark)))

(defun neo-elfeed-search-untag-unread-to-point (arg)
  "Untag articles above the point, removing `unread'.
If prefix ARG is present, tag articles below the point."
  (interactive "P")
  (save-excursion
    (push-mark (point) t t)
    (if arg
        (goto-char (point-max))
      (goto-char (point-min)))
    (elfeed-search-untag-all-unread)
    (pop-mark)))

(defun neo-elfeed-search-untag-unread-everything (arg)
  "Untag all articles, removing `unread'.
If prefix ARG is present, tag articles as `unread'."
  (interactive "P")
  (save-excursion
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (if arg
        (elfeed-search-tag-all-unread)
      (elfeed-search-untag-all-unread))
    (pop-mark)))

(use-package elfeed
  :ensure t
  :commands (elfeed)
  :bind (("C-c n" . elfeed)
         :map elfeed-search-mode-map
         ("+" . elfeed-search-tag-all)
         ("_" . elfeed-search-untag-all)
         ("=" . neo-elfeed-search-tag-saved)
         ("-" . neo-elfeed-search-untag-saved)
         ("A" . neo-elfeed-search-untag-unread-everything)
         ("R" . neo-elfeed-search-untag-unread-to-point)
         ("U" . neo-elfeed-search-tag-unread-to-point))
  :config (elfeed-org))

(use-package elfeed-org
  :ensure t
  :commands (elfeed-org)
  :config
  (setq rmh-elfeed-org-files
        (list (no-littering-expand-etc-file-name "elfeed.org.gpg"))))

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
  :if (file-exists-p "~/.emacs.d/dev/geolocation")
  :load-path "~/.emacs.d/dev/geolocation"
  :bind (("C-c ql" . neo-open-map-at-location)
         ("C-c qp" . geolocation-update-position))
  :commands (geolocation-update-position
             geolocation-get-position
             geolocation-scan-wifi)
  :init
  (add-hook 'geolocation-update-hook #'geolocation-update-calendar))

(use-package geolocation
  :if (not (file-exists-p "~/.emacs.d/dev/geolocation"))
  :ensure t
  :bind (("C-c ql" . neo-open-map-at-location)
         ("C-c qp" . geolocation-update-position))
  :commands (geolocation-update-position
             geolocation-get-position
             geolocation-scan-wifi)
  :init
  (add-hook 'geolocation-update-hook #'geolocation-update-calendar))

(use-package forecast
  :ensure t
  :bind (("C-c qf" . forecast))
  :config (setq forecast-units 'us
                forecast-api-key
                (auth-source-pick-first-password
                 :host "api.darksky.net"
                 :user "forecast.el")))

(use-package wolfram
  :ensure t
  :bind (("C-c ew" . wolfram-alpha))
  :config (setq wolfram-alpha-app-id
                (auth-source-pick-first-password
                 :host "api.wolframalpha.com"
                 :user "wolfram.el")))
