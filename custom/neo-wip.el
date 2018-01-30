;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT

;;; DIMMER: https://github.com/gonewest818/dimmer.el
(if (setq nokamoto-dimmer-use-dev t)    ; set true to use dev version
    (let ((dimmer-path (concat user-emacs-directory "dev/dimmer.el")))
      (when (file-directory-p dimmer-path)
        (add-to-list 'load-path dimmer-path)
        (use-package dimmer
          :pin manual
          :config
          (setq dimmer-fraction 0.33)
          (dimmer-mode t))))
  (use-package dimmer
    :ensure t
    :config
    (setq dimmer-fraction 0.33)
    (dimmer-mode t)))

;;; MINIATURE-CALENDAR, as a splash screen
(let ((mincal-path (concat user-emacs-directory "dev/mincal.el")))
  (when (file-directory-p mincal-path)
    (add-to-list 'load-path mincal-path)
    (use-package mincal
      :pin manual
      :commands (mincal-display mincal-retrieve)
      :bind ("C-c am" . mincal-display))))

;;; DASHBOARD: https://github.com/gonewest818/emacs-dashboard
(defun nokamoto-dashboard-config (&rest label)
  (setq dashboard-banner-logo-title (concat "Emacs " emacs-version
                                            " | " (current-time-string)
                                            (if label (concat " | " label))))
  (setq dashboard-startup-banner (mincal-retrieve))
  (setq dashboard-image-banner-max-width 350)
  (setq dashboard-image-banner-max-height 350)
  (setq dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-setup-startup-hook))

(if (setq nokamoto-dashboard-use-dev nil) ; set true to use dev version
    (let ((dashboard-path (concat user-emacs-directory "dev/emacs-dashboard")))
      (when (file-directory-p dashboard-path)
        (add-to-list 'load-path dashboard-path)
        (use-package dashboard
          :pin manual
          :config (nokamoto-dashboard-config "*DEV*"))))
  (use-package dashboard
    :ensure t
    :config (nokamoto-dashboard-config)))

