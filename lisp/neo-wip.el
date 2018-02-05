;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

