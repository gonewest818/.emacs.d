;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT


;;; MINIATURE-CALENDAR, as a splash screen
(let ((mincal-path (concat user-emacs-directory "dev/mincal.el")))
  (when (file-directory-p mincal-path)
    (add-to-list 'load-path mincal-path)
    (use-package mincal
      :pin manual
      :commands (mincal-display mincal-retrieve)
      :bind ("C-c qm" . mincal-display))))

