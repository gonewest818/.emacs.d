;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :commands (org
             org-capture
             org-mode
             orgtbl-mode
             ;update-org-hours
             ;my-term-agenda
             ;dired-notes
             ;jump-to-org-agenda
             )
  :init
  (setq org-directory "~/Dropbox/org"))

(use-package ox-md
  :after (org))

(use-package ox-hugo
  :ensure t
  :after ox)
