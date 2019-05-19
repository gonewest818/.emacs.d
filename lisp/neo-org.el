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
  (setq org-directory "~/Dropbox/org")
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"))

(use-package ox-md
  :after (org))

