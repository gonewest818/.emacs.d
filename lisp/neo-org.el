;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

(use-package org
  :mode "\\.org$"
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :commands (orgtbl-mode))

(use-package ox-md
  :after (org))
