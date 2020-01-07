;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :init
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files '("~/Dropbox/org"))
  (setq org-capture-templates
        '(("t" "ToDo" entry
           (file+headline "~/Dropbox/org/index.org" "Unfiled Tasks")
           "* TODO %?\nSCHEDULED: %t\n  %i\n  %a")
          ("s" "Shopping Item" entry
           (file+headline "~/Dropbox/org/index.org" "Shopping")
           "* BUY %?\nSCHEDULED: %t\n")
          ("d" "Entries with Deadlines (beorg)")
          ("dt" "ToDo with Deadline" entry
           (file+headline "~/Dropbox/org/index.org" "Unfiled Tasks")
           "* TODO %?\nDEADLINE: %(org-time-stamp nil) SCHEDULED: %t\n  %i\n  %a")
          ("ds" "Shopping Item with Deadline" entry
           (file+headline "~/Dropbox/org/index.org" "Shopping")
           "* BUY %?\nDEADLINE: %(org-time-stamp nil) SCHEDULED: %t\n")
          ("dj" "Journal ToDo with Deadline" entry
           (file+datetree "~/Dropbox/org/journal.org")
           "* TODO %? %^g\nDEADLINE: %(org-time-stamp nil) SCHEDULED: %t\n  %i\n")
          ("j" "Job Search Journal")
          ("jl" "Journal Log" entry
           (file+datetree "~/Dropbox/org/journal.org")
           "* %? %^g\n  LOGGED: %U\n  %i\n")
          ("jt" "Journal ToDo" entry
           (file+datetree "~/Dropbox/org/journal.org")
           "* TODO %? %^g\nSCHEDULED: %t\n  %i\n"))))

(use-package ox-md
  :after ox)

(use-package ox-hugo
  :ensure t
  :after ox)
