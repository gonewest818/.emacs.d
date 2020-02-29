;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c a"  . org-agenda)
         ("C-c l"  . org-store-link)
         ("C-c c"  . org-capture))
  :init
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files '("~/Dropbox/org"))
  (setq org-refile-targets
        '((nil :maxlevel . 2)
          (org-agenda-files :maxlevel . 2)))
  (setq org-capture-templates
        '(("t" "ToDo" entry
           (file+headline "~/Dropbox/org/index.org" "Unfiled Tasks")
           "* TODO %?\n  %t\n  %i\n  %a")
          ("s" "Shopping Item" entry
           (file+headline "~/Dropbox/org/index.org" "Shopping")
           "* BUY %?\n  %t\n")
          ("j" "Journal ToDo" entry
           (file+olp+datetree "~/Dropbox/org/journal.org")
           "* TODO %? %^g\n  %t\n  %i\n"))))

(use-package ox-md
  :after ox)

(use-package ox-hugo
  :ensure t
  :after ox)
