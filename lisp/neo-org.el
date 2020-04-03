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
  (setq org-highest-priority ?A
        org-lowest-priority  ?D
        org-default-priority ?C)
  (setq org-capture-templates
        '(("t" "ToDo" entry
           (file+headline "~/Dropbox/org/index.org" "Unfiled Tasks")
           "* TODO %?\n  %t\n  %i\n  %a")
          ("s" "Shopping Item" entry
           (file+headline "~/Dropbox/org/index.org" "Shopping")
           "* BUY %?\n  %t\n")
          ("j" "Journal" entry
           (file+olp+datetree "~/Dropbox/org/journal.org")
           "* TODO %? %^g\n  %t\n  %i\n")
          ("p" "Project Log" entry
           (file+olp+datetree "~/Dropbox/org/project.org")
           "* TODO %? %^g\n  %t\n  %i\n"))))

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (setq org-super-agenda-groups
        '((:name "Today" :time-grid t)
          (:name "Important" :priority "A")
          (:name "Next" :priority<= "B")
          (:todo "IN-PROGRESS" :order 1)
          (:todo "SCHEDULED" :order 2)
          (:tag "job" :order 3)
          (:tag "personal" :order 4)))
  (org-super-agenda-mode 1))

(use-package org-web-tools
  :ensure t
  :bind (("C-c il" . org-web-tools-insert-link-for-url)
         ("C-c ie" . org-web-tools-insert-web-page-as-entry)))

(use-package ox-md
  :after ox)

(use-package ox-hugo
  :ensure t
  :after ox)
