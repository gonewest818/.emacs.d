;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c a"  . org-agenda)
         ("C-c l"  . org-store-link)
         ("C-c c"  . org-capture))
  :init
  (setq org-directory "~/Box/neilo/org")
  (setq org-agenda-files '("~/Box/neilo/org"))
  (setq org-refile-targets
        '((nil :maxlevel . 2)
          (org-agenda-files :maxlevel . 2)))
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  (setq org-highest-priority ?A
        org-lowest-priority  ?D
        org-default-priority ?C)
  (setq org-capture-templates
        '(("t" "ToDo" entry
           (file+headline "~/Box/neilo/org/index.org" "Unfiled Tasks")
           "* TODO %?\n  %t\n  %i\n  %a")
          ("s" "Shopping Item" entry
           (file+headline "~/Box/neilo/org/index.org" "Shopping")
           "* BUY %?\n  %t\n")
          ("j" "Journal" entry
           (file+olp+datetree "~/Box/neilo/org/journal.org")
           "* TODO %? %^g\n  %t\n  %i\n")
          ("p" "Project Log" entry
           (file+olp+datetree "~/Box/neilo/org/project.org")
           "* TODO %? %^g\n  %t\n  %i\n"))))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:name "Today" :time-grid t)
          (:name "Important" :priority "A")
          (:name "Next" :priority<= "B")
          (:name "In Progress" :todo "IN-PROGRESS")
          (:name "Job" :tag "job")
          (:name "Personal" :tag "personal")))
  (org-super-agenda-mode))

(use-package org-web-tools
  :ensure t
  :bind (("C-c il" . org-web-tools-insert-link-for-url)
         ("C-c ie" . org-web-tools-insert-web-page-as-entry)))

(use-package verb
  :ensure t
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package ox-hugo
  :ensure t
  :after ox)
