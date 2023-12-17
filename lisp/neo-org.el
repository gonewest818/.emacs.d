;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c a"  . org-agenda)
         ("C-c l"  . org-store-link)
         ("C-c c"  . org-capture))
  :init
  (setq org-directory "~/Box/Users/Neil Okamoto/org-mode")
  (setq org-agenda-files '("~/Box/Users/Neil Okamoto/org-mode"))
  (setq org-refile-targets
        '((nil :maxlevel . 2)
          (org-agenda-files :maxlevel . 2)))
  (setq org-capture-templates
        `(("t" "Task" entry
           (file+headline ,(expand-file-name "tasks.org" org-directory) "Unfiled Tasks")
           "* TODO %?\n  %t\n  %i\n  %a")
          ("r" "Read Later" entry
           (file+headline ,(expand-file-name "reading.org" org-directory) "Reading")
           "* TODO %?\n  %t\n")
          ("p" "Pipeline" entry
           (file+olp+datetree ,(expand-file-name "pipeline.org" org-directory))
           "* TODO %? %^g\n  %t\n  %a\n")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (sqlite . t))))

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

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-global-mode)
  :config
  (setq org-ai-openai-api-token "")
  ;; (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  )

(use-package ox-md
  :after ox)

(use-package ox-hugo
  :ensure t
  :after ox)
