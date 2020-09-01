;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON DEVELOPMENT

(use-package python-environment
  :ensure t
  :init
  (setq python-environment-directory "~/python-venv/")
  (setq python-environment-default-root-name "company-jedi"))

(use-package company-jedi
  :quelpa (company-jedi
           :fetcher github
           :repo "emacsorphanage/company-jedi"
           :branch "master")
  :ensure t
  :after (company python-environment)
  :init
  (add-to-list 'company-backends 'company-jedi))

