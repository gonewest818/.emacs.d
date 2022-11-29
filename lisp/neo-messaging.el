;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MESSAGING

(use-package alert
  :ensure t
  :commands (alert alert-add-rule)
  :config
  (when (string-equal system-type "darwin")
    (setq alert-default-style 'notifier)))

(use-package mastodon
  :ensure t
  :after quelpa-use-package
  :quelpa (mastodon :fetcher git :url "https://codeberg.org/martianh/mastodon.el")
  :bind (("C-c em" . mastodon))
  :init
  (setq mastodon-client--token-file
        (no-littering-expand-var-file-name "mastodon.plstore"))
  :config
  (setq mastodon-instance-url "https://mastodon.online"
        mastodon-active-user "neilo"))

(use-package plz
  :ensure t
  :after quelpa-use-package
  :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))

(use-package ement
  :ensure t
  :after quelpa-use-package
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el")
  :bind (("C-c ee" . ement-connect))
  :config
  (setq ement-save-sessions t))

(use-package slack
  :ensure t
  :commands (slack-start)
  :bind (("C-c es" . slack-start)
         ("C-x j" . slack-select-rooms)
         :map slack-mode-map
         ("C-c C-d" . slack-message-delete)
         ("C-c C-e" . slack-message-edit)
         ("C-c C-k" . slack-channel-leave)
         ("@" . slack-message-embed-mention))
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  (setq slack-display-team-name nil) ; ???
  (setq slack-buffer-function #'switch-to-buffer) ; ???
  (setq slack-completing-read-function #'ivy-completing-read)
  :config
  (if-let ((secret (auth-source-pick-first-password
                    :host "novaalumn.slack.com")))
      (slack-register-team
       :name "novaalumn"
       :default t
       :token secret
       :subscribed-channels '(novaops general)
       :full-and-display-names t)))
