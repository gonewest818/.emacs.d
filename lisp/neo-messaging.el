;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MESSAGING

(use-package alert
  :ensure t
  :commands (alert alert-add-rule)
  :config
  (when (string-equal system-type "darwin")
    (setq alert-default-style 'notifier)))

(use-package plz
  :ensure t
  :after quelpa-use-package
  :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))

(use-package ement
  :ensure t
  :after quelpa-use-package
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

(use-package erc
  :commands (erc erc-tls)
  :bind ("C-c ee" . erc)
  :init
  ;; Convenience key bindings
  (global-set-key "\C-cel" (lambda ()
                             (interactive)
                             (erc-tls
                              :server   "irc.libera.chat"
                              :port     "6697"
                              :nick     "gonewest818"
                              :password (auth-source-pick-first-password
                                         :host "irc.libera.chat"
                                         :user "gonewest818"))))
  (global-set-key "\C-cef" (lambda ()
                             (interactive)
                             (erc-tls
                              :server   "irc.freenode.net"
                              :port     "6697"
                              :nick     "gonewest818"
                              :password (auth-source-pick-first-password
                                         :host "irc.freenode.net"
                                         :user "gonewest818"))))
  (global-set-key "\C-ceq" (lambda ()
                             (interactive)
                             (erc
                              :server   "irc.quakenet.org"
                              :port     "6667"
                              :nick     "gonewest818"
                              :password (auth-source-pick-first-password
                                         :host "irc.quakenet.org"
                                         :user "gonewest818"))))
  (global-set-key "\C-ceg" (lambda ()
                             (interactive)
                             (erc-tls
                              :server   "irc.gitter.im"
                              :port     "6697"
                              :nick     "gonewest818"
                              :password (auth-source-pick-first-password
                                         :host "irc.gitter.im"))))

  :config
  ;(require 'tls)

  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT") ; or try erc-hide-list
        erc-lurker-threshold-time 3600)
  (setq erc-join-buffer 'bury) ; useful especially on reconnect

  ;; Automate nickserv logins
  (push 'services erc-modules)
  (erc-update-modules)
  (setq erc-prompt-for-nickserv-password nil)

  ;; Read channel list from a separate file
  (load-file (no-littering-expand-etc-file-name "erc-channels.el.gpg"))

  ;; Track mode configuration
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-track-exclude-server-buffer t))

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
