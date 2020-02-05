;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MESSAGING

(use-package alert
  :ensure t
  :commands (alert alert-add-rule)
  :config
  (when (string-equal system-type "darwin")
    (setq alert-default-style 'notifier)))

(use-package erc
  :commands erc
  :bind ("C-c ee" . erc)
  :init
  ;; Keep passwords out of GitHub
  (setq erc-nickserv-passwords
        `((freenode
           (("gonewest"    . ,(neo/secret "irc.freenode.net" "gonewest"))
            ("gonewest818" . ,(neo/secret "irc.freenode.net" "gonewest818"))))
          (QuakeNet
           (("gonewest818" . ,(neo/secret "irc.quakenet.org" "gonewest818"))))))

  ;; Convenience key bindings
  (global-set-key "\C-cef" (lambda ()
                             (interactive)
                             (erc-tls
                              :server   "irc.freenode.net"
                              :port     "6697"
                              :nick     "gonewest818")))
  (global-set-key "\C-ceq" (lambda ()
                             (interactive)
                             (erc
                              :server   "irc.quakenet.org"
                              :port     "6667"
                              :nick     "gonewest818")))
  (global-set-key "\C-ceg" (lambda ()
                             (interactive)
                             (erc-tls
                              :server   "irc.gitter.im"
                              :port     "6697"
                              :nick     "gonewest818"
                              :password (neo/secret "irc.gitter.im"))))

  :config
  (require 'tls)

  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT") ; or try erc-hide-list
        erc-lurker-threshold-time 3600)
  (setq erc-join-buffer 'bury) ; useful especially on reconnect

  ;; Automate nickserv logins
  (push 'services erc-modules)
  (erc-update-modules)
  (setq erc-prompt-for-nickserv-password nil)

  ;; Automate joining channels at startup
  (setq erc-autojoin-channels-alist '(("freenode" "#docker"
                                                  "#clojure"
                                                  "#clojurescript"
                                                  "#clojure-emacs"
                                                  "#emacs"
                                                  "#datomic"
                                                  "#leiningen")
                                      ("QuakeNet" "#overwatch")))

  ;; Track mode configuration
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-track-exclude-server-buffer t))

(use-package slack
  :ensure t
  :commands (slack-start)
  :bind (("C-x j" . slack-select-rooms)
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
  (if-let ((secret (split-string (neo/secret "slack.com") "/")))
      (slack-register-team
       :name "novaalumn"
       :default t
       :client-id (nth 0 secret)
       :token (nth 1 secret)
       :subscribed-channels '(novaops general)
       :full-and-display-names t)))
