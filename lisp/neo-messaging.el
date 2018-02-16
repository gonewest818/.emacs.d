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
  (let ((ercpass "~/.emacs.d/.erc-auth")) ; <== (setq my-freenode-pass "...")
    (if (file-readable-p ercpass)
        (progn
          (load ercpass)
          (setq erc-nickserv-passwords
                `((freenode (("gonewest"    . ,my-freenode-pass)
                             ("gonewest818" . ,my-freenode-pass)))
                  (QuakeNet (("gonewest818" . ,my-quakenet-pass))))))))

  ;; Convenience key bindings
  (global-set-key "\C-cef" (lambda ()
                             (interactive)
                             (erc
                              :server   "irc.freenode.net"
                              :port     "6667"
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
                              :password my-gitter-pass)))

  :config
  (require 'tls)

  (setq erc-join-buffer 'bury) ; useful especially on reconnect

  ;; Automate nickserv logins
  (push 'services erc-modules)
  (erc-update-modules)
  (setq erc-prompt-for-nickserv-password nil)

  ;; Automate joining channels at startup
  (setq erc-autojoin-channels-alist '(("freenode" "#clojure"
                                                  "#clojurescript"
                                                  "#clojure-beginners"
                                                  "#clojure-emacs"
                                                  "#emacs"
                                                  "#datomic"
                                                  "#leiningen")
                                      ("QuakeNet" "#overwatch")))

  ;; Track mode configuration
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-track-exclude-server-buffer t))
