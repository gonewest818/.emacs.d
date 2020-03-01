;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BROWSING

(defun neo-elfeed-search-tag-saved ()
  "Tag article at point with `saved'."
  (interactive)
  (save-excursion
    (push-mark (point) t t)
    (elfeed-search-tag-all 'saved)
    (pop-mark)))

(defun neo-elfeed-search-untag-saved ()
  "Tag article at point with `saved'."
  (interactive)
  (save-excursion
    (push-mark (point) t t)
    (elfeed-search-untag-all 'saved)
    (pop-mark)))

(defun neo-elfeed-search-tag-unread-to-point (arg)
  "Tag articles above the point with `unread'.
If prefix ARG is present, tag articles below the point."
  (interactive "P")
  (save-excursion
    (push-mark (point) t t)
    (if arg
        (goto-char (point-max))
      (goto-char (point-min)))
    (elfeed-search-tag-all-unread)
    (pop-mark)))

(defun neo-elfeed-search-untag-unread-to-point (arg)
  "Untag articles above the point, removing `unread'.
If prefix ARG is present, tag articles below the point."
  (interactive "P")
  (save-excursion
    (push-mark (point) t t)
    (if arg
        (goto-char (point-max))
      (goto-char (point-min)))
    (elfeed-search-untag-all-unread)
    (pop-mark)))

(defun neo-elfeed-search-untag-unread-everything (arg)
  "Untag all articles, removing `unread'.
If prefix ARG is present, tag articles as `unread'."
  (interactive "P")
  (save-excursion
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (if arg
        (elfeed-search-tag-all-unread)
      (elfeed-search-untag-all-unread))
    (pop-mark)))

(use-package elfeed
  :ensure t
  :commands (elfeed)
  :bind (("C-c n" . elfeed)
         :map elfeed-search-mode-map
         ("+" . elfeed-search-tag-all)
         ("_" . elfeed-search-untag-all)
         ("=" . neo-elfeed-search-tag-saved)
         ("-" . neo-elfeed-search-untag-saved)
         ("A" . neo-elfeed-search-untag-unread-everything)
         ("R" . neo-elfeed-search-untag-unread-to-point)
         ("U" . neo-elfeed-search-tag-unread-to-point))
  :config (elfeed-org))

(use-package elfeed-org
  :ensure t
  :commands (elfeed-org)
  :config
  (setq rmh-elfeed-org-files
        (list (no-littering-expand-etc-file-name "elfeed.org.gpg"))))

(use-package wolfram
  :ensure t
  :bind (("C-c ew" . wolfram-alpha))
  :config (setq wolfram-alpha-app-id
                (auth-source-pick-first-password
                 :host "api.wolframalpha.com"
                 :user "wolfram.el")))
