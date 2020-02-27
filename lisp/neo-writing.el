;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRITING TOOLS

(use-package lorem-ipsum
  :ensure t
  :bind (("C-c wl" . lorem-ipsum-insert-paragraphs)
         ("C-c ws" . lorem-ipsum-insert-sentences)))

(use-package writegood-mode
  :ensure t
  :hook ((text-mode . writegood-mode)
         (org-mode  . writegood-mode))
  :bind (("C-c wg"  . writegood-grade-level)
         ("C-c we"  . writegood-reading-ease)))

(use-package flyspell
  :commands (flyspell-mode)
  :hook ((text-mode . flyspell-mode)
         (org-mode  . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :bind (:map flyspell-mouse-map
              ([mouse-2] . nil) ; can't do this on track pad
              ([mouse-3] . #'flyspell-correct-word)))
