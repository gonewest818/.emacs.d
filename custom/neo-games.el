;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAMES

(use-package adafruit-wisdom
  :ensure t
  :commands (adafruit-wisdom adafruit-wisdom-select)
  :bind ("C-c aw" . adafruit-wisdom))

(use-package gnugo
  :ensure t
  :pin melpa-stable
  :commands (gnugo
             gnugo-image-display-mode
             gnugo-imgen-create-xmps)
  :hook (gnugo-start-game . gnugo-image-display-mode)
  :config
  (setq gnugo-option-history (list "--komi 5.5 --boardsize 13"))
  (setq gnugo-xmps #'gnugo-imgen-create-xmps))

(use-package 2048-game
  :ensure t
  :defer t
  :commands (2048-game))
