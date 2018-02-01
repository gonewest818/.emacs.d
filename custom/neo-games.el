;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAMES

(use-package adafruit-wisdom
  :ensure t
  :commands (adafruit-wisdom adafruit-wisdom-select)
  :bind ("C-c aw" . adafruit-wisdom))

(use-package gnugo
  :ensure t
  :pin gnu
  :bind ("C-c ag" . gnugo)
  :config
  (setq gnugo-option-history (list "--komi 5.5 --boardsize 13"))
  (setq gnugo-xpms #'gnugo-imgen-create-xpms)
  (add-hook 'gnugo-start-game-hook #'gnugo-image-display-mode))

(use-package 2048-game
  :ensure t
  :defer t
  :bind ("C-c a2" . 2048-game))
