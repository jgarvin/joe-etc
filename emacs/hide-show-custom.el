(add-hook 'prog-mode-hook
          (lambda () (hs-minor-mode)))

(define-key hs-minor-mode-map (kbd "C-c h") #'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c f") #'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c u") #'hs-hide-level)
(define-key hs-minor-mode-map (kbd "C-c H") #'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c U") #'hs-show-all)

