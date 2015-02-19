;; TODO: update the frame/buffer names to reflect $PWD,
;; get it out of /proc/$pid/environ

;; TODO: mini-window lets you edit commands, then 'submit'
;; to the real ansi-term window. But how to do completion?

(defun etc-term-mode-setup ()
  ;; make the frames with terminals easy to select
  (setq frame-title-format "term: %b")
  ;; without this you can overwrite the prompt and emacs doesn't know where
  ;; the home key should take you
  (setq term-prompt-regexp "^--([^)]*)-- ")
  ;; let me click on gcc errors and click them
  ;; this doesn't like my prompt
  ;;(compilation-minor-mode)
  
  )

(add-hook 'term-mode-hook 'etc-term-mode-setup)

(defun etc-terminal-name ()
  (concat "$" (file-name-directory (buffer-file-name (current-buffer)))))

(defun etc-start-terminal ()
  (interactive)
  (ansi-term "zsh" (etc-terminal-name)))

(defun etc-start-or-open-terminal ()
  (interactive)
  (let ((buf (get-buffer (concat "*" (etc-terminal-name) "*"))))
    (if buf
	(switch-to-buffer buf)
      (etc-start-terminal))))

(defun etc-start-or-open-top ()
  (interactive)
  (let ((buf (get-buffer "*Proced*")))
    (if buf
	(switch-to-buffer buf)
    (proced)
    (delete-other-windows))))

(defvar-local etc-last-read-only 0)

;; (setq-default etc-last-read-only -1)

(defun etc-make-term-output-read-only ()
  (interactive)
  (let ((start-point (+ 1 etc-last-read-only))
        (end-prompt (process-mark (get-buffer-process (current-buffer)))))
    (add-text-properties start-point end-prompt '(read-only t))
    (setq etc-last-read-only end-prompt)))

;; (defvar-local etc-original-process-filter
;;   (process-filter (get-buffer-process (current-buffer))))

;; setup process filter to make things read-only and call originalrewrite 

;; (defadvice term-line-mode (after etc-term-line-mode-advice activate)
;;   (when (process-live-p)
;;     (read-only-mode t)))

;; (defadvice term-char-mode (after etc-term-char-mode-advice activate)
;;   (read-only-mode nil))

;; (defun send-sigwinch ()
;;   (when (equal major-mode 'term-mode)
;;     ))

;; (add-hook 'window-size-change-functions 'send-sigwinch)
