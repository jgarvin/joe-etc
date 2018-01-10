(require 'eshell)

;; Thanks to J David Smith on Stackoverflow
;; http://emacs.stackexchange.com/a/18569/2301
(setq eshell-save-history-on-exit nil)
(defun etc-eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t',
and write out only the most recent history item, since we're going to
execute after every command."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

;; setup in comint-custom.el
;; can't use because there is no process-mark
;;(remove-hook 'eshell-output-filter-functions #'etc-setup-delayed-truncate)

;; strangely setting eshell-mode-map doesn't work
;; unless you are in an eshell-mode buffer.
(defun etc-eshell-mode-hook ()
  (eshell/addpath "~/etc/bin")
  (eshell/addpath "~/opt/bin")
  ;; make sure first folder is in history
  (push (eshell/pwd) eshell-hist-dirs)
  ;; for helm
  (require 'helm-eshell)
  (eshell-cmpl-initialize)
  (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
  ;; for me
  (define-key eshell-mode-map (kbd "<home>") #'eshell-bol)
  (define-key eshell-mode-map (kbd "<up>") #'previous-line)
  (define-key eshell-mode-map (kbd "<down>") #'next-line)
  (define-key eshell-mode-map (kbd "M-RET") #'ace-jump-mode)
  ;; smartparens ignores special buffers by default
  (smartparens-mode 1)
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "nethack")
  (add-to-list 'eshell-visual-commands "iotop")
  (add-to-list 'eshell-visual-commands "watch")
  (setenv "PAGER" "cat")
  ;; apparently have to add this locally to get it to work
  (add-hook 'eshell-post-command-hook #'etc-eshell-append-history nil t))
(add-hook 'eshell-mode-hook #'etc-eshell-mode-hook)

;; (setq eshell-visual-commands (delq "top" 'eshell-visual-commands))
;;*--- track cd history ------------------------------------------------*/
(defvar-local eshell-hist-dirs nil)
(defvar-local eshell-hist-index 0)

(defun etc-eshell-update-hist-dir ()
  (when (not (equal (car (last eshell-hist-dirs)) (eshell/pwd)))
    (push (eshell/pwd) eshell-hist-dirs)))

(add-hook 'eshell-directory-change-hook #'etc-eshell-update-hist-dir)

(defun eshell-forward (n)
  (unless eshell-hist-dirs
    (user-error "eshell-hist-dirs is empty, cd a few times"))
  (let ((dirs eshell-hist-dirs))
    (prog1 (eshell/cd (nth (setq eshell-hist-index
                                 (if (> n 0)
                                     (min (- (length eshell-hist-dirs) 1) (+ eshell-hist-index n))
                                   (max 0 (+ eshell-hist-index n))))
                           dirs))
      (setq eshell-hist-dirs dirs))))

(defun eshell/b ()
  (eshell-forward 1))

(defun eshell/f ()
  (eshell-forward -1))

(setq eshell-history-size 10000)

(setq eshell-aliases-file "~/etc/emacs/eshell-aliases")

(defun etc-open-eshell (arg)
  "Switch to shell in same folder as current buffer. If one doesn't already
exist, make one. If we're already in a shell, switch to the next shell in the
same folder. If given prefix argument always make a new shell."
  (interactive "P")
  (let* ((dir (file-truename default-directory))
         (existing (sort (-filter (lambda (x)
                                    ;; had to insert some extra conditions
                                    ;; before the with-current-buffer call,
                                    ;; because sometimes it can cause tramp
                                    ;; to try and ask for passwords to nonexistent buffers
                                    ;; that I think might have been minibuffers based
                                    ;; on the output before crashing.
                                    ;; ... probably some very weird emacs bug
                                    (and (not (minibufferp x))
                                         (not (buffer-live-p x))
                                         (with-current-buffer x
                                           (and (equal dir (file-truename default-directory))
                                                (derived-mode-p 'eshell-mode)))))
                                  (buffer-list))
                         (lambda (x y)
                           (string< (buffer-name x)
                                    (buffer-name y))))))
    (if (and existing (not arg))
        (let ((pos (position (current-buffer) existing)))
          (if pos
              (progn
                (switch-to-buffer (nth (% (1+ pos) (length existing)) existing)))
            (switch-to-buffer (car existing))))
      (eshell (generate-new-buffer-name "eshell-")))))

(global-set-key (kbd "C-z") #'etc-open-eshell)

;; Thanks to 'Ben'
;; http://stackoverflow.com/a/28160819/50385
(defun etc-unstick-ansi-color-codes ()
  (interactive)
  ;;(end-of-buffer)
  (eshell-reset))

(remove-hook 'eshell-before-prompt-hook #'etc-unstick-ansi-color-codes)