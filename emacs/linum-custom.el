(defvar-local etc-enabled-line-numbers nil)

;; before we had native line numbering
(when (< emacs-major-version 26)
  (load-file "~/etc/emacs/linum-relative-custom.el")
  (linum-relative-on)
  (with-eval-after-load "linum"
    ;; set `linum-delay' so that linum uses `linum-schedule' to update linums.
    (setq linum-delay t)

    ;; create a new var to keep track of the current update timer.
    (defvar-local my-linum-current-timer nil)

    ;; rewrite linum-schedule so it waits for 1 second of idle time
    ;; before updating, and so it only keeps one active idle timer going
    (defun linum-schedule ()
      (when (timerp my-linum-current-timer)
        (cancel-timer my-linum-current-timer))
      (setq my-linum-current-timer
            (run-with-idle-timer 0.1 nil #'linum-update-current))))
  (global-linum-mode))

(defun etc-enable-line-numbers ()
  (when (and (>= emacs-major-version 26)
             (not etc-enabled-line-numbers)) ;; was seeing weird 100% cpu usage w/o this
    (setq-default display-line-numbers-type 'visual)
    (setq display-line-numbers-type 'visual)
    (global-display-line-numbers-mode 1)
    (setq etc-enabled-line-numbers t)
    ))

(etc-enable-line-numbers)
;; without this magit-status for some unknown reason won't display why numbers...
;; you have to toggle the mode after the buffer is created for them to show up
(add-hook 'buffer-list-update-hook #'etc-enable-line-numbers)
