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

(when (>= emacs-major-version 26)
  (global-display-line-numbers-mode)
  (setq display-line-numbers-type 'visual))