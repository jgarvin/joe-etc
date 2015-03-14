(defvar etc-already-shrinking nil)

(defun etc-shrink-help ()
  (unless etc-already-shrinking
    (let ((etc-already-shrinking t))
      (dolist (name-regex (list "\\*Help\\*" "\\*Help <[^>]+>\\*" "\\*Ido Completions\\*"))
        (dolist (w (window-list))
          (when (string-match-p name-regex (buffer-name (window-buffer w)))
            (fit-window-to-buffer w (/ (frame-height) 2))))))))

(add-hook 'window-configuration-change-hook #'etc-shrink-help)

;; doesn't help
;;(make-variable-frame-local 'help-xref-following)

(defun etc-help-mode-hook () )

;; (defun etc-help-mode-hook ()
;;   (set (make-local-variable 'print-length) 100))

;; This almost works but it breaks help buttons, error about selecting deleted buffer.
;; (defun etc-help-mode-hook ()
;;   ;; with this version, local is invalid function
;;   ;; (add-function :override (symbol-function 'help-buffer) (local 'current-buffer))
;;   ;; with this version (local current-buffer) is invalid function
;;   ;;(add-function :override (local (symbol-function 'current-buffer)) (symbol-function 'help-buffer))
;;   (when (eq (window-frame (get-buffer-window nil 1)) (selected-frame))
;;     (let* ((name (format "*Help <%s>*" (frame-parameter (selected-frame) 'outer-window-id)))
;;            (buf (get-buffer name)))
;;       (when buf
;;         (kill-buffer buf))
;;       (rename-buffer name))))

;; run these inbetween ;)
;;(remove-function :override (symbol-function 'help-buffer))
;;(advice-remove 'help-buffer '(local current-buffer))
(add-hook 'help-mode-hook #'etc-help-mode-hook)
