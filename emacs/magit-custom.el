(setq magit-diff-refine-hunk 'all)

;; from: https://emacs.stackexchange.com/questions/19672/magit-gerrit-push-to-other-branch
(defun magit-push-to-gerrit ()
  (interactive)
  (magit-git-command-topdir "git push origin HEAD:refs/for/master"))

(transient-append-suffix 'magit-push "p"
  '("m" "Push to gerrit" magit-push-to-gerrit))

(defun etc-show-origin-master ()
  (interactive)
  (magit-find-file-other-window "origin/master" (buffer-file-name)))

(global-set-key (kbd "C-c m h") #'magit-find-file)
(global-set-key (kbd "C-c m o") #'etc-show-origin-master)





;; ;; Code attempting to get rid of it git index lock problem
;; (defvar postpone-auto-revert-buffers nil)

;; (defvar postpone-auto-revert-interval nil)

;; (defadvice auto-revert-buffers (around maybe-postpone-auto-revert-buffers)
;;   "Delay `auto-revert-buffers' if `postpone-auto-revert-buffers' is non-nil."
;;   (if postpone-auto-revert-buffers
;;       ;; Do not run `auto-revert-buffers', but make its timer run more
;;       ;; frequently in the meantime, so that it will run promptly once
;;       ;; it's safe.  Remember the original `auto-revert-interval'.
;;       (unless postpone-auto-revert-interval
;;         (setq postpone-auto-revert-interval auto-revert-interval)
;;         (setq auto-revert-interval 0.5)
;;         (auto-revert-set-timer))
;;     ;; We are no longer postponed, so restore the original
;;     ;; `auto-revert-interval', and run `auto-revert-buffers'.
;;     (when postpone-auto-revert-interval
;;       (setq auto-revert-interval postpone-auto-revert-interval)
;;       (setq postpone-auto-revert-interval nil)
;;       (auto-revert-set-timer))
;;     ad-do-it)) ;; Run `auto-revert-buffers'.

;; (ad-activate 'auto-revert-buffers)

;; (defadvice magit-start-process (before magit-start-inhibit-revert)
;;   (setq postpone-auto-revert-buffers t))

;; (defadvice magit-process-finish (after magit-stop-inhibit-revert)
;;   (setq postpone-auto-revert-buffers nil))

;; (ad-activate 'magit-start-process)
;; (ad-activate 'magit-process-finish)
