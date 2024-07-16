;; things like 'less' only work in real terminals
(setenv "PAGER" "cat")

;; (require 'subr-x)

;; (let ((s "*special*")) (string-match "\\*\\([^*]+\\)\\*\\'" s) (match-string 1 s))

(defun etc-shell-name ()
  (let ((name (buffer-name (current-buffer))))
    (when (string-match "\\*\\([^*]+\\)\\*\\'" name)
      (setq name (match-string 1 name)))
    (concat "$" name)))

;; emacs default
;; (setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")

;; (setq fedora-shell-pattern "\\(\\[.+?@.+? .+?\\] *\\)")

;; (setq ubuntu-shell-pattern "\\[.*?@.*?:<.*?>\\]")

;; (setq shell-prompt-pattern
;;       (concat
;;        "\\(" fedora-shell-pattern "\\)"
;;        "\\|"
;;        "\\(" ubuntu-shell-pattern "\\)"
;;        "\\|"
;;        "\\(" shell-prompt-pattern "\\)"
;;        ))

;; (let ((s "[jgarvin@jgarvin:<~/etc/emacs>]$"))
;;   (string-match shell-prompt-pattern s))

;; (progn
;;   (string-match "^[A-Za-z0-9]+?@[A-Za-z0-9]+?:\\([-/~A-Za-z0-9]+\\)\\$"
;;                 "user@host:~/miniquad/target/wasm32-unknown-unknown/release$")
;;   (match-string 1 "user@host:~/miniquad/target/wasm32-unknown-unknown/release$")
;;   )

(defun etc-shell-mode-hook ()
  ;; (rename-buffer (generate-new-buffer-name (etc-shell-name)))
  ;;(rename-uniquely)
  ;; make it so I can hit enter on error messages from gcc
  ;; to open the file at that location
  ;;(compilation-shell-minor-mode 1)
  (shell-dirtrack-mode 0)
  (setq dirtrack-list '("^[-_A-Za-z0-9]+?@[-_A-Za-z0-9]+?:\\([-_/~A-Za-z0-9]+\\)\\$" 1))
  (dirtrack-mode 1)
  ;;(setq dirtrack-list '("^[^@:\n]+@[^:\n]+:\\([^]]+\\)][$#]" 1))
;;  (setq dirtrack-list '("^\\[[^<\n]*<\\([^>\n]+\\)>][$#]" 1))
  ;; (ansi-color-for-comint-mode-on)
  )

(defun etc-write-shell-history-advice (original-function &rest args)
  ;; with my settings zsh incrementally writes history as you execute
  ;; commands and also appends, so you can share history between shells.
  ;; emacs doesn't emulate these behaviors and just overwrites the whole
  ;; file, which is not what we want. We still want emacs to read ~/.zhistory
  ;; just not write to it, since zsh will take care of it.
  (unless (equal major-mode 'shell-mode)
    (apply original-function args)))

(advice-add #'comint-write-input-ring :around #'etc-write-shell-history-advice)

;;(let ((s "[</ssh:prophet@panopticon:/home/prophet>]$ "))
;; (let ((s "[prophet@panopticon:<~/etc/shell>]$ "))
;;   (string-match "^\\[[^<\n]*<\\([^>\n]+\\)>][$#]" s)
;;   (match-string-no-properties 1 s))

(defun etc-open-shell (arg)
  "Switch to shell in same folder as current buffer. If one doesn't already
exist, make one. If we're already in a shell, switch to the next shell in the
same folder. If given prefix argument always make a new shell."
  (interactive "P")
  (let* ((dir (file-truename default-directory))
         (existing (sort (-filter (lambda (x)
                                    (with-current-buffer x
                                      (let ((process (get-buffer-process (current-buffer))))
                                        (and (equal dir (file-truename default-directory))
                                             (derived-mode-p 'shell-mode)
                                             process
                                             (string-match-p ".*?\\(zsh\\|bash\\)\\'" (car (process-command process)))
                                             (not (string-match-p "run,.*" (buffer-name)))
                                             (not (string-match-p "compile,.*" (buffer-name)))))))
                                  (buffer-list))
                         (lambda (x y)
                           (string< (buffer-name x)
                                    (buffer-name y))))))
    (message "%S" existing)
    (if (and existing (not arg))
        (let ((pos (position (current-buffer) existing)))
          (if pos
              (switch-to-buffer (nth (% (1+ pos) (length existing)) existing))
            (switch-to-buffer (car existing))))
      (shell (generate-new-buffer-name "*shell*")))))

;; switched to eshell
(global-set-key (kbd "C-z") #'etc-open-shell)

(define-key shell-mode-map (kbd "C-c C-z") #'self-insert-command)

(add-hook 'shell-mode-hook #'etc-shell-mode-hook)


;; (defun etc-setup-delayed-truncate (&optional unused)
;;   (unless etc-next-truncate-allowed-timer
;;     (etc-comint-truncate)))

;; ;; If you truncate continously it causes too much performance trouble,
;; ;; the truncation starves I/O to all other processes, so mandimus gets
;; ;; disconnected.
;; (add-hook 'comint-output-filter-functions #'etc-setup-delayed-truncate)
