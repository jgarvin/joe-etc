(require 'term)

(defvar-local etc-last-read-only 0)

(setq-default
 ;;tramp-default-method "ssh"          ; uses ControlMaster
 term-scroll-to-bottom-on-input t  ; always insert at the bottom
 term-scroll-to-bottom-on-output nil ; always add output at the bottom
 term-scroll-show-maximum-output t ; scroll to show max possible output
 term-completion-autolist t     ; show completion list when ambiguous
 term-input-ignoredups t           ; no duplicates in command history
 term-completion-addsuffix t       ; insert space/slash after file completion
 term-buffer-maximum-size 20000    ; max length of the buffer in lines
 ;;comint-prompt-read-only t         ; if this is t, it breaks shell-command
 term-get-old-input (lambda ( "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 term-input-ring-size 5000         ; max shell history size
 )

;; TODO: mini-window lets you edit commands, then 'submit'
;; to the real ansi-term window. But how to do completion?

(defun etc-make-term-output-read-only (&rest args) )

;; TODO: to make robust against running top, have to take into account
;; that buffer gets cleared. So instead of etc-last-read-only, maybe
;; should always run on (window-start) to (window-end) ?
;; should check to make sure etc-last-read-only is before the
;; beginning of the change passed in by hook
(defun etc-make-term-output-read-only (&rest args)
  (interactive)
  (save-restriction
    (widen)
    (let ((inhibit-modification-hooks t))
      (when (or (= 0 etc-last-read-only)
                ;; when the screen is cleared so is the buffer, so
                ;; sometimes etc-last-read-only needs to be reset
                (> etc-last-read-only (point-max)))
        (setq etc-last-read-only (point-min)))
      (let ((end-prompt))
        (save-excursion
          (save-match-data
            (goto-char (point-max))
            ;; We manually search for the term-prompt ourself because functions like term-bol
            ;; have fallback functionality that we don't want went no prompt is present that
            ;; get confused by the trailing newlines littered in the buffer.
            (when (re-search-backward term-prompt-regexp etc-last-read-only t)
              (message "Match substring: %s" (buffer-substring (match-beginning 0) (match-end 0)))
              (setq end-prompt (match-end 0)))))
        (when (and end-prompt (> end-prompt etc-last-read-only))
          (message "end-prompt: %d" end-prompt)
          (add-text-properties etc-last-read-only end-prompt
                               '(read-only t rear-nonsticky (read-only)
                                           front-sticky (read-only)))
          ;; We don't make read-only front sticky because zsh ZLE mode
          ;; (which makes arrow keys and Ctrl-R for history work) puts
          ;; a LF character at the end of the line that any text you type
          ;; gets inserted in front of.
          ;; (add-text-properties etc-last-read-only end-prompt
          ;;                      '(read-only t rear-nonsticky (read-only)))
          (let ((inhibit-read-only t))
            (remove-text-properties end-prompt (point-max) '(read-only nil)))
          (setq etc-last-read-only end-prompt))))))

;; (with-current-buffer "*scratch*"
;;   (add-text-properties (point-min) (point-max)
;;                                '(read-only t rear-sticky (read-only)
;;                                            front-sticky nil)))
  

;; (with-current-buffer "*scratch*"
;;   ;;(add-text-properties (point-min) (point-max) '(read-only t))
;;   ;; (add-text-properties (point-min) (point-max)
;;   ;;                              '(read-only t rear-sticky (read-only)
;;   ;;                                          front-sticky nil))
;;   ;; (add-text-properties (point-min) (point-max)
;;   ;;                              '(read-only t rear-sticky t
;;   ;;                                          front-sticky nil))
;;   ;; (add-text-properties (point-min) (point-max)
;;   ;;                              '(read-only t rear-sticky nil
;;   ;;                                          front-sticky nil))
;;   ;; (add-text-properties (point-min) (point-max)
;;   ;;                      '(read-only t rear-sticky nil
;;   ;;                                  front-sticky t))
;;   ;; (add-text-properties (point-min) (1- (point-max))
;;   ;;                      '(read-only t rear-sticky nil
;;   ;;                                  front-sticky t))
;;     ;; (add-text-properties (point-min) (- (point-max) 1)
;;     ;;                            '(read-only t rear-sticky (read-only)
;;     ;;                                        front-sticky nil))
;;   ;; (add-text-properties (point-min) (1- (point-max))
;;   ;;                              '(read-only t :rear-sticky t
;;   ;;                                          front-sticky nil))
;;   ;; (add-text-properties (point-min) (1- (point-max))
;;   ;;                              '(read-only t :rear-sticky nil
;;   ;;                                          front-sticky nil))
;;   ;; (add-text-properties (point-min) (1- (point-max))
;;   ;;                              '(read-only t :rear-sticky nil
;;   ;;                                          front-sticky t))
;;   (add-text-properties (point-min) (1- (point-max))
;;                                '(read-only t rear-nonsticky (read-only)))
  
;;   )

;; (with-current-buffer "*scratch*"
;;   (let ((inhibit-read-only t))
;;     (remove-text-properties (point-min) (point-max) '(read-only nil)))
;;   )

;; (with-current-buffer "*ansi-term*"
;;   (= (point) (point-max))
;;   (point))
;;   (buffer-narrowed-p))

(defun etc-term-mode-setup ()
  ;; make the frames with terminals easy to select
  (setq frame-title-format "term: %b")
  ;; without this you can overwrite the prompt and emacs doesn't know where
  ;; the home key should take you
  ;;(setq term-prompt-regexp "^--(\\(.+?:.+?:.+?\\))-- ")
  (setq term-prompt-regexp "^\\[[^<\n]*<\\([^>\n]+\\)>][$#] ")
  ;; let me click on gcc errors and click them
  ;; this doesn't like my prompt
  ;;(compilation-minor-mode)
  (add-hook 'after-change-functions #'etc-make-term-output-read-only nil t)
  )

(add-hook 'term-mode-hook 'etc-term-mode-setup)

;; (let ((s "--(23:51:master)-- "))
;;   (string-match  s)
;;   (match-string 1 s))

(defun etc-line-mode (&rest args)
  (term-line-mode))

;; Use line mode by default. Putting this in term-mode-hook doesn't work for
;; some reason.
(advice-add #'ansi-term :after #'etc-line-mode)
;;(advice-remove #'ansi-term #'etc-line-mode)

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

;; The undo history should be cleared when we send input. We shouldn't be able
;; to undo things that were inserted by the process, only things we typed.
(defun etc-clear-term-undo ()
  (setq buffer-undo-list nil))

(defun etc-inhibit-ro (original-function &rest args)
  (let ((inhibit-read-only t))
    (apply original-function args)))

(advice-add #'term-send-input :after #'etc-clear-term-undo)
(advice-add #'term-send-input :around #'etc-inhibit-ro)
(advice-add #'term-emulate-terminal :around #'etc-inhibit-ro)

;; (defmacro with-char-mode (&rest body)
;;   (declare (indent 1) (debug t))
;;   (let ((tmp (make-symbol "was-in-line-mode")))
;;     `(let ((,tmp (term-in-line-mode)))
;;        (unwind-protect
;;            (progn
;;              (term-char-mode)
;;              ,@body)
;;          (when ,tmp
;;            (term-line-mode))))))

;; (defun etc-term-previous-history ()
;;   (interactive)
;;   (let))

(define-key term-mode-map "\C-a" 'term-bol)
(define-key term-mode-map (kbd "M-p") #'term-send-up)
(define-key term-mode-map (kbd "M-n") #'term-send-down)

;; not more robust than using after change function
;;(advice-add #'term-emulate-terminal :after #'etc-make-term-output-read-only)
;;(advice-remove #'term-emulate-terminal #'etc-make-term-output-read-only)
;;(advice-remove #'term-emulate-terminal #'etc-inhibit-ro)
;;(advice-remove #'term-send-input #'etc-clear-term-undo)
;;(advice-remove #'term-send-input #'etc-inhibit-ro)

;;(global-hl-line-mode 0)

;; (with-current-buffer "*ansi-term*"
;;   (goto-char (point-max))
;;   (term-previous-prompt 1)
;;   (let ((start (point))
;;         (change))
;;     (put-text-property (1- start) start 'read-only t)
;;     (while (not (bobp))
;;       (setq change (previous-single-property-change (point) 'read-only))
;;       (if change
;;           (progn
;;             (put-text-property change start 'read-only t)
;;             (goto-char change))
;;         (goto-char (point-min))))))

;; (defvar-local etc-original-process-filter
;;   (process-filter (get-buffer-process (current-buffer))))

;; setup process filter to make things read-only and call originalrewrite 

;; (defadvice term-line-mode (after etc-term-line-mode-advice activate)
;;   (when (process-live-p)
;;     (read-only-mode t)))

;; (defadvice term-char-mode (after etc-term-char-mode-advice activate)
;;   (read-only-mode nil))

;; (defun send-sigwinch ()
;;   (when (derived-mode-p 'term-mode)
;;     ))

;; (add-hook 'window-size-change-functions 'send-sigwinch)

;; (defun etc-start-or-open-top ()
;;   (interactive)
;;   (let ((buf (get-buffer "*Proced*")))
;;     (if buf
;; 	(switch-to-buffer buf)
;;     (proced)
;;     (delete-other-windows))))
