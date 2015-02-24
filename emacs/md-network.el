;; needed for with-timeout
(require 'timer)

(defvar md-server-clients '())
(defvar md-server-eval-timeout 5)

(defun md-server-start nil
  (interactive)
  (unless (process-status "mandimus-eval-server")
    (make-network-process :name "mandimus-eval-server"
                          :server t
                          :service 23233
                          :buffer "*mandimus-server*"
                          :family 'ipv4
                          :sentinel 'md-server-sentinel
                          :filter 'md-server-filter)
    (setq md-server-clients '())))

(defun md-server-stop nil
  (interactive)
  (while md-server-clients
    (delete-process (car (car md-server-clients)))
    (setq md-server-clients (cdr md-server-clients)))
  (delete-process "mandimus-eval-server"))

(defun md-server-restart ()
  (message "restarting mandimus server")
  (when (process-status "mandimus-eval-server")
    (md-server-stop))
  (md-server-start))

(defun md-server-filter (proc string)   
  (or (catch 'restart
        (let ((pending (assoc proc md-server-clients))
              message
              index
              command
              result)
          ;;create entry if required
          (unless pending
            (setq md-server-clients (cons (cons proc "") md-server-clients))
            (setq pending  (assoc proc md-server-clients)))
          (setq message (concat (cdr pending) string))
          (while (setq index (string-match "\n" message))
            (setq index (1+ index))
            (setq command (substring message 0 index))
            (unwind-protect
                (setq result
                      (condition-case err
                          (with-timeout (md-server-eval-timeout
                                         (progn
                                           (message "Timeout exceeded while running: [%S]" command)
                                           (throw 'restart nil)))
                            (eval (car (read-from-string command))))
                        (user-error (message "Error: %s" (error-message-string err)))
                        (error (message "Mandimus error: [%S] in [%S]" (error-message-string err) command))))
              ;; We always want to send the newline because the client will block until
              ;; it receives it.
              (setq result (format "%S" result))
              ;; because newline is the protocol delimeter we have to nuke it
              (setq result (replace-regexp-in-string "\n" "" result))
              (setq result (concat result "\n"))
              (process-send-string proc result)
              (setq message (substring message index))
              (md-server-log command proc)
              (setcdr pending message)))
          t))
      (md-server-restart)))

(defun md-server-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq md-servers-clients (assq-delete-all proc md-server-clients))
    (md-server-log (format "client %s has quit" proc))))

;;from server.el
(defun md-server-log (string &optional client)
  "If a *mandimus-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*mandimus-server*")
      (with-current-buffer "*mandimus-server*"
        (unless buffer-read-only
          (read-only-mode t))
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (current-time-string)
                  (if client (format " %s:" client) " ")
                  string)
          (or (bolp) (newline))))))

;; always run so mandimus can hook up
(condition-case nil
    (md-server-start)
  (file-error (message "ERROR: Mandimus server already running!")))

(require 'buffer-tail)
(toggle-buffer-tail "*mandimus-server*" "on")
