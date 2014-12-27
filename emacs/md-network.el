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
    (message "test1")
    (delete-process (car (car md-server-clients)))
    (message "test2")
    (setq md-server-clients (cdr md-server-clients)))
  (message "test3")
  (delete-process "mandimus-eval-server"))

(defun md-server-filter (proc string)   
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
      (setq result
            (condition-case err
                (with-timeout (md-server-eval-timeout (message "Timeout exceeded"))
                  (eval (car (read-from-string command))))
              (error (message "Mandimus error: [%S] in [%S]" (error-message-string err) command))))
      (process-send-string proc (format "%S\n" result))
      (md-server-log  (substring message 0 index) proc)
      (setq message (substring message index)))
    (setcdr pending message)))

(defun md-server-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq md-servers-clients (assq-delete-all proc md-server-clients))
    (md-server-log (format "client %s has quit" proc))))

;;from server.el
(defun md-server-log (string &optional client)
  "If a *mandimus-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*mandimus-server*")
      (with-current-buffer "*mandimus-server*"
        (goto-char (point-max))
        (insert (current-time-string)
                (if client (format " %s:" client) " ")
                string)
        (or (bolp) (newline)))))

;; always run so mandimus can hook up
(condition-case nil
    (md-server-start)
  (file-error (message "ERROR: Mandimus server already running!")))
