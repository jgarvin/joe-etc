;; -*- lexical-binding: t -*-

(require 'dash)
(require 'realgud)

(defvar etc-build-choice nil) ;; maps project name to build choice
(defvar etc-run-choice nil) ;; maps project name to run choice
(defvar etc-run-debug nil)
(defvar etc-run-valgrind nil)
(defvar-local etc-compilation-run-command nil)
(defvar-local etc-compilation-project nil)
(defvar-local etc-compilation-compile-command nil)
(defvar-local etc-compilation-invoking-buffer nil)
(defvar-local etc-run-finished-status nil)
(defvar etc-most-recent-build-buffer nil) ;; maps project name to stack of build jobs
(defvar etc-most-recent-run-buffer nil) ;; maps project name to stack of run jobs
(defvar-local etc-interrupted-once nil)

(defun etc-stop-most-recent-impl (type)
  "Stop the most recent one or build associated with this project, keeping a stack."
  (let* ((build-buffer-list-symbol (if (eq type 'build)
                                       'etc-most-recent-build-buffer
                                     'etc-most-recent-run-buffer))
         (project-name (etc-get-project))
         (build-buffer-list (alist-get project-name (symbol-value build-buffer-list-symbol) nil nil #'equal))
         (found nil)
         )
    ;; (message "==========================")
    ;; (message "%S %S %S %S " (symbol-value build-buffer-list-symbol) project-name build-buffer-list found)
    (setf build-buffer-list
         (-filter (lambda (buffer)
                    ;; (message "**************************")
                    ;; (message "%S" (list buffer
                    ;;                     (buffer-live-p buffer)
                    ;;                     (get-buffer-process buffer)
                    ;;                     (not found)))
                    (if (and (buffer-live-p buffer)
                             (get-buffer-process buffer)
                             (not found))
                        (progn
                          (setq found t)
                          (condition-case e
                              (with-current-buffer buffer
                                (if (eq type 'build)
                                    (kill-compilation)
                                  (etc-interrupt-subjob)))
                            (error (user-error "%s" e)))
                          nil)
                      (and (buffer-live-p buffer) (get-buffer-process buffer))))
                  build-buffer-list))
    )
  )

(defun etc-stop-most-recent-run ()
  (interactive)
  (etc-stop-most-recent-impl 'run))

(defun etc-stop-most-recent-build ()
  (interactive)
  (etc-stop-most-recent-impl 'build)
  )

(defun etc-push-recent-buffer (type buffer)
  (let* ((build-buffer-list-symbol (if (eq type 'build)
                                      'etc-most-recent-build-buffer
                                    'etc-most-recent-run-buffer))
         (project-name (etc-get-project))
         (existing (alist-get project-name (symbol-value build-buffer-list-symbol) nil nil #'equal))
         )
    (setf (alist-get project-name (symbol-value build-buffer-list-symbol) nil nil #'equal)
          (cons buffer existing))
    ))

(defun etc-toggle-debug ()
  (interactive)
  (setq etc-run-debug (not etc-run-debug))
  (message "Running in debugger is now: %s" etc-run-debug))

(defun etc-toggle-valgrind ()
  (interactive)
  (setq etc-run-valgrind (not etc-run-valgrind))
  (message "Running in valgrind is now: %s" etc-run-valgrind))

(defun etc-build-menu (type)
  (let* ((cmd (gensym))
         (scripts)
         (flag (if (eq type 'build) "b" "r"))
         (original-buffer (current-buffer)) ;; because the pop-up menu creates its own
         (actions))
    (with-temp-buffer
      (shell-command (format "find-builds -%s -l" flag) (current-buffer))
      (setq scripts (split-string (buffer-substring-no-properties (point-min) (point-max))
                                  (regexp-quote "\n") t)))
    (if scripts
        (progn
          (setq scripts (cl-sort scripts #'string< :key))
          (let ((chosen-script (completing-read "Choose script: " scripts)))
            (message "Chosen: %s" chosen-script)
            (etc-set-build-cmd type (file-truename chosen-script))))
      (user-error "No %s scripts found!" (if (eq type 'build) "build" "run")))))

(defun etc-build-cmd (type)
  (let ((project-name (etc-get-project)))
    (if (eq type 'build)
        (alist-get project-name etc-build-choice nil nil #'equal)
      (alist-get project-name etc-run-choice nil nil #'equal))))

(defun etc-update-or-add-alist (alist-var key value)
  ;; taken from: https://gist.github.com/j8takagi/aef5115954d40341996d7caa5a19b2d3
  "If KEY in ALIST, update VALUE of the KEY.
Unless, cons cell (KEY . VALUE) is added."
  (interactive)
  (let (aconscell (alist (symbol-value alist-var)))
   (if (setq aconscell (assoc key alist))
       (unless (equal (cdr aconscell) value)
         (setf (cdr aconscell) value))
     (set alist-var (push (cons key value) alist)))
   alist))

(defun etc-set-build-cmd (type script)
  (let ((project-name (etc-get-project)))
    (if (eq type 'build)
        (etc-update-or-add-alist 'etc-build-choice project-name script)
      (etc-update-or-add-alist 'etc-run-choice project-name script))))

(defun etc-compile (&optional arg)
  (interactive "P")
  (if arg
      (etc-build-menu 'build)
    (etc-compile-and-run-impl (etc-build-cmd 'build) nil)))

(defun etc-debug-run ()
  (interactive)
  (let ((etc-run-debug t)
        (old-val (getenv "GDB")))
    (unwind-protect
        (progn
          (setenv "GDB" "on")
          (etc-run-impl (etc-build-cmd 'run) t))
      (setenv "GDB" old-val))))

(defun etc-stale-run (&optional arg)
  (interactive "P")
  (if arg
      (etc-build-menu 'run)
    (etc-run-impl (etc-build-cmd 'run))))

(defun etc-compile-and-run ()
  (interactive)
  (etc-compile-and-run-impl (etc-build-cmd 'build) (etc-build-cmd 'run)))

(defun etc-quit-run (&optional kill-buffer)
  (interactive "P")
  (quit-window kill-buffer (selected-window)))

(defun etc-build-buffer-name (type cmd)
  (format "*%s,%s,%s*"
          (if (eq type 'build) "compile" "run")
          (file-name-base cmd)
          (etc-get-project))) ;; project name must be included to make buffer name unique

(defun etc-run-impl (cmd &optional debugging)
  (let ((rust-format-on-save t))
    (etc-save-if-necessary))
  ;; (when flycheck-inline-mode
  ;;   (flycheck-inline-clear-phantoms)
  ;;   (flycheck-inline-mode 0)
  ;;   (flycheck-inline-mode 1))
  (let* ((buff-name (etc-build-buffer-name 'run cmd))
         ;; if there is an existing run buffer and the run has finished
         ;; then recycle it. otherwise generate a new one.
         (buff-real-name
          (if (and (get-buffer buff-name)
                   (get-buffer-process buff-name))
              (generate-new-buffer-name buff-name)
            buff-name))
         ;; We temporarily customize display-buffer-alist to not pop up
         ;; a new window if the buffer is already displayed in one.
         (existing-window (get-buffer-window buff-real-name t))
         (display-buffer-alist
          (if existing-window
              (cons (cons (regexp-quote buff-real-name) (cons #'display-buffer-no-window '())) display-buffer-alist)
            display-buffer-alist)))
    ;; (message "%S %S %S" buff-name (get-buffer buff-name) (and (get-buffer buff-name) (with-current-buffer (get-buffer buff-name))))
    (when (get-buffer buff-real-name)
      (kill-buffer buff-real-name))
    (let ((default-directory (etc-get-project-root))
          (temp-file (make-temp-file (concat "/tmp/" (file-name-nondirectory cmd) "."))))
      ;; (message "directory: %S" default-directory)
      (copy-file cmd temp-file t nil nil t)
      (async-shell-command temp-file buff-real-name))
    (if existing-window
        (set-window-buffer existing-window buff-real-name))
    (with-current-buffer buff-real-name
      (setq etc-interrupted-once nil)
      (etc-push-recent-buffer 'run (current-buffer))
      (local-set-key (kbd "C-c C-k") #'etc-interrupt-subjob)
      ;; didn't work
      ;; (buffer-disable-undo (current-buffer))
      ;; trying this instead
      (setq buffer-undo-list t)
      (if debugging
          (progn
            (realgud-track-mode))
        (goto-char (point-max))
        (font-lock-mode -1)
        (buffer-disable-undo)
        ;; (setq buffer-read-only t)
        (setq buffer-undo-list t)))))

(defun etc-post-compile-run (comp-buf finish-status)
  (with-current-buffer comp-buf
    (setq etc-run-finished-status (string-trim finish-status)) ;; trailing newline
    ;;(font-lock-mode 1)
    (if (string= "finished" etc-run-finished-status)
        (when (and (derived-mode-p major-mode 'compilation-mode)
                   ;; the variable will only be set if compile is invoked by our custom command
                   ;; we don't want to do any of this if the user does M-x compile
                   etc-compilation-invoking-buffer
                   etc-compilation-run-command)
          (etc-run-impl etc-compilation-run-command)))))

(add-hook 'compilation-finish-functions #'etc-post-compile-run)

(defun etc-compile-and-run-impl (comp-command run-command &optional arg)
  (unless comp-command
    (user-error "No compile command set."))
  (let ((rust-format-on-save t))
    (etc-save-if-necessary))

  ;; Capture the current environment before any buffer switching occurs
  (let* (;; capture the environment before we switch active buffers.
         ;; this is necessary for direnv-mode to work as expected,
         ;; because we copy the compile script to tmp and run it out
         ;; of there, so it's not considered to be in the project
         ;; directory, which would clear the direnv environment.
         (current-env process-environment)
         ;; Properly merge with existing compilation-environment,
         ;; since intention of customizing the
         ;; `compilation-environment` variable normally is to inject
         ;; extra env vars specific for compiles.
         (compilation-environment (append compilation-environment current-env))
         ;; make the compilation buffer depend on the command name and the project,
         ;; this makes sure we can have multiple compiles going
         (default-directory (etc-get-project-root))
         (buf-name (etc-build-buffer-name 'build comp-command))
         (compilation-buffer-name-function (lambda (mode) buf-name))
         ;; Pass info on for how to run things from the buffer we invoke in, which
         ;; in turn could be getting from project or elsewhere.
         (runc run-command)
         (comc comp-command)
         (proj (etc-get-project))
         (invoking (current-buffer))
         (compilation-mode-hook
          (cons (lambda (&rest unused)
                  (etc-push-recent-buffer 'build (current-buffer))
                  (font-lock-mode 0)
                  (setq etc-compilation-comp-command comc)
                  (setq etc-compilation-project proj)
                  (setq etc-compilation-run-command runc)
                  (setq etc-compilation-invoking-buffer invoking))
                compilation-mode-hook))
         (temp-file (make-temp-file "bld.")))
    (message "directory: %S" default-directory)
    (copy-file comp-command temp-file t nil nil t)
    ;; (message "=============================================")
    ;; (message "Environment: %s" process-environment)
    (compile temp-file arg)
    ;; (message "=============================================")
    ))

(defun etc-make-build-scripts-executable ()
  ;; code taken from executable-make-buffer-file-executable-if-script-p
  (when (and (string-match-p "[^.]+\\.\\(bld\\|run\\)\\.sh" (buffer-file-name))
             (derived-mode-p 'sh-mode))
    (with-demoted-errors "Unable to make file executable: %s"
      (let* ((current-mode (or (file-modes (buffer-file-name)) (default-file-modes)))
             (add-mode (logand ?\111 (default-file-modes))))
        (or (/= (logand ?\111 current-mode) 0)
            (zerop add-mode)
            (set-file-modes (buffer-file-name)
                            (logior current-mode add-mode)))))))

(add-hook 'after-save-hook #'etc-make-build-scripts-executable)

(require 'subr-x)

(defun etc-new-script-impl (type letter script-name)
  (let ((folder (string-trim-right (shell-command-to-string "find-builds -d"))))
    (find-file (concat (file-name-as-directory folder) (char-to-string letter) "-" script-name "."
                       (if (eq type 'build) "bld" "run")
                       ".sh"))))

(defun etc-new-build-script (letter script-name)
  (interactive "cScript trigger letter: \nMScript name: ")
  (etc-new-script-impl 'build letter script-name))

(defun etc-new-run-script (letter script-name)
  (interactive "cScript trigger letter: \nMScript name: ")
  (etc-new-script-impl 'run letter script-name))

(defun etc-open-build-script ()
  (interactive)
  (if-let ((script (etc-build-cmd 'build)))
      (find-file script)
    (user-error "No build script found!")))

(defun etc-open-run-script ()
  (interactive)
  (if-let ((script (etc-build-cmd 'run)))
      (find-file script)
    (user-error "No run script found!")))



(defun etc-interrupt-subjob ()
  (interactive)
  (let ((inhibit-read-only t))
    (if etc-interrupted-once
        (comint-kill-subjob)
      (comint-interrupt-subjob)
      (setq etc-interrupted-once t)) ;; not strong enough sometimes
    ;; TODO: resort to killing on a timer
    ;;
    ))

(global-set-key (kbd "C-c b") #'etc-compile)
(global-set-key (kbd "C-c s b") #'etc-stop-most-recent-build)
(global-set-key (kbd "<f10>") #'etc-compile)
(global-set-key (kbd "C-c r") #'etc-stale-run)
(global-set-key (kbd "C-c s r") #'etc-stop-most-recent-run)
(global-set-key (kbd "<f11>") #'etc-stale-run)
(global-set-key (kbd "C-c c") #'etc-compile-and-run)
(global-set-key (kbd "<f12>") #'etc-compile-and-run)
(global-set-key (kbd "C-c n b") #'etc-new-build-script)
(global-set-key (kbd "C-c n r") #'etc-new-run-script)
(global-set-key (kbd "C-c o r") #'etc-open-run-script)
(global-set-key (kbd "C-c o b") #'etc-open-build-script)
(global-set-key (kbd "C-c t d") #'etc-toggle-debug)
(global-set-key (kbd "C-c t v") #'etc-toggle-valgrind)
(global-set-key (kbd "C-c d") #'etc-debug-run)
