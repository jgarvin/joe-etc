;; -*- lexical-binding: t -*-

;; very useful for defining quick menus
;; for builds and such
(add-to-list 'load-path "~/etc/popup-keys")
(require 'popup-keys)

(defvar etc-build-choice "a")
(defvar etc-run-choice "a")

(defvar-local etc-compilation-run-command nil)
(defvar-local etc-compilation-project nil)
(defvar-local etc-compilation-compile-command nil)
(defvar-local etc-compilation-invoking-buffer nil)

(defun etc-build-menu (type)
  (let* ((cmd (gensym))
         (scripts)
         (flag (if (eq type 'build) "b" "r"))
         (choice-sym (if (eq type 'build) 'etc-build-choice 'etc-run-choice))
         (actions))
    (with-temp-buffer
      (shell-command (format "find-builds -%s -l" flag) (current-buffer))
      (setq scripts (split-string (buffer-substring-no-properties (point-min) (point-max))
                                  (regexp-quote "\n") t)))
    (if scripts
        (progn
          (setq actions (mapcar (lambda (x)
                                  (save-match-data
                                    ;; files are typically of the form: ./x-foo.run.sh
                                    (string-match "\\(\\./\\)?\\([^-]\\)-\\([^.]+\\).*" x)
                                    (let ((letter (match-string 2 x)))
                                      (message (match-string 3 x))
                                      (list letter
                                            (match-string 3 x)
                                            (lambda () (set choice-sym letter))))))
                                scripts))
          (popup-keys:new
           cmd
           :buf-name (format "*choose %s menu*" (if (eq type 'build) "build" "run"))
           :actions actions)
          (funcall cmd))
      (user-error "No %s scripts found!" (if (eq type 'build) "build" "run")))))

(defun etc-build-cmd (type)
  (format "find-builds -%s -e %s"
          (if (eq type 'build) "b" "r")
          (if (eq type 'build) etc-build-choice etc-run-choice)))

(defun etc-compile (&optional arg)
  (interactive "P")
  (if arg
      (etc-build-menu 'build)
    (etc-compile-and-run-impl (etc-build-cmd 'build) nil)))

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
  (format "%s|%s|%s"
          (if (eq type 'build) "compile" "run")
          (etc-get-project)
          cmd))

(defun etc-run-impl (cmd)
  (let* ((buff-name (etc-build-buffer-name 'run cmd))
         ;; We temporarily customize display-buffer-alist to not pop up
         ;; a new window if the buffer is already displayed in one.
         (display-buffer-alist
          (if (get-buffer-window buff-name t)
              (cons (cons (regexp-quote buff-name) (cons #'display-buffer-no-window '())) display-buffer-alist)
            display-buffer-alist))
         (buff (get-buffer-create buff-name)))
    (async-shell-command cmd buff)
    (with-current-buffer buff
      (font-lock-mode -1)
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'etc-quit-run))))

(defun etc-post-compile-run (comp-buf finish-status)
  (setq finish-status (string-trim finish-status)) ;; trailing newline
  (if (string= "finished" finish-status)
      (with-current-buffer comp-buf
        (when (and (derived-mode-p major-mode 'compilation-mode)
                   ;; the variable will only be set if compile is invoked by our custom command
                   ;; we don't want to do any of this if the user does M-x compile
                   etc-compilation-invoking-buffer
                   etc-compilation-run-command)
          (etc-run-impl etc-compilation-run-command)))))

(add-hook 'compilation-finish-functions #'etc-post-compile-run)

(defun etc-get-project ()
  (if (projectile-project-p)
      (projectile-project-name)
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      (default-directory))))

(defun etc-compile-and-run-impl (comp-command run-command &optional arg)
  (let* (;; make the compilaton buffer depend on the command name and the project,
         ;; this makes sure we can have multiple compiles going
         (buf-name (etc-build-buffer-name 'build comp-command))
         (compilation-buffer-name-function (lambda (mode) buf-name))
         ;; Pass info on for how to run things from the buffer we invoke in, which
         ;; in turn could be getting from project or elsewhere.
         (runc run-command)
         (comc comp-command)
         (proj (etc-get-project))
         (invoking (current-buffer))
         (compilation-mode-hook (cons (lambda (&rest unused)
                                        (setq etc-compilation-comp-command comc)
                                        (setq etc-compilation-project proj)
                                        (setq etc-compilation-run-command runc)
                                        (setq etc-compilation-invoking-buffer invoking)) compilation-mode-hook)))
    (compile comp-command arg)))

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

(global-set-key (kbd "C-c b") #'etc-compile)
(global-set-key (kbd "C-c r") #'etc-stale-run)
(global-set-key (kbd "C-c c") #'etc-compile-and-run)
(global-set-key (kbd "C-c n b") #'etc-new-build-script)
(global-set-key (kbd "C-c n r") #'etc-new-run-script)
