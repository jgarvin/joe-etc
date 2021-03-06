
(defvar md-utterance-changed-buffers nil)
(defvar-local md-collapse-undo-marker nil)

(defun md-undo-collapse-begin (marker)
  "Mark the beginning of a collapsible undo block.
This must be followed with a call to undo-collapse-end with a marker
eq to this one.

Taken from jch's stackoverflow answer here:
http://emacs.stackexchange.com/a/7560/2301
"
  (push marker buffer-undo-list))

;; TODO: this should not modify the list
;; when the marker can't be found!
(defun md-undo-collapse-end (marker)
  "Collapse undo history until a matching marker.

Taken from jch's stackoverflow answer here:
http://emacs.stackexchange.com/a/7560/2301"
  (cond
    ((eq (car buffer-undo-list) marker)
     (setq buffer-undo-list (cdr buffer-undo-list)))
    (t
     (let ((l buffer-undo-list))
       (while (not (eq (cadr l) marker))
         (cond
           ((null (cdr l))
            (error "md-undo-collapse-end with no matching marker"))
           ((eq (cadr l) nil)
            (setf (cdr l) (cddr l)))
           (t (setq l (cdr l)))))
       ;; remove the marker
       (setf (cdr l) (cddr l))))))

(defmacro md-with-undo-collapse (&rest body)
  "Execute body, then collapse any resulting undo boundaries.

Taken from ideasman42's stackoverflow answer here:
https://emacs.stackexchange.com/a/54412/2301"
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
        (success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
            ;; Don't truncate any undo data in the middle of this.
            (undo-outer-limit nil)
            (undo-limit most-positive-fixnum)
            (undo-strong-limit most-positive-fixnum)
            (,success nil))
       (unwind-protect
         (progn
           (activate-change-group ,handle)
           (prog1 ,(macroexp-progn body)
             (setq ,success t)))
         (if ,success
           (progn
             (accept-change-group ,handle)
             (undo-amalgamate-change-group ,handle))
           (cancel-change-group ,handle))))))

(defmacro md-with-undo-collapse (&rest body)
  "Execute body, then collapse any resulting undo boundaries.

Taken from jch's stackoverflow answer here:
http://emacs.stackexchange.com/a/7560/2301"
  (declare (indent 0))
  (let ((marker (list 'apply 'identity nil)) ; build a fresh list
        (buffer-var (make-symbol "buffer")))
    `(let ((,buffer-var (current-buffer)))
       (unwind-protect
           (progn
             (md-undo-collapse-begin ',marker)
             ,@body)
         (with-current-buffer ,buffer-var
           (md-undo-collapse-end ',marker))))))

;; (defun md-check-undo-before-change (beg end)
;;   "When a modification is detected, we push the current buffer
;; onto a list of buffers modified this utterance."
;;   (unless (or
;;            ;; undo itself causes buffer modifications, we
;;            ;; don't want to trigger on those
;;            undo-in-progress
;;            ;; we only collapse utterances, not general actions
;;            (not md-in-utterance)
;;            ;; ignore undo disabled buffers
;;            (eq buffer-undo-list t)
;;            ;; ignore read only buffers
;;            buffer-read-only
;;            ;; ignore buffers we already marked
;;            (memq (current-buffer) md-utterance-changed-buffers)
;;            ;; ignore buffers that have been killed
;;            (not (buffer-name)))
;;     (push (current-buffer) md-utterance-changed-buffers)
;;     (setq md-collapse-undo-marker (list 'apply 'identity nil))
;;     (undo-boundary)
;;     (md-undo-collapse-begin md-collapse-undo-marker)))

(defun md-pre-utterance-undo-setup ()
  (setq md-utterance-changed-buffers nil)
  (setq md-collapse-undo-marker nil))

(defun md-post-utterance-collapse-undo ()
  (unwind-protect
      (dolist (i md-utterance-changed-buffers)
        ;; killed buffers have a name of nil, no point
        ;; in undoing those
        (when (buffer-name i)
          (with-current-buffer i
            ;; Some modes clear the undo list, like ERC
            ;; when you hit enter. Also if the buffer undo list
            ;; has meanwhile been disabled (by being set to t)
            ;; we don't want to change it either.
            (when (and buffer-undo-list (not (eq buffer-undo-list t))) 
              (condition-case data
                  (md-undo-collapse-end md-collapse-undo-marker)
                (error (message "Couldn't collapse undo in buffer %S: %S" i data)))))))
    (setq md-utterance-changed-buffers nil)
    (setq md-collapse-undo-marker nil)))

(defun md-force-collapse-undo (&rest unused)
  "Forces undo history to collapse, we invoke when the user is
trying to do an undo command so the undo itself is not collapsed."
  (when (memq (current-buffer) md-utterance-changed-buffers)
    (md-undo-collapse-end md-collapse-undo-marker)
    (setq md-utterance-changed-buffers (delq (current-buffer) md-utterance-changed-buffers))))

(defun md-resume-collapse-after-undo (&rest unused)
  "After the 'undo' part of the utterance has passed, we still want to
collapse anything that comes after."
  (when md-in-utterance
    (md-check-undo-before-change nil nil)))

(defun md-enable-utterance-undo ()
  (setq md-utterance-changed-buffers nil)
  (when (featurep 'undo-tree)
    (advice-add #'undo-tree-undo :before #'md-force-collapse-undo)
    (advice-add #'undo-tree-undo :after #'md-resume-collapse-after-undo)
    (advice-add #'undo-tree-redo :before #'md-force-collapse-undo)
    (advice-add #'undo-tree-redo :after #'md-resume-collapse-after-undo))
  (advice-add #'undo :before #'md-force-collapse-undo)
  (advice-add #'undo :after #'md-resume-collapse-after-undo)
  (add-hook 'before-change-functions #'md-check-undo-before-change)
  (add-hook 'md-start-utterance-hooks #'md-pre-utterance-undo-setup)
  (add-hook 'md-end-utterance-hooks #'md-post-utterance-collapse-undo))

(defun md-disable-utterance-undo ()
  ;;(md-force-collapse-undo)
  (when (featurep 'undo-tree)
    (advice-remove #'undo-tree-undo #'md-force-collapse-undo)
    (advice-remove #'undo-tree-undo #'md-resume-collapse-after-undo)
    (advice-remove #'undo-tree-redo #'md-force-collapse-undo)
    (advice-remove #'undo-tree-redo #'md-resume-collapse-after-undo))
  (advice-remove #'undo #'md-force-collapse-undo)
  (advice-remove #'undo #'md-resume-collapse-after-undo)
  (remove-hook 'before-change-functions #'md-check-undo-before-change)
  (remove-hook 'md-start-utterance-hooks #'md-pre-utterance-undo-setup)
  (remove-hook 'md-end-utterance-hooks #'md-post-utterance-collapse-undo))

(md-enable-utterance-undo)
;; (md-disable-utterance-undo)

