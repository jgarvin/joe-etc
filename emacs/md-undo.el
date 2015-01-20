(defvar md-utterance-changed-buffers nil)
(defvar-local md-collapse-undo-marker nil)

;; begin/end/collapse Taken from jch's stackoverflow answer here:
;; http://emacs.stackexchange.com/a/7560/2301

(defun md-undo-collapse-begin (marker)
  "Mark the beginning of a collapsible undo block.
This must be followed with a call to undo-collapse-end with a marker
eq to this one."
  (push marker buffer-undo-list))

(defun md-undo-collapse-end (marker)
  "Collapse undo history until a matching marker."
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
  "Execute body, then collapse any resulting undo boundaries."
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

(defun md-check-undo-before-change (beg end)
  (unless (or (eq buffer-undo-list t) ;; undo tracking disabled
              buffer-read-only
              (memq (current-buffer) md-utterance-changed-buffers))
    (push (current-buffer) md-utterance-changed-buffers)
    (setq md-collapse-undo-marker (list 'apply 'identity nil))
    (md-undo-collapse-begin md-collapse-undo-marker)))

(defun md-pre-utterance-undo-setup ()
  (setq md-utterance-changed-buffers nil))

(defun md-post-utterance-collapse-undo ()
  (dolist (i md-utterance-changed-buffers)
    (with-current-buffer i
      (md-undo-collapse-end md-collapse-undo-marker)))
  (setq md-utterance-changed-buffers nil))

(add-hook 'before-change-functions #'md-check-undo-before-change)
(add-hook 'md-start-utterance-hooks #'md-pre-utterance-undo-setup)
(add-hook 'md-end-utterance-hooks #'md-post-utterance-collapse-undo)
