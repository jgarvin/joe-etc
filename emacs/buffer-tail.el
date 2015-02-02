;; buffer-tail
;; written by elbeardmorez
;; copied from stackoverflow (http://stackoverflow.com/a/6341139/10771)

;; keeps focus on the bottom of a given buffer

;; install by putting it in your load path, and doing a (require 'buffer-tail)
;; in your init.el

;; usage:
;; provides the toggle-buffer-tail function to toggle the tailling of a buffer
;; can force it to "on" or "off"
;; (toggle-buffer-tail "foo.log")

;alist of 'buffer-name / timer' items
(defvar buffer-tail-alist nil)
(defun buffer-tail (name)
  "follow buffer tails"
  (cond ((or (equal (buffer-name (current-buffer)) name)
         (string-match "^ \\*Minibuf.*?\\*$" (buffer-name (current-buffer)))))
        ((get-buffer name)
      (with-current-buffer (get-buffer name)
        (goto-char (point-max))
        (let ((windows (get-buffer-window-list (current-buffer) nil t)))
          (while windows (set-window-point (car windows) (point-max))
         (with-selected-window (car windows) (recenter -3)) (setq windows (cdr windows))))))))

(defun toggle-buffer-tail (name &optional force)
  "toggle tailing of buffer NAME. when called non-interactively, a FORCE arg of 'on' or 'off' can be used to to ensure a given state for buffer NAME"
  (interactive (list (cond ((if name name) (read-from-minibuffer 
      (concat "buffer name to tail" 
        (if buffer-tail-alist (concat " (" (caar buffer-tail-alist) ")") "") ": ")
    (if buffer-tail-alist (caar buffer-tail-alist)) nil nil
           (mapcar '(lambda (x) (car x)) buffer-tail-alist)
        (if buffer-tail-alist (caar buffer-tail-alist)))) nil)))
  (let ((toggle (cond (force force) ((assoc name buffer-tail-alist) "off") (t "on")) ))
    (if (not (or (equal toggle "on") (equal toggle "off"))) 
      (error "invalid 'force' arg. required 'on'/'off'") 
      (progn 
        (while (assoc name buffer-tail-alist) 
           (cancel-timer (cdr (assoc name buffer-tail-alist)))
           (setq buffer-tail-alist (remove* name buffer-tail-alist :key 'car :test 'equal)))
        (if (equal toggle "on")
            (add-to-list 'buffer-tail-alist (cons name (run-at-time t 1 'buffer-tail name))))
        (message "toggled 'tail buffer' for '%s' %s" name toggle)))))

;;(global-set-key (kbd "C-c C-S-b") #') 

(provide 'buffer-tail)
