;; taken from: https://gist.githubusercontent.com/gilbertw1/

(defun counsel-projectile-bookmark ()
  "Jump to a projectile bookmark or create a new one if it doesn't exist."
  (interactive)
  (require 'bookmark)

  ;; Use a different variable name to avoid shadowing the function
  (let* ((proj-bookmarks (projectile-bookmarks))
         ;; Determine if proj-bookmarks is an alist or a list of strings
         (bookmark-names
          (if (and (listp proj-bookmarks)
                   (consp (car proj-bookmarks)))
              (mapcar #'car proj-bookmarks)  ; Extract names from alist
            proj-bookmarks)))                ; Assume it's a list of strings

    ;; Debugging: Uncomment the next line to inspect bookmark-names
    ;; (message "Bookmark Names: %S" bookmark-names)

    (ivy-read "Create or jump to bookmark: "
              bookmark-names
              :action (lambda (x)
                        (cond
                         ;; If avoiding dired and the bookmark is a directory
                         ((and (bound-and-true-p counsel-bookmark-avoid-dired)
                               (member x bookmark-names)
                               (file-directory-p (bookmark-location x)))
                          (with-ivy-window
                            (let ((default-directory (bookmark-location x)))
                              (counsel-find-file))))

                         ;; If the bookmark exists, jump to it
                         ((member x bookmark-names)
                          (with-ivy-window
                            (bookmark-jump x)))

                         ;; Otherwise, create a new bookmark
                         (t
                          (bookmark-set x))))
              :caller 'counsel-projectile-bookmark)))

(ivy-set-actions
 'counsel-projectile-bookmark
 '(("d" bookmark-delete "delete")
   ("e" bookmark-rename "edit")))

(define-key projectile-mode-map (kbd "C-c p b") #'counsel-projectile-bookmark)

(defun projectile-bookmarks ()
  (let ((bmarks (bookmark-all-names)))
    (cl-remove-if-not #'workspace-bookmark-p bmarks)))

(defun workspace-bookmark-p (bmark)
  (let ((bmark-path (expand-file-name (bookmark-location bmark))))
    (string-prefix-p (etc-get-project-root) bmark-path)))