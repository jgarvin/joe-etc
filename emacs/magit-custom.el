(setq magit-diff-refine-hunk 'all)

;; from: https://emacs.stackexchange.com/questions/19672/magit-gerrit-push-to-other-branch
(defun magit-push-to-gerrit ()
  (interactive)
  (magit-git-command-topdir "git push origin HEAD:refs/for/master"))
(magit-define-popup-action 'magit-push-popup
  ?h
  "Push to gerrit"
  'magit-push-to-gerrit)

(defun etc-show-origin-master ()
  (interactive)
  (magit-find-file-other-window "origin/master" (buffer-file-name)))

(global-set-key (kbd "C-c m h") #'magit-find-file)
(global-set-key (kbd "C-c m o") #'etc-show-origin-master)