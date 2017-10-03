(setq magit-diff-refine-hunk 'all)

;; from: https://emacs.stackexchange.com/questions/19672/magit-gerrit-push-to-other-branch
(defun magit-push-to-gerrit ()
  (interactive)
  (magit-git-command-topdir "git push origin HEAD:refs/for/master"))
(magit-define-popup-action 'magit-push-popup
  ?h
  "Push to gerrit"
  'magit-push-to-gerrit)