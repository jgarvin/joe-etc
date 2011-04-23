(if (file-exists-p "/home/udesktop178/joeg/global-install/share/gtags/gtags.el")
    (load-file "/home/udesktop178/joeg/global-install/share/gtags/gtags.el"))

;; Function to generate tags with GNU Global
;; from here: http://emacs-fu.blogspot.com/2009/01/navigating-through-source-code-using.html
(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
    (let ((olddir default-directory)
          (topdir (read-directory-name
                    "gtags: top of source tree:" default-directory)))
	  (when (not (string= topdir ""))
		  (progn
			(cd topdir)
			(start-process-shell-command "gtags create"
										 "gtags_buffer"
										 "~/etc/utils/remote_launch gtags -q && echo 'created tagfile'")
			(cd olddir)))) ; restore
    ;;  tagfile already exists; update it
    (start-process-shell-command "gtags update"
								 "gtags_buffer"
								 "~/etc/utils/remote_launch global -u 2> /dev/null && echo 'updated tagfile'")))

;; Rebind the normal find tag functions to use the GNU global versions
(add-hook 'gtags-mode-hook
  (lambda()
    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
    (local-set-key (kbd "M-,") 'gtags-find-rtag)))  ; reverse tag

(require 'gtags)
(gtags-mode t)
(djcb-gtags-create-or-update)

(add-to-list 'load-path "~/etc/autopair-read-only")

(require 'autopair)
(autopair-mode)
(local-set-key (kbd "C-y") 'yank-and-indent)

(setq require-final-newline t)

;; Don't indent whole files because they're in a namespace block
(c-set-offset 'innamespace 0)