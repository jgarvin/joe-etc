(use-package
    ein
    :ensure t
    :init
    (progn
      ;; fix smartparens not wanting to write closing parenthises when highlighting a region
      (defun insert-open-parens-or-wrap (&optional arg)
        (interactive "P")
        (if (region-active-p)
            (insert-parentheses arg)
          (insert "()")
          (backward-char))
        )

      (defun setup-key-hack ()
        (define-key ein:notebook-mode-map (kbd "(") #'insert-open-parens-or-wrap))

      (add-hook 'ein:notebooklist-mode-hook #'setup-key-hack)
      )
  )

(setq ein:jupyter-default-server-command (format "%s/mlregression/jupyter-with-anaconda.sh" (getenv "HOME")))

(setenv "CONDA_DEFAULT_ENV" "virtual_environment_name")
(setenv "CONDA_PREFIX" (format "%s/anaconda3/envs/virtual_environment_name" (getenv "HOME")))

;; otherwise spawns external viewer
(setq ein:output-area-inlined-images t)

;; https://emacs.stackexchange.com/questions/20574/default-inline-image-background-in-org-mode
;; without this images with transparent backgrounds are impossible to read with a dark theme!
(defun create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background "white")
            props)))

(advice-add 'create-image :filter-args
            #'create-image-with-background-color)
