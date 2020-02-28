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