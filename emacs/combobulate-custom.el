;; (use-package treesit
;;   :mode (("\\.tsx\\'" . tsx-ts-mode))
;;   :preface
;;   (defun mp-setup-install-grammars ()
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              ;; Note the version numbers. These are the versions that
;;              ;; are known to work with Combobulate *and* Emacs.
;;              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;                (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
;;                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
;;                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;                (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
;;                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;;                (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
;;                (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
;;                (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
;;                (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.24.1"))
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;;                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       ;; Only install `grammar' if we don't already have it
;;       ;; installed. However, if you want to *update* a grammar then
;;       ;; this obviously prevents that from happening.
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))

;;   ;; Optional. Combobulate works in both xxxx-ts-modes and
;;   ;; non-ts-modes.

;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
;;   ;; that this does *not* extend to hooks! Make sure you migrate them
;;   ;; also
;;   (dolist (mapping
;;            '((python-mode . python-ts-mode)
;;              (css-mode . css-ts-mode)
;;              (typescript-mode . typescript-ts-mode)
;;              (js2-mode . js-ts-mode)
;;              (bash-mode . bash-ts-mode)
;;              (bash-mode . bash-ts-mode)
;;              (conf-toml-mode . toml-ts-mode)
;;              (go-mode . go-ts-mode)
;;              (css-mode . css-ts-mode)
;;              (json-mode . json-ts-mode)
;;              (c-mode . c-ts-mode)
;;              (c++-mode . c++-ts-mode)
;;              (js-json-mode . json-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))
;;   :config
;;   (mp-setup-install-grammars)
;;   ;; Do not forget to customize Combobulate to your liking:
;;   ;;
;;   ;;  M-x customize-group RET combobulate RET
;;   ;;
;;   (use-package combobulate
;;     :custom
;;     ;; You can customize Combobulate's key prefix here.
;;     ;; Note that you may have to restart Emacs for this to take effect!
;;     (combobulate-key-prefix "C-c o")
;;     :hook ((prog-mode . combobulate-mode))
;;     ;; Amend this to the directory where you keep Combobulate's source
;;     ;; code.
;;     :load-path ("~/etc/emacs/combobulate")))
