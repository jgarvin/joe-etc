(defun etc-mobile-browser (ignored) 
       "Pretend to be a mobile browser."
       (concat
        "User-Agent: "
        "Mozilla/5.0 (Linux; Android 4.0.4; Galaxy Nexus Build/IMM76B) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.133 Mobile Safari/535.19\r\n"))

(advice-add 'url-http-user-agent-string :around #'etc-mobile-browser)


(require 'load-theme-buffer-local)

(defun etc-eww-mode-hook ()
  ;; so we have white background color
  ;; (load-theme-buffer-local 'tango (current-buffer))
  )

(add-hook 'eww-mode-hook #'etc-eww-mode-hook)

(setq browse-url-browser-function 'eww-browse-url)
