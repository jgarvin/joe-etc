(defun android-log ()
  (terminal-emulator "android_log" "zsh" '("-c" "adb" "logcat")))

;; android-mode
(if (file-exists-p "~/opt/android-mode")
	(progn
	  (add-to-list 'load-path "~/opt/android-mode")
	  (require 'android-mode)
	  (setq android-mode-sdk-dir (getenv "ANDROID_PATH"))
	  (android-log)))

(defun android-debug ()
  (progn
	(setq gud-jdb-use-classpath t)
	(setq gud-jdb-classpath (format "%s/src:%s/bin/classes" (android-root) (android-root)))
	(setq gud-jdb-sourcepath (format "%s/src" (android-root)))
	)
  (jdb "jdb -attach localhost:8700")
  )
