;; -*- lexical-binding: t -*-

(require 'dash)
(require 'cl)
(require 'subword)

(defvar md-initial-vec-size 500)
(defvar md-max-tokens 200)
(defvar giant-buffer-size (* 1 1024 1024))
(defvar giant-line (* 4 4096))
(defvar md-safe-scan-limit 5000)
(defvar-local md-sym-tracking-vec (make-vector md-initial-vec-size 0))
(defvar-local md-sym-tracking-capacity 0)
(defvar md-refresh-timer nil)
(defvar-local md-symbols-cache nil)
(defvar-local md-word-cache nil)
(defvar md-symbols-cache-refresh-hook nil)
(defvar md-mode-keywords nil)
(defvar md-min-symbol-length 3)
(defvar md-max-symbol-length 50)
(defvar md-nick-scan-limit 5000)
(defvar-local md-active-erc-nicknames nil)

(defconst md-max-global-cache-size 400)
(defvar md-global-word-cache nil)
(defvar md-global-symbol-cache nil)
(defvar md-global-refresh-timer nil)

(defun md-register-mode-keywords (mode keywords)
  (let ((entry (assoc mode md-mode-keywords)))
    (if entry
        (setcdr entry keywords)
      (push (cons mode keywords) md-mode-keywords)))
  (when (eq mode 'emacs-lisp-mode)
    (mapc (lambda (x)
            (when (and (fboundp 'md-gen-elisp-snippet) (fboundp (intern x)))
              (md-gen-elisp-snippet (intern x))))
          (cdr (assoc 'emacs-lisp-mode md-mode-keywords)))))

(defun md-normalize (value lowest highest)
  (/ (float (- value lowest)) (- highest lowest)))
(byte-compile 'md-normalize)

(defun md-score-token (frequency min-frequency max-frequency
                                 distance min-distance max-distance)
  (sqrt (+ (/ 1 (expt (md-normalize frequency min-frequency max-frequency) 2))
           (expt (* 0.5 (md-normalize distance min-distance max-distance)) 2))))
(byte-compile 'md-score-token)

(defvar md-symbol-filter-faces '(font-lock-constant-face
                                 font-lock-comment-face
                                 font-lock-doc-face
                                 font-lock-string-face
                                 font-lock-builtin-face
                                 font-lock-keyword-face
                                 erc-nick-default-face
                                 erc-nick-my-face
                                 erc-notice-face))

(defun md-get-all-faces (sym-start sym-end)
  (let ((i sym-start)
        (result))
    (when (< sym-end sym-start)
      (setq sym-start sym-end)
      (setq sym-end i))
    (while (and (< i sym-end) (null result))
      (let ((face-or-faces (get-char-property i 'face)))
        (unless (listp face-or-faces)
          (setq face-or-faces (list face-or-faces)))
        (setq result (nconc result face-or-faces))
        (setq i (next-char-property-change i sym-end))))
    result))

(defun md-get-faces-at-pos (text pos)
  "Get the font faces at TEXT."
  (get-char-property pos 'face text))
(byte-compile 'md-get-faces-at-pos)

(defun md-string-contains-faces (sym-start sym-end faces)
  (let ((i sym-start)
        (result))
    (when (< sym-end sym-start)
      (setq sym-start sym-end)
      (setq sym-end i))
    (while (and (< i sym-end) (null result))
      (let ((face-or-faces (get-char-property i 'face)))
        (if (listp face-or-faces)
            ;; Sometimes you get a cons cell of face . "color"
            (when (stringp (cdr face-or-faces))
              (setq face-or-faces (list (car face-or-faces))))
          ;; Sometimes you get a single symbol not in a list
          (setq face-or-faces (list face-or-faces)))
        (when (-intersection face-or-faces faces)
          (setq result t)))
      (setq i (next-char-property-change i sym-end)))
    result))
(byte-compile 'md-string-contains-faces)

;; taken from: http://stackoverflow.com/a/11848341/50385
(defun md-how-many-str (regexp str &optional max)
  (save-match-data
    (loop with start = 0
          for count from 0
          while (and (string-match regexp str start) (or (not max) (< count max)))
          do (setq start (match-end 0))
          finally return count)))
(byte-compile 'md-how-many-str)

(defun md-filter-symbol (sym sym-start sym-end &optional dont-check-max dont-check-faces)
  (let ((entry))
    (cond
     ;; filter snippet placeholder
     ((and (boundp 'md-placeholder)
           (string-match-p (regexp-opt (list (char-to-string md-placeholder))) sym)) t)
     ;; filter syms with unprintable chars
     ;; TODO: this is wrong, it excludes unicode
     ((string-match "[^\t\n\r\f -~]" sym) t)
     ((let ((non-ws-char-count
             (md-how-many-str "[^\\\n[:space:]]" sym
                              (+ 1 md-min-symbol-length))))
        (or (< non-ws-char-count md-min-symbol-length)
            (and (not dont-check-max) (> non-ws-char-count md-max-symbol-length)))) t)
     ((and (setq entry (assoc major-mode md-mode-keywords))
           (member sym (cdr entry))) t)
     ((= (md-how-many-str "[^0-9]" sym 1) 0) t)
     ((and (not dont-check-faces)
           sym-start
           sym-end
           (md-string-contains-faces sym-start sym-end md-symbol-filter-faces)) t)
     (t nil))))
(byte-compile 'md-filter-symbol)

(defun md-quick-sort (vec p q pred)
  (let ((r))
    (when (< p q)
      (setq r (md-partition vec p q pred))
      (md-quick-sort vec p r pred)
      (md-quick-sort vec (+ r 1) q pred)))
  vec)
(byte-compile 'md-quick-sort)

(defun md-partition (vec p q pred)
  (let ((x (aref vec p))
        (i p)
        (j (+ p 1)))
    (while (< j q)
      (when (funcall pred (aref vec j) x)
        (setq i (+ i 1))
        (md-swap vec i j))
      (setq j (+ j 1)))
    (md-swap vec i p)
    i))
(byte-compile 'md-partition)

(defun md-swap (vec a b)
  (let ((temp (aref vec a)))
    (aset vec a (aref vec b))
    (aset vec b temp)))
(byte-compile 'md-swap)

;; TODO: This would be faster if it used a heap
;; TODO: only run on changed parts of buffer? hard to 'subtract out'
(defun md-get-unit-impl (start end unit)
  "Get all the units (symbols or words) between start and end"
  (save-match-data
    (save-excursion
      (let ((search-opener (if (equal unit 'symbol) "\\_<" "\\<"))
            (search-closer (if (equal unit 'symbol) "\\_>" "\\>"))
            (sym-counts (make-hash-table :test 'equal))
            (current-sym)
            (starting-point (point))
            (min-distance most-positive-fixnum)
            (max-distance 0)
            (max-frequency 0)
            (vec-size 0)
            (last-point))
        (goto-char start)
        (beginning-of-line)
        (condition-case nil
            (while (> end (point))
              ;; guarantee forward progress even if subword regex doesn't
              (when (and last-point (>= last-point (point)))
                (goto-char (1+ last-point)))
              (setq last-point (point))
              (if (and subword-mode (equal unit 'word))
                  (let ((case-fold-search nil))
                    (re-search-forward subword-forward-regexp end))
                (re-search-forward (concat search-opener "\\(.+?\\)" search-closer) end))
              (setq current-sym (match-string-no-properties 1))
              (when (not (md-filter-symbol current-sym (match-beginning 1) (match-end 1)))
                (let ((entry (gethash current-sym sym-counts))
                      (cur-distance (abs (- (point) starting-point))))
                  (if entry
                      (progn
                        (let ((inner-vec (aref md-sym-tracking-vec entry)))
                          (aset inner-vec 1 (+ (aref inner-vec 1) 1))
                          (aset inner-vec 2 (min (aref inner-vec 2) cur-distance))))
                    (progn
                      (when (>= vec-size md-sym-tracking-capacity)
                        (let ((new-vec (make-vector (max (* md-sym-tracking-capacity 2) 1) 0))
                              (i 0))
                          (while (< i vec-size)
                            (aset new-vec i (aref md-sym-tracking-vec i))
                            (setq i (+ i 1)))
                          (setq md-sym-tracking-vec new-vec)
                          (setq md-sym-tracking-capacity (length md-sym-tracking-vec))))
                      (aset md-sym-tracking-vec vec-size (vector current-sym 1 cur-distance 0))
                      (puthash current-sym vec-size sym-counts)
                      (setq entry vec-size)
                      (setq vec-size (+ vec-size 1))))
                  (let* ((inner-vec (aref md-sym-tracking-vec entry))
                         (frequency (aref inner-vec 1))
                         (distance (aref inner-vec 2)))
                    (setq max-frequency (max max-frequency frequency))
                    (setq min-distance (min min-distance distance))
                    (setq max-distance (max max-distance distance))))))
          (search-failed nil))
        (let ((i 0))
          (while (< i vec-size)
            (let* ((inner-vec (aref md-sym-tracking-vec i))
                   (frequency (aref inner-vec 1))
                   (distance (aref inner-vec 2)))
              (aset inner-vec 3 (md-score-token frequency 0 max-frequency distance min-distance max-distance)))
            (setq i (+ i 1))))
        (md-quick-sort md-sym-tracking-vec 0 vec-size (lambda (a b) (> (aref a 3) (aref b 3))))
        (let ((i 0)
              (results '()))
          (while (and (< i vec-size) (< i md-max-tokens))
            (let ((inner-vec (aref md-sym-tracking-vec i)))
              (setq results (cons (aref inner-vec 0) results)))
            (setq i (+ i 1)))
          results)))))
(byte-compile 'md-get-unit-impl)

(defun md-safe-get-unit-impl (start end unit)
  (let ((distance (abs (- end start)))
        (safe-start start)
        (safe-end end))
    (when (> distance md-safe-scan-limit)
      (setq safe-start (max (point-min) (- (point) md-safe-scan-limit)))
      (setq safe-end (min (point-max) (+ (point) md-safe-scan-limit))))
    (md-get-unit-impl safe-start safe-end unit)))
(byte-compile 'md-safe-get-unit-impl)

(defun md-refresh-unit-cache (buf v unit)
  ;;(setq md-debug-temp mark-active)
  ;;(message "refreshing symbol cache %S" md-debug-temp)
  (unwind-protect 
      (with-current-buffer buf
        (let ((old (symbol-value v))) 
          (set v (md-safe-get-unit-impl (point-min) (point-max) unit))
          (when (not (equal old (symbol-value v))) 
            ;;(message "running hook")
            (run-hooks 'md-symbols-cache-refresh-hook))))
    (when md-refresh-timer
      (cancel-timer md-refresh-timer)
      (setq md-refresh-timer nil))))
(byte-compile 'md-refresh-unit-cache)

(defun md-refresh-symbols (&optional _start _end _old-length)
  (unless (or (window-minibuffer-p)
              (minibufferp)
              (not (get-buffer-window (current-buffer) 'visible)))
    (md-run-when-idle-once 'md-refresh-timer
                           (lambda ()
                             (md-refresh-unit-cache (current-buffer) 'md-symbols-cache 'symbol)
                             (md-refresh-unit-cache (current-buffer) 'md-word-cache 'word)
                             ) 0.5 nil)))
(byte-compile 'md-refresh-symbols)

(add-hook 'after-change-functions #'md-refresh-symbols)
;; (remove-hook 'after-change-functions #'md-refresh-symbols)
(add-hook 'md-window-selection-hook #'md-refresh-symbols)
;; (remove-hook 'md-window-selection-hook #'md-refresh-symbols)

(defun md-get-all-modes (&optional mode)
  (unless mode
    (setq mode major-mode))
  (remq nil
        (cons mode
              (loop as m = mode then p while m as p = (get m 'derived-mode-parent) collect p))))

(defun md-modes-in-common (a b)
  (-intersection (with-current-buffer a (md-get-all-modes))
                 (with-current-buffer b (md-get-all-modes))))

(defun md-get-visible-buffers ()
  (let ((result))
    (dolist (b (buffer-list (selected-frame)))
      (when (get-buffer-window b 'visible)
        (push b result)))
    result))

(defun md-sort-buffers-by-priority (a b)
  (let ((w (window-buffer (selected-window))))
    (>= (length (md-modes-in-common a w)) (length (md-modes-in-common b w)))))

(defun md-list-less-p (a b &optional reverse)
  ;; (message "comparing [%s] [%s]" a b)
  (assert (= (length a) (length b)))
  (let ((a-item (car a))
        (b-item (car b))
        (a-rest (cdr a))
        (b-rest (cdr b))
        (lesser (if reverse #'> #'<))
        (greater (if reverse #'< #'>)))
    (cond
     ((funcall lesser a-item b-item) t)
     ((funcall greater a-item b-item) nil)
     ((= a-item b-item)
      (if (and a-rest b-rest)
          (md-list-less-p a-rest b-rest reverse)
        ;; equal all the way through, so not less
        nil)))))
  
(defun md-buffer-priority-key (x proj-buffers)
  (let* ((w (window-buffer (selected-window)))
         (modes-in-common (length (md-modes-in-common x w)))
         (disp-time)
         (read-only-temp)
         (special-temp)
         (result))
    (with-current-buffer x
      (setq disp-time (with-current-buffer x buffer-display-time))
      (unless disp-time
        ;; some buffers have never been displayed and return nil
        (setq disp-time '(0 0 0 0)))
      (setq read-only-temp (with-current-buffer x buffer-read-only))
      (setq special-temp (with-current-buffer x (derived-mode-p 'special-mode))))
    (setq result (append
                  ;; selected buffer is top priority
                  (list (if (eq w x) 1 0))
                  ;; visible buffers in the same project in the same mode
                  (list (if (and (memq x proj-buffers)
                                 (get-buffer-window x)
                                 (> modes-in-common 0)) modes-in-common 0))
                  ;; buffers in the same project in the same mode
                  (list (if (and (memq x proj-buffers)
                                 (> modes-in-common 0)) modes-in-common 0))
                  ;; buffers in the same project
                  (list (if (memq x proj-buffers) modes-in-common 0))
                  ;; visible buffers
                  (list (if (get-buffer-window x) modes-in-common 0))
                  (list modes-in-common)
                  disp-time
                  (list (if (or (buffer-file-name x) (get-buffer-process x)) 1 0))
                  disp-time
                  ;; more likely to want tokens from editable buffers
                  (list (if read-only-temp 0 1))
                  disp-time
                  ;; special mode buffers considered last
                  (list (if special-temp 0 1))
                  disp-time))
    ;; (message "name: [%s] length: [%s] result: [%s]" x (length result) result)
    result))

(defun md-token-elidgible-buffers ()
  (-filter (lambda (x)
             (with-current-buffer x
               (and (not (minibufferp x))
                    (not (derived-mode-p 'special-mode))
                    (or (buffer-file-name x)
                        (get-buffer-process x))))) (buffer-list)))

(defun md-get-real-projectile-buffers ()
  "(projectile-project-buffers) appears to be bugged, it thinks lots
of buffers are part of the project that are not..."
  (let ((proot (projectile-project-root)))
    (-filter (lambda (x) (or (and (buffer-file-name x)
                                  (md-file-inside-folder (buffer-file-name x) proot))
                             (get-buffer-process x))) (projectile-project-buffers))))
  

(require 'f)
(defun md-file-inside-folder (f dir)
  (string= (f-common-parent (list f dir)) dir))
;;(defun md-file-inside-folder (f dir )nil)

(defun md-get-prioritized-buffer-list ()
  (let ((proj-buffers (if (projectile-project-p)
                          (md-get-real-projectile-buffers)
                        (md-token-elidgible-buffers))))
      (cl-sort (md-token-elidgible-buffers)
               (lambda (a b)
                 (md-list-less-p a b t))
               :key (lambda (x)
                      (md-buffer-priority-key x proj-buffers)))))

(defun md-normalize-words (word)
  "Cleanup words because in some modes punctuation gets stuck
on the ends. Also we want to store as lowercase."
  (save-match-data
    (when (string-match "\\([a-zA-z]+\\)" word)
      (downcase (match-string 1 word)))))

(defun md-refresh-global-cache-unit (cache buffers presence-map &optional normalize)
  (let ((count 0)
        (new-unit-cache)
        (word)
        ;; Even though the per buffer symbol filtering filters language keywords we
        ;; have to do it again here because words that are keywords in our language may
        ;; appear in buffers where those words are not considered keywords. This happens
        ;; between python and elisp when working on mandimus a lot ;)
        (kwlist (cdr (assoc major-mode md-mode-keywords))))
    (catch 'max-hit
      (dolist (b buffers)
        (with-current-buffer b
          (dolist (w (symbol-value cache))
            (when normalize
              (setq w (funcall normalize w)))
            (when (and w (length w) (not (member w kwlist)))
              (unless (gethash w presence-map)
                (puthash w 1 presence-map)
                (when (> (incf count) md-max-global-cache-size)
                  (throw 'max-hit nil))
                (push w new-unit-cache)))))))
    (setq new-unit-cache (nreverse new-unit-cache))
    new-unit-cache))

(defun md-refresh-global-caches-impl ()
  (let ((buffers (md-get-prioritized-buffer-list)))
    (with-current-buffer (window-buffer (selected-window)) 
      (setq md-global-word-cache (md-refresh-global-cache-unit
                                  'md-word-cache buffers
                                  (make-hash-table :test 'equal)
                                  #'md-normalize-words))
      (setq md-global-symbol-cache (md-refresh-global-cache-unit
                                    'md-symbols-cache buffers
                                    (make-hash-table :test 'equal))))))

(defun md-refresh-global-caches ()
  (md-run-when-idle-once 'md-global-refresh-timer #'md-refresh-global-caches-impl 0.5 nil))

(add-hook 'md-symbols-cache-refresh-hook #'md-refresh-global-caches)
(add-hook 'md-window-selection-hook #'md-refresh-global-caches)

(defun md-get-active-erc-nicknames (&optional max-results)
  (when (derived-mode-p 'erc-mode)
    (let ((candidates (erc-get-channel-nickname-list)))
      (when candidates
        (let* ((regex (concat "\\(" (mapconcat (lambda (x) (concat "\\<" (regexp-quote x) "\\>")) candidates "\\|") "\\)"))
               (scan-limit (max (point-min) (- (point-max) (max md-nick-scan-limit (- (window-end) (window-start))))))
               (scan-start (point-max))
               (presence (make-hash-table :test 'equal))
               (results))
          (save-excursion
            (goto-char (point-max))
            (while (and (not (bobp))
                        (> (point) scan-limit)
                        (or (not max-results) (< (length results) max-results))
                        (re-search-backward regex scan-limit 1))
              (when (md-string-contains-faces (match-beginning 0) (match-end 0) '(erc-nick-default-face))
                (let* ((match (match-string-no-properties 0))
                       (entry (gethash match presence))
                       (cur-distance (- scan-start (point))))
                  (unless entry
                    (puthash match 1 presence)
                    (push match results))))))
          (nreverse results))))))

(defun md-channel-buffer-p ()
  (and (derived-mode-p 'erc-mode)
       ;; channels only, no server buffers
       (equal 0 (string-match "#.*" (buffer-name)))))

(defun md-update-active-nicks ()
  (when (and (md-channel-buffer-p)
             (erc-buffer-visible (current-buffer))
             (< 1 (buffer-size)))
    (save-restriction
      ;; erc-insert-post-hook runs in a narrowed buffer
      (widen)
      (setq md-active-erc-nicknames (md-get-active-erc-nicknames 50)))))

(defun md-normalize-token (a)
  (downcase (replace-regexp-in-string "[^A-Za-z]" "" a)))

(defun md-cycle-token (unit)
  (let* ((token (thing-at-point unit))
         (bounds (bounds-of-thing-at-point unit))
         (normal-token (md-normalize-token token))
         (cache (if (equal unit 'word) md-global-word-cache md-global-symbol-cache))
         (candidates (sort (-filter (lambda (x)
                                      (message "Comparing [%s] [%s]" x normal-token)
                                      (string= (md-normalize-token x) normal-token))
                                    cache) #'string<))
         (token-pos (position token candidates :test #'equal)))
    (unless (and candidates token-pos)
      (user-error "No alternative candidates for: [%s]" token))
    (save-excursion
      (goto-char (car bounds))
      (save-match-data
        (re-search-forward ".*" (cdr bounds))
        (replace-match (nth (% (1+ token-pos) (length candidates)) candidates) t)))))

(add-hook 'erc-insert-post-hook #'md-update-active-nicks)
(add-hook 'md-window-selection-hook #'md-update-active-nicks)
;;(remove-hook 'erc-insert-post-hook #'md-update-active-nicks)
;;(remove-hook 'md-window-selection-hook #'md-update-active-nicks)

