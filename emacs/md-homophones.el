(require 'subr-x) 

(defvar md-homophone-table nil)

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun md-pluralize (word ending)
  "Pluralize a word using a given ending. Doesn't handle nearly all cases
but it doesn't need to, just the ones that show up in the homophones file."
  (cond
   ((or (and (string-match-p "y\\'" word) (string= ending "ies"))
        (and (string-match-p "e\\'" word) (string= ending "es"))
        (and (string-match-p "o\\'" word) (string= ending "oes")))
    (concat (substring word 0 (1- (length word))) ending))
   (t (concat word ending))))

;; (md-pluralize "mode" "es")
;; (md-pluralize "food" "s")
;; (md-pluralize "party" "ies")

;; TODO: there are two problems with this implementation
;; because it isn't stateful, it can't handle multiple word homophones
;; "I see" -> "I sea" rather than "icy"
;; also because it isn't stateful, it doesn't handle homophonic abbreviations correctly
;; "IC" -> "ICY" rather than "icy"
;; the capitalization should obey the capitalization you started with when you began cycling

(defun md-setup-homophone-table ()
  (let ((main-list))
    (setq md-homophone-table (make-hash-table :test #'equal))
    (dolist (i (read-lines "~/etc/emacs/homophones"))
      (unless (equal (aref i 0) ?\#)
        (let ((homophones (split-string i "," t))
              core-list
              plural-list)
          (dolist (h homophones)
            (string-match "\\([^(]+\\)\\((-\\([^)]+\\))\\)?" h)
            (let ((base-word (string-trim (match-string 1 h)))
                  (plural-ending (match-string 3 h)))
              (push base-word core-list)
              (when plural-ending
                (setq plural-ending (string-trim plural-ending))
                (push (md-pluralize base-word plural-ending) plural-list)))
            ;; (let ((plural-split (split-string h " " t)))
            ;;   ;;(message "%S" plural-split)
            ;;   (assert (<= (length plural-split) 2))
            ;;   (push (car plural-split) core-list)
            ;;   (when (= (length plural-split) 2)
            ;;     ;; strip off surrounding parens and dash from plural ending, (-es) -> es
            ;;     (let*((plural-ending (replace-regexp-in-string "(-\\([^)]+\\))" "\\1" (cadr plural-split))))
            ;;       (push (md-pluralize (car plural-split) plural-ending) plural-list))))
            )
          (push core-list main-list)
          (when plural-list
            (push plural-list main-list)))))
    (dolist (group main-list)
      (dolist (word group)
        (puthash word group md-homophone-table)))
    md-homophone-table))

(defun md-cycle-homophones-at-point ()
  (interactive)
  (unless md-homophone-table
    ;; lazy loading
    (md-setup-homophone-table))
  (let* ((word (thing-at-point 'word))
         (lowercase-word (downcase word))
         (bounds (bounds-of-thing-at-point 'word))
         (entry (or (gethash word md-homophone-table)
                    (gethash lowercase-word md-homophone-table))))
    (if entry
        (progn
          (message "%S "entry)
          (let ((next-homophone (nth (% (1+ (or (position word entry :test #'equal)
                                                (position lowercase-word entry :test #'equal)))
                                        (length entry)) entry)))
            (save-excursion
              (goto-char (car bounds))
              ;; (query-replace word next-homophone nil (car bounds) (cdr bounds))
              (re-search-forward ".*" (cdr bounds))
              (replace-match next-homophone)
              )))
      (user-error "No homophones of [%s]" word))))
