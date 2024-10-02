;; Define a buffer-local hash table to store overlays identified by symbols
(defvar-local dh-overlays (make-hash-table :test 'eq)
  "Hash table mapping symbols to overlays for debug highlights.")

;; Define a shorthand color mapping
(defconst dh-color-alist
  '((b . "blue")
    (r . "red")
    (g . "green")
    (y . "yellow")
    (m . "magenta")
    (c . "cyan")
    (w . "white")
    (k . "black"))
  "Association list mapping shorthand symbols to color names for debug highlighting.")

;;;###autoload
(defun dh (symbol face start &optional end)
  "Highlight the region between START and END using an overlay identified by SYMBOL.

If an overlay with SYMBOL already exists in the current buffer, remove it first
before creating a new one.

SYMBOL is used as the key to identify the overlay uniquely within the buffer.

FACE specifies the highlight color and can be:
- A single-letter symbol (e.g., 'b for blue, 'r for red).
- A face name or property list for more customization.

START and END define the region to be highlighted. If END is not provided,
the highlight will cover only the character at START.

This function is useful for visualizing affected text regions during debugging."
  (interactive
   (let* ((input-symbol (intern (read-string "Enter symbol to identify the highlight: ")))
          (input-face (read-string "Choose face for highlighting (single letter like b for blue or full face spec, default: y): " nil nil "y"))
          (beg (if (use-region-p) (region-beginning) (point)))
          (ed (if (use-region-p) (region-end) (1+ (point))))
          ;; Determine the face
          (face-spec (if (string= input-face "")
                         'y
                       (intern input-face))))
     (list
      input-symbol
      face-spec
      beg
      (unless (use-region-p) ed))))
  ;; Validate inputs
  (unless (symbolp symbol)
    (error "The first argument must be a symbol"))
  (unless (and (number-or-marker-p start)
               (number-or-marker-p (or end (1+ start))))
    (error "START and END must be valid positions"))

  ;; Determine the end position
  (let ((end-pos (or end (1+ start))))
    ;; Determine the face to use
    (let (face-attr)
      (cond
       ;; If face is a single-letter symbol, look it up
       ((and (symbolp face) (assoc face dh-color-alist))
        (setq face-attr `(:background ,(cdr (assoc face dh-color-alist)))))
       ;; If face is provided as a property list or face name, use it directly
       ((or (facep face)
            (and (consp face) (plist-get face :background)))
        (setq face-attr face))
       ;; Default face
       (t
        (setq face-attr '(:background "yellow"))))

      ;; Remove existing overlay associated with SYMBOL, if any
      (let ((existing-overlay (gethash symbol dh-overlays)))
        (when (and existing-overlay (overlayp existing-overlay))
          (delete-overlay existing-overlay)
          (remhash symbol dh-overlays)))

      ;; Create a new overlay
      (let ((ov (make-overlay start end-pos)))
        ;; Set the face for highlighting
        (overlay-put ov 'face face-attr)
        ;; Optionally, you can make the overlay intangible or non-evaporable
        ;; depending on your debugging needs.
        ;; (overlay-put ov 'intangible t)
        ;; (overlay-put ov 'evaporate t)

        ;; Store the overlay in the hash table with SYMBOL as the key
        (puthash symbol ov dh-overlays)))))

;;;###autoload
(defun dh-clear-all ()
  "Clear all debug highlights in the current buffer."
  (interactive)
  (maphash (lambda (_symbol overlay)
             (when (overlayp overlay)
               (delete-overlay overlay)))
           dh-overlays)
  (clrhash dh-overlays))
