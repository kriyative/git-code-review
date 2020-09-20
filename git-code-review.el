;;; Git Code Review minor mode

(defun gcr--new-line ()
  (beginning-of-line)
  (open-line 1)
  (indent-to-left-margin))

(defun gcr--insert (text)
  (gcr--new-line)
  (insert text))

(defvar gcr--review-slug "REVIEW")

(defvar gcr--comment-separator
  "----------------")

(defvar gcr--review-end
  "----------------------------------------------------------------")

(defun gcr--thing-at-point ()
  (thing-at-point 'word t))

(defvar gcr--save-callback nil)
(defvar gcr--save-callback-args nil)
(defvar gcr--state nil)

(defun gcr--comment-prefix ()
  (concat comment-start
          (when (< (length comment-start) 2)
            comment-start)
          " "))

(defun gcr--commentize (text)
  (let ((comment-prefix (gcr--comment-prefix)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (replace-regexp "^" comment-prefix)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gcr--decommentize (text)
  (let ((comment-prefix (gcr--comment-prefix)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (replace-regexp (concat "^" comment-prefix) "")
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gcr--save-changes (text m1 &optional m2)
  (let ((new-comment-p (plist-get gcr--state :new-comment-p))
        (edit-comment-p (plist-get gcr--state :edit-comment-p)))
    (with-current-buffer (marker-buffer m1)
      (goto-char (marker-position m1))
      (when m2
        (beginning-of-line)
        (delete-region (point) (marker-position m2)))
      (unless (or new-comment-p edit-comment-p)
        (gcr--insert (gcr--commentize gcr--comment-separator))
        (next-line))
      (gcr--insert (gcr--commentize (string-trim text))))))

(defun gcr--editor-save ()
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (apply gcr--save-callback text gcr--save-callback-args)
    (quit-window)))

(defun gcr--editor-cancel ()
  (interactive)
  (erase-buffer)
  (quit-window))

(defun gcr--open-editor (slug-template
                           state
                           save-callback
                           m1
                           &optional m2)
  (let ((buf (get-buffer-create "*gcr-edit*")))
    (with-current-buffer buf
      (erase-buffer)
      (set-variable 'gcr--save-callback save-callback t)
      (set-variable 'gcr--save-callback-args (list m1 m2) t)
      (set-variable 'gcr--state state t)
      (cond
       ((stringp slug-template)
        (progn
          (gcr--insert
           (format slug-template user-login-name))
          (save-excursion (insert "Comment subject"))))
       ((functionp slug-template)
        (funcall slug-template)))
      (set-variable 'fill-column 60 t)
      (auto-fill-mode 1)
      (local-set-key (kbd "\C-c\C-c") 'gcr--editor-save)
      (local-set-key (kbd "\C-c\C-k") 'gcr--editor-cancel))
    (pop-to-buffer buf)))

(defun gcr--make-marker ()
  (let ((m1 (make-marker)))
    (set-marker m1 (point))
    m1))

(defun gcr-prev-review ()
  (interactive)
  (beginning-of-line)
  (re-search-backward (format "%s%s"
                              (gcr--comment-prefix)
                              gcr--review-slug)
                      nil t))

(defun gcr-next-review ()
  (interactive)
  (re-search-forward gcr--review-end nil t)
  (re-search-forward gcr--review-slug nil t))

(defun gcr-new-review ()
  (interactive)
  (gcr--open-editor (concat gcr--review-slug "(%s): ")
                      '(:new-comment-p t)
                      'gcr--save-changes
                      (gcr--make-marker)))

(defun gcr-add-comment ()
  (interactive)
  (if (not (or (equal gcr--review-slug (gcr--thing-at-point))
               (re-search-backward gcr--review-slug nil t)))
      (gcr-new-review)
    (re-search-forward gcr--review-end nil t)
    (beginning-of-line)
    (gcr--open-editor "(%s): "
                        '(:new-comment-p nil)
                        'gcr--save-changes
                        (gcr--make-marker))))

(defun gcr-edit-comment ()
  (interactive)
  (let ((author (format "(%s)" user-login-name)))
    (if (not (or (equal (thing-at-point 'list t) author)
                 (re-search-backward author nil t)))
        (gcr-new-review)
      (let ((here (point))
            (m1 (gcr--make-marker)))
        (re-search-forward gcr--comment-separator nil t)
        (beginning-of-line)
        (let ((text (gcr--decommentize
                     (buffer-substring-no-properties here (point)))))
          (gcr--open-editor (lambda () (insert text))
                              '(:edit-comment-p t)
                              'gcr--save-changes
                              m1
                              (gcr--make-marker)))))))

(defvar gcr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cvn" 'gcr-next-review)
    (define-key map "\C-c\C-vn" 'gcr-next-review)
    (define-key map "\C-c\C-vp" 'gcr-prev-review)
    (define-key map "\C-c\C-vr" 'gcr-new-review)
    (define-key map "\C-c\C-vc" 'gcr-add-comment)
    (define-key map "\C-c\C-ve" 'gcr-edit-comment)
    map))

(define-minor-mode gcr-mode
  "Git Code Review"
  nil                                   ; initial-value
  :keymap gcr-mode-map
  :lighter " GCR")

(provide 'git-code-review)
