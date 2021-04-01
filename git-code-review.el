;;; git-code-review.el --- Git Code Review minor mode

(require 'magit)

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
            (concat comment-start comment-start))
          " "))

(defun gcr--commentize (text)
  (let ((comment-prefix (gcr--comment-prefix)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match comment-prefix))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gcr--decommentize (text)
  (let ((comment-prefix (gcr--comment-prefix)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward (concat "^" comment-prefix) nil t)
        (replace-match ""))
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
        (forward-line))
      (gcr--insert (gcr--commentize (string-trim text)))
      (when new-comment-p
        (forward-line)
        (gcr--insert (gcr--commentize gcr--review-end))))))

(defun gcr--editor-save ()
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (apply gcr--save-callback text gcr--save-callback-args)
    (quit-window)))

(defun gcr--editor-cancel ()
  (interactive)
  (erase-buffer)
  (quit-window))

;;; (ram): GCR editor needs its own mode
;;; The GCR editor is just a Fundamental mode buffer
;;; currently. It should be its own (minor) mode, so it could
;;; support mixed mode editing
;;; ----------------------------------------------------------------
(defun gcr--open-editor (slug-template
                         state
                         save-callback
                         m1
                         &optional m2)
  (let ((buf (get-buffer-create "*gcr-edit*")))
    (with-current-buffer buf
      (indented-text-mode)
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
      (local-set-key (kbd "\C-c\C-k") 'gcr--editor-cancel)
      (setq header-line-format
            "Edit, then exit with 'C-c C-c' to save or 'C-c C-k' to cancel"))
    (pop-to-buffer buf)))

(defun gcr--make-marker ()
  (let ((m1 (make-marker)))
    (set-marker m1 (point))
    m1))

(defun gcr--review-prefix ()
  (format "%s%s" (gcr--comment-prefix) gcr--review-slug))

(defun gcr-prev-review ()
  (interactive)
  (beginning-of-line)
  (unless (re-search-backward (gcr--review-prefix) nil t)
    (message "No more reviews")))

(defun gcr-next-review ()
  (interactive)
  (re-search-forward gcr--review-end nil t)
  (unless (re-search-forward (gcr--review-prefix) nil t)
    (message "No more reviews")))

(defun gcr--in-review-p ()
  (save-excursion
   (let ((end (point)))
     (re-search-backward (gcr--review-prefix) nil t)
     (beginning-of-line)
     (and (not (= (point) end))
          (comment-only-p (point) end)))))

(defun gcr-new-review ()
  "This command will open a Git Code Review editor buffer and a
new REVIEW block will be created from its contents."
  (interactive)
  (gcr--open-editor (concat gcr--review-slug "(%s): ")
                    '(:new-comment-p t)
                    'gcr--save-changes
                    (gcr--make-marker)))

(defun gcr-add-comment ()
  "If point is within an inline review block, this command will
open a Git Code Review editor buffer whose contents can be saved
as a new comment in the review block. If point is not in an
inline review block, a new REVIEW block will be created from the
contents of the Git Code Review editor buffer."
  (interactive)
  (if (not (gcr--in-review-p))
      (gcr-new-review)
    (beginning-of-line)
    (comment-forward (point-max))
    (forward-line -1)
    (gcr--open-editor "(%s): "
                      '(:new-comment-p nil)
                      'gcr--save-changes
                      (gcr--make-marker))))

(defun gcr-edit-comment ()
  "When point is on an inline review comment, this command will
open the comment text in a separate Git Code Review editor
buffer."
  (interactive)
  (save-excursion
    (let ((author (format "(%s)" user-login-name)))
      (if (not (or (equal (thing-at-point 'list t) author)
                   (re-search-backward author nil t)))
          (gcr-new-review)
        (let ((here (progn
                      (forward-line 0)
                      (point)))
              (m1 (gcr--make-marker)))
          (re-search-forward gcr--comment-separator nil t)
          (beginning-of-line)
          (let ((text (gcr--decommentize
                       (buffer-substring-no-properties here (point)))))
            (gcr--open-editor (lambda () (insert text))
                              '(:edit-comment-p t)
                              'gcr--save-changes
                              m1
                              (gcr--make-marker))))))))

(defvar gcr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cRn" 'gcr-next-review)
    (define-key map "\C-cRp" 'gcr-prev-review)
    (define-key map "\C-cRr" 'gcr-new-review)
    (define-key map "\C-cRc" 'gcr-add-comment)
    (define-key map "\C-cRe" 'gcr-edit-comment)
    map))

(define-minor-mode gcr-mode
  "Git Code Review"
  nil                                   ; initial-value
  :keymap gcr-mode-map
  :lighter " GCR")

(defun gcr--sh (command &rest args)
  (let ((buf (get-buffer-create "*gcr-sh*")))
    (unwind-protect
         (let ((rc (apply 'call-process command nil buf nil args)))
           (with-current-buffer buf
             (list :rc rc
                   :out (buffer-substring-no-properties
                         (point-min)
                         (point-max)))))
      (kill-buffer buf))))

(defun gcr--workdir-dirty-p ()
  (destructuring-bind (&key rc out)
      (gcr--sh "git" "diff" "--quiet")
    (< 0 rc)))

(defun gcr-review-branch (&optional branch-name)
  (interactive (list
                (magit-read-branch "Branch")))
  (when (gcr--workdir-dirty-p)
    (if (y-or-n-p "Dirty workdir, stash changes? ")
        (magit-stash-both
         (format "GCR-review-%s"
                 (format-time-string "%FT%T")))
      (error "Unstashed changes are in the way")))
  (let ((local-branch (string-remove-prefix "origin/" branch-name)))
    (if (not (magit-commit-p local-branch))
        (magit-branch-and-checkout local-branch branch-name)
      (magit-checkout local-branch)
      (magit-pull-branch local-branch '()))
    (magit-diff-range (concat "master..." local-branch))))

(provide 'git-code-review)
