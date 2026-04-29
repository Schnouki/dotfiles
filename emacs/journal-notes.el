;;; journal-notes --- Custom journaling helpers for org-roam  -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; This module provides a custom journaling setup for org-roam, with separate
;;; daily and weekly notes. It doesn't rely on org-capture, which doesn't really
;;; fit my workflow; instead, there are templates for creating daily and weekly
;;; notes, which can then be manually edited as needed.
;;;
;;; Code:

(require 'org)
(require 'org-roam)

(defcustom journal-notes-daily-directory "daily"
  "Directory for daily notes, relative to `org-roam-directory'."
  :type 'string
  :group 'journal-notes)
(defcustom journal-notes-weekly-directory "weekly"
  "Directory for weekly notes, relative to `org-roam-directory'."
  :type 'string
  :group 'journal-notes)
(defcustom journal-notes-daily-template (file-name-concat org-roam-directory "templates/daily.org")
  "Template file for daily notes."
  :type 'string
  :group 'journal-notes)
(defcustom journal-notes-weekly-template (file-name-concat org-roam-directory "templates/weekly.org")
  "Template file for weekly notes."
  :type 'string
  :group 'journal-notes)
(defcustom journal-notes-daily-filename-format "%Y-%m-%d.org"
  "Filename format for daily notes, passed to `format-time-string'."
  :type 'string
  :group 'journal-notes)
(defcustom journal-notes-weekly-filename-format "%Y-w%V.org"
  "Filename format for weekly notes, passed to `format-time-string'."
  :type 'string
  :group 'journal-notes)
(defcustom journal-notes-daily-title-format "%Y-%m-%d"
  "Title format for daily notes, passed to `format-time-string'."
  :type 'string
  :group 'journal-notes)
(defcustom journal-notes-weekly-title-format "%Y-w%V"
  "Title format for weekly notes, passed to `format-time-string'."
  :type 'string
  :group 'journal-notes)


(defvar journal-notes-map (make-sparse-keymap)
  "Keymap for `journal-notes'.")

(defun journal-notes--get-filename (type date)
  (cl-case type
    (daily (file-name-concat (expand-file-name journal-notes-daily-directory org-roam-directory)
                             (format-time-string journal-notes-daily-filename-format date)))
    (weekly (file-name-concat (expand-file-name journal-notes-weekly-directory org-roam-directory)
                              (format-time-string journal-notes-weekly-filename-format date)))
    (t (user-error "Invalid type"))))

(defun journal-notes--get-title (type date)
  (cl-case type
    (daily (format-time-string journal-notes-daily-title-format date))
    (weekly (format-time-string journal-notes-weekly-title-format date))
    (t (user-error "Invalid type"))))

(defun journal-notes--get-template (type)
  (cl-case type
    (daily (org-file-contents journal-notes-daily-template))
    (weekly (org-file-contents journal-notes-weekly-template))
    (t (user-error "Invalid type"))))

(defun journal-notes--goto-date (type date)
  (let* ((filename (journal-notes--get-filename type date))
         (buffer (find-file filename)))
    (unless (file-exists-p filename)
      (with-current-buffer buffer
        (org-id-get-create)
        (goto-char (point-max))
        (insert "#+title: " (journal-notes--get-title type date) "\n\n"
                (journal-notes--get-template type))))
    (pop-to-buffer-same-window buffer)))

(defun journal-notes--goto-daily (delta)
  (journal-notes--goto-date 'daily (time-add (* 86400 delta) (current-time))))

(defun journal-notes--goto-weekly (delta)
  (journal-notes--goto-date 'weekly (time-add (* 86400 7 delta) (current-time))))

(defun journal-notes-goto-date ()
  "Go to a daily note for a specific date."
  (interactive)
  (let ((date (org-read-date nil t)))
    (journal-notes--goto-date 'daily date)))

(defun journal-notes-goto-today ()
  "Go to today's daily note."
  (interactive)
  (journal-notes--goto-daily 0))

(defun journal-notes-goto-tomorrow ()
  "Go to tomorrow's daily note."
  (interactive)
  (journal-notes--goto-daily +1))

(defun journal-notes-goto-yesterday ()
  "Go to yesterday's daily note."
  (interactive)
  (journal-notes--goto-daily -1))

(defun journal-notes-goto-week ()
  "Go to a weekly note for a specific date."
  (interactive)
  (let ((date (org-read-date nil t)))
    (journal-notes--goto-date 'weekly date)))

(defun journal-notes-goto-this-week ()
  "Go to this week's weekly note."
  (interactive)
  (journal-notes--goto-weekly 0))

(defun journal-notes-goto-next-week ()
  "Go to next week's weekly note."
  (interactive)
  (journal-notes--goto-weekly +1))

(defun journal-notes-goto-previous-week ()
  "Go to the previous week's weekly note."
  (interactive)
  (journal-notes--goto-weekly -1))

(defun journal-notes--daily-note-p (&optional file)
  "Return t if FILE is an Org-roam journal-note daily note, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (when-let* ((path (expand-file-name
                     (or file
                         (buffer-file-name (buffer-base-buffer)))))
              (daily-directory (expand-file-name journal-notes-daily-directory org-roam-directory)))
    (save-match-data
      (and
       (org-roam-file-p path)
       (org-roam-descendant-of-p path daily-directory)))))

(defun journal-notes--weekly-note-p (&optional file)
  "Return t if FILE is an Org-roam journal-note weekly note, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (when-let* ((path (expand-file-name
                     (or file
                         (buffer-file-name (buffer-base-buffer)))))
              (weekly-directory (expand-file-name journal-notes-weekly-directory org-roam-directory)))
    (save-match-data
      (and
       (org-roam-file-p path)
       (org-roam-descendant-of-p path weekly-directory)))))

(defun journal-notes-insert-daily-link (&optional arg)
  "Insert a link to a daily note."
  (interactive "P")
  (when-let* ((node
               (if arg
                   (org-roam-node-from-title-or-alias
                    (format-time-string journal-notes-daily-title-format
                                        (org-read-date nil t)))
                 (org-roam-node-read (format-time-string journal-notes-daily-title-format)
                                     (lambda (node)
                                       (journal-notes--daily-note-p
                                        (org-roam-node-file node)))
                                     nil t "Date: "))))
    (let ((id (org-roam-node-id node))
          (description (org-roam-node-title node)))
      (insert (org-link-make-string
               (concat "id:" id)
               description))
      (run-hook-with-args 'org-roam-post-node-insert-hook
                          id
                          description))))

(defun journal-notes--find-next-note (&optional n)
  "Find the next daily or weekly note.

With numeric argument N, find the note N days/weeks in the future (or in
the past if N is negative)."
  (interactive "p")
  (setq n (or n 1))
  (when (= n 0)
    (user-error "Use with non-zero value"))
  (let* ((note-path (expand-file-name (buffer-file-name (buffer-base-buffer))))
         (note-type (cond ((journal-notes--daily-note-p) 'daily)
                          ((journal-notes--weekly-note-p) 'weekly)
                          (t (user-error "Invalid type"))))
         (note-p (if (eql note-type 'daily) #'journal-notes--daily-note-p
                   #'journal-notes--weekly-note-p))
         (notes-directory (if (eql note-type 'daily)
                              (expand-file-name journal-notes-daily-directory org-roam-directory)
                            (expand-file-name journal-notes-weekly-directory org-roam-directory)))
         (regexp (rx-to-string `(seq (literal ".") (or ,@org-roam-file-extensions) eos)))
         (all-notes (seq-remove (lambda (file)
                                  (let ((name (file-name-nondirectory file)))
                                    (or (auto-save-file-name-p name)
                                        (backup-file-name-p name)
                                        (string-match "^\\." name))))
                                (directory-files-recursively notes-directory regexp)))
         (position (cl-position note-path all-notes :test #'string=)))
    (unless position
      (user-error "Can't find current note file. Have you saved it yet?"))
    (when (< n 0)
      (setq all-notes (nreverse all-notes)
            position (- (length all-notes) position 1)
            n (- n)))
    (cl-do ((next-notes (nthcdr (1+ position) all-notes)
                        (cdr next-notes))
            (i 0))
        ((null next-notes))
      (when (funcall note-p (car next-notes))
        (cl-incf i)
        (when (= i n)
          (cl-return (car next-notes)))))))

(defun journal-notes-goto-next (&optional n)
  "Go to the next existing note.

With numeric argument, find the note N days/weeks in the future (or in
the past), and go to that one instead."
  (interactive "p")
  (let ((filename (journal-notes--find-next-note n)))
    (if filename
        (find-file-existing filename)
      (user-error "No next note found."))))

(defun journal-notes-goto-previous ()
  "Go to the previous existing note."
  (interactive)
  (let ((filename (journal-notes--find-next-note -1)))
    (if filename
        (find-file-existing filename)
      (user-error "No previous note found."))))

(provide 'journal-notes)

;;; journal-notes.el ends here
