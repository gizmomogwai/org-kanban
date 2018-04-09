;;; org-kanban.el --- kanban dynamic block for org-mode. -*- lexical-binding: t -*-
;; Copyright (C) 2016 Christian Köstlin

;; This file is NOT part of GNU Emacs.

;; Author: Christian Köstlin <christian.koestlin@gmail.com>
;; Contributors:
;;         Aldric Giacomoni <trevoke@gmail.com>
;; Keywords: org-mode, org, kanban, tools
;; Package-Requires: ((dash "2.13.0") (emacs "24.4"))
;; Package-Version: 0.4.0
;; Homepage: http://github.com/gizmomogwai/org-kanban

;;; Commentary:
;; To create a kanban table for an org file, simply put the dynamic block
;; `
;; #+BEGIN: kanban
;; #+END:
;; '
;; somewhere and run `C-c C-c' on it.  You can use
;; `org-kanban/initialize' to get this generated.

;;; Code:

(require 'org)
(require 'dash)
(require 'subr-x)

(defgroup org-kanban nil
  "Settings for org-kanban."
  :group 'org
  :prefix "org-kanban")
(defcustom org-kanban/prev-key "j"
  "Key for promoting an entry of a kanban table to the previous state."
  :group 'org-kanban)
(defcustom org-kanban/next-key "k"
  "Key for promoting an entry of a kanban table to the next state."
  :group 'org-kanban)

(defun org-kanban//get-title (todo)
  "Get the title from a heading TODO."
  (nth 4 todo))

(defun org-kanban//get-todo (todo)
  "Get the todo keyword from a heading TODO."
  (nth 2 todo))

(defun org-kanban//link (file heading kanban search-for)
  "Create a link to FILE and HEADING if the KANBAN value is equal to SEARCH-FOR."
  (if (and (stringp kanban) (string-equal search-for kanban))
    (format "[[file:%s::%s][%s]]" file heading heading) ""))

(defun org-kanban//todo-keywords (files mirrored)
  "Get list of org todos from FILES.  MIRRORED describes if keywords should be reversed."
  (save-window-excursion
    (let* (
        (list-of-keywords (-map
          (lambda(file)
            (find-file file)
            (if mirrored (reverse org-todo-keywords-1) org-todo-keywords-1))
          files))
        (res (--reduce-from (-union acc it) (car list-of-keywords) list-of-keywords)))
      res)))

(defun org-kanban//row-for (file-and-todo todo-keywords)
  "Convert a kanban FILE-AND-TODO to a row of a org-table.
TODO-KEYWORDS are all the current org todos."
  (let* (
      (file (nth 0 file-and-todo))
      (todo (nth 1 file-and-todo))
      (title (org-kanban//get-title todo))
      (kanban (org-kanban//get-todo todo))
      (row-entries (-map (lambda(i) (org-kanban//link file title i kanban)) todo-keywords))
      (row (string-join row-entries "|")))
    (format "|%s|" row)))

(require 're-builder)
(setq reb-re-syntax 'string)
(defun org-kanban//find ()
  "Search for a todo matching to the current kanban table row.
Return file and marker."
  (let* (
      (line-start (save-excursion
        (move-beginning-of-line 1)
        (point)))
      (line-end (save-excursion
        (move-end-of-line 1)
        (point)))
      (line (buffer-substring-no-properties line-start line-end))
      (pattern "\\[\\[file:\\(.*\\)::\\(.*\\)\\]\\[.*\\]")
      (match (string-match pattern line))
      (file (and match (match-string 1 line)))
      (heading (and match (match-string 2 line)))
      (entry (and heading (save-excursion
        (find-file file)
        (org-find-exact-headline-in-buffer heading)))))
    (list file entry)))

(defun org-kanban/next ()
  "Move the todo entry in the current line of the kanban table to the next state."
  (interactive)
  (org-kanban//move 'right))

(defun org-kanban/prev ()
  "Move the todo entry in the current line of the kanban table to the previous state."
  (interactive)
  (org-kanban//move 'left))

(defun org-kanban/shift (&optional left-or-right)
  "Move todo to LEFT-OR-RIGHT (repeatedly)."
  (interactive)
  (org-kanban//move (if left-or-right left-or-right 'right))
  (message (format "Use %s and %s to shift" org-kanban/prev-key org-kanban/next-key))
  (set-transient-map
    (let* (
        (map (make-sparse-keymap)))
        (define-key map org-kanban/prev-key (lambda () (interactive) (org-kanban/shift 'left)))
        (define-key map org-kanban/next-key (lambda () (interactive) (org-kanban/shift 'right)))
      map)))

(defun org-kanban/initialize (&optional arg)
  "Create an org-kanban dynamic block at position ARG."
  (interactive "p")
  (cond (
    (eq arg nil) (org-kanban/initialize-here))
    ((eq arg 1) (org-kanban/initialize-here))
    ((eq arg 4) (org-kanban/initialize-at-beginning))
    ((eq arg 16) (org-kanban/initialize-at-end))
    (t (error (message "Unsupported universal argument %s" arg)))))

(defun org-kanban/initialize-at-beginning ()
  "Create an org-kanban dynamic block at the beginning of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (org-kanban//initialize-mirrored-kanban-at-point)))

(defun org-kanban/initialize-at-end ()
  "Create an org-kanban dynamic block at the end of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (newline)
    (org-kanban//initialize-mirrored-kanban-at-point)))

(defun org-kanban/initialize-here ()
  "Create an org-kanban dynamic block at the point."
  (interactive)
  (save-excursion
    (org-kanban//initialize-mirrored-kanban-at-point)))

(defun org-kanban//initialize-mirrored-kanban-at-point ()
  "Create an org-kanban dynamic block at the point."
  (save-excursion
    (insert "#+BEGIN: kanban :mirrored t\n#+END:\n"))
  (org-ctrl-c-ctrl-c))

(defun org-kanban//move (direction)
  "Move the todo entry in the current line of the kanban table to the next state in direction DIRECTION."
  (save-window-excursion
  (if (memq direction (list 'left 'right))
    (let* (
        (file-and-marker (org-kanban//find))
        (line (line-number-at-pos)))
      (if file-and-marker
        (let* (
            (file (nth 0 file-and-marker))
            (marker (nth 1 file-and-marker)))
          (progn
            (save-excursion
              (find-file file)
              (goto-char marker)
              (let* (
                  (current (substring-no-properties (org-get-todo-state)))
                  (border (car (if (eq direction 'right) (reverse org-todo-keywords-1) org-todo-keywords-1)))
                  (change (not (string-equal current border))))
                (if change (org-todo direction))))
            (org-dblock-update)
            (goto-char 0)
            (forward-line (1- line))
            (goto-char (search-forward "[[")))))))))

;;;###autoload
(defun org-dblock-write:kanban (params)
  "Create the kanban dynamic block.  PARAMS are ignored right now."
  (insert
   (let*
       (
        (mirrored (plist-get params :mirrored))
        (files (or (mapcar 'symbol-name (plist-get params :files)) (list buffer-file-name)))
        (todo-keywords (org-kanban//todo-keywords files mirrored))
        (todos (org-map-entries (lambda() (list (current-buffer)
                                           (org-heading-components) org-todo-keywords-1)) t files))
        (row-for (lambda(i) (org-kanban//row-for i todo-keywords)))
        (rows (-map row-for (-filter
                             (lambda(todo)
                               (-intersection
                                (list (org-kanban//get-todo (nth 1 todo)))
                                (nth 2 todo))) todos)))
        (table (--reduce (format "%s\n%s" acc it) rows))
        (table-title (string-join todo-keywords "|"))
        )
     (format "|%s|\n|--|\n%s" table-title table)))
  (org-table-align))

(provide 'org-kanban)
;;; org-kanban.el ends here
