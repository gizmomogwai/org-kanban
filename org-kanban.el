;;; org-kanban.el --- kanban dynamic block for org-mode. -*- lexical-binding: t -*-
;; Copyright (C) 2016 Christian Köstlin

;; This file is NOT part of GNU Emacs.

;; Author: Christian Köstlin <christian.koestlin@gmail.com>
;; Contributors:
;;         Aldric Giacomoni <trevoke@gmail.com>
;; Keywords: org-mode, org, kanban, tools
;; Package-Requires: ((dash "2.13.0") (emacs "24.4") (org "9.1"))
;; Package-Version: 0.4.8
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

(defun org-kanban//sanity-check-parameters (context layout)
  "Check for CONTEXT if LAYOUT is consistent."
  (let ((link-abbreviation (car layout))
         (link-max-length (cdr layout)))
    (if link-max-length
      (if (>= (length link-abbreviation) link-max-length)
        (error "Problem with %s link-abbreviation (%s) is >= link-max-length (%s)" context link-abbreviation link-max-length)))))

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
(defcustom org-kanban/layout (cons "..." 1000)
  "Layout for long descriptions."
  :type '(cons
           (string :tag  "abbreviation")
           (integer :tag "max-length  "))
  :set (lambda (symbol value)
         (progn
           (org-kanban//sanity-check-parameters "customize-set" value)
           (set-default symbol value)))
  :group 'org-kanban)

(defun org-kanban//todo-info-extract ()
  "..."
  (list
    (current-buffer)
    (org-heading-components)
    org-todo-keywords-1
    (org-entry-get nil "CUSTOM_ID")
    (org-entry-get nil "ID")
    ))

(defun org-kanban//todo-info-get-file (todo-info)
  "Get the buffer from a TODO-INFO."
  (nth 0 todo-info))

(defun org-kanban//todo-info-get-heading (todo-info)
  "Get the heading info from a TODO-INFO."
  (nth 1 todo-info))

(defun org-kanban//todo-info-get-keywords (todo-info)
  "Get the allowed keywords for a TODO-INFO."
  (nth 2 todo-info))

(defun org-kanban//todo-info-get-custom-id (todo-info)
  "Get the CUSTOM_ID from a heading TODO-INFO."
  (nth 3 todo-info))

(defun org-kanban//todo-info-get-id (todo-info)
  "Get the ID from a heading TODO-INFO."
  (nth 4 todo-info))

(defun org-kanban//heading-get-title (heading)
  "Get the title from an `org-mode` HEADING."
  (nth 4 heading))

(defun org-kanban//heading-get-todo-keyword (todo)
  "Get the TODO keyword from an `org-mode` HEADING."
  (nth 2 todo))

(defun org-kanban//heading-to-description (heading layout)
  "Create a description from a HEADING.  The description is truncated according to the LAYOUT cons (e.g. (\"...\" . 10))."
  (org-kanban//sanity-check-parameters "sanity-check" layout)
  (let* ((link-abbreviation (car layout))
          (max-length (cdr layout)))
    (if (> (length heading) max-length)
      (concat
        (substring heading 0 (- max-length (length link-abbreviation)))
        link-abbreviation)
      heading)))

(defun org-kanban//link-for-custom-id (custom-id use-file file description)
  "Create a link for CUSTOM-ID, optionally USE-FILE FILE and DESCRIPTION."
  (if custom-id
    (if use-file
      (format "[[file:%s::#%s][%s]]" file custom-id description)
      (format "[[#%s][%s]]" custom-id description))
    nil))

(defun org-kanban//link-for-id (id description)
  "Create a link for ID with DESCRIPTION."
  (if id
    (format "[[id:%s][%s]]" id description)
    nil))

(defun org-kanban//link-for-heading (heading use-file file description)
  "Create a link for a HEADING optionally USE-FILE a FILE and DESCRIPTION."
  (if heading
    (if use-file
      (format "[[file:%s::*%s][%s]]" file heading description)
      (format "[[%s][%s]]" heading description))
    (error "Illegal state")))

(defun org-kanban//link (file heading kanban search-for multi-file custom-id id layout)
  "Create a link to FILE and HEADING if the KANBAN value is equal to SEARCH-FOR.
MULTI-FILE indicates if the link must work across several files.
CUSTOM-ID links are used if given.
ID links are used if given.
LAYOUT is the specification to layout long links.
This means, that the org-kanban table links are in one of several forms:
 - file:#custom-id
 - #custom-id
 - id:id
 - file:heading
 - heading"
  (if
    (and (stringp kanban) (string-equal search-for kanban))
    (let* (
            (description (funcall layout heading))
            (use-file (and multi-file (not (eq file (current-buffer)))))
            )
      (or (org-kanban//link-for-custom-id custom-id use-file file description)
        (org-kanban//link-for-id id description)
        (org-kanban//link-for-heading heading use-file file description)
        ))
    ""))

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

(defun org-kanban//row-for (todo-info todo-keywords multi-file layout)
  "Convert a kanban TODO-INFO to a row of a org-table.
TODO-KEYWORDS are all the current org todos.
MULTI-FILE indicates, if simple file links may be used.
LAYOUT specification."
  (let* (
          (file (org-kanban//todo-info-get-file todo-info))
          (heading (org-kanban//todo-info-get-heading todo-info))
          (title (org-kanban//heading-get-title heading))
          (kanban (org-kanban//heading-get-todo-keyword heading))
          (custom-id (org-kanban//todo-info-get-custom-id todo-info))
          (id (org-kanban//todo-info-get-id todo-info))
          (row-entries (-map (lambda(i) (org-kanban//link file title i kanban multi-file custom-id id layout)) todo-keywords))
          (row (string-join row-entries "|")))
    (format "|%s|" row)))

(require 're-builder)
(setq reb-re-syntax 'string)

(defun org-kanban//find-by-file-and-custom-id (line)
  "Search for a todo in a LINE with file and custom_id."
  (let* (
          (pattern "\\[\\[file:\\(.*\\)::#\\(.*\\)\\]\\[.*\\]")
          (match (string-match pattern line))
          (file (and match (match-string 1 line)))
          (custom-id (and match (match-string 2 line)))
          (entry (and custom-id (save-excursion
                                  (find-file file)
                                  (org-find-property "CUSTOM_ID" custom-id)))))
    (if entry (list file entry) nil)))

(defun org-kanban//find-by-file-and-heading (line)
  "Search for a todo in a LINE with file and heading."
  (let* (
          (pattern "\\[\\[file:\\(.*\\)::\\(.*\\)\\]\\[.*\\]")
          (match (string-match pattern line))
          (file (and match (match-string 1 line)))
          (heading (and match (match-string 2 line)))
          (entry (and heading (save-excursion
                                (find-file file)
                                (org-find-exact-headline-in-buffer heading)))))
    (if entry (list file entry) nil)))

(defun org-kanban//find-by-custom-id (line)
  "Try to find a todo by custom id in LINE."
  (message "find by custom id %s" line)
  (let* (
          (pattern "\\[\\[#\\(.*\\)\\]\\[.*\\]")
          (match (string-match pattern line))
          (custom-id (and match (match-string 1 line)))
          (entry (and custom-id (org-find-property "CUSTOM_ID" custom-id))))
    (if entry (list (buffer-file-name) entry) nil)))

(defun org-kanban//find-by-heading (line)
  "Try to find a todo by heading in LINE."
  (let* (
          (pattern "\\[\\[\\(.*\\)\\]\\[.*\\]")
          (match (string-match pattern line))
          (heading (and match (match-string 1 line)))
          (entry (and heading (org-find-exact-headline-in-buffer heading))))
    (if entry (list (buffer-file-name) entry) nil)))

(defun org-kanban//find-by-id (line)
  "Try to find a todo by id in LINE."
  (let* (
          (pattern "\\[\\[id:\\(.*\\)\\]\\[.*\\]")
          (match (string-match pattern line))
          (id (and match (match-string 1 line)))
          (entry (and id (org-find-entry-with-id id))))
    (if entry (list (buffer-file-name) entry) nil)))

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
          (line (buffer-substring-no-properties line-start line-end)))
    (or (org-kanban//find-by-file-and-custom-id line)
      (org-kanban//find-by-file-and-heading line)
      (org-kanban//find-by-custom-id line)
      (org-kanban//find-by-id line)
      (org-kanban//find-by-heading line))))

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

;;;###autoload
(defun org-kanban/initialize (&optional arg)
  "Create an org-kanban dynamic block at position ARG."
  (interactive "p")
  (cond (
          (eq arg nil) (org-kanban/initialize-here))
    ((eq arg 1) (org-kanban/initialize-here))
    ((eq arg 4) (org-kanban/initialize-at-beginning))
    ((eq arg 16) (org-kanban/initialize-at-end))
    (t (error (message "Unsupported universal argument %s" arg)))))

;;;###autoload
(defun org-kanban/initialize-at-beginning ()
  "Create an org-kanban dynamic block at the beginning of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (org-kanban//initialize-mirrored-kanban-at-point)))

;;;###autoload
(defun org-kanban/initialize-at-end ()
  "Create an org-kanban dynamic block at the end of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (newline)
    (org-kanban//initialize-mirrored-kanban-at-point)))

;;;###autoload
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

(defun org-kanban//params-layout (params)
  "Calculate layout func based on PARAMS."
  (let* (
          (l (plist-get params :layout))
          (layout (pcase l
                    (`nil (lambda(s) (org-kanban//heading-to-description s org-kanban/layout)))
                    ((pred functionp) (lambda(s) (funcall l s)))
                    ((pred symbolp) (lambda(s) (org-kanban//heading-to-description s (symbol-value l))))
                    ((pred consp) (lambda(s) (org-kanban//heading-to-description s l)))
                    (_ (error (format "Unknown type %s" l))))))
    layout))

(defun org-kanban//params-files (params)
  "Calculate files based on PARAMS."
  (let* (
          (scope (plist-get params :scope))
          (files (pcase scope
                   (`nil (list buffer-file-name))
                   (`tree (list buffer-file-name))
                   (_  (-map (lambda(file) (symbol-name file)) scope)))))
    files))

(defun org-kanban//params-scope (params files)
  "Calculate scope based on PARAMS and FILES."
  (let* (
          (scope (plist-get params :scope))
          (result (pcase scope
                    (`nil files)
                    (`tree scope)
                    (_ files))))
    result))

;;;###autoload
(defun org-dblock-write:kanban (params)
  "Create the kanban dynamic block.
PARAMS may contain `:mirrored`, `:match`, `:scope` and `:layout`."
  (insert
    (let*
      (
        (mirrored (plist-get params :mirrored))
        (match (plist-get params :match))
        (layout (org-kanban//params-layout params))
        (files (org-kanban//params-files params))
        (scope (org-kanban//params-scope params files))
        (multi-file (> (length files) 1))
        (todo-keywords (org-kanban//todo-keywords files mirrored))
        (todo-infos (org-map-entries 'org-kanban//todo-info-extract match scope))
        (row-for (lambda(todo-info) (org-kanban//row-for todo-info todo-keywords multi-file layout)))
        (rows (-map row-for (-filter
                              (lambda(todo-info)
                                (-intersection
                                  (list (org-kanban//heading-get-todo-keyword (org-kanban//todo-info-get-heading todo-info)))
                                  (org-kanban//todo-info-get-keywords todo-info)))
                              todo-infos)))
        (table (--reduce (format "%s\n%s" acc it) rows))
        (table-title (string-join todo-keywords "|"))
        )
      (format "|%s|\n|--|\n%s" table-title table)))
  (org-table-align))

(defun org-kanban/version ()
  "Print org-kanban version."
  (interactive)
  (message "org-kanban 0.4.8"))

(define-derived-mode org-kanban-configure-mode special-mode
  '("org-kanban-configure"))

(define-button-type 'org-kanban--match-button
  'help-echo "Change match string"
  'action #'org-kanban--match-action)

(define-button-type 'org-kanban--apply-button
  'help-echo "Apply change"
  'action #'org-kanban--apply-action)

(define-button-type 'org-kanban--mirrored-button
  'help-echo "Change mirrored type"
  'action #'org-kanban--mirrored-button-action)
(define-button-type 'org-kanban--layout-button
  'help-echo "Change layout"
  'action #'org-kanban--layout-action)
(define-button-type 'org-kanban--scope-button
  'help-echo "Change scope"
  'action #'org-kanban--scope-action)

(defun org-kanban--scope-action (button)
  "Set scope from a BUTTON."
  (let* (
          (position (point))
          (parameters (button-get button 'parameters))
          (scope (plist-get parameters :scope))
          (delete (button-get button 'delete)))
    (if delete
      (plist-put parameters :scope nil)
      (let* (
              (default-scope (if scope (format "%s" scope) nil))
              (new-scope (read-string "Scope: " default-scope)))
        (plist-put parameters :scope new-scope)))
    (org-kanban//show-configure-buffer
      (button-get button 'buffer)
      (button-get button 'beginning)
      parameters
      position)))

(defun org-kanban--layout-action (button)
  "Set layout from a BUTTON."
  (let* (
          (position (point))
          (parameters (button-get button 'parameters))
          (layout (plist-get parameters :layout))
          (delete (button-get button 'delete)))
    (if delete
      (plist-put parameters :layout nil)
      (let* (
              (default-s (if layout (car layout) (car org-kanban/layout)))
              (default-max (if layout (cdr layout) (cdr org-kanban/layout)))
              (s (read-string "Abbreviation: " default-s))
              (max (read-string "Max width: " (format "%s" default-max))))
        (plist-put parameters :layout (cons s max))))
    (org-kanban//show-configure-buffer
      (button-get button 'buffer)
      (button-get button 'beginning)
      parameters
      position)))

(defun org-kanban--match-action (button)
  "Set the current match string from a BUTTON."
  (let* (
          (position (point))
          (parameters (button-get button 'parameters))
          (match (plist-get parameters :match))
          (delete (button-get button 'delete)))
    (if delete
      (plist-put parameters :match nil)
      (plist-put parameters :match
        (let ((match-default (if match match "")))
          (read-from-minibuffer "Match string: " match-default))))
    (org-kanban//show-configure-buffer
      (button-get button 'buffer)
      (button-get button 'beginning)
      parameters
      position)))

(defun org-kanban--mirrored-button-action (button)
  "Set the current mirrored setting from a BUTTON."
  (let* (
          (position (point))
          (parameters (button-get button 'parameters))
          (mirrored (plist-get parameters :mirrored)))
    (plist-put parameters :mirrored (not mirrored))
    (org-kanban//show-configure-buffer
      (button-get button 'buffer)
      (button-get button 'beginning)
      parameters
      position)))

(defun org-kanban--dynamicblock-from-parameters (parameters)
  "Create heading of a dynamic block from PARAMETERS."
  (let* (
          (mirrored (plist-get parameters :mirrored))
          (match (plist-get parameters :match))
          (layout (plist-get parameters :layout))
          (scope (plist-get parameters :scope)))
    (setq res "#+BEGIN: kanban")
    (if mirrored
      (setq res (concat res " :mirrored t")))
    (if match
      (setq res (concat res (format " :match \"%s\"" match))))
    (if layout
      (setq res (concat res (format " :layout (\"%s\" . %s)" (car layout) (cdr layout)))))
    (if scope
      (setq res (concat res (format " :scope %s" scope))))
    res))

(defun org-kanban--apply-action (button)
  "Apply the current settings via BUTTON."
  (with-current-buffer (button-get button 'buffer)
    (goto-char (button-get button 'beginning))
    (kill-line)
    (insert (org-kanban--dynamicblock-from-parameters (button-get button 'parameters))))
  (kill-buffer)
  (org-ctrl-c-ctrl-c))

(defun org-kanban//show-configure-buffer (buffer beginning parameters position)
  "Create the configure buffer.
BUFFER is the target-buffer,
BEGINNING the position there and
PARAMETERS the org-kanban parameters.
POSITION in the configure buffer."
  ;;(message "configure %s %s %s %s" buffer beginning parameters position)
  (let ((configure-buffer (get-buffer-create "*org-kanban-configure*")))
    (switch-to-buffer configure-buffer)
    (let (
           (inhibit-read-only t)
           (mirrored (plist-get parameters :mirrored))
           (match (plist-get parameters :match))
           (scope (plist-get parameters :scope))
           (layout (plist-get parameters :layout)))
      (erase-buffer)

      ;; mirrored
      (insert (propertize "Mirrored: " 'face 'font-lock-constant-face))
      (if (eq mirrored t)
        (insert-button "false"
          :type 'org-kanban--mirrored-button
          'mirrored nil
          'buffer buffer
          'beginning beginning
          'parameters parameters)
        (insert-button "true"
          :type 'org-kanban--mirrored-button
          'mirrored t
          'buffer buffer
          'beginning beginning
          'parameters parameters))
      (insert (format " currently [%s]" (propertize (format "%s" mirrored) 'face 'font-lock-keyword-face)))
      (insert "\n")
      (insert (propertize "  t|nil" 'face 'font-lock-comment-face))
      (insert "\n")

      ;; match
      (insert (propertize "Match: " 'face 'font-lock-constant-face))
      (insert-button "change"
        :type 'org-kanban--match-button
        'buffer buffer
        'beginning beginning
        'parameters parameters)
      (insert " ")
      (if (eq match nil)
        (insert "delete")
        (insert-button "delete"
          :type 'org-kanban--match-button
          'buffer buffer
          'beginning beginning
          'parameters parameters
          'delete t))
      (insert (format " currently [%s]" (propertize (format "%s" match) 'face 'font-lock-keyword-face)))
      (insert "\n")
      (insert (propertize "  e.g. urgent|important, see https://orgmode.org/manual/Matching-tags-and-properties.html ." 'face 'font-lock-comment-face))
      (insert "\n")

      ;; layout
      (insert (propertize "Layout: " 'face 'font-lock-constant-face))
      (insert-button "change"
        :type 'org-kanban--layout-button
        'buffer buffer
        'beginning beginning
        'parameters parameters)
      (insert " ")
      (if (eq layout nil)
        (insert "delete")
        (insert-button "delete"
          :type 'org-kanban--layout-button
          'buffer buffer
          'beginning beginning
          'parameters parameters
          'delete t))
      (insert (format " currently [%s]" (propertize (format "%s" layout) 'face 'font-lock-keyword-face)))
      (insert "\n")
      (insert (propertize "  Max width must be bigger than abbreviation." 'face 'font-lock-comment-face))
      (insert "\n")

      ;; scope
      (insert (propertize "Scope: " 'face 'font-lock-constant-face))
      (insert-button "change"
        :type 'org-kanban--scope-button
        'buffer buffer
        'beginning beginning
        'parameters parameters)
      (insert " ")
      (if (eq scope nil)
        (insert "delete")
        (insert-button "delete"
          :type 'org-kanban--scope-button
          'buffer buffer
          'beginning beginning
          'parameters parameters
          'delete t))
      (insert (format " currently [%s]" (propertize (format "%s" scope) 'face 'font-lock-keyword-face)))
      (insert "\n")
      (insert (propertize "  nil|tree|(file1.org ...)" 'face 'font-lock-comment-face))
      (insert "\n")

      ;; preview
      (insert "\n")
      (insert (format "%s %s"
                (propertize "Result: " 'face 'font-lock-constant-face)
                (propertize (org-kanban--dynamicblock-from-parameters parameters) 'face 'font-lock-function-name-face)))
      (insert "\n")

      ;; apply
      (insert "\n")
      (insert-button "apply" :type
        'org-kanban--apply-button
        'face 'font-lock-string-face
        'buffer buffer
        'beginning beginning
        'parameters parameters))
    (org-kanban-configure-mode)
    (goto-char position)))

;;;###autoload
(defun org-kanban/configure-block ()
  "Configure the current org-kanban dynamic block."
  (interactive)
  (with-demoted-errors "Error: %S"
    (let* (
            (beginning (org-beginning-of-dblock))
            (parameters (org-prepare-dblock)))
      ;;(message "start configure for %s@%s" (current-buffer) beginning)
      (org-kanban//show-configure-buffer (current-buffer) beginning parameters 0))))

(provide 'org-kanban)
;;; org-kanban.el ends here
