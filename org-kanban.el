;;; org-kanban.el --- kanban dynamic block for org-mode. -*- lexical-binding: t -*-
;; Copyright (C) 2016 Christian Köstlin

;; This file is NOT part of GNU Emacs.

;; Author: Christian Köstlin <christian.koestlin@gmail.com>
;; Contributors:
;;         Aldric Giacomoni <trevoke@gmail.com>
;; Keywords: org-mode, org, kanban, tools
;; Package-Requires: ((s) (dash "2.13.0") (emacs "24.4") (org "9.1"))
;; Package-Version: 0.4.22
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

(require 's)
(require 'org)
(require 'dash)
(require 'subr-x)
(require 'widget)
(require 'wid-edit)

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
(defcustom org-kanban/prev-keys "ah"
  "Key for promoting an entry of a kanban table to the previous state."
  :type 'string
  :group 'org-kanban)
(defcustom org-kanban/next-keys "dl"
  "Key for promoting an entry of a kanban table to the next state."
  :type 'string
  :group 'org-kanban)
(defcustom org-kanban/subtree-up-keys "wk"
  "Key for moving the subtree of an entry up in the org document."
  :type 'string
  :group 'org-kanban)
(defcustom org-kanban/subtree-down-keys "sj"
  "Key for moving the subtree of an entry down in the org document."
  :type 'string
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
  "Extract all required infos from a todo.  e.g. buffer, heading-components, allowed keywords, ids, priority, ..."
  (list
    (current-buffer)
    (org-heading-components)
    org-todo-keywords-1
    (org-entry-get nil "CUSTOM_ID")
    (org-entry-get nil "ID")
    (org-entry-get nil "PRIORITY")
    (org-entry-get nil "TODO")
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
(defun org-kanban//todo-info-get-priority (todo-info)
  "Get the PRIORITY from a heading TODO-INFO."
  (nth 5 todo-info))
(defun org-kanban//todo-info-get-state (todo-info)
  "Get the STATE from a heading TODO-INFO."
  (nth 6 todo-info))

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

(defun org-kanban//link-for-custom-id (custom-id file description)
  "Create a link for CUSTOM-ID, optionally USE-FILE FILE and DESCRIPTION."
  (if custom-id
    (format "[[file:%s::#%s][%s]]" file custom-id description)
    nil))

(defun org-kanban//link-for-id (id description)
  "Create a link for ID with DESCRIPTION."
  (if id
    (format "[[id:%s][%s]]" id description)
    nil))

(defun org-kanban//link-for-heading (heading file description)
  "Create a link for a HEADING optionally USE-FILE a FILE and DESCRIPTION."
  (if heading
      (format "[[file:%s::*%s][%s]]" file heading description)
    (error "Illegal state")))

(defun org-kanban//escape-description (description)
  "Cleanup DESCRIPTION for use in an org link."
  (s-replace "|" "｜"
    (s-replace "]" "}"
      (s-replace "[" "{"
        (replace-regexp-in-string "\\[\\[.*]\\[\\(.*\\)]]" (lambda (x) (match-string 1 x)) description)))))

(defun org-kanban//escape-heading (heading)
  "Cleanup HEADING for use in an org link."
  (s-replace "|" "｜"
    (replace-regexp-in-string "\s*\\[.*]" "" (replace-regexp-in-string "\\[\\[.*]\\[\\(.*\\)]]" (lambda (x) (match-string 1 x)) heading))))

(defun org-kanban//unescape-heading (heading)
  "Transform HEADING from org link to real heading."
  (s-replace  "｜" "|" heading))

(defun org-kanban//link (file heading kanban search-for custom-id id layout)
  "Create a link to FILE and HEADING if the KANBAN value is equal to SEARCH-FOR.
MULTI-FILE indicates if the link must work across several files.
CUSTOM-ID links are used if given.
ID links are used if given.
LAYOUT is the specification to layout long links.
This means, that the org-kanban table links are in one of several forms:
 - file:#custom-id
 - #custom-id
 - id:id
 - file:*heading
 - heading"
  (if
    (and (stringp kanban) (string-equal search-for kanban))
    (let* (
            (description (org-kanban//escape-description (funcall layout heading)))
            )
      (or
        (org-kanban//link-for-custom-id custom-id file description)
        (org-kanban//link-for-id id description)
        (org-kanban//link-for-heading (org-kanban//escape-heading heading) file description)
        ))
    ""))

(defun org-kanban//todo-keywords (files mirrored range-fun)
  "Get list of org todos from FILES.  MIRRORED describes if keywords should be reversed.  RANGE-FUN filters keywords."
  (save-window-excursion
    (let* (
            (list-of-keywords (-flatten (-map
                                          (lambda(file)
                                            (find-file file)
                                            org-todo-keywords-1)
                                          files)))
            (filtered (-distinct (-filter (lambda (i) (funcall range-fun i list-of-keywords)) list-of-keywords)))
            (keywords (if mirrored (reverse filtered) filtered)))
      keywords)))

(defun org-kanban//row-for (todo-info todo-keywords layout)
  "Convert a kanban TODO-INFO to a row of a org-table.
TODO-KEYWORDS are all the current org todos.
LAYOUT specification."
  (let* (
          (file (org-kanban//todo-info-get-file todo-info))
          (heading (org-kanban//todo-info-get-heading todo-info))
          (title (org-kanban//heading-get-title heading))
          (kanban (org-kanban//heading-get-todo-keyword heading))
          (custom-id (org-kanban//todo-info-get-custom-id todo-info))
          (id (org-kanban//todo-info-get-id todo-info))
          (row-entries (-map (lambda(i) (org-kanban//link file title i kanban custom-id id layout)) todo-keywords))
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
          (pattern "\\[\\[file:\\(.*\\)::\\*\\(.*\\)\\]\\[.*\\]")
          (match (string-match pattern line))
          (file (and match (match-string 1 line)))
          (heading (and match (org-kanban//unescape-heading (match-string 2 line))))
          (entry (and heading (save-excursion
                                (find-file file)
                                (org-find-exact-headline-in-buffer heading)))))
    (if entry (list file entry) nil)))

(defun org-kanban//find-by-custom-id (line)
  "Try to find a todo by custom id in LINE."
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
          (heading (and match (org-kanban//unescape-heading (match-string 1 line))))
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
    (or
      (org-kanban//find-by-file-and-custom-id line)
      (org-kanban//find-by-file-and-heading line)
      (org-kanban//find-by-custom-id line)
      (org-kanban//find-by-id line)
      (org-kanban//find-by-heading line))))

(defun org-kanban/move-subtree-up ()
  "Move the todo entry in the current line of the kanban table up."
  (interactive)
  (org-kanban//move-subtree 'up))

(defun org-kanban/move-subtree-down ()
  "Move the todo entry in the current line of the kanban table down."
  (interactive)
  (org-kanban//move-subtree 'down))

(defun org-kanban//move-subtree (&optional direction)
  "Move subtree into DIRECTION."
  (save-window-excursion
    (if (-contains? (list 'up 'down) direction)
      (let* (
              (file-and-marker (org-kanban//find))
              (file (nth 0 file-and-marker))
              (marker (nth 1 file-and-marker)))
        (when (and file-and-marker file marker)
          (with-demoted-errors
            ;;(org-kanban//move-table-entry direction)
            (let ((new-pos 0))
              (save-excursion
              (find-file file)
              (goto-char marker)
              (if (eq direction 'up)
                (org-move-subtree-up)
                (org-move-subtree-down))
              (setq new-pos (point)))
              (org-dblock-update)
              (org-beginning-of-dblock)
              (if (org-kanban//search-element file new-pos)
                (goto-char (search-forward "[["))))))))))

(defun org-kanban//search-element (required-file required-point)
  "Search for the org-kanban table entry that points to REQUIRED-FILE and REQUIRED-POINT."
  (let* (
          (done-p nil))
    (while (not done-p)
      (let* (
              (file-and-marker (org-kanban//find))
              (file (nth 0 file-and-marker))
              (marker (nth 1 file-and-marker)))
        (if marker
          (if (and
                (eq (marker-position marker) required-point)
                (eq file required-file))
            (setq done-p t)
            (forward-line 1))
          (forward-line 1))))))
    
(defun org-kanban/next ()
  "Move the todo entry in the current line of the kanban table to the next state."
  (interactive)
  (org-kanban//move 'right))

(defun org-kanban/prev ()
  "Move the todo entry in the current line of the kanban table to the previous state."
  (interactive)
  (org-kanban//move 'left))

(defun org-kanban/shift (&optional direction)
  "Move todo to DIRECTION (repeatedly)."
  (interactive)
  (message "Use %s and %s to change todo state, %s and %s to reorder subtrees" org-kanban/prev-keys org-kanban/next-keys org-kanban/subtree-up-keys org-kanban/subtree-down-keys)
  (cond
    ((eq 'left direction) (org-kanban//move direction))
    ((eq 'right direction) (org-kanban//move direction))
    ((eq 'up direction) (org-kanban//move-subtree direction))
    ((eq 'down direction) (org-kanban//move-subtree direction)))
  (set-transient-map
    (let* (
            (map (make-sparse-keymap)))
      (org-kanban//define-keys map org-kanban/prev-keys (lambda () (interactive) (org-kanban/shift 'left)))
      (org-kanban//define-keys map org-kanban/next-keys (lambda () (interactive) (org-kanban/shift 'right)))
      (org-kanban//define-keys map org-kanban/subtree-down-keys (lambda () (interactive) (org-kanban/shift 'down)))
      (org-kanban//define-keys map org-kanban/subtree-up-keys (lambda () (interactive) (org-kanban/shift 'up)))
      map)))

(defun org-kanban//define-keys (map keys fun)
  "Define FUN for all KEYS in MAP."
  (dolist (key (append keys nil))
    (define-key map (format "%c" key) fun)))

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

(defun org-kanban//get-dynamic-block-parameters ()
  "Get the dynamic block parameters if point is inside a block."
  (save-excursion
    (let* (
            (_ (org-beginning-of-dblock))
            (__ (unless (looking-at org-dblock-start-re)
                     (user-error "Not at a dynamic block")))
            (name (org-no-properties (match-string 1))))
      (append (list :name name) (read (concat "(" (match-string 3) ")"))))))

(defun org-kanban//move-table-entry (direction)
  "Move the cell/entry in kanban table in direction DIRECTION."
  (org-narrow-to-element)
  ;; goes to the element in the row, assume we are anywhere on the line
  (or (search-forward "[[" nil t)
    (search-backward "[["))
  (unwind-protect
    (org-table-move-column (if (eq 'left direction) 't nil))
    (widen)
    (org-table-align)))

(defun org-kanban//get-table-todo ()
  "Return todo state in given column in kanban table."
  (save-excursion
    (let* ((cur-col (org-table-current-column)))
      (org-table-goto-line 1)
      (org-table-goto-column cur-col)
      (-> (org-table-get-field)
        (org-no-properties)
        (s-trim)))))

(defmacro org-kanban//measure-time (title &rest body)
  "Print time prefixed with TITLE it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s %.06f" ,title (float-time (time-since time)))))

(defun org-kanban//move (direction)
  "Move the todo entry in the current line of the kanban table to the next state in direction DIRECTION."
  (save-window-excursion
    (if (-contains? (list 'left 'right) direction)
      (let* (
              (file-and-marker (org-kanban//find))
              (file (nth 0 file-and-marker))
              (marker (nth 1 file-and-marker)))
        (when (and file-and-marker file marker)
          (with-demoted-errors
            (org-kanban//move-table-entry direction)
            (let ((todo (org-kanban//get-table-todo)))
              (save-excursion
                (find-file file)
                (goto-char marker)
                (org-todo todo)))))))))

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
  (let* ((scope (plist-get params :scope)))
    (pcase scope
      (`nil files)
      (`tree scope)
      (_ files))))

(defun org-kanban//params-compare-by-priority (a b f)
  "Compare A and B by priority and function F."
  (funcall f (org-kanban//todo-info-get-priority a) (org-kanban//todo-info-get-priority b)))

(defun org-kanban//params-compare-by-state (a b all-keywords f)
  "Compare A with B with the help of ALL-KEYWORDS and F."
  (let* (
          (idx-1 (or (-elem-index (org-kanban//todo-info-get-state a) all-keywords) 0))
          (idx-2 (or (-elem-index (org-kanban//todo-info-get-state b) all-keywords) 0)))
    (funcall f idx-1 idx-2)))

(defun org-kanban//params-comparator (params all-keywords)
  "Calculate comparator based on ALL-KEYWORDS and PARAMS."
  (let* ((c (plist-get params :comparator)))
    (pcase c
      (`< (lambda (a b) (org-kanban//params-compare-by-priority a b 'string<)))
      (`> (lambda (a b) (org-kanban//params-compare-by-priority a b 'string>)))
      (`s (lambda (a b) (org-kanban//params-compare-by-state a b all-keywords '<)))
      (`S (lambda (a b) (org-kanban//params-compare-by-state a b all-keywords '>)))
      (_ nil))))

(defun org-kanban//range-fun (value keywords from to)
  "Return if VALUE is between FROM and TO in KEYWORDS."
  (if from
    (if to
      (let* (
              (from-idx (-elem-index from keywords))
              (to-idx (-elem-index to keywords))
              (value-idx (-elem-index value keywords))
              )
        (and from-idx to-idx value-idx (>= value-idx from-idx) (<= value-idx to-idx)))
      t)
    t))

(defun org-kanban//todo-comparator (todo-a todo-b)
  "Compare TODO-A against TODO-B."
  (message "%s" todo-a)
  (message "%s" todo-b)
  t
  )

;;;###autoload
(defun org-dblock-write:kanban (params)
  "Create the kanban dynamic block.
PARAMS may contain `:mirrored`, `:match`, `:scope`, `:layout` and `:range`."
  (insert
    (let*
      (
        (mirrored (plist-get params :mirrored))
        (match (plist-get params :match))
        (range (plist-get params :range))
        (layout (org-kanban//params-layout params))
        (files (org-kanban//params-files params))
        (scope (org-kanban//params-scope params files))
        (todo-keywords (org-kanban//todo-keywords files mirrored (lambda (value keywords) (org-kanban//range-fun value keywords (car range) (cdr range)))))
        (comparator (org-kanban//params-comparator params todo-keywords))
        (todo-infos (org-map-entries 'org-kanban//todo-info-extract match scope))
        (sorted-todo-infos (if comparator (-sort comparator todo-infos) todo-infos))
        (filtered-todo-infos (-filter (lambda (todo-info)
                                        (org-kanban//range-fun
                                          (nth 2 (org-kanban//todo-info-get-heading todo-info))
                                          (org-kanban//todo-info-get-keywords todo-info)
                                          (car range)
                                          (cdr range)))
                               sorted-todo-infos))
        (row-for (lambda (todo-info) (org-kanban//row-for todo-info todo-keywords layout)))
        (rows (-map row-for (-filter
                              (lambda (todo-info)
                                (-intersection
                                  (list (org-kanban//heading-get-todo-keyword (org-kanban//todo-info-get-heading todo-info)))
                                  (org-kanban//todo-info-get-keywords todo-info)))
                              filtered-todo-infos)))
        (table (if rows
                 (--reduce (format "%s\n%s" acc it) rows)
                 ""
                 ))
        (table-title (string-join todo-keywords "|"))
        )
      (format "|%s|\n|--|\n%s" table-title table)))
  (org-table-align))

(defun org-kanban/version ()
  "Print org-kanban version."
  (interactive)
  (message "org-kanban 0.4.22"))

(defun org-kanban--scope-action (button)
  "Set scope from a BUTTON."
  (let* (
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
      parameters)))

(defun org-kanban--layout-action (button)
  "Set layout from a BUTTON."
  (let* (
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
      parameters)))

(defun org-kanban--match-action (button)
  "Set the current match string from a BUTTON."
  (let* (
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
      parameters)))

(defun org-kanban--mirrored-button-action (button)
  "Set the current mirrored setting from a BUTTON."
  (let* (
          (parameters (button-get button 'parameters))
          (mirrored (plist-get parameters :mirrored)))
    (plist-put parameters :mirrored (not mirrored))
    (org-kanban//show-configure-buffer
      (button-get button 'buffer)
      (button-get button 'beginning)
      parameters)))

(defun org-kanban--dynamicblock-from-parameters (parameters)
  "Create heading of a dynamic block from PARAMETERS."
  (let* (
          (mirrored (plist-get parameters :mirrored))
          (match (plist-get parameters :match))
          (layout (plist-get parameters :layout))
          (scope (plist-get parameters :scope)))
    (s-join " " (delq nil
                  (list "#+BEGIN: kanban"
                    (if mirrored ":mirrored t")
                    (if match (format " :match \"%s\"" match))
                    (if layout (format " :layout (\"%s\" . %s)" (car layout) (cdr layout)))
                    (if scope (format " :scope %s" scope)))))))

(defun org-kanban--calculate-preview (mirrored match layout scope range comparator)
  "Calculate the org-kanban header for MIRRORED, MATCH, LAYOUT, SCOPE, RANGE and COMPARATOR."
  (s-join " " (delq nil
                (list "#+BEGIN: kanban"
                  (if mirrored ":mirrored t")
                  (if (and match (> (length match) 0))
                    (format ":match \"%s\"" match))
                  (if layout (format ":layout (\"%s\" . %s)" (car layout) (cdr layout)))
                  (format ":scope %s" scope)
                  (if range (format ":range (\"%s\" . \"%s\")" (car range) (cdr range)))
                  (if comparator (format ":comparator %s" comparator))))))

(defun org-kanban--update-preview (preview mirrored match layout scope range comparator)
  "Update the PREVIEW widget with the org-kanban header for MIRRORED, MATCH, LAYOUT, SCOPE, RANGE and COMPARATOR."
  (widget-value-set preview (org-kanban--calculate-preview mirrored match layout scope range comparator)))

(defun org-kanban//show-configure-buffer (buffer beginning parameters)
  "Create the configuration form for BUFFER.
BEGINNING the position there and
PARAMETERS the org-kanban parameters."
  (switch-to-buffer "*org-kanban-configure*")
  (let (
         (inhibit-read-only t)
         (mirrored (plist-get parameters :mirrored))
         (match (plist-get parameters :match))
         (scope (plist-get parameters :scope))
         (range (plist-get parameters :range))
         (comparator (plist-get parameters :comparator))
         (layout (or (plist-get parameters :layout) org-kanban/layout))
         (preview nil)
         (match-widget nil))

    (erase-buffer)
    (remove-overlays)

    (widget-insert (propertize "Mirrored: " 'face 'font-lock-keyword-face))
    (widget-create 'toggle
      :value mirrored
      :notify (lambda (widget &rest _ignore)
                (setq mirrored (widget-value widget))
                (org-kanban--update-preview preview mirrored match layout scope range comparator)))
    (widget-insert (propertize "  see https://theagileist.wordpress.com/tag/mirrored-kanban-board/ for details" 'face 'font-lock-doc-face))
    (widget-insert "\n\n")

    (widget-insert (propertize "Match: " 'face 'font-lock-keyword-face))
    (setq match-widget (widget-create 'editable-field
                         :value (format "%s" (or match ""))
                         :size 30
                         :notify (lambda (widget &rest _ignore)
                                   (setq match (widget-value widget))
                                   (org-kanban--update-preview preview mirrored match layout scope range comparator))))
    (widget-insert " ")
    (widget-create 'push-button
      :notify (lambda (_widget &rest _ignore)
                (widget-value-set match-widget "")
                (setq match nil)
                (org-kanban--update-preview preview mirrored match layout scope range comparator))
      (propertize "Delete" 'face 'font-lock-string-face))
    (widget-insert "\n")
    (widget-insert (propertize "  match to tags e.g. urgent|important" 'face 'font-lock-doc-face))
    (widget-insert "\n\n")

    (widget-insert (propertize "Range:\n" 'face 'font-lock-keyword-face))
    (widget-insert (propertize "  from: " 'face 'font-lock-keyword-face))
    (setq range-from-widget (widget-create 'editable-field
                              :value (format "%s" (or (car range) ""))
                              :size 7
                              :notify (lambda (widget &rest _ignore)
                                        (setq range (cons (widget-value widget) (cdr range)))
                                        (org-kanban--update-preview preview mirrored match layout scope range comparator))))
    (widget-insert (propertize " to: " 'face 'font-lock-keyword-face))
    (setq range-to-widget (widget-create 'editable-field
                            :value (format "%s" (or (cdr range) ""))
                            :size 7
                            :notify (lambda (widget &rest _ignore)
                                      (setq range (cons (car range) (widget-value widget)))
                                      (org-kanban--update-preview preview mirrored match layout scope range comparator))))
    (widget-insert " ")
    (widget-create 'push-button
      :notify (lambda (_widget &rest _ignore)
                (widget-value-set range-from-widget "")
                (widget-value-set range-to-widget "")
                (setq range nil)
                (org-kanban--update-preview preview mirrored match layout scope range comparator))
      (propertize "Delete" 'face 'font-lock-string-face))
    (widget-insert "\n")
    (widget-insert (propertize "  from and to should be keywords" 'face 'font-lock-doc-face))
    (widget-insert "\n\n")

    (widget-insert (propertize "Layout:\n" 'face 'font-lock-keyword-face))
    (widget-insert (propertize "  Abbreviation: " 'face 'font-lock-keyword-face))
    (widget-create 'editable-field
      :value (format "%s" (if layout (car layout) ""))
      :size 5
      :notify (lambda (widget &rest _ignore)
                (setq layout (cons (widget-value widget) (cdr layout)))
                (org-kanban--update-preview preview mirrored match layout scope range comparator)))
    (widget-insert (propertize " Max-width: " 'face 'font-lock-keyword-face))
    (widget-create 'editable-field
      :value (format "%s" (if layout (cdr layout) ""))
      :size 1
      :notify (lambda (widget &rest _ignore)
                (setq layout (cons (car layout) (widget-value widget)))
                (org-kanban--update-preview preview mirrored match layout scope range comparator)))
    (widget-insert "\n")
    (widget-insert (propertize "  max-width should be bigger then the length of the abbreviation" 'face 'font-lock-doc-face))
    (widget-insert "\n\n")

    (widget-insert (propertize "Scope " 'face 'font-lock-keyword-face))
    (let ((default-file-list (cond
                               ((eq scope 'tree) "(file1.org file2.org ...)")
                               ((eq scope nil) "(file1.org file2.org ...)")
                               (t (format "%s" scope)))))
      (widget-create 'menu-choice
        :tag "change type"
        :value (cond
                 ((eq scope 'tree) "tree")
                 ((eq scope nil) "nil")
                 (t (format "%s" scope)))
        :notify (lambda (widget &rest _ignore)
                  (let ((scope-string (widget-value widget)))
                    (setq scope
                      (cond
                        ((string-equal scope-string "tree") "tree")
                        ((string-equal scope-string "nil") nil)
                        (t (let (
                                  (_ (setq default-file-list scope-string))
                                  (res (car (read-from-string scope-string))))
                             res))))
                    (org-kanban--update-preview preview mirrored match layout scope range comparator)))
        :value-set (lambda (widget &rest value)
                     (widget-default-value-set widget
                       (cond
                         ((equal value '("tree")) "tree")
                         ((equal value '("nil")) "nil")
                         (t (format "%s" default-file-list)))))
        '(item :tag "tree" :menu-tag "tree" :value "tree")
        '(item :tag "whole file" :menu-tag "whole file" :value "nil")
        '(editable-field :menu-tag "list of files" default-file-list)))
    (widget-insert (propertize "  Scope of the org-kanban table. e.g. nil, tree or a list of files.\n" 'face 'font-lock-doc-face))
    (widget-insert "\n")

    (widget-insert (propertize "Comparator " 'face 'font-lock-keyword-face))
    (widget-create 'menu-choice
      :tag "change type"
      :value (cond
               ((eq comparator '<) "<")
               ((eq comparator '>) ">")
               ((eq comparator 's) "s")
               ((eq comparator 'S) "S")
               ((eq comparator 'nil) nil))
      :notify (lambda (widget &rest _ignore)
                (setq comparator (widget-value widget))
                (org-kanban--update-preview preview mirrored match layout scope range comparator))
      :value-set (lambda (widget &rest value)
                   (widget-default-value-set widget
                     (cond
                       ((equal value '("<")) "<")
                       ((equal value '(">")) ">")
                       ((equal value '("s")) "s")
                       ((equal value '("S")) "S")
                       ((equal value nil)))))
       '(item :tag "<" :menu-tag "<" :value "<")
       '(item :tag ">" :menu-tag ">" :value ">")
       '(item :tag "s" :menu-tag "s" :value "s")
       '(item :tag "S" :menu-tag "S" :value "S")
       '(item :tag "nil" :menu-tag "nil" :value nil))
    (widget-insert (propertize "  Comparator for sorting table entries. e.g. <, > (for priority sorting) or s, S (for state sorting) or nil\n" 'face 'font-lock-doc-face))

    (widget-insert "\n")
    (widget-insert (propertize "Result: " 'face 'font-lock-keyword-face))
    (setq preview
      (widget-create 'const))

    (widget-create 'push-button
      :notify (lambda(_widget &rest _ignore)
                (with-current-buffer buffer
                  (goto-char beginning)
                  (kill-line)
                  (insert (org-kanban--calculate-preview mirrored match layout scope range comparator)))
                (kill-buffer)
                (org-ctrl-c-ctrl-c))
      (propertize "Apply" 'face 'font-lock-comment-face))
    (widget-insert " ")
    (widget-create 'push-button
      :notify (lambda (_widget &rest _ignore)
                (kill-buffer))
      (propertize "Cancel" 'face 'font-lock-string-face))

    (org-kanban--update-preview preview mirrored match layout scope range comparator)
    (use-local-map widget-keymap)
    (widget-setup)))

;;;###autoload
(defun org-kanban/configure-block ()
  "Configure the current org-kanban dynamic block."
  (interactive)
  (with-demoted-errors "Error: %S"
    (let* (
            (beginning (org-beginning-of-dblock))
            (parameters (org-prepare-dblock)))
      (org-kanban//show-configure-buffer (current-buffer) beginning parameters))))

(provide 'org-kanban)
;;; org-kanban.el ends here
