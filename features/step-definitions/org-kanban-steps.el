;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I open file \"\\([^\"]+\\)\"$"
  (lambda (arg)
    (find-file arg)))

(Given "^I insert these lines:$"
  (lambda (lines)
    (insert (mapconcat #'car lines "\n") "\n")))

(When "^I update the kanban table$"
  (lambda ()
    (goto-char (point-min))
    (search-forward "#+BEGIN: kanban")
    (beginning-of-line)
    (org-update-dblock)
    (font-lock-ensure)))

(Then "^the kanban table should be:$"
  (lambda (expected)
    (save-excursion
      (goto-char (point-min))
      (search-forward "#+BEGIN: kanban")
      (forward-line)
      ;; Org inserts U+200B to prevent a trailing ] from closing a link.
      ;; Spell it visibly in fixtures and failure messages; ignore cell padding.
      (let ((actual (--map (--map (s-replace "\u200b" "<ZWSP>"
                                           (substring-no-properties it)) it)
                           (delq 'hline (org-table-to-lisp)))))
        (should (equal actual expected))))))

(Then "^the kanban links should open these headings:$"
  (lambda (expected)
    (save-window-excursion
      (goto-char (point-min))
      (search-forward "#+BEGIN: kanban")
      (forward-line)
      (let ((links (save-restriction
                     (narrow-to-region (org-table-begin) (org-table-end))
                     (org-element-map (org-element-parse-buffer) 'link
                       (lambda (link) (org-element-property :begin link)))))
            (org-link-search-must-match-exact-headline t))
        (should (= (length links) (length expected)))
        (cl-mapc (lambda (position row)
                   (save-window-excursion
                     (goto-char position)
                     (org-open-at-point)
                     (should (equal (org-get-heading t t t t) (car row)))))
                 links expected)))))

(And "^I run \\(.+\\)$"
     (lambda (function)
       (funcall (intern function))
       ))

(When "^I shorten \"\\([^\"]+\\)\" to length \"\\([^\"]+\\)\" with abbreviation \"\\([^\"]+\\)\" I should get \"\\([^\"]+\\)\"$"
  (lambda (heading length abbreviation desc)
    (should (string= (org-kanban//heading-to-description heading (cons abbreviation (string-to-number length))) desc))))
