;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I open file \"\\([^\"]+\\)\"$"
  (lambda (arg)
    (find-file arg)))

(And "^I run \\(.+\\)$"
     (lambda (function)
       (funcall (intern function))
       ))

(When "^I shorten \"\\([^\"]+\\)\" to length \"\\([^\"]+\\)\" with abbreviation \"\\([^\"]+\\)\" I should get \"\\([^\"]+\\)\"$"
  (lambda (heading length abbreviation desc)
    (should (string= (org-kanban//heading-to-description heading (cons abbreviation (string-to-number length))) desc))))

