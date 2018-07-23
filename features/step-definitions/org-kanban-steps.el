;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\(.+\\)\"$"
       (lambda (something)
         ;; ...
         ))

(When "^I have \"\\(.+\\)\"$"
      (lambda (something)
        ;; ...
        ))

(Then "^I should have \"\\(.+\\)\"$"
      (lambda (something)
        ;; ...
        ))

(And "^I have \"\\(.+\\)\"$"
     (lambda (something)
       ;; ...
       ))

(But "^I should not have \"\\(.+\\)\"$"
     (lambda (something)
       ;; ...
       ))

(And "^I run \\(.+\\)$"
     (lambda (function)
       (funcall (intern function))
       ))

(When "^I shorten \"\\([^\"]+\\)\" to length \"\\([^\"]+\\)\" with abbreviation \"\\([^\"]+\\)\" I should get \"\\([^\"]+\\)\"$"
  (lambda (heading length abbreviation desc)
    (should (string= (org-kanban//heading-to-description heading (cons abbreviation (string-to-number length))) desc))))

(Given "^I open file \"\\([^\"]+\\)\"$"
  (lambda (arg)
    (find-file arg)
    (message "opened buffer %s" (current-buffer))))
