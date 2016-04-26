;;; org-kanban-test.el --- oh my
;;; Commentary:

;;; Code:

(require 'ert-expectations)
(require 'org)

(expectations
  (desc "should place in correct column")
  (expect "|[[Test]]|||" (progn
                           (setq org-todo-keywords-1 (list "A" "B" "C"))
                           (org-kanban/row-for (list 1 2 "A" nil "Test"))))
  )
;;; org-kanban-test.el ends here
