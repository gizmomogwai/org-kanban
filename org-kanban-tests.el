(require 'org-kanban)
(require 'ert-expectations)

(expectations
  (desc "should place in correct column")
  (expect "|[[Test]]|||" (progn
                           (setq org-todo-keywords-1 (list "A" "B" "C"))
                           (org-kanban/row-for (list 1 2 "A" nil "Test"))))
  )
o
