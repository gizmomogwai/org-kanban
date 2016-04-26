(require 'f)

(defvar org-kanban-support-path
  (f-dirname load-file-name))

(defvar org-kanban-features-path
  (f-parent org-kanban-support-path))

(defvar org-kanban-root-path
  (f-parent org-kanban-features-path))

(add-to-list 'load-path org-kanban-root-path)

(require 'org-kanban)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
