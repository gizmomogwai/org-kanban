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
 (message "Setup")
 ;; Before anything has run
 )

(Before
 (message "Before")
 ;; Before each scenario is run
;; (erase-buffer)
 )

(After
 (message "After")
 ;; After each scenario is run
 (message "Killing buffer %s" (current-buffer))
 (kill-buffer (current-buffer))
 )

(Teardown
 (message "Teardown")
 ;; After when everything has been run
 )
