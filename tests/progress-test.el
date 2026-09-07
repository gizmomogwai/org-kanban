;;; progress-test.el --- Link cookie regression -*- lexical-binding: t -*-
(require 'ert)
(require 'org-kanban)

(ert-deftest org-kanban-progress-links ()
  (dolist (cookie '("[1/2]" "[33%]"))
    (dolist (kind '(heading id custom-id))
      (let ((file (make-temp-file "kanban-progress" nil ".org")))
        (unwind-protect
            (with-current-buffer (find-file-noselect file)
              (insert "* TODO Task " cookie "\n"
                      (pcase kind
                        ('id ":PROPERTIES:\n:ID: task\n:END:\n")
                        ('custom-id ":PROPERTIES:\n:CUSTOM_ID: task\n:END:\n")
                        (_ ""))
                      "* Board\n#+BEGIN: kanban\n#+END:\n")
              (dotimes (_ 3)
                (goto-char (point-min))
                (search-forward "#+BEGIN:")
                (beginning-of-line)
                (org-update-dblock)
                (font-lock-ensure)
                (let* ((tree (org-element-parse-buffer))
                       (link (car (org-element-map tree 'link #'identity)))
                       (description (buffer-substring-no-properties
                                     (org-element-property :contents-begin link)
                                     (org-element-property :contents-end link))))
                  (should (equal (string-replace "\u200b" "" description)
                                 (concat "Task " cookie))))))
          (when-let* ((buffer (get-file-buffer file)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))
          (delete-file file))))))

(ert-deftest org-kanban-progress-preserves-heading-escaping ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/kanban.org")
    (let* ((heading "Task [[https://example.org][example]] [1/2]")
           (escaped (org-kanban//escape-heading heading))
           (link (org-kanban//link-for-heading escaped (current-buffer)
                                             "Task example [1/2]")))
      (should (string-prefix-p (concat "[[file:kanban.org::*" escaped "][")
                               link)))))
