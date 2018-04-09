FROM alpine:latest
RUN apk --no-cache add emacs
RUN mkdir -p /workspace
RUN emacs --batch --execute "(progn (package-initialize) (message \"1\") (setq network-security-level 'low) (message \"2\") (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (message \"3\") (package-list-packages) (message \"4\") (sleep-for 4) (package-install 'org-kanban))"
CMD ["emacs"]
