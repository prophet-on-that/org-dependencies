(require 'org)

(when (not (boundp 'org-goto-map))
  (org-goto-map))

(defvar org-block-blocked-keyword "BLOCKED"
  "Todo keyword representing a blocked state.")
(defvar org-block-todo-keyword "TODO"
  "Todo keyword representing an active state.")
(defvar org-block-blocks-property "BLOCKS"
  "Property name to store dependents of a headline.")
(defvar org-block-depends-upon-property "DEPENDS-UPON"
  "Property name to store dependencies of a headline.")

(defun org-update-dependency-graph (&optional pom)
  "Update dependent tasks given the state of the task at
point-or-marker POM, or the current headline if unprovided."
  (let ((pom (or pom (point))))
    (let ((current-state (org-get-todo-state-at-pom pom)))
      (when (or (org-done-keyword-p current-state)
		(org-not-done-keyword-p current-state))
	(dolist (dep-id (org-entry-get-multivalued-property pom org-block-blocks-property))
	  (let* ((marker (org-id-find dep-id t))
		 (state (org-get-todo-state-at-pom marker)))
	    (cond ((and (org-not-done-keyword-p current-state)
			(org-not-done-keyword-p state))
		   ;; Set entry as pending if not done
		   (save-excursion
		     (goto-char marker)
		     (org-todo org-block-blocked-keyword)))
		  
		  ((and (org-done-keyword-p current-state)
			(org-not-done-keyword-p state))
		   ;; Set entry as todo if not done and all dependencies
		   ;; are done
		   (save-excursion
		     (goto-char marker)
		     (org-todo (if (org-all-dependencies-done-p marker) org-block-todo-keyword org-block-blocked-keyword)))))
	    (org-update-dependency-graph marker)))))))

(add-hook 'org-after-todo-state-change-hook 'org-update-dependency-graph)

;;; TODO: do not allow headline to set itself as a dependency
(defun org-add-dependency-by-id (dep-id is-dependent)
  "Add DEP-ID as a dependency or dependent (according to
IS-DEPENDENT) of the headline at the current point."
  (let ((dep (org-id-find dep-id t)))
    (when dep
      (org-entry-add-to-multivalued-property
       (point)
       (if is-dependent
	   org-block-blocks-property
	 org-block-depends-upon-property)
       dep-id)
      (let ((current-id (org-id-get-create)))
	(org-entry-add-to-multivalued-property
	 dep
	 (if is-dependent
	     org-block-depends-upon-property
	   org-block-blocks-property)
	 current-id))
      (org-update-dependency-graph
       (if is-dependent
	   (point)
	 dep)))))

(defun org-add-dependency (is-dependent)
  "Prompt for and add a headline as a dependency of the current
headline. With the \\\[universal-argument] prefix, add the
selected headline as a dependent."
  (interactive "P")
  (let ((pos (car (org-get-location (current-buffer) org-goto-help))))
    (when pos
      (org-add-dependency-by-id
       (save-excursion
	 (goto-char pos)
	 (org-id-get-create))
       is-dependent))))

(defun org-all-dependencies-done-p (pom)
  "Test if all dependencies are done of entry at point-or-marker
POM."
  (cl-flet
      ((entry-is-done (id)
		      (let ((marker (org-id-find id t)))
			(save-excursion
			  (goto-char marker)
			  (org-entry-is-done-p)))))
    (cl-every #'entry-is-done (org-entry-get-multivalued-property pom org-block-depends-upon-property))))

(defun org-done-keyword-p (keyword)
  "Test if KEYWORD is a done keyword."
  (member keyword org-done-keywords))

(defun org-not-done-keyword-p (keyword)
  "Test if KEYWORD is a not done keyword."
  (member keyword org-not-done-keywords))

(defun org-get-todo-state-at-pom (pom)
  "Get the todo state at point-or-marker POM."
  (save-excursion
    (goto-char pom)
    (org-get-todo-state)))

(defun org-show-dependencies (show-dependents)
  "Show direct dependencies of the current headline as a sparse
tree in the current buffer. With the \\\[universal-argument]
prefix, show the direct dependents, instead."
  (interactive "P")
  (let* ((id (org-id-get-create))
	 (prop
	  (if show-dependents
	      org-block-depends-upon-property
	    org-block-blocks-property))
	 (match (concat prop "={" id "}"))
	 (todo-only nil))
    (org-scan-tags 'sparse-tree (cdr (org-make-tags-matcher match)) todo-only)))
