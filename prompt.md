## General Prompts

- Ingest all the 5 attached files and acknowledge their version numbers. Acknowledge that you have read these files. Also show me somehow that you understand all the files. You, however, don't have to determine the next task.

- You have the `GREEN LIGHT` to execute the rewrite.

- Now lets review and audit the Dabbrev subsection. Audit and review this subsection thoroughly in great detail. Then find any errors and issues. Also determine if the optimal settings are being used. Search the web and think longer for these task and make sure you have the latest information till July 23, 2026. Explain everything in great detail for me to review.

- Find and fix any errors and issues in your latest rewrite of Helpful subsection. Search the web and think longer for this task and explain everything back to me.

- Now lets work on the subsection Evil MC subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now audit and review the subsection: Org Capture. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from project_operationals.yaml file. Look at upstream documentations and source codes for org-capture. Also look at the upstream code and documentation for doom emacs in https://github.com/doomemacs/modules and https://github.com/doomemacs/core . Then determine how doom emacs handles org-capture. Keep in mind this Org Capture subsection has to integrate with the entirety of Org Mode and Second Brain main sectio, Then explain everything to me in detail and present me how you plan to deal with this configuration.I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 20 2026.

- Now audit and review the subsection: Breadcrumb. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from project_operationals.yaml file. Look at upstream documentations and source codes for breadcrumb. Then explain everything to me in detail. Search the web and think longer for this task and make sure you have the latest information till July 23 2026.

- Are you sure there are no more errors and issues in this iteration of Undo Fu subsection? And are you sure there are not any more extra configurations you need to add by search the web? Answer and explain everything to me in great detail.Search the web and think longer for this task and make sure the information you get is latest till July 2026.

- Now determine the purpose of denote-wordcloud and how it might improve the second brain from org mode and second brain main section. Look at denote-wordcloud upstream documentation and source code. Search the web and think longer for this task and make sure you have the latest information till July 20, 2026. Then explain everything back to me.

- Lets work on Undo Fu subsection again. You need to come up with a plan to write a config org block from scratch. Look at upstream documentations and source codes for both denote and consult-denote packages. Search the web and think longer for this task and make sure you have the latest information till July 20, 2026. Then explain everything back to me.

- Review and audit your latest iteration of Org Eldoc subsection and the custom ar-org-eldoc.el file. Find any errors and issues. Search the web and think longer for this task and make sure you have the latest information till July 20, 2026. Then explain everything back to me.

- Now we are going to work on the Development Tools main section. First ingest the attached ide-features.md, acknowledge that you have ingested this file and show me some how that you understand everything in this file.

- Ingest the attached files, acknowledge that you ingest and state their version numbers. You don't have to know what task is next though.

- Now ingest the attached ide-features.md file and the source code file for flymake attached as a txt file. Acknowledge that you understand everything.

- There appears to be errors in Org Super Agenda and Org Agenda Custom Commands because of (cl-defun org-super-agenda--group-dispatch-take (items (n group)) ...)

- Again you are fucking wrong. You are task is simple in concept. Because breadcrumb utilizes caching mechanism, I want to rely on breadcrumb to generate the echo area header behaviour like A -> B -> C but not rely on inferior code of legacy of org-eldoc.el. Breadcrumb is technically involved in both org-mode and prog-mode buffers because its mechanism does not rely on lsp server like breadcrumb behaviour from lsp-mode. However, whereas for prog-mode, I would breadcrumb to occupy the top-bar of the buffer or the echo area, in the case of org-mode, I want breadcrumb to integrate my superior org-eldoc.el file to work exclusively in the echo area. Utilizing the echo area for breadcrumb features using imenu, eldoc, etc is top priority because echo area in emacs is one of the core strengths of emacs. Look at an old org-eldoc configuration I had previously which was flawed in many ways but it captured the essence of how beautifully the echo area can be utilized for single or two line behaviour:

```el
(defun ar/org-eldoc-breadcrumb (callback &rest _)
  "Show org heading breadcrumb path in eldoc."
  (when (derived-mode-p 'org-mode)
    (let* ((face-sep     '(:inherit shadow :height 0.8))
           (face-path    '(:inherit shadow))
           (face-current '(:inherit font-lock-keyword-face :weight bold))
           (sep-str (propertize " → " 'face face-sep)))
      (condition-case nil
          (unless (org-in-src-block-p)
            (cond
             ((or (eobp)
                  (save-excursion (forward-line 1) (eobp)))
              (funcall callback (propertize "Bottom Level" 'face face-path)))

             ((let ((path (ignore-errors (org-get-outline-path t))))
                (when path
                  (funcall callback
                           (mapconcat
                            (lambda (heading)
                              (if (string= heading (car (last path)))
                                  (propertize heading 'face face-current)
                                (propertize heading 'face face-path)))
                            path
                            sep-str)))))

             (t
              (funcall callback (propertize "Top Level" 'face face-path)))))
        (error nil)))))

(defun ar/org-eldoc-heading-info (callback &rest _)
  "Show TODO keyword, priority, and tags for current heading."
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (when (eq type 'headline)
            (let* ((todo     (org-element-property :todo-keyword element))
                   (priority (org-element-property :priority element))
                   (tags     (org-element-property :tags element))
                   (info-parts '()))
              (when todo
                (push (propertize todo 'face (org-get-todo-face todo))
                      info-parts))
              (when priority
                (push (propertize (format "[#%c]" priority) 'face
                                  (org-get-priority-face priority))
                      info-parts))
              (when tags
                (push (propertize (concat ":" (string-join tags ":") ":")
                                  'face 'org-tag)
                      info-parts))
              (when info-parts
                (funcall callback (string-join (nreverse info-parts) " "))))))
      (error nil))))

(defun ar/org-eldoc-timestamps (callback &rest _)
  "Show scheduling info (SCHEDULED, DEADLINE, CLOSED) for current heading."
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (when (eq type 'headline)
            (let* ((scheduled (org-element-property :scheduled element))
                   (deadline  (org-element-property :deadline  element))
                   (closed    (org-element-property :closed    element))
                   (timestamps '()))
              (when scheduled
                (push (concat
                       (propertize "SCHEDULED: " 'face 'org-special-keyword)
                       (org-element-property :raw-value scheduled))
                      timestamps))
              (when deadline
                (push (concat
                       (propertize "DEADLINE: " 'face 'org-special-keyword)
                       (org-element-property :raw-value deadline))
                      timestamps))
              (when closed
                (push (concat
                       (propertize "CLOSED: " 'face 'org-special-keyword)
                       (org-element-property :raw-value closed))
                      timestamps))
              (when timestamps
                (funcall callback (string-join (nreverse timestamps) " │ "))))))
      (error nil))))

(defun ar/org-eldoc-link (callback &rest _)
  "Show link type and target."
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (when-let ((context (org-element-context)))
          (when (eq (org-element-type context) 'link)
            (let* ((type (org-element-property :type context))
                   (path (org-element-property :path context))
                   (desc (when (org-element-property :contents-begin context)
                           (buffer-substring-no-properties
                            (org-element-property :contents-begin context)
                            (org-element-property :contents-end context)))))
              (when (and type path)
                (funcall callback
                         (concat
                          (propertize "Link: " 'face 'bold)
                          (propertize (concat type ":" path) 'face 'link)
                          (when desc
                            (format " [%s]"
                                    (propertize desc 'face 'font-lock-doc-face)))))))))
      (error nil))))

(defun ar/org-eldoc-table (callback &rest _)
  "Show table cell position and formula information."
  (when (and (derived-mode-p 'org-mode)
             (org-at-table-p)
             (not (org-at-table.el-p)))
    (condition-case nil
        (let* ((col (org-table-current-column))
               (row (save-excursion
                      (org-table-goto-line 1)
                      (count-lines (org-table-begin) (line-beginning-position))))
               (formula (org-table-get-formula col)))
          (funcall callback
                   (concat
                    (propertize (format "Cell[%d,%d]" row col)
                                'face 'font-lock-keyword-face)
                    (when (and formula (not (string-empty-p formula)))
                      (format " Formula: %s"
                              (propertize formula 'face 'font-lock-function-name-face))))))
      (error nil))))

(defun ar/org-eldoc-drawer (callback &rest _)
  "Show current drawer name."
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (save-excursion
          (beginning-of-line)
          (when (looking-at org-drawer-regexp)
            (let ((drawer (match-string 1)))
              (funcall callback
                       (propertize (format "Drawer: :%s:" drawer)
                                   'face 'org-drawer)))))
      (error nil))))

(defun ar/org-eldoc-property (callback &rest _)
  "Show property at point."
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (let ((context (org-element-context)))
          (when (eq (org-element-type context) 'node-property)
            (let ((key   (org-element-property :key   context))
                  (value (org-element-property :value context)))
              (funcall callback
                       (format "%s: %s"
                               (propertize key   'face 'org-special-keyword)
                               (propertize value 'face 'font-lock-string-face))))))
      (error nil))))

(defun ar/org-eldoc-clock (callback &rest _)
  "Show currently clocked item if any."
  (when (and (derived-mode-p 'org-mode)
             (featurep 'org-clock)
             (bound-and-true-p org-clock-current-task))
    (condition-case nil
        (funcall callback
                 (format "%s %s"
                         (propertize "CLOCKING:" 'face 'org-special-keyword)
                         (propertize org-clock-current-task 'face 'org-clock-overlay)))
      (error nil))))

;; On-demand functions (not in eldoc rotation)
(defun ar/org-show-statistics ()
  "Show statistics about current subtree (word count, task count)."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (if (eq type 'headline)
              (save-excursion
                (org-narrow-to-subtree)
                (let* ((words (count-words (point-min) (point-max)))
                       ;; FIX: org-map-entries requires a callable as its first
                       ;; argument. The original passed bare `t', which throws
                       ;; (wrong-type-argument functionp t) at runtime.
                       (tasks (length (org-map-entries
                                       (lambda () t)
                                       "/+TODO|DONE|NEXT|WAIT|HOLD" 'tree)))
                       (done  (length (org-map-entries
                                       (lambda () t)
                                       "/+DONE" 'tree))))
                  (widen)
                  (message (concat
                            (when (> words 0)
                              (format "%s words"
                                      (propertize (number-to-string words)
                                                  'face 'font-lock-constant-face)))
                            (when (and (> words 0) (> tasks 0)) " │ ")
                            (when (> tasks 0)
                              (format "Tasks: %s/%s"
                                      (propertize (number-to-string done)
                                                  'face 'org-done)
                                      (propertize (number-to-string tasks)
                                                  'face 'org-todo)))))))
            (message "Not in an Org heading")))
      (error (message "Failed to calculate statistics")))))

(defun ar/org-show-src-block-info ()
  "Show source block language and important parameters."
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (org-in-src-block-p))
    (condition-case nil
        (save-excursion
          (let* ((element    (org-element-at-point))
                 (lang       (org-element-property :language   element))
                 (switches   (org-element-property :switches   element))
                 (parameters (org-element-property :parameters element)))
            (when lang
              (message (concat
                        (propertize "Source: " 'face 'bold)
                        (propertize lang 'face 'font-lock-keyword-face)
                        (when switches
                          (format " %s" (propertize switches 'face 'font-lock-comment-face)))
                        (when (and parameters (not (string-empty-p parameters)))
                          (format " [%s]"
                                  (propertize parameters 'face 'font-lock-comment-face))))))))
      (error (message "Not in a source block or failed to retrieve info")))))

;; Keybindings for on-demand functions
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c i s") #'ar/org-show-statistics)
  (define-key org-mode-map (kbd "C-c i b") #'ar/org-show-src-block-info))

;; Register eldoc functions for org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-clock          -100 t)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-link            -80 t)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-table           -70 t)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-property        -60 t)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-drawer          -50 t)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-heading-info    -40 t)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-timestamps      -30 t)
            (add-hook 'eldoc-documentation-functions #'ar/org-eldoc-breadcrumb      -10 t)))
```

Similar ideas like above can be extended to make the echo area more useful to core emacs libraries like eldoc, imenu, treesit, etc. There are numerous clever ways you can make the echo area extremely useful for basic information without relying on external packages like org-contrib. You need to utilize emacs's C-level optimizations, ast etc, to build org-eldoc from scratch but also utilize the intelligent design of breadcrumb. Search the web and think longer for these tasks. Re-ingest all the files you need to come up with a new plan from scratch.

## Python script to build and install emacs from source

- Write a python script to build and install emacs from source. It will specifically install emacs-pretest package for now until emacs 31 has been released. It must have the following features and functionalities:
  1. The script must follow best python practices.
  2. It must be idempotent
  3. It must use the paru package manager to install necessary packages
  4. It must install python packages, if there are any, needed to run the script directly from arch linux repos using paru
  5. It must also install build and runtime dependencies for emacs-pretest.
  6. It must be able to update the emacs package
  7. It must ask permisson at each step. The format would be y/N. Only pressing y approves the step. Pressing N or any other key denies the step
  8. It must provide all the instructions to build and install emacs from source. This is the main task. For now, it will download emacs-pretest. It must use the correct --config flags something like
