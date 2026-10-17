;;; config-regressions.el --- Package-free config regression checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'comint)
(require 'python)
(require 'org)
(require 'ob-core)
(require 'project)
(require 'use-package)

(defconst ar/test-root (make-temp-file "ar-config-regressions-" t))
(defvar ar/config-dir default-directory)
(defvar ar/var-dir (expand-file-name "var/" ar/test-root))
(defvar ar/etc-dir (expand-file-name "etc/" ar/test-root))
(defvar ar/org-directory (expand-file-name "org/" ar/test-root))
(defvar ar/recentf-flush-timer nil)

(defun ar/var-file (name)
  (expand-file-name name (expand-file-name "var/" ar/test-root)))

(defun ar/etc-file (name)
  (expand-file-name name (expand-file-name "etc/" ar/test-root)))

(defconst ar/test-real-use-package (symbol-function 'use-package))

(defun ar/test-load-core-primitives ()
  "Load the shared guard definitions without evaluating personal settings."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "lisp/core-config.el" ar/config-dir))
    (goto-char (point-min))
    (let ((loaded 0))
      (while (< loaded 3)
        (let ((form (read (current-buffer))))
          (when (or (and (eq (car-safe form) 'defgroup)
                         (eq (cadr form) 'ar))
                    (and (eq (car-safe form) 'defun)
                         (eq (cadr form) 'too-long-file-p))
                    (and (eq (car-safe form) 'defmacro)
                         (eq (cadr form) 'ar/define-large-file-guard)))
            (eval form)
            (setq loaded (1+ loaded))))))))

(defmacro use-package (&rest _arguments)
  "Skip package activation in package-free regression tests.")

(defmacro ar/global-leader (&rest _bindings)
  "Skip keymap installation in package-free regression tests.")

(defmacro general-define-key (&rest _bindings)
  "Skip keymap installation in package-free regression tests.")

(defvar ar/test-project-switch-calls nil)
(defun ar/test-project-switch-project (directory)
  "Record a project switch to DIRECTORY for the workspace regression test."
  (interactive (list (project-prompt-project-dir)))
  (push (list 'project directory) ar/test-project-switch-calls))

(defun ar/test-eval-use-package-form (file package)
  "Evaluate PACKAGE's actual `use-package' form from FILE."
  (let ((found nil)
        (stub-use-package (symbol-function 'use-package)))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file ar/config-dir))
      (goto-char (point-min))
      (condition-case nil
          (while (not found)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) package))
                (setq found t)
                (unwind-protect
                    (progn
                      (fset 'use-package ar/test-real-use-package)
                      (eval form))
                  (fset 'use-package stub-use-package)))))
        (end-of-file nil)))
    (unless found
      (error "No use-package form for %S in %s" package file))))

(ar/test-load-core-primitives)
(load (expand-file-name "lisp/ui-config.el" ar/config-dir) nil 'nomessage)
(load (expand-file-name "lisp/dev-config.el" ar/config-dir) nil 'nomessage)
(load (expand-file-name "lisp/lang-config.el" ar/config-dir) nil 'nomessage)
(load (expand-file-name "lisp/org-config.el" ar/config-dir) nil 'nomessage)
(load (expand-file-name "lisp/vc-projects-config.el" ar/config-dir) nil 'nomessage)

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (file-directory-p ar/test-root)
              (delete-directory ar/test-root t))))

(ert-deftest ar-popup-ignore-rule-is-respected ()
  (let ((buffer (get-buffer-create "*Capture*")))
    (unwind-protect
        (with-current-buffer buffer
          (should-not (ar/popup--match-p buffer)))
      (kill-buffer buffer))))

(ert-deftest ar-popup-comint-remains-writable ()
  (with-temp-buffer
    (comint-mode)
    (let ((saved-mode-line mode-line-format)
          (saved-header-line header-line-format))
      (unwind-protect
          (progn
            (ar/popup-buffer-mode 1)
            (should-not buffer-read-only)
            (should-not mode-line-format)
            (ar/popup-buffer-mode -1)
            (should (equal mode-line-format saved-mode-line))
            (should (equal header-line-format saved-header-line)))
        (when ar/popup-buffer-mode
          (ar/popup-buffer-mode -1))))))

(ert-deftest ar-popup-quit-advice-follows-mode-state ()
  (unwind-protect
      (progn
        (ar/popup-global-mode 1)
        (should (advice-member-p #'ar/popup-close-on-quit #'keyboard-quit))
        (ar/popup-global-mode -1)
        (should-not (advice-member-p #'ar/popup-close-on-quit #'keyboard-quit)))
    (when ar/popup-global-mode
      (ar/popup-global-mode -1))))

(ert-deftest ar-transient-menus-available-on-first-use ()
  (let ((stub-use-package (symbol-function 'use-package)))
    (unwind-protect
        (progn
          (fset 'use-package ar/test-real-use-package)
          (load (expand-file-name "lisp/keybinds-config.el" ar/config-dir)
                nil 'nomessage)
          (should (featurep 'transient))
          (should (fboundp 'ar/toggles-vc-menu)))
      (fset 'use-package stub-use-package))))

(ert-deftest ar-python-base-hook-activates-project-venv ()
  (let* ((project-root (make-temp-file "ar-python-project-" t))
         (venv (expand-file-name ".venv" project-root))
         (activated nil))
    (unwind-protect
        (progn
          (ar/test-eval-use-package-form "lisp/lang-config.el" 'pyvenv)
          (make-directory venv)
          (should (memq #'ar/pyvenv-activate-project-venv
                        python-base-mode-hook))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "main.py" project-root))
            (cl-letf (((symbol-function 'pyvenv-activate)
                       (lambda (path) (setq activated path))))
              (run-hooks 'python-base-mode-hook)))
          (should (equal activated venv))
          (let ((activated nil)
                (other-root (make-temp-file "ar-python-no-venv-" t)))
            (unwind-protect
                (with-temp-buffer
                  (setq buffer-file-name (expand-file-name "main.py" other-root))
                  (cl-letf (((symbol-function 'pyvenv-activate)
                             (lambda (_path) (setq activated t))))
                    (run-hooks 'python-base-mode-hook))
                  (should-not activated))
              (delete-directory other-root t))))
      (delete-directory project-root t))))

(ert-deftest ar-lsp-inlay-hints-has-large-file-guard ()
  (should (advice-member-p 'guard-large-files
                           #'lsp-inlay-hints-mode)))

(ert-deftest ar-denote-silo-rejects-escaping-names ()
  (dolist (name '("" "." ".." "../escape" "/tmp/absolute"))
    (should-error (ar/create-denote-project-silo name) :type 'user-error))
  (should-not (file-exists-p (expand-file-name "projects/" ar/org-directory)))
  (should
   (equal (ar/create-denote-project-silo "project-a")
          (expand-file-name "projects/project-a" ar/org-directory))))

(ert-deftest ar-org-babel-load-caches-only-success ()
  (let ((org-babel-load-languages nil)
        (original-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional file noerror)
                 (if (memq feature '(ob-ar-test-missing ar-test-missing))
                     nil
                   (funcall original-require feature file noerror)))))
      (ar/org--babel-lazy-load 'ar-test-missing)
      (should-not (assq 'ar-test-missing org-babel-load-languages)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _file _noerror)
                 (eq feature 'ob-ar-test-present))))
      (ar/org--babel-lazy-load 'ar-test-present)
      (should (equal (assq 'ar-test-present org-babel-load-languages)
                     '(ar-test-present . t))))))

(ert-deftest ar-workspace-project-switch-uses-one-project-root ()
  (let* ((project-root (make-temp-file "ar-project-root-" t))
         (prompt-count 0)
         (persp-name nil)
         (ar/test-project-switch-calls nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'project-prompt-project-dir)
                     (lambda () (setq prompt-count (1+ prompt-count)) project-root))
                    ((symbol-function 'persp-switch)
                     (lambda (name)
                       (setq persp-name name)
                       (push (list 'persp name) ar/test-project-switch-calls)))
                    ((symbol-function 'project-switch-project)
                     #'ar/test-project-switch-project))
            (ar/workspace-project-switch))
          (should (= prompt-count 1))
          (should (equal persp-name (abbreviate-file-name project-root)))
          (should (equal (nreverse ar/test-project-switch-calls)
                         (list (list 'persp (abbreviate-file-name project-root))
                               (list 'project project-root)))))
      (delete-directory project-root t))))

(ert-deftest ar-recentf-flush-timer-is-not-scheduled-in-batch ()
  (require 'recentf)
  (make-directory ar/var-dir t)
  (let ((recentf-save-file (ar/var-file "recentf-save.el"))
        (ar/recentf-flush-timer nil))
    (unwind-protect
        (progn
          (ar/test-eval-use-package-form "lisp/core-config.el" 'recentf)
          (should-not (timerp ar/recentf-flush-timer)))
      (when (timerp ar/recentf-flush-timer)
        (cancel-timer ar/recentf-flush-timer))
      (when (bound-and-true-p recentf-mode)
        (recentf-mode -1)))))

(provide 'config-regressions)
;;; config-regressions.el ends here
