- Ingest the 5 attached files and make sure you have read everything to the letter and follow all the instructions to the letter.

- Now lets work on the Misc subsection. Review this section thoroughly in great detail. Then fix any errors and issues. But do not rewrite the Misc subsection yet under any circumstances. Explain everything in great detail for me to review.

- Now lets work on the subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now review the next subsection, optimize it and rewrite it. Follow the protocols and instructions from the system-prompt-protocol.md file and make sure the documentation is concise. Also determine if there are additional configurations that might be useful

- Are you certain there are no more errors and issues? And are there any additional configuration settings that you think might be useful? Also the documentation must be more concise

Now lets move onto the main section: Folding. Instead of writing separate org blocks, you will be writing a single org block with all the packages. First review and analyze the commented config code in Treesit Fold subsection and the config code from Vimish fold subsection. Fix any errors and issues. Then look at code from the folding module in doom emacs given below:

```el
;;; editor/fold/config.el -*- lexical-binding: t; -*-

(defcustom +fold-ellipsis " [...] "
  "The ellipsis to show for ellided regions (folds).

`org-ellipsis' and `truncate-string-ellipsis' are set to this."
  :type 'string
  :group '+fold)

(defface +fold-hideshow-folded-face
  `((t (:inherit font-lock-comment-face :weight light)))
  "Face to hightlight `hideshow' overlays."
  :group 'doom-themes)


;;
;;; Global config

(when (modulep! :editor evil)
  ;; Add vimish-fold, outline-mode & hideshow support to folding commands
  (define-key! 'global
    [remap evil-toggle-fold]   #'+fold/toggle
    [remap evil-close-fold]    #'+fold/close
    [remap evil-open-fold]     #'+fold/open
    [remap evil-open-fold-rec] #'+fold/open-rec
    [remap evil-close-folds]   #'+fold/close-all
    [remap evil-open-folds]    #'+fold/open-all)
  (after! evil
    (evil-define-key* 'motion 'global
      "zj" #'+fold/next
      "zk" #'+fold/previous
      "zf" #'evil-vimish-fold/create
      "zF" #'evil-vimish-fold/create-line
      "zd" #'vimish-fold-delete
      "zE" #'vimish-fold-delete-all)))

(after! org
  (setq org-ellipsis +fold-ellipsis))


;;
;;; Packages

(use-package! hideshow ; built-in
  :commands (hs-toggle-hiding
             hs-hide-block
             hs-hide-level
             hs-show-all
             hs-hide-all)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn)

  (defadvice! +fold--hideshow-ensure-mode-a (&rest _)
    "Ensure `hs-minor-mode' is enabled when we need it, no sooner or later."
    :before '(hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
    (+fold--ensure-hideshow-mode))

  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((vimrc-mode "{{{" "}}}" "\"")
             (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                        ""
                        "#"
                        +fold-hideshow-forward-block-by-indent-fn nil)
             (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
             (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                        "end\\|[]}]"
                        "#\\|=begin"
                        ruby-forward-sexp)
             (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                          "end"
                          nil (lambda (_arg) (matlab-forward-sexp)))
             (nxml-mode "<!--\\|<[^/>]*[^/]>"
                        "-->\\|</[^/>]*[^/]>"
                        "<!--" sgml-skip-tag-forward nil)
             (latex-mode
              ;; LaTeX-find-matching-end needs to be inside the env
              ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
              "\\\\end{[a-zA-Z*]+}"
              "%"
              (lambda (_arg)
                ;; Don't fold whole document, that's useless
                (unless (save-excursion
                          (search-backward "\\begin{document}"
                                           (line-beginning-position) t))
                  (LaTeX-find-matching-end)))
              nil))
           hs-special-modes-alist
           '((t))))))


(use-package! evil-vimish-fold
  :when (modulep! :editor evil)
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
             evil-vimish-fold/delete evil-vimish-fold/delete-all
             evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (doom-profile-cache-dir t "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode +1))


;; Will be autoloaded by fold commands
(use-package! treesit-fold
  :defer t
  :config (global-treesit-fold-mode +1))

```

Then determine if there are any useful configurations you can borrow for my vanilla emacs configuration in config.org. Then search the web and determine if there are any other emacs packages built-in or external that can be useful as well to integrate into my emacs configuration. Then present me an elaborate proposal on how you plan to rewrite the whole Folding main section. Explain everything to me in great detail. But you are not allowed to write any org config blocks until I give you the greenlight. Search the web and think longer for all the tasks and make sure you have the latest information till July 2026.

Re-evaluate all your analyses and your execution plan by searching the web and making sure you have the latest information till July 2026. Then explain everything back to me. You still have not answered all the questions from the previous prompt: "Now review your latest rewrite of folding section making sure there are no errors and issues. Also search the web and determine if there are other emacs packages built-in or external that might be useful to emacs configuration. Also make sure the whole folding section well integrated with the rest of my emacs configuration. Next in your general-define-key section you don't need with-eval-after-load 'evil since the general.el use-package config already has :after evil. Also are you sure ar/fold-ellipsis will work universally like you plan. And are you sure there are no errors in your custom functions as well. Analyze everything very carefully. Then explain everything back to me in detail. But you do not permission to rewrite the fold org block yet. Search the web and think longer for all the tasks." And you still not have permission to rewrite fold org block yet. Search the web and think longer for all the tasks and make sure you have the latest information till July 2026.

Now lets move onto the next subssection: Stripspace that will be renamed to Whitespace. First review and analyze the config in this subsection. Fix any errors and issues. Then look at code from the whitespace module in doom emacs given below:

```el
;;; editor/whitespace/config.el -*- lexical-binding: t; -*-

(defvar +whitespace-guess-excluded-modes
  '(pascal-mode
    so-long-mode
    ;; Variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
    ;; and `editorconfig' would force fixed indentation on elisp.
    emacs-lisp-mode
    ;; See #5823: indent detection is slow and inconclusive in these major modes
    ;; so they are disabled there.
    coq-mode
    ;; Automatic indent detection in org files is meaningless. Not to mention, a
    ;; non-standard `tab-width' causes an error in org-mode.
    org-mode)
  "A list of major modes where indentation shouldn't be auto-detected.")

(defvar +whitespace-guess-in-projects nil
  "If non-nil, indentation settings will be guessed in project files.

This is off by default because, generally, indent-guessing is less useful for
projects, which have many options for configuring editors (editorconfig,
.dir-locals.el, global settings, etc). While single files have fewer options and
are more likely to use varied styles (and would be a pain to accommodate on a
per-file basis).")

(defvar-local +whitespace-guess-inhibit nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to guess
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")


;;
;;; Packages

(use-package! whitespace
  :defer t
  :init
  (add-hook! 'after-change-major-mode-hook :append
    (defun +whitespace-highlight-incorrect-indentation-h ()
      "Highlight whitespace at odds with `indent-tabs-mode'.

That is, highlight tabs if `indent-tabs-mode' is `nil', and highlight spaces at
the beginnings of lines if `indent-tabs-mode' is `t'. The purpose is to make
incorrect indentation in the current buffer obvious to you, so it can be noticed
and corrected.

Does nothing if `whitespace-mode' or `global-whitespace-mode' is already active
or if the current buffer is read-only or not file-visiting."
      (unless (or (eq major-mode 'fundamental-mode)
                  (bound-and-true-p global-whitespace-mode)
                  (null buffer-file-name)
                  buffer-read-only)
        (require 'whitespace)
        (set (make-local-variable 'whitespace-style)
             (cl-union (if indent-tabs-mode
                           '(indentation)
                         '(tabs tab-mark))
                       (when whitespace-mode
                         (remq 'face whitespace-active-style))))
        (cl-pushnew 'face whitespace-style) ; must be first
        (whitespace-mode +1))))

  ;; Fix #8573: Editorconfig may change the tab-width or indent style later in a
  ;;   file's init process, so whitespace-mode needs refreshing.
  (add-hook! 'editorconfig-after-apply-functions :append
    (defun +whitespace-highlight-incorrect-indentation-again-h (props)
      (when (and (bound-and-true-p whitespace-mode)  ; in case user disabled it
                 (gethash 'indent_style props))
        (+whitespace-highlight-incorrect-indentation-h))))

  :config
  (setq whitespace-line-column nil
        whitespace-style
        '(face indentation tabs tab-mark spaces space-mark newline newline-mark
          trailing lines-tail)
        whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))

  ;; HACK: `whitespace-mode' inundates child frames with whitespace markers, so
  ;;   disable it to fix all that visual noise.
  (defun +whitespace--in-parent-frame-p () (null (frame-parameter nil 'parent-frame)))
  (add-function :before-while whitespace-enable-predicate #'+whitespace--in-parent-frame-p))


(use-package! dtrt-indent
  :when (modulep! +guess)
  ;; Automatic detection of indent settings
  :unless noninteractive
  ;; HACK: I'm not using `global-dtrt-indent-mode' because it has hard-coded and
  ;;   rigid major mode checks and activates itself too late (before
  ;;   indent-aware plugins like `indent-bars-mode' are likely to be activated).
  :hook ((change-major-mode-after-body read-only-mode) . +whitespace-guess-indentation-h)
  :config
  (defun +whitespace-guess-indentation-h ()
    (unless (or (not after-init-time)
                (bound-and-true-p so-long-detected-p)
                +whitespace-guess-inhibit
                (eq major-mode 'fundamental-mode)
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (apply #'derived-mode-p +whitespace-guess-excluded-modes)
                buffer-read-only
                (and (not +whitespace-guess-in-projects)
                     (doom-project-root)))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1))))

  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; Doom sets `tab-width' and `evil-shift-width' for us in `doom-set-indent'.
  (dolist (var (get 'tab-width 'indent-vars))
    (cl-callf2 rassq-delete-all var dtrt-indent-hook-generic-mapping-list))

  ;; Add missing language support
  ;; REVIEW: PR these upstream.
  (add-to-list 'dtrt-indent-hook-mapping-list '(gdscript-mode default gdscript-indent-offset))
  (add-to-list 'dtrt-indent-hook-mapping-list '(graphviz-mode graphviz-dot-indent-width))
  (add-to-list 'dtrt-indent-hook-mapping-list '(janet-mode janet janet-indent))

  (defadvice! +whitespace--guess-smie-modes-a (fn &optional arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config--guess (beg end)
                (funcall symbol-config--guess beg (min end 10000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[WARNING] Indent detection: %s"
                                  (error-message-string e))
                         (message ""))))) ; warn silently
        (funcall fn arg)))))


;; a less intrusive `delete-trailing-whitespaces' on save
(use-package! ws-butler
  :when (modulep! +trim)
  :hook (doom-first-buffer . ws-butler-global-mode)
  :config
  (dolist (mode '(special-mode
                  comint-mode
                  term-mode
                  eshell-mode
                  diff-mode))
    (add-to-list 'ws-butler-global-exempt-modes mode)))
```

Then determine if there are any useful configurations you can borrow from this whitespace module for my vanilla emacs configuration in config.org. Then search the web and determine if there are any other emacs packages built-in or external that can be useful as well to integrate into my emacs configuration. Then present me an elaborate proposal on how you plan to rewrite this subsection. Explain everything to me in great detail. But you are not allowed to write any org config blocks until I give you the greenlight. Search the web and think longer for all the tasks and make sure you have the latest information till July 2026. Do not hallucinate. Do not guess.
