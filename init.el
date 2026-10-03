;;; init.el --- Package bootstrap and module loader for a vanilla Emacs 31.1 -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is the single source of truth for package management and
;; module wiring.  Packages are owned by straight.el: a bootstrap fetches
;; straight itself, the complete manifest (`ar/packages') is cloned and
;; built in one pass under a loud failure contract, all state files are
;; routed into etc/ and var/ natively (early-init.el, no no-littering),
;; and the modules under lisp/ are loaded in a load-bearing order.
;; early-init.el runs before this file and owns everything that must
;; happen before the first frame.

;;; Code:

;; straight.el bootstrap.  straight owns every external package: it
;; clones recipe repositories and builds from git, which also sidesteps
;; networks that reset TLS to the elpa.gnu.org / elpa.nongnu.org hosts --
;; everything arrives from GitHub mirrors instead.  Fetching the
;; installer over HTTPS from raw.githubusercontent.com is the documented
;; bootstrap (version 7).  `straight-check-for-modifications' must be set
;; before straight loads: dropping `find-when-checking' keeps normal
;; startups free of filesystem scans, while edits picked up by
;; `check-on-save' (and `reload-init-file' loading source directly)
;; still take effect.
(defvar straight-check-for-modifications) ;; declared so the setq compiles clean
(setq straight-check-for-modifications '(check-on-save))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Repos and builds live under var/straight/ (early-init.el routes
;; `straight-base-dir' there) and are native-compiled into
;; var/eln-cache.  No package.el activation happens at all: early-init.el
;; sets `package-enable-at-startup' to nil.

;; Resolve dependencies against the Emacs 31.1 bundled libraries.  A
;; plain manifest pass would let straight rebuild bundled libraries
;; (org, transient, ...) whenever any package's PACKAGE-REQUIRES
;; mentions them; registering them as :type built-in keeps the Emacs
;; build's own versions on `load-path' instead (the old config did the
;; same with :straight (:type built-in) forms).  eglot left the list
;; with the lsp-mode migration (build/design-lsp.md §5.2): nothing in
;; the manifest references it any more.  jsonrpc stays for dape, whose
;; PACKAGE-REQUIRES still name it.
(dolist (built-in '(compat eldoc external-completion flymake
                    jsonrpc org project seq transient xref))
  (straight-use-package (list built-in :type 'built-in)))

;; The external package manifest.  init.el is the only place that installs
;; anything: every package the later modules declare is listed here, as
;; either a plain package name (straight's default recipe repositories
;; resolve melpa and gnu-elpa-mirror recipes) or an explicit straight
;; recipe for git-only packages.  The built-ins that ship with Emacs 31.1
;; (use-package, which-key, flymake, org, transient, project, jsonrpc,
;; seq, compat, ...) are configured by the modules but never
;; installed, and dependencies resolve automatically at clone time.
;; Grouped by the module that configures each entry.

(defvar ar/packages
  '(;; core
    buffer-terminator
    ;; editor (evil stack and editing aids)
    general evil evil-collection evil-commentary evil-args evil-numbers
    evil-exchange evil-goggles evil-lion evil-surround evil-embrace
    evil-matchit evil-textobj-tree-sitter evil-easymotion evil-snipe
    evil-multiedit evil-anzu anzu undo-fu undo-fu-session goto-chg expreg
    avy link-hint iedit move-text super-save hl-todo sudo-edit
    jinx helpful
    ;; git-only: evil-ts-obj ships its .el files under lisp/ in the repo,
    ;; so the default melpa recipe's file spec needs this override
    (evil-ts-obj :host github :repo "dvzubarev/evil-ts-obj"
                 :files ("lisp/*.el" "README.org"))
    ;; git-only: ws-butler is served from the NonGNU ELPA mirror branch,
    ;; packaged exactly like Doom's editor/whitespace module -- the
    ;; :local-repo keeps this checkout isolated from the shared
    ;; nongnu_elpa repository that hosts many other packages
    (ws-butler :host github :repo "emacsmirror/nongnu_elpa"
               :branch "elpa/ws-butler"
               :local-repo "ws-butler"
               :pin "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a")
    ;; ui
    doom-themes doom-modeline solaire-mode posframe nerd-icons colorful-mode
    rainbow-delimiters indent-bars treesit-fold vimish-fold evil-vimish-fold
    ;; completion (vertico/consult/embark + corfu/cape)
    vertico orderless marginalia consult embark embark-consult consult-dir
    corfu cape nerd-icons-completion nerd-icons-corfu consult-todo
    ;; org (denote second brain)
    org-modern org-super-agenda evil-org org-transclusion consult-denote
    denote denote-silo denote-sequence denote-org denote-journal denote-menu
    denote-review denote-journal-capture denote-regexp denote-solo
    denote-explore denote-wordcloud denote-sections universal-sidecar
    markdown-mode
    ;; git-only denote/heading companions absent from every recipe repo
    (denote-merge :host github :repo "protesilaos/denote-merge")
    (org-block-wrap :host gitlab :repo "vegasharmon/org-block-wrap")
    ;; dev (lsp-mode/flymake/dape/apheleia tooling)
    lsp-mode lsp-treemacs consult-lsp eldoc-box
    dape apheleia envrc yasnippet yasnippet-snippets
    consult-yasnippet yasnippet-capf
    ;; vc-projects (magit, dired, projects, ghostel terminal)
    magit forge diff-hl git-timemachine blamer magit-todos dirvish diredfl
    treemacs treemacs-nerd-icons treemacs-evil treemacs-magit projection
    ibuffer-vc nerd-icons-ibuffer bufferlo ghostel evil-ghostel
    ;; keybinds
    ace-window popper)
  "Every external package this config installs, grouped by module.
Entries are package names (straight's default recipe repositories
resolve them) or explicit straight recipes for git-only packages.")

;; Install contract: one straight pass over the manifest.  Every entry is
;; cloned and built on the first run; subsequent runs are no-ops thanks
;; to `straight-check-for-modifications' (set above).  Failures are
;; collected instead of aborting the loop, and any failure at all raises
;; a hard `error' at the end so batch runs exit non-zero and interactive
;; startup aborts with a message naming every failing package.
(declare-function straight-use-package "straight")

(let ((failures '()))
  (dolist (entry ar/packages)
    (condition-case err
        (straight-use-package entry)
      (error
       (push (format "straight-use-package %S: %s"
                     (if (consp entry) (car entry) entry)
                     (error-message-string err))
             failures))))
  (when failures
    (error "Package bootstrap FAILED (%d):\n%s"
           (length failures) (string-join failures "\n"))))

;; State hygiene: etc/ and var/ routing is owned entirely by
;; early-init.el -- it must predate the straight bootstrap so that
;; packages (and straight itself) never write into the configuration
;; root.  What remains here is loading the user's custom-file, which
;; early-init.el already located in etc/.

;; Load immediately instead of on after-init-hook: the hook never runs in
;; batch, and the old deferral existed for a stale package-manager reason only.
(load custom-file 'noerror 'nomessage)

;; Module wiring.  Every module lives in lisp/ with a descriptive name
;; chosen to never shadow a bundled library on `load-path' (a module
;; literally named org.el once did, and the collision cost a guarded
;; loader plus an absolute-file-name load to work around).
(defvar ar/config-dir) ;; the real configuration root, set by early-init.el

(add-to-list 'load-path (expand-file-name "lisp" ar/config-dir))

;; The bundled use-package stays purely declarative:
;; `straight-use-package-by-default' is deliberately left nil, so the
;; use-package forms in the modules configure but never install --
;; installation is owned by the manifest pass above.
(require 'use-package)
(setopt use-package-expand-minimally t
        use-package-enable-imenu-support t)

;; Module load order is load-bearing: core-config defines the shared
;; helpers every later module uses, editor-config defines the
;; `ar/global-leader' and `ar/local-leader' definers, and keybinds-config
;; binds commands from all of them -- so this order must not change.
;; org-config requires the real Org library eagerly by design (see its
;; module commentary for the trade against `with-eval-after-load'
;; deferral).
(defvar ar-modules
  '(core-config editor-config ui-config completion-config org-config
    dev-config vc-projects-config keybinds-config)
  "Module features this file loads, in this exact order.
`reload-init-file' re-evaluates these files after startup.")

;; Byte-compiled module cache (build/design-lsp.md §8.3).  Machine-written
;; state under var/ per the config's routing philosophy.  var/bytecode/
;; rides on `load-path' only while EVERY cached .elc is current:
;; `load-prefer-newer' compares suffix variants within the first matching
;; load-path entry only (verified on this Emacs 31.1: a stale .elc in an
;; earlier entry beats fresher source in a later one), so a cache holding
;; any stale copy is withheld entirely -- source loads this session and the
;; idle helper below rebuilds the cache for the next startup.  That keeps
;; the design's staleness rule: a current .elc wins over its source, a
;; stale .elc never does.
(defvar ar/bytecode-dir (ar/var-file "bytecode/")
  "Directory holding byte-compiled copies of the `ar-modules' files.")

(let ((lisp-dir (expand-file-name "lisp" ar/config-dir))
      (fresh t))
  (dolist (mod ar-modules)
    (let* ((name (concat (symbol-name mod) ".el"))
           (src (expand-file-name name lisp-dir))
           (dest (expand-file-name (concat name "c") ar/bytecode-dir)))
      (when (and (file-exists-p src)
                 (or (not (file-exists-p dest))
                     (file-newer-than-file-p src dest)))
        (setq fresh nil))))
  (when fresh
    (add-to-list 'load-path ar/bytecode-dir)))

(dolist (mod ar-modules)
  (require mod))

;; Keep the var/bytecode/ copies fresh: one-shot idle timer so batch runs
;; never fire it (the pattern dev-config and completion-config rely on), and
;; compilation in an `emacs --batch' subprocess so the session never blocks.
(defun ar/byte-compile-stale-modules ()
  "Byte-compile `ar-modules' whose cached .elc is missing or stale.
Stale means the .elc under `ar/bytecode-dir' is older than its
source under lisp/.  Compilation runs in an `emacs --batch'
subprocess so the session never blocks; output lands in the
*ar-byte-compile* buffer."
  ;; `file-writable-p' is nil for a missing directory even under a writable
  ;; parent (verified on this Emacs), so the directory is created first --
  ;; make-directory with PARENTS is idempotent -- and writability checked
  ;; after, making the helper a no-op wherever it cannot write.
  (ignore-errors (make-directory ar/bytecode-dir t))
  (when (file-writable-p ar/bytecode-dir)
    (let* ((lisp-dir (expand-file-name "lisp" ar/config-dir))
           (stale
            (delq nil
                  (mapcar
                   (lambda (mod)
                     (let* ((name (concat (symbol-name mod) ".el"))
                            (src (expand-file-name name lisp-dir))
                            (dest (expand-file-name (concat name "c")
                                                    ar/bytecode-dir)))
                       (when (and (file-exists-p src)
                                  (or (not (file-exists-p dest))
                                      (file-newer-than-file-p src dest)))
                         (cons src dest))))
                   ar-modules))))
      (when stale
        (apply #'start-process
               "ar-byte-compile" "*ar-byte-compile*"
               (concat invocation-directory invocation-name)
               ;; --batch only: `--quiet' is not an Emacs option and makes
               ;; the subprocess exit 255 (verified on this Emacs 31.1).
               "--batch"
               ;; Each stale source maps to its var/bytecode destination
               ;; through one printed alist (prin1 keeps the path strings
               ;; quoted, so the subprocess reads them back as strings); no
               ;; .elc is ever written next to the lisp/ sources.
               "--eval" (concat "(setq byte-compile-dest-file-function"
                                " (lambda (f) (or (cdr (assoc f '"
                                (prin1-to-string stale)
                                ")) f)))")
               ;; Same load pair the design's byte-compile gate uses: module
               ;; sources call `ar/var-file' at compile time (use-package
               ;; :preface), so the subprocess needs early-init and init.
               "-l" (expand-file-name "early-init.el" ar/config-dir)
               "-l" (expand-file-name "init.el" ar/config-dir)
               "-f" "batch-byte-compile"
               (mapcar #'car stale))))))

(unless noninteractive
  (run-with-idle-timer 8 nil #'ar/byte-compile-stale-modules))

(provide 'init)
;;; init.el ends here
