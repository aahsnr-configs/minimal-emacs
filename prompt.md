- Ingest the 5 attached files. Then acknowlegde that you have read all 5 files.

- You have the `GREEN LIGHT` to execute the rewrite.

- Now lets work on the Misc subsection. Review this section thoroughly in great detail. Then fix any errors and issues. But do not rewrite the Misc subsection yet under any circumstances. Explain everything in great detail for me to review.

- Now lets work on the subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now review the next subsection: grep. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system_prompt_protocol.yaml file. Also determine if there are additional configurations that might be useful. Also determine if there keybindings you should add using general.el and SPC as leader keys. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for these task and make sure you have the latest information till July 2026. Don't review evil-multi-iedit.

- Now review the subsection: Grep. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from system_prompt_protocol.yaml file. Search the web determine if there are additional configuration settings that might be useful. Then explain everything to me in detail and present me how you plan to deal with this configuration.I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

- Are you certain there are no errors and issues in this iteration of Grep Edit Mode? Search the web and determine if there are there any additional configuration settings that you think might be useful? Also are you sure you are using the best emacs practices? Search the web and think longer for these tasks. You must not perform another rewrite. The information you get must be the latest till July 2026. Explain everything to me in detail.

- Are you sure there are no more errors and issues in this iteration? And are you sure there are not any more extra configurations you need to add by search the web? Answer and explain everything to me in great detail.Search the web and think longer for this task and make sure the information you get is latest till July 2026.

- [ ] When any issues are found based on information in sources 3 or more years ago, the AI must verify if these issues still exist and wrap the fix around a conditional
- [ ] Keybindings using any sort of general keybindings should be outside a use-package block
- [ ] Don't rely on trained data. Rely on your web search as your main source for everything using web_search and web_extractor tools

- Write a python script to build and install emacs from source. It will specifically install emacs-pretest package for now until emacs 31 has been released. It must have the following features and functionalities:
  1. The script must follow best python practices.
  2. It must be idempotent
  3. It must use the paru package manager to install necessary packages
  4. It must install python packages, if there are any, needed to run the script directly from arch linux repos using paru
  5. It must also install build and runtime dependencies for emacs-pretest.
  6. It must be able to update the emacs package
  7. It must ask permisson at each step. The format would be y/N. Only pressing y approves the step. Pressing N or any other key denies the step
  8. It must provide all the instructions to build and install emacs from source. This is the main task. For now, it will download emacs-pretest. It must use the correct --config flags something like

Now review the next subsection: iedit. First look at the source files:

```el

```

Then also look at its readme and if there are any useful configurations you can borrow:

```txt

```

Then, find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system_prompt_protocol.yaml file. Also determine if there are additional configurations that might be useful. Also determine why posframe for ace-window would be needed. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

Are you certain there are no more errors and issues in this iteration of [subsection]? Search the web and determine if there are there any additional configuration settings that you think might be useful? Also are you sure you are using the best emacs practices? Search the web thoroughly and think longer for these tasks. The information you get must be the latest till July 2026. Explain everything to me in detail for me to review.

And are you certain there are no errors and issues in the Grep Edit Mode Test? Search the web, think for longer and explain everything to me

You have 2 tasks regarding updating and rewrite 2 yaml files: 1. Use the attached config.org.txt file to generate the project_state.yaml file. 2. The system_prompt_protocol.yaml file must have the following additional instructions: (1) the header documentation should desribe the broader scope of the all the subheaders using non-technical layman terms. (2) all subheader documentations should describe the purpose of their respective packages/source code blocks in non-technical layman terms (3) inside org source code blocks there should 1-2 lines descriptions using technical language only

There must be version tracking for project_prompt_protocol.yaml and project_state.yaml

---

Using the elisp code as a template for transient menu configuration:

```el
(with-eval-after-load 'transient
  (defun ar/devtools-title ()
    (concat
     (if (fboundp 'nerd-icons-faicon)
         (nerd-icons-faicon "nf-fa-laptop_code" :face 'transient-heading :v-adjust 0.02)
       "")
     (propertize " Development Tools" 'face 'transient-heading)))

  (transient-define-prefix ar/toggles-devtools ()
    "Development Tools Dashboard"
    [:description ar/devtools-title

     ;; Group 1: LSP Code Actions
     ["Actions"
      ("a" "Code Action" eglot-code-actions)
      ("r" "Rename Symb" eglot-rename)
      ("f" "Frmt Buf" apheleia-format-buffer)
      ("i" "Org Imports" eglot-code-action-organize-imports)
      ("h" "Toggle Hint" eglot-inlay-hints-mode :transient t :if (lambda () (fboundp 'eglot-inlay-hints-mode)))
      ("R" "Reconn LSP" eglot-reconnect)]

     ;; Group 2: Navigation & Lookup
     ["Navigate"
      ("d" "Define" xref-find-definitions)
      ("D" "Refer" xref-find-references)
      ("t" "Type Def" eglot-find-typeDefinition)
      ("I" "Implem" eglot-find-implementation)
      ("A" "Apropos" xref-find-apropos)
      ("." "Go Back" xref-go-back)]

     ;; Group 3: Search & Outline
     ["Search"
      ("s" "LSP Symbols" consult-eglot-symbols)
      ("o" "Outline" consult-outline)
      ("m" "Imenu" consult-imenu)
      ("M" "Imenu Multi" consult-imenu-multi)
      ("l" "Search Line" consult-line)]

     ;; Group 4: Error Management
     ["Diagn"
      ("n" "Next Err" ar/flymake-next-error :transient t)
      ("p" "Prev Err" ar/flymake-prev-error :transient t)
      ("e" "Buf Err" flymake-show-buffer-diagnostics)
      ("E" "Proj Err" consult-flymake)
      ("!" "Check" flymake-start :transient t)]

     ;; Group 5: Code Folding
     ["Folding"
      ("TAB" "Toggle" treesit-fold-toggle :transient t)
      ("O" "Open All" treesit-fold-open-all :transient t)
      ("C" "Close All" treesit-fold-close-all :transient t)
      ("v" "Vim Toggle" vimish-fold-toggle :transient t)]

     ;; Group 6: Documentation
     ["Docs"
      ("k" "Hover Box" eldoc-box-help-at-point :transient t)
      ("K" "Echo Doc" eldoc)
      ("?" "Info Man" consult-info)
      ("M" "Man Page" woman)
      ("<escape>" "" transient-quit-one :format " ")]]))
```

write a transient menu configuration for avy using the following source code from the casual avy configuration:

```el
;;; casual-avy.el --- Transient UI for Avy -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual-avy
;; Keywords: tools
;; Version: 2.0.2
;; Package-Requires: ((emacs "29.1") (avy "0.5.0") (casual "2.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Casual Avy is an opinionated Transient-based menu for Avy.

;; INSTALLATION
;; (require 'casual-avy) ; optional if using autoloaded menu
;; (keymap-global-set "M-g" #'casual-avy-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:

(require 'transient)
(require 'avy)
(require 'display-line-numbers)
(require 'imenu)
(require 'org)
(require 'casual-lib)
(require 'casual-avy-version)

(define-obsolete-variable-alias 'casual-avy-use-unicode-symbols
  'casual-lib-use-unicode
  "1.2.0")

(defcustom casual-avy-use-unicode-symbols nil
  "If non-nil then use Unicode symbols whenever appropriate for labels."
  :type 'boolean
  :group 'avy)

(make-obsolete-variable 'casual-avy-imenu-modes nil "1.4.0")

(defcustom casual-avy-imenu-modes '(prog-mode makefile-mode)
  "List of modes to enable Imenu item in `casual-avy-tmenu'."
  :type '(repeat symbol)
  :group 'avy)

(defun casual-avy--customize-casual-avy-imenu-modes ()
  "Customize variable `casual-avy-imenu-modes'."
  (interactive)
  (customize-variable 'casual-avy-imenu-modes))

(defun casual-avy--customize-avy-group ()
  "Call the Avy customization group."
  (interactive)
  (customize-group "avy"))

(defconst casual-avy-unicode-db
  '((:scope . '("⬍" "#"))
    (:previous . '("↑" "Previous"))
    (:next . '("↓" "Next")))
  "Unicode symbol DB to use for Avy Transient menus.")

(defun casual-avy-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-avy-unicode-db))

(defun casual-avy-org-mode-p ()
  "Predicate to test if `org-mode' is enabled."
  (derived-mode-p 'org-mode))

(defun casual-avy-imenu-support-p ()
  "Predicate to test if current mode supports `imenu'."
  (if imenu--index-alist t nil))

(defun casual-avy-select-above-below (avy-fname &optional t-args)
  "Select Avy above or below function name AVY-FNAME given T-ARGS.

- AVY-FNAME function name.
- T-ARGS list of options which can include ‘--above’, ‘--below’

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (let ((t-args (if (not t-args)
                    (transient-args transient-current-command)
                  t-args)))
    (cond
     ((and (member "--above" t-args)
           (member "--below" t-args))
      ;;(message "all")
      (call-interactively (intern avy-fname)))

     ((member "--above" t-args)
      ;;(message "above")
      (call-interactively (intern (concat avy-fname "-above"))))

     ((member "--below" t-args)
      ;;(message "below")
      (call-interactively (intern (concat avy-fname "-below"))))

     (t
      ;;(message "all")
      (call-interactively (intern avy-fname))))))

(defun casual-avy-avy-goto-line (&optional t-args)
  "Jump to a line start in current buffer using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-line' (default)
• `avy-goto-line-above' (--above)
• `avy-goto-line-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-line" t-args))

(defun casual-avy-avy-goto-word-1 (&optional t-args)
  "Jump to the currently visible char at a word start using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-word-1' (default)
• `avy-goto-word-1-above' (--above)
• `avy-goto-word-1-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-word-1" t-args))

(defun casual-avy-avy-goto-symbol-1 (&optional t-args)
  "Jump to the currently visible char at a symbol start using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-symbol-1' (default)
• `avy-goto-symbol-1-above' (--above)
• `avy-goto-symbol-1-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-symbol-1" t-args))

(defun casual-avy-avy-goto-whitespace-end (&optional t-args)
  "Jump to the end of a whitespace sequence using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-whitespace-end' (default)
• `avy-goto-whitespace-end-above' (--above)
• `avy-goto-whitespace-end-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-whitespace-end" t-args))

(defun casual-avy-avy-goto-char-2 (&optional t-args)
  "Jump to the currently visible char1 followed by char2 using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-char-2' (default)
• `avy-goto-char-2-above' (--above)
• `avy-goto-char-2-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-char-2" t-args))

(defun casual-avy-about-avy ()
  "Casual Avy is a Transient menu for Avy.

Learn more about using Casual Avy at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual-avy/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual-avy/issues'

If you enjoy using Casual Avy, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Avy was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Avy.

Always choose love."
  (ignore))

(defun casual-avy-about ()
  "About information for Casual Avy."
  (interactive)
  (describe-function #'casual-avy-about-avy))

(defun casual-avy-scope-label (template)
  "Generate formatted Avy scope label with TEMPLATE string."
  (format template (casual-avy-unicode-get :scope)))

;;;###autoload (autoload 'casual-avy-tmenu "casual-avy" nil t)
(transient-define-prefix casual-avy-tmenu ()
  "Casual Avy Transient menu."
  ["Scope (applies to ⬍)"
   :description (lambda () (casual-avy-scope-label "Scope (applies to (%s))"))
   :class transient-row
   ("a" "Above" "--above")
   ("b" "Below" "--below")]
  [["Goto Thing"
    ("c" "Character" avy-goto-char-timer :transient nil)
    ("2" "2 Characters ⬍" casual-avy-avy-goto-char-2
     :description (lambda () (casual-avy-scope-label "2 Characters (%s)"))
     :transient nil)
    ("w" "Word ⬍" casual-avy-avy-goto-word-1
     :description (lambda () (casual-avy-scope-label "Word (%s)"))
     :transient nil)
    ("s" "Symbol ⬍" casual-avy-avy-goto-symbol-1
     :description (lambda () (casual-avy-scope-label "Symbol (%s)"))
     :transient nil)
    ("W" "Whitespace end ⬍" casual-avy-avy-goto-whitespace-end
     :description (lambda () (casual-avy-scope-label "Whitespace end (%s)"))
     :transient nil)
    ("p" "Pop mark" avy-pop-mark :transient nil)]

   ["Goto Line"
    :pad-keys t
    ("l" "Line ⬍" casual-avy-avy-goto-line
     :description (lambda () (casual-avy-scope-label "Line (%s)"))
     :transient nil)
    ("e" "End of line" avy-goto-end-of-line :transient nil)
    ("o" "Org heading" avy-org-goto-heading-timer
     :if casual-avy-org-mode-p
     :transient nil)
    ("n" "Line number" goto-line
     :if casual-lib-display-line-numbers-mode-p
     :transient nil)]

   ["Edit Other Line"
    ("C" "Copy" avy-kill-ring-save-whole-line :transient nil)
    ("k" "Kill" avy-kill-whole-line
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("m" "Move to above current line" avy-move-line
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("d" "Duplicate to above current line" avy-copy-line
     :if casual-lib-buffer-writeable-p
     :transient nil)]]

  [["Edit Other Region (choose two lines)"
    ("r" "Copy" avy-kill-ring-save-region :transient nil)
    ("K" "Kill" avy-kill-region
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("M" "Move to above current line" avy-move-region
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("D" "Duplicate to above current line" avy-copy-region
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("t" "Transpose lines in active region" avy-transpose-lines-in-region
     :if casual-lib-buffer-writeable-and-region-active-p
     :transient nil)]

   ["Index"
    ("g" "Org Goto…" org-goto :if casual-avy-org-mode-p)
    ("i" "Index…" imenu :if casual-avy-imenu-support-p)]

   ["Occur/Grep/Error"
    ("M-p" "Previous" previous-error
     :description (lambda () "%s" (format (casual-avy-unicode-get :previous)))
     :transient nil)
    ("M-n" "Next" next-error
     :description (lambda () "%s" (format (casual-avy-unicode-get :next)))
     :transient nil)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings›" casual-avy-settings-tmenu :transient nil)
   ("RET" "Exit Avy" transient-quit-all)])

(transient-define-prefix casual-avy-settings-tmenu ()
  ["Customize"
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)
   ("m" "Customize Imenu Modes" casual-avy--customize-casual-avy-imenu-modes)
   ("A" "Customize Avy Group" casual-avy--customize-avy-group)]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-avy-about :transient nil)
          ("v" "Version" casual-avy-version :transient nil)
          (casual-lib-quit-all)])

(provide 'casual-avy)
;;; casual-avy.el ends here

```

in a single org config block. The toggle menu for this transient-menu should be ar/global-leader outside the use-package. If ar/global-leader binding is not possible, then suggest what to use. Then propose a plan to create this custom transient menu for avy
