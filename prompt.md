## General Prompts

- Ingest the attached 5 files. Then acknowledge that you have read all 5 files and state the versions for files that have it.

- You have the `GREEN LIGHT` to execute the rewrite.

- Now lets work on the Misc subsection. Review this section thoroughly in great detail. Then fix any errors and issues. But do not rewrite the Misc subsection yet under any circumstances. Explain everything in great detail for me to review.

- Now lets work on the subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now review the next subsection: grep. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system_prompt_protocol.yaml file. Also determine if there are additional configurations that might be useful. Also determine if there keybindings you should add using general.el and SPC as leader keys. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for these task and make sure you have the latest information till July 2026. Don't review evil-multi-iedit.

- Now review the subsection: Grep. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from system_prompt_protocol.yaml file. Search the web determine if there are additional configuration settings that might be useful. Then explain everything to me in detail and present me how you plan to deal with this configuration.I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

- Determine if there are any errors and issues in latest iteration of Dirvish. Search the web and determine if there are there any additional configuration settings that you think might be useful. Determine if you are using the best emacs practices Search the web and think longer for these tasks. You must not perform another rewrite. The information you get must be the latest till July 2026. Explain everything to me in detail.

- Are you sure there are no more errors and issues in this iteration? And are you sure there are not any more extra configurations you need to add by search the web? Answer and explain everything to me in great detail.Search the web and think longer for this task and make sure the information you get is latest till July 2026.

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

## Sample large prompt

Now review the next subsection: iedit. First look at the source files:

```el

```

Then also look at its readme and if there are any useful configurations you can borrow:

```txt

```

Then, find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system_prompt_protocol.yaml file. Also determine if there are additional configurations that might be useful. Also determine why posframe for ace-window would be needed. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

Are you certain there are no more errors and issues in this iteration of [subsection]? Search the web and determine if there are there any additional configuration settings that you think might be useful? Also are you sure you are using the best emacs practices? Search the web thoroughly and think longer for these tasks. The information you get must be the latest till July 2026. Explain everything to me in detail for me to review.

And are you certain there are no errors and issues in the Grep Edit Mode Test? Search the web, think for longer and explain everything to me

---

Using the elisp code as a template for transient menu configuration:

```el

```

write a transient menu configuration for avy using the following source code from the casual avy configuration:

```el

```

in a single org config block. The toggle menu for this transient-menu should be ar/global-leader outside the use-package. If ar/global-leader binding is not possible, then suggest what to use. Then propose a plan to create this custom transient menu for avy

For your above iteration of Dirvish config org block, remove these keybindings for now:

## Use the following transient menu template:

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
