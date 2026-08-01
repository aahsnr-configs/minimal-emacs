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

- [ ]

- Review and audit your latest iteration of Org Eldoc subsection and the custom ar-org-eldoc.el file. Find any errors and issues. Search the web and think longer for this task and make sure you have the latest information till July 20, 2026. Then explain everything back to me.

- Now we are going to work on the Development Tools main section. First ingest the attached ide-features.md, acknowledge that you have ingested this file and show me some how that you understand everything in this file.

- Ingest the attached files, acknowledge that you ingest and state their version numbers. You don't have to know what task is next though.

- Now ingest the attached ide-features.md file and the source code file for flymake attached as a txt file. Acknowledge that you understand everything.

- There appears to be errors in Org Super Agenda and Org Agenda Custom Commands because of (cl-defun org-super-agenda--group-dispatch-take (items (n group)) ...)

- In eglot, all eldoc related documentation should be shown using eldoc-childframe.el file. I don't want eldoc to appear in the echo area or a minibuffer under any circumstances. To make this happen, eldoc-childframe.el must implement a fixed small size for eldoc-childframe popups and then implement scroll features using keybindings like how lsp-ui does for lsp-ui-doc. Only vertical scrolling is needed. And the width of the eldoc-childframe window should be set to 3/4 of the emacs buffer itself so that the eldoc-childframe popup wraps the documentation for eldoc. For Issue 1 ub eldoc-childframe.el and my eldoc-childframe subsection, ingest the eldoc.el source code I attached as .el.txt file and then decide how to approach Issue 1. For Issue 2, also check with the eldoc source code and only then apply the fix for Issue 2. Determine what to do with Issue 3, given the fact I only use emacs as a GUI using emacsclient instances. Also determine what the best approach is to fix Issue 4. Fix Issues 5 and 6 like the way you wanted.
- For peek.el file issues, apply the fixes as you see fit.
- For the conflicts between eldoc-childframe.el and peek.el determine what is the approach to deal with this and apply the fixes to either of the two files.
- Also apply your recommendation for eldoc-childframe and markdown-ts-view-mode rendering.
- For both eldoc-childframe.el and peek.el apply the emacs 31 specific recommendations as you see fit.
  For all these tasks, as always, use best emacs 31 practices, optimizations and APIs. Search the web and think longer for these tasks. Then rewrite the next versions of both eldoc-childframe and peek.el with the changes, corrections and optimizations with GREEN LIGHT in 2 separate emacs-lisp markdown code blocks

- symbol's function definition is void: ar/org-template-transient

- I need to optimize Development Tools section as a whole. I also need to comment out config lines that are not needed. I also need optimize lsp ecosystem and flycheck as much as possible. I also need you to determine if there are useful configuration options I can borrow from https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/ and https://github.com/doomemacs/modules/blob/main/modules/tools/lsp/%2Blsp.el . Then explain everything back to me in detail. Search the web and think longer for these tasks. Make sure you have the latest information till July 30, 2026.

1. Will the finalized eldoc subsection mess with lsp-ui-doc in any way, keeping mind that I only mean to use lsp-ui-doc using hover as the primary documentation source in lsp-mode? Furthermore, does the eldoc subsection require any further changes to integrate better with the  lsp-mode ecosystem. Search the web and think longer for these tasks. Then explain everything back to me. Then you have the GREEN LIGHT to rewrite the Eldoc subsection 

2. Now lets work on the xref subsection. Similarly, does the xref subsection require any further changes other than the ones you mentioned to integrate better with the lsp-mode ecosystem? Search the web and think longer for these tasks. Then explain everything back to me. Then you have the GREEN LIGHT to rewrite the xref subsection. And then state what next subsection I want you to look at.

3. Now lets work on the Language Server Client subsection. For Language Server Client subsection, I have the following observations:
   - You need to further justify why 0.5 is better for lsp-idle-delay since lsp-mode documentation only mentions this value. It does not necessarily recommend it. 
   - You need to further analyze whether lsp-signature related settings are correct or whether additional settings need to be applied for optimizations as well. Furthermore, I don't completely understand lsp signature actually does in prog-modes. Explain its purpose in simple words with respect to python buffers.
  - You need to set lsp-enable-suggest-server-download to nil since I let direnv manage the lsp server clients
  - Furthermore, I don't really know what lsp-auto-configure actually does when set to t, so you need to explain this variable to me as well.
  
Other than what I mentioned so far, everything what you said for Subsection 3 ** TODO Language Server Client is correct and should be done according to the way you recommended. Search the web and think longer for these tasks. Then explain everything back to me. Then you have the GREEN LIGHT to rewrite the Language Server Client subsection. And then state what next subsection I want you to look at.

4. For Language Server Visuals subsections, the following are my own opinions on the points you raised:
  - For  lsp-ui-doc-delay you recommend setting this to 0.75 according to how Doom sets this and then you refer to the emacs-lsp-mode documentation to justify the use of this value, but the issue the document you point out to only sets the lsp-idle-delay, not lsp-ui-doc-delay. Furthermore, why would flickering be an issue at 0.2 since I only toggle the lsp-ui-doc on command or keybinding. It is not like like documentation hover is shown automatically when the cursor is in position. The naggy part that Doom emacs mentions is not a technical justification. So your recommendation to use 0.75 or any other value than the default does not have logical or mathematical ground to stand on. You should think carefully instead of falsely hallucinating your justifications. 
  - Furtheremore, since I automatically disable lsp-ui-sideline why would it matter what the other lsp-ui-sideline variables should be?
  - You need to explain to me why the following statement is true if lsp-auto-configure is set to t: "it prevents lsp-mode from double-enabling lsp-ui if lsp-auto-configure is t."

---

---

---

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
