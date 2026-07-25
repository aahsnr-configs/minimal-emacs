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
