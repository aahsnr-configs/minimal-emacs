## General Prompts

- Ingest the attached 5 files. Then acknowledge that you have read all 5 files and state the versions for files that have it.

- You have the `GREEN LIGHT` to execute the rewrite.

- Now lets work on the Misc subsection. Review this section thoroughly in great detail. Then fix any errors and issues. But do not rewrite the Misc subsection yet under any circumstances. Explain everything in great detail for me to review.

- Now lets work on the subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now audit and review the subsection: Org Habit. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from project_operationals.yaml file. Search the web determine if there are additional configuration settings that might be useful. Then explain everything to me in detail and present me how you plan to deal with this configuration.I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 18 2026.

- Are you sure there are no more errors and issues in this iteration of Workspaces? And are you sure there are not any more extra configurations you need to add by search the web? Answer and explain everything to me in great detail.Search the web and think longer for this task and make sure the information you get is latest till July 2026.

- Lets work on denote-sequence subsection next. You need to come up with a plan to write a config org block from scratch. Look at denote-sequence upstream documentation and source code. Search the web and think longer for this task and make sure you have the latest information till July 19, 2026. Then explain everything back to me.

- Review and audit your latest iteration of denote-sequence subsection. Find and fix any errors. Look at upstream documentations and source codes for both denote and denote-sequence packages for the review and audit. Search the web and think longer for this task and make sure you have the latest information till July 19, 2026. Then explain everything back to me.

- Now determine the purpose of denote-refs and how it might improve the second brain from org mode and second brain main section. Look at denote-refs upstream documentation and source code. Search the web and think longer for this task and make sure you have the latest information till July 19, 2026. Then explain everything back to me.

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
