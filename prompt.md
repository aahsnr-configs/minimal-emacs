- Ingest the 4 attached files and make sure you have read everything to the letter and follow all the instructions to the letter. Then acknowlegde that you have read all 4 files.

- Ingest all the 4 attached files and perform the following tasks:
  1. Update and rewrite the system_prompt_protocol.xml file so that it has the following instructions and constraints:
  - [ ] Make sure system-prompt-protocol.xml instructs to allows use best emacs practices
  - [ ] Also the Excecution Plan should never change status from TODO to DONE. system-prompt-protocol.xml should only change status when the user prompt has the phrase `change status from TODO to DONE`
  - [ ] :commands () should have a single command per line
  - [ ] dolist should also have a single mode per line
  - [ ] header and subheader descriptions should only include descriptions of what the package does.
  2. Using the config.org.txt update and rewrite the project_state.xml.

  Both xml files should be in separate xml markdown code blocks.

- You have the `GREEN LIGHT` to execute the rewrite.

- Now lets work on the Misc subsection. Review this section thoroughly in great detail. Then fix any errors and issues. But do not rewrite the Misc subsection yet under any circumstances. Explain everything in great detail for me to review.

- Now lets work on the subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now review the next subsection: Evil Easymotion. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system-prompt-protocol.md file. Also determine if there are additional configurations that might be useful. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

- Now review the next subsection: Link Hint. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system-prompt-protocol.xml file. Also determine if there are additional configurations that might be useful. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

- Are you certain there are no more errors and issues in this iteration of evil-easymotion? And are there any additional configuration settings that you think might be useful? Also are you sure you are using the best emacs practices? Search the web and think longer for these tasks. You must not perform another rewrite. The information you get must be the latest till July 2026. Explain everything to me in detail.

- Are you sure there are no more errors and issues in the 3rd iteration? And are you sure there are not any more extra configurations you need to add? Answer and explain everything to me in great detail. Do not perform a 4th rewrite. Search the web and think longer for this task and make sure the information you get is latest till July 2026.

- Write a python script to build and install emacs from source. It will specifically install emacs-pretest package for now until emacs 31 has been released. It must have the following features and functionalities:
  1. The script must follow best python practices.
  2. It must be idempotent
  3. It must use the paru package manager to install necessary packages
  4. It must install python packages, if there are any, needed to run the script directly from arch linux repos using paru
  5. It must also install build and runtime dependencies for emacs-pretest.
  6. It must be able to update the emacs package
  7. It must ask permisson at each step. The format would be y/N. Only pressing y approves the step. Pressing N or any other key denies the step
  8. It must provide all the instructions to build and install emacs from source. This is the main task. For now, it will download emacs-pretest. It must use the correct --config flags something like

  ***

  ***

  ***

  But you did not
