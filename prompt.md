## General Prompts

- Ingest the attached 5 files. Then acknowledge that you have read all 5 files and state the versions for files that have it.

- You have the `GREEN LIGHT` to execute the rewrite.

- Now lets work on the Misc subsection. Review this section thoroughly in great detail. Then fix any errors and issues. But do not rewrite the Misc subsection yet under any circumstances. Explain everything in great detail for me to review.

- Now lets work on the subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now review the subsection: Grep. Find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from system_prompt_protocol.yaml file. Search the web determine if there are additional configuration settings that might be useful. Then explain everything to me in detail and present me how you plan to deal with this configuration.I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

- Are you sure there are no more errors and issues in this iteration of Workspaces? And are you sure there are not any more extra configurations you need to add by search the web? Answer and explain everything to me in great detail.Search the web and think longer for this task and make sure the information you get is latest till July 2026.

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

Now review the next subsection: Treemacs. First study the upstream source code files in https://github.com/Alexander-Miller/treemacs to get a basic idea how the project is setup and how to configure the package for emacs. Keep in mind that the extras in src folder need to be installed invididually. Then review the existing treemacs org config block in my existing emacs configuration. Then look at the readme in https://raw.githubusercontent.com/Alexander-Miller/treemacs/refs/heads/master/README.org to determine if there are some useful configuration you can borrow from it. Also keep in mind treemacs-icon-dired is prohibited from being installled. Then finally look at the treemacs module from the doom emacs project in https://raw.githubusercontent.com/doomemacs/modules/refs/heads/main/modules/ui/treemacs/config.el to also borrow possible useful configurations. Then, find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system_prompt_protocol.yaml file. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

## Sample large prompt

Now review the next subsection: Treemacs. First look at the source files:

```el

```

Then also look at its readme and if there are any useful configurations you can borrow:

```txt

```

Then, find and fix any errors and issues. Optimize this configuration. Follow the protocols and instructions from the system_prompt_protocol.yaml file. Also determine if there are additional configurations that might be useful. Also determine why posframe for ace-window would be needed. Then explain everything to me in detail and present me how you plan to deal with this configuration. Do not attempt the 1st iteration of the rewrite yet since I need to review everything. Search the web and think longer for this task and make sure you have the latest information till July 2026.

---

Using the elisp code as a template for transient menu configuration:

```el

```

write a transient menu configuration for avy using the following source code from the casual avy configuration:

```el

```

in a single org config block. The toggle menu for this transient-menu should be ar/global-leader outside the use-package. If ar/global-leader binding is not possible, then suggest what to use. Then propose a plan to create this custom transient menu for avy

---

---

---

## Project Management

Lets work on the Workspacses subsection using the projection emacs package. Use its upstream sources include the sources for the built-in emacs packages this package depends on. Propose a plan to write the configuration from scratch. Make sure this subsection integrates with the Workflow Management main section. Also make sure the configuration follows principles from the doom emacs project closely as can while integrating with my emacs configuration. Make sure you verify everything. Use the attached files: use_package.yaml, elpaca_package_manager.yaml files as guide. Explain everything in great detail for me to review. Propose a plan on you plan to write/rewrite the 1st iteration of the Workspaces subsection. Follow the instructions and protocols in the system_prompt_protocol.yaml file. Do not hallucinate and do not introduce errors. You are not allowed to use any sources/resources older than 3 years and you are not allowed to use any china-base sources/resources as well. And all the information you get must be the latest till July 2026.

---

Now lets work on improving the Triad. Determine if there are any errors, issues and hallucinations in this latest iteration of Buffer Management, Project Management and Workspaces subsections. Search the web and determine if there are there any additional configuration settings that you think might be useful. Also determine if you need borrow useful ideas from the doom emacs project that doom emacs implements using projectile but you would use `ibuffer + project.el + projection + bufferlo`, whereas doom emacs would use `ibuffer + projectile + persp-mode`. Determine how doom emacs does things and if you think you can borrow any ideas from the doom emacs project for my whole Workflow Management section. Determine if you are using the best emacs practices. Search the web and think longer for these tasks. You must not perform another rewrite. You must verify everything. The information you get must be the latest till July 2026. Explain everything to me in detail.

---

---

Using the attached v0.10 of the config.org.txt file, update and rewrite the attached v21 of the project_state.yaml and generate the v22 as a result.

---

Re-evaluate everything again by ingesting the readme for bufferlo from https://raw.githubusercontent.com/florommel/bufferlo/refs/heads/main/README.org and its source code: https://raw.githubusercontent.com/florommel/bufferlo/refs/heads/main/bufferlo.el

Also ingest the readme for projection from https://raw.githubusercontent.com/mohkale/projection/refs/heads/master/README.org and also ingest the upstream source code files from https://github.com/mohkale/projection/tree/master/src

Ingest all these files and then Re-evaluate everything again for workflow_management.yaml file before working on the Buffer Management subsection

---

---

---

# Project Management

I use the following prompt to generate the analysis in that the attached project-management.md:

"Lets work on the Project Management subsection using the projection emacs package. Use its upstream sources include the sources for the built-in emacs packages this package depends on. Propose a plan to write the configuration from scratch. Make sure this subsection integrates with the other 2 subsections. Also make sure the configuration follows principles from the doom emacs project closely as can while integrating with my emacs configuration. Make sure you verify everything. Use the attached files: use_package.yaml, elpaca_package_manager.yaml files as guide, but anything in it can be subject to change if you determine there are issues in this yaml file. Explain everything in great detail for me to review. Propose a plan on you plan to write/rewrite the 1st iteration of the Project Management section. Follow the instructions and protocols in the system_prompt_protocol.yaml file. Do not hallucinate and do not introduce errors. You are not allowed to use any sources/resources older than 3 years and you are not allowed to use any china-base sources/resources as well. And all the information you get must be the latest till July 2026."

Re-evaluate the plan in this project-management.md file by looking at upstream source code files for both project.el, ibuffer, and projection from https://github.com/mohkale/projection/tree/master/src and rewrite your detailed plan again with verification and sources. You will need to use the attached workflow_management.yaml as guide for planning and configuring Project Management subsection as well.

---

Ingest the attached txt file that details everything about emacs 31. There are many changes in UI and other stuff that the latest iteration of the attached emacs-ide-features.html file needs to take into account. According to this file you may have to change packages and their descriptions for the IDE features. Also reformat the html file so that it matched the style, features, functionalities and interactivity of the attached lsp-mode-and-lsp-ui-reference.html file. Then you have the GREEN LIGHT to rewrite the whole emacs-ide-features.html file again

For question 1, I prefer a toggleable and collapsible left sidebar TODO. For question 2, it should tucked inside but also collapsible. For question 3, yes Green/Red Cards are preferable. For the whole template, when I am a specific main section, I want almost everything to be collapsible so that I can view some elements at a time so that it is not distracting but I would also like the whole content of a single main heading viewing also at once in a full screen with a central toggle. In the emacs_31_intell...html I like the card system of the dashboard part, the table system of the Commands and Keys part, and the code block system of the configuration part, but I am sure about the organization of the Architecture and Integration part. I also don't like how the head tile in emacs_31_inte...html is shown, it feels like it shoving its title down my throat. On the other hand for the ai_studio_code.html file,m
