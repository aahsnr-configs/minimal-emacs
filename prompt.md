- Ingest the 5 attached files and make sure you have read everything to the letter and follow all the instructions to the letter.

- Now lets work on the Misc subsection. Review this section thoroughly in great detail. Then fix any errors and issues. But do not rewrite the Misc subsection yet under any circumstances. Explain everything in great detail for me to review.

- Now lets work on the subsection. Search the web, think for longer and write a new configuration for expreg and treesit-expreg from scratch. First determine if these packages are needed together and if they work together. But do not write any config for this subsection yet. Present me a plan on how you want to approach in writing this configuration. Explain everything in great detail for me to review.

- Okay then you have the signal to rewrite the Session Management subsection. Follow the protocols and instructions from system-prompt-protocols.md file and make sure the documentation is very concise as well.

- Now review the next subsection, optimize it and rewrite it. Follow the protocols and instructions from the system-prompt-protocol.md file and make sure the documentation is concise. Also determine if there are additional configurations that might be useful

- Are you certain there are no more errors and issues? And are there any additional configuration settings that you think might be useful? Also the documentation must be more concise

You did not follow my fucking instructions from my previous prompt at all. Did you even fucking read it? Answer me honestly. You were supposed to rewrite the whole system-prompt-protocol.md file in its fullest. It is not a universal fact that all built-in packages that need to have `(mode 1)` in the :config. It is based on a built-in package per basis. You have to fucking search the web and then determine whether `(mode 1)` belongs in :init block or :config block. It is not fucking upto you to decide unilaterally that all `(mode 1)` belong in :config block. Since you did not follow my instructions, I have lost my fucking trust in your ability to fucking think or even read. You must rewrite everything again from the previous prompt. You must fucking rewrite all org blocks that previously had `:hook(elpaca-after-init` before you latest rewrite. You must search the web and determine if these built-in packages need (mode 1) in :config or :init. Then rewrite all those org blocks again. The documentation for show-paren-mode must be more brief than the iteration before the latest iteration and rewrite that org block as well. Do I make myself clear? Do you understand my fucking instructions and have them fucking hammered into your head? Answer these question about the instructions before I can allow you to rewrite anything.

Now lets work on the Evil Textobj Tree-Sitter subsection. Write a comprehensive configuration from scratch. Use the following sources for inspiration to write this configuration:

- https://raw.githubusercontent.com/meain/evil-textobj-tree-sitter/refs/heads/master/README.md

- https://raw.githubusercontent.com/doomemacs/modules/refs/heads/main/modules/tools/tree-sitter/config.el

Stop the task immediately if you fail to retrieve these links using your web_extractor tool. For the second link
`evil-textobj-tree-sitter` config has been commented out but use it nevertheless for information.

Make sure this configuration integrates well with the rest of the emacs configuration. Then present to me your proposal on how you plan to proceed with this org block configuration. But do not rewrite this subsection yet under any circumstances. Explain everything in great detail for me to review.

The general keybindings section already uses :after evil. So using with-eval-after-load evil is redundant. Fix that and rewrite the org block again. Do not add with-eval-after-load gen eral either since I plan to relocate general use-package block. And I will also add :ensure(:wait t) to general.el use-package and then fragment the general keybindings section. So general-define-key, ar/global-leader and ar/local-leader can work outside general.el use-package config. In addition to rewriting Treesit Navigate Thing org block, also confirm and verify my general.el configuration strategy. I will need you to decide where to place the following initial general.el use-package org block

```el
(use-package general
  :ensure (:wait t)
  :demand t
  :after evil
  :config
  ;; Set up leader keys
  (general-create-definer ar/global-leader
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer ar/local-leader
    :states '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m"))
```

Then the rest of the keybindings blocks can be placed where they are needed. Search the web for all these tasks,
