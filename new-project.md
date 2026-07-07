Here is the final, definitive Standard Operating Procedure (SOP) incorporating all the exact UI constraints, the 1000-character limit workaround, the mandatory tool toggles, and the token-saving strategies.

You can copy and save this directly to your local machine as your master guide.

---

# 📘 Emacs Literate Architect: Qwen Studio Project SOP (Final)

## 1. Platform & Architecture Overview

- **Platform:** `chat.qwen.ai` (100% Free Tier).
- **Workspace Type:** **New Project** (Sidebar button).
- **Core Engine:** **Qwen3.7-Max** (or Qwen-Max).
- **Mandatory Toggles:** **Web Search** & **Deep Thinking** (Must be toggled ON in the chat UI).
- **Token Strategy:** "Silent Read Protocol" to eliminate the "Echo-Back" token trap, ensuring your free daily quota lasts through the entire multi-day project.

---

## 2. Step-by-Step Project Creation

### Step 1: Initialize the Workspace

1. Navigate to **[chat.qwen.ai](https://chat.qwen.ai)** and log in.
2. Locate the left sidebar and click on **New Project**.
3. **Project Name:** Enter `Emacs Literate Architect`.

### Step 2: Inject the "Brain" (Instructions Box)

Locate the **Instructions** text box under Advanced Settings. This box has a strict **1000-character limit**.
Copy and paste the exact text block below. It is ~955 characters and acts as a compressed "Activation Bootloader" that forces the AI to read your uploaded files and obey the core rules.

```text
ROLE: Expert Vanilla Emacs Lisp & Org Architect.

DIRECTIVE: Ingest uploaded Files (config.org.txt, early-init.el.txt, project-state.md, system-prompt-protocol.md). Parse .txt as Org/Elisp.

HANDSHAKE:
1. Read project-state.md & config.org.txt.
2. Identify next non-DONE subsection.
3. Output EXACTLY: "I have ingested the 4 source-of-truth files and the system protocol. I acknowledge the strict formatting rules, the negative constraints, and the current project state (Version [X]). The next pending subsection is [Name]. I am locked in and will write zero code until you give the signal."
4. STOP. Await signal.

RULES:
- FORMAT: Wrap output in ONE markdown `org` block. ZERO markdown bleed. Use Org syntax (=code=, *bold*).
- SILENT READ: NEVER echo old code. Output ONLY the delta + brief summary.
- TOOLS: MANDATORY Deep Thinking for use-package physics. MANDATORY Web Search for Emacs 31/APIs.
- NEVER: Doom macros, consult-projectile, minibuffer Corfu.
```

### Step 3: Upload the "Memory" (Files Box)

Locate the **Files** section under Advanced Settings. This is your Knowledge Base. Upload these exact **4 files**:

1.  `config.org.txt` _(The massive literate canvas)_
2.  `early-init.el.txt` _(The bootstrap sequence)_
3.  `project-state.md` _(The persistent state tracker)_
4.  `system-prompt-protocol.md` _(The FULL, uncompressed master protocol. The Instructions box tells the AI to read this file for deep context)._

_Note: If the UI asks for parsing settings, choose **Full Text** or **Code** to prevent RAG chunking from fracturing your Org blocks._

### Step 4: Handle the "Memory" Setting

Locate the **Memory** section under Advanced Settings.

- **Action:** Turn Auto-Memory **OFF** (or leave it default/disabled).
- **Why:** You do not want the AI automatically summarizing and compressing your Emacs config state, as it will lose the exact `DONE` keyword tracking. Your `project-state.md` file _is_ your memory.

### Step 5: Save and Configure Chat Tools

1. Save the Project and open a **New Chat** thread inside it.
2. **Model Selection:** Open the model dropdown at the top of the chat and select **Qwen3.7-Max** (or **Qwen-Max**).
3. **Enable Web Search:** Click the **Web Search** (globe icon) toggle near the prompt box to turn it **ON**.
4. **Enable Deep Thinking:** Click the **Thinking Mode** (brain/lightbulb icon) toggle to turn it **ON**. This forces the Qwen3 model to use its internal `<thought>` blocks to mathematically verify Elisp execution order before generating code.

---

## 3. The Initial Handshake (First Prompt)

Because your files are permanently attached to the project workspace, you do not need to re-upload them. Paste this exact initialization prompt to trigger the 4-File Handshake:

```text
I have uploaded the 4 source-of-truth files to the project Files, and your Instructions contain the bootloader.

Please ingest them, acknowledge the strict formatting rules, the Silent Read Protocol, and the current project state. Identify the exact next pending subsection based on the DONE keywords in config.org.txt, and wait for my signal to write code. Use your Deep Thinking process to verify the load-order physics of the upcoming section, and use Web Search if you need to verify any upstream package APIs.
```

**Expected AI Behavior:** The AI will use its thinking block to scan `project-state.md`, identify that the _Completion Framework_ is fully finalized, and pinpoint **`** Undo Fu`\** (under *Vim Emulation\*) as the next pending subsection. It will output the exact acknowledgment phrase and wait.

---

## 4. Daily Operational Workflow (Multi-Day Strategy)

Because you are on the free tier, you must manage context bloat and daily token limits using the **Checkpoint & Reset** method.

### Phase A: Active Development

- **Give the Signal:** _"Proceed with `** Undo Fu`. Remember the Silent Read Protocol, use Web Search to verify the latest undo-fu-session API, and wrap the output in a single Markdown `org` code block."_
- **Review:** The AI will output the documentation and the `#+begin_src emacs-lisp` block. Review it for Doom-macro hallucinations or Markdown bleed.

### Phase B: End of Day Checkpoint (Crucial)

When you are done for the day, or if the AI starts acting sluggish/forgetful due to context limits, issue this command:

```text
End of session. Generate an updated project-state.md, increment the version to v4, merge the current progress, and update the Finalized/Pending lists. Do not write any more code.
```

### Phase C: The Thread Reset (Next Day)

1.  Copy the newly generated `project-state.md` from the AI's output and save it locally on your computer (overwriting your old v3 file).
2.  **Close that chat thread.**
3.  Click on your **Project Name** (`Emacs Literate Architect`) in the sidebar to open a **brand new chat thread**.
4.  **Re-upload** the newly saved `project-state.md` (v4) directly into the new chat thread.
5.  Send the **Initial Handshake** prompt from Step 3 again. The AI will instantly resume exactly where you left off with zero re-explaining required.

---

## 5. Troubleshooting & Edge Cases

- **The "Echo-Back" Trap:** If the AI ever starts repeating your existing code back to you before writing new code, immediately reply: _"Stop. You are violating the Silent Read Protocol. You are burning my output tokens. Acknowledge and apply the Silent Read Protocol immediately."_
- **Context Truncation:** If the AI says _"My context window has truncated the file"_, do not argue. Simply download your latest `config.org.txt`, re-upload it to the current chat thread, and tell the AI to re-ingest it.
- **Thinking Mode Laziness:** If the AI gives a shallow answer regarding complex TRAMP or `persp-mode` edge cases, append `/think` to your prompt to force the Qwen3 model into deep reasoning mode for that specific turn.
- **Web Search Hallucinations:** If the AI searches the web but tries to implement a Doom Emacs wrapper (e.g., `+vertico/...`), remind it of the **Negative Constraints** located in the full `system-prompt-protocol.md` file it ingested: _"Translate Doom logic into native Vanilla Emacs equivalents. Never copy Doom-specific wrapper functions."_
