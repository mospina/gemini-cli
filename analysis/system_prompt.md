Yes, absolutely. The Gemini CLI has a very detailed and sophisticated initial system prompt. It's a cornerstone of the application's architecture, as it defines the agent's persona, its core operating principles, safety guidelines, and its entire methodology for interacting with the user and its tools.

This is not just a simple "You are a helpful assistant." It is a comprehensive "constitution" for the agent.

### **Location of the System Prompt**

The logic for generating this prompt is located in the `core` package, within this file:

*   **File:** `packages/core/src/core/prompts.ts`
*   **Function:** `getCoreSystemPrompt(userMemory?: string)`

### **Architectural Design and Purpose**

The design of the system prompt is noteworthy for several reasons:

1.  **Defines the Agent's Persona:** It explicitly tells the model to act as an "interactive CLI agent specializing in software engineering tasks." This immediately sets the context and tone.
2.  **Establishes Core Mandates:** It lays out non-negotiable rules for the agent, such as adhering to project conventions, never assuming libraries are available, and mimicking existing code style. This is crucial for making the agent a safe and useful coding partner.
3.  **Prescribes Workflows:** It defines specific, step-by-step processes for common tasks like "Software Engineering Tasks" (Understand -> Plan -> Implement -> Verify) and creating "New Applications". This structured approach guides the model's reasoning and prevents it from taking random, unhelpful actions.
4.  **Provides Operational Guidelines:** It covers the "soft skills" of the agent, such as being concise, using Markdown correctly, and handling situations where it cannot fulfill a request.
5.  **It is Dynamically Generated:** This is a key architectural feature. The prompt is not a single static string. The `getCoreSystemPrompt` function dynamically constructs the prompt based on the user's current environment.

### **Key Components of the System Prompt**

The prompt is built from several key sections:

*   **Core Mandates:** The fundamental principles the agent must follow (e.g., "Conventions," "Libraries/Frameworks," "Style & Structure").
*   **Primary Workflows:** The step-by-step guides for complex tasks.
*   **Operational Guidelines:** Rules for tone, style, and security (e.g., "Explain Critical Commands," "Use absolute paths").
*   **Tool Usage Instructions:** Specific guidance on when and how to use key tools like `save_memory` (`MemoryTool.Name`) and `run_shell_command` (`ShellTool.Name`).
*   **Examples:** A list of few-shot examples that demonstrate the desired terse, tool-first interaction style.
*   **Dynamic Sections:** The function programmatically adds sections based on runtime context:
    *   **Sandbox Status:** It checks `process.env['SANDBOX']` and injects specific instructions for operating within a generic sandbox, a macOS Seatbelt sandbox, or directly on the user's system.
    *   **Git Repository Status:** It calls `isGitRepository()` and, if true, appends a detailed section on how to interact with Git repositories (e.g., using `git status`, `git diff`, and matching commit message styles).

### **Extensibility and Customization**

The system prompt's design also allows for user-level customization, which is a powerful feature:

1.  **User Memory:** The `getCoreSystemPrompt` function accepts an optional `userMemory` string. If provided, this string is appended to the end of the base prompt, separated by a `---` line. This is the primary way for a user to add their own persistent instructions or context to the agent.

2.  **Environment Variable Override (`GEMINI_SYSTEM_MD`):** The code in `prompts.ts` contains a mechanism to **completely override** the built-in prompt. If the environment variable `GEMINI_SYSTEM_MD` is set to a file path, the CLI will read and use the content of that file as its system prompt instead of the default one. This allows advanced users to completely redefine the agent's behavior.

3.  **Prompt Writing (`GEMINI_WRITE_SYSTEM_MD`):** To aid in customization, if the `GEMINI_WRITE_SYSTEM_MD` environment variable is set to a file path, the CLI will write its *default* system prompt to that file and then exit. This gives users a perfect template to start from if they wish to create their own custom prompt.

In summary, the initial system prompt is a central and dynamically constructed architectural component that acts as the agent's core programming. It is far more than a simple instruction; it is a detailed set of rules, workflows, and safety constraints that guide every action the Gemini CLI takes.
