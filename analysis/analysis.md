### **Project Pierre: Gemini CLI Architectural Analysis & Haskell Rewrite Blueprint**

**Objective:** To conduct a thorough design and architectural analysis of the Gemini CLI TypeScript codebase. The final output of this analysis will be a set of architectural documents and models sufficient to serve as a blueprint for a complete, functional rewrite of the application in Haskell.

**Guiding Principles:**

1.  **Top-Down Approach:** We will start with the macro-architecture (packages, entry points) and progressively drill down into specific components and modules.
2.  **Focus on Abstractions & Interfaces:** We will prioritize understanding the core data structures, interfaces, and communication protocols over implementation minutiae. This is key for a cross-paradigm rewrite.
3.  **Trace the Data Flow:** We will follow the lifecycle of a typical user command to understand how different components interact, how state is managed, and where side effects occur.
4.  **Artifact-Driven:** Each phase will produce specific documents or diagrams (conceptual, of course) that will form the final blueprint for the Haskell rewrite.

---

### **Analysis Plan: Phased Approach**

#### **Phase 1: High-Level Reconnaissance (The "Map Making" Phase)**

This initial phase focuses on understanding the overall structure and the primary responsibilities of each major part of the application, based on the provided directory structure.

*   **Objective:** Create a high-level component diagram and define the role of each package.
*   **Key Files/Modules to Inspect:** `structure.md`, top-level `package.json` files in `packages/cli`, `packages/core`, and `packages/vscode-ide-companion`.
*   **Questions to Answer:**
    1.  **Monorepo Strategy:** What is the clear separation of concerns between `packages/cli`, `packages/core`, and `packages/vscode-ide-companion`? We will hypothesize their roles (e.g., `core` = engine, `cli` = user interface, `vscode-ide-companion` = IDE integration).
    2.  **Entry Points:** Where does the application execution begin? (Identified as `packages/cli/index.ts`). How does it bootstrap the main application?
    3.  **Key Dependencies:** What are the primary external libraries in use (e.g., `@google/genai`, `react`, `ink`, `yargs`, `msw`)? This will inform our understanding of I/O, UI rendering, and API communication.
*   **Artifact:** A high-level C4-style "Context" or "Container" diagram showing the relationship between the packages and external systems (Gemini API, File System, User Terminal).

#### **Phase 2: Core Functionality & Data Flow Analysis (The "Tracer Bullet" Phase)**

This phase traces the journey of a single user command through the system to understand the primary data flows and component interactions.

*   **Objective:** Model the lifecycle of a command, from user input to rendered output.
*   **Key Files/Modules to Inspect:** `packages/cli/src/gemini.ts`, `packages/cli/src/ui/App.tsx`, `packages/cli/src/ui/hooks/useGeminiStream.ts`, `packages/core/src/core/client.ts`, `packages/core/src/core/turn.ts`.
*   **Questions to Answer:**
    1.  **The Main Loop:** How is the interactive session managed? Where is the primary state loop that waits for input, processes it, and renders output? (Likely within `App.tsx` and its custom hooks).
    2.  **Data Models:** What are the core data structures that represent the conversation? We will deeply analyze the types defined in `packages/core` and `packages/cli/src/ui/types.ts` (e.g., `Content`, `Part`, `Turn`, `HistoryItem`). This is critical for defining our Haskell Algebraic Data Types (ADTs).
    3.  **API Interaction:** How is the connection to the Gemini API abstracted? We will analyze `GeminiClient` and `GeminiChat` to understand how requests are constructed and how streaming responses are handled.
*   **Artifact:** A sequence diagram illustrating the "happy path" of a user prompt that invokes a tool and receives a streaming text response.

#### **Phase 3: Deep Dive into Architectural Components**

This is the most intensive phase, where we dissect each major component identified in Phase 1 to understand its internal design.

*   **Objective:** Document the design, responsibilities, and key abstractions of each major subsystem.
*   **Sub-Phases & Key Files:**

    1.  **Configuration Management (`packages/cli/src/config`)**
        *   **Objective:** Understand how settings are loaded, merged, and accessed.
        *   **Files:** `config.ts`, `settings.ts`, `settingsSchema.ts`, `auth.ts`.
        *   **Questions:** What is the hierarchy of configuration (system, user, workspace)? How is authentication state managed for different providers? This will inform the `ReaderT` or configuration record in our Haskell app.

    2.  **UI Layer (`packages/cli/src/ui`)**
        *   **Objective:** Deconstruct the UI into logical components and state management patterns.
        *   **Files:** `App.tsx`, `components/`, `hooks/`, `contexts/`.
        *   **Questions:** What is the component hierarchy? How are custom hooks (`useKeypress`, `useHistoryManager`, `useGeminiStream`) used to manage complex state and side effects? How are React Contexts used for state propagation? The logic here needs to be extracted from the presentation for the Haskell rewrite.

    3.  **Command & Service Architecture (`packages/cli/src/commands`, `packages/cli/src/services`)**
        *   **Objective:** Understand the command discovery and dispatch mechanism.
        *   **Files:** `CommandService.ts`, `BuiltinCommandLoader.ts`, `FileCommandLoader.ts`, `mcp.ts`.
        *   **Questions:** How does the application implement the Command pattern? How are commands discovered from different sources (built-in, files)? How are conflicts resolved? This will inform a type-safe command ADT in Haskell.

    4.  **Tooling Abstraction (`packages/core/src/tools`)**
        *   **Objective:** Define the interface for tools and how they are managed.
        *   **Files:** `tools.ts`, `tool-registry.ts`, `shell.ts`, `edit.ts`.
        *   **Questions:** What is the base abstraction for a `Tool`? How does the `ToolRegistry` work? How do tools declare their parameters and handle execution? How are confirmations and side effects (like file I/O in `EditTool`) handled? This is crucial for modeling effects in Haskell.

#### **Phase 4: Synthesis & Haskell Rewrite Blueprint**

This final phase consolidates our findings into a concrete plan for the Haskell implementation. We will not write Haskell code, but we will define the "shape" of it.

*   **Objective:** Produce the foundational architectural artifacts for the Haskell rewrite.
*   **Tasks:**
    1.  **Define Core Data Types (ADTs):** Based on the analysis in Phase 2 & 3, we will define the Haskell ADTs for key concepts:
        *   `data Command = Help | Clear | Quit | ...`
        *   `data AppEvent = UserInput Text | ApiResponse Chunk | ToolCallRequest Tool ...`
        *   `data AppState = AppState { history :: [HistoryItem], ... }`
        *   `data Tool = Tool { name :: Text, schema :: Aeson.Value, execute :: Value -> IO ToolResult }`

    2.  **Map Components to Haskell Modules:** Propose a module structure for the Haskell project (e.g., `Gemini.CLI.UI`, `Gemini.CLI.Core`, `Gemini.CLI.Tools`, `Gemini.CLI.Config`).

    3.  **Model Side Effects (The `IO` Boundary):** List all points of interaction with the outside world (network requests to Gemini API, file system I/O, reading user input, writing to terminal). This will define the boundary of our `IO` monad and guide the separation of pure and impure code.

    4.  **Define the Main Application Loop:** Describe the main loop in functional terms. It will be a function with a signature akin to:
        `appLoop :: AppConfig -> AppState -> IO AppState`. We will outline the steps within this loop: wait for input, parse event, update state, perform effects, render output.

*   **Artifact:** A "Haskell Architectural Blueprint" document containing the outputs from the tasks above.
### **Phase 1 Artifact: High-Level Architectural Overview**

This document outlines the macro-architecture of the Gemini CLI, defining the roles of its primary components and their relationships with each other and external systems.

#### **1. Component Breakdown & Responsibilities**

The Gemini CLI monorepo is logically divided into three distinct, high-level components (packages), each with a clear separation of concerns:

*   `packages/core`: **The Engine/Headless Library.** This package contains the core business logic and is designed to be environment-agnostic.
    *   **Responsibilities:**
        *   **API Abstraction:** Encapsulates all direct communication with the Google Gemini API and authentication services (`code_assist`, `mcp`, `google-auth-library`). It provides a clean interface (`GeminiClient`, `GeminiChat`) to the rest of the application.
        *   **Tooling Framework:** Defines the abstractions for "tools" (`tools/tools.ts`), manages their discovery and registration (`tools/tool-registry.ts`), and provides implementations for core functionalities (e.g., `edit.ts`, `shell.ts`).
        *   **Service Layer:** Provides services that abstract away complex functionalities like file system discovery, Git interactions, and chat recording (`services/`).
        *   **Data Models:** Defines the primary data structures used throughout the application (e.g., `Content`, `Turn`, `ToolResult`).

*   `packages/cli`: **The User Interface & Application Host.** This is the primary user-facing executable. It acts as the host application that consumes `packages/core` and renders the interactive terminal experience.
    *   **Responsibilities:**
        *   **Application Entry Point:** Contains the main executable entry point (`index.ts`) that bootstraps the entire application.
        *   **UI Rendering:** Manages the entire terminal user interface using the Ink library (inferred from `.tsx` files and React dependencies). This includes rendering history, prompts, spinners, and dialogs (`ui/`).
        *   **State Management:** Holds the UI state, primarily through React hooks (`ui/hooks/`). This includes managing the conversation history, user input, and streaming state.
        *   **Command Parsing & Dispatch:** Handles user input, parses slash commands (`/help`, `/clear`), and dispatches them to the appropriate services or actions (`services/CommandService.ts`).
        *   **Configuration Loading:** Responsible for loading and merging configuration from various sources (e.g., user settings, project settings) (`config/`).

*   `packages/vscode-ide-companion`: **The IDE Integration Server.** This is a VS Code extension that runs within the IDE. It provides an MCP (Model Context Protocol) server that the CLI can connect to for context-aware operations.
    *   **Responsibilities:**
        *   **IDE Server:** Implements an MCP server (`ide-server.ts`) that exposes IDE-specific functionalities to the CLI.
        *   **Context Provider:** Gathers context from the IDE, such as the list of open files, the active file, and selected text (`open-files-manager.ts`).
        *   **Diff View Management:** Manages the creation and state of visual diff views within the IDE (`diff-manager.ts`), allowing the CLI to request that the user review changes in their native editor.

#### **2. Key External Dependencies**

The `package.json` files reveal a modern TypeScript stack that informs the architecture:

*   **`@google/genai` & `google-auth-library`:** Confirms that `packages/core` directly handles the Gemini API communication and authentication.
*   **`react` & `ink`:** Confirms that `packages/cli` uses a declarative, component-based model for its terminal UI, which is a sophisticated choice over simple stdout printing.
*   **`yargs`:** Used in `packages/cli` for initial command-line argument parsing before the interactive UI takes over.
*   **`@modelcontextprotocol/sdk`:** This is a key dependency in `packages/vscode-ide-companion` and `packages/core`, confirming the use of the Model Context Protocol for communication between the CLI and the IDE extension.
*   **`vitest` & `msw`:** A modern testing stack, indicating a commitment to unit and integration testing with a focus on mocking network requests.

#### **3. High-Level System Context Diagram (Conceptual)**

This diagram illustrates the interactions between the components and external systems.

```
+---------------------------+       +------------------------+
|      User's Terminal      |------>|      packages/cli      |
+---------------------------+       | (UI & Application Host)|
                                    +-----------+------------+
                                                |
                                                | (Consumes)
                                                v
+---------------------------+       +-----------+------------+
|        File System        |<----->|      packages/core     |
+---------------------------+       |   (Engine & Library)   |
                                    +-----------+------------+
                                                |        ^
                                                |        | (Communicates via MCP)
                                                v        |
+---------------------------+       +-----------+--------+---+      +-------------------------+
|        Google Cloud       |<----->|      Gemini API      |      | packages/vscode-ide-companion |
| (Authentication, Gemini)  |       +------------------------+      |   (IDE Integration)   |
+---------------------------+                                       +-------------------------+
                                                                                ^
                                                                                | (Runs within)
                                                                                |
                                                                    +-----------+------------+
                                                                    |   VS Code / Zed IDE    |
                                                                    +------------------------+
```

#### **Conclusion of Phase 1**

The Gemini CLI employs a well-structured, multi-package architecture that clearly separates its core logic from its user-facing and integration-specific components. This design is highly modular, allowing the `core` engine to be potentially reused in other contexts beyond the interactive terminal. The use of React/Ink for the CLI and a dedicated MCP server for IDE integration points to a sophisticated and feature-rich application design.

This high-level understanding provides a solid foundation for the next phase.
### **Phase 2 Artifact: Core Functionality & Data Flow Analysis**

#### **1. The Lifecycle of a User Command**

The interactive Gemini CLI is a stateful application that manages a conversation loop. The journey of a single user prompt can be broken down into the following key stages:

1.  **Input & Initial Dispatch:** The user types a prompt into the `InputPrompt` component (`packages/cli/src/ui/components/InputPrompt.tsx`). Upon submission, this input is passed to the `submitQuery` function within the central `useGeminiStream` hook (`packages/cli/src/ui/hooks/useGeminiStream.ts`).

2.  **Pre-processing & Routing:** The `submitQuery` function acts as the main orchestrator. It first inspects the input:
    *   If the input is a **slash command** (e.g., `/help`), it is handled by the `useSlashCommandProcessor` hook. This is a local action that does not involve the Gemini API.
    *   If the input is a **shell command** (in shell mode), it is handled by the `useShellCommandProcessor` hook.
    *   If the input is a **natural language prompt**, it is prepared for the Gemini API. The prompt is added to the UI history via `useHistoryManager`.

3.  **API Abstraction Layer (`packages/core`):** The prompt is passed from the UI layer (`packages/cli`) to the core logic layer.
    *   `GeminiClient.sendMessageStream` is called. This client acts as the primary public interface for the core engine.
    *   The `GeminiClient` creates a `Turn` object (`packages/core/src/core/turn.ts`) to manage the state of this specific interaction (one prompt and its corresponding responses).
    *   The `Turn.run()` method is called, which in turn calls `this.chat.sendMessageStream` (from the underlying `@google/genai` SDK) to initiate the streaming API request.

4.  **Streaming Response Handling:** The `Turn.run()` method is an async generator that yields events as they are received from the Gemini API. The `useGeminiStream` hook consumes these events:
    *   `Content` event: A chunk of text arrives. The hook updates the pending message in the UI, giving the appearance of streaming text.
    *   `ToolCallRequest` event: The model requests to execute a tool. The `useGeminiStream` hook receives this and passes the tool call details to the `useReactToolScheduler` hook.

5.  **Tool Execution Lifecycle:**
    *   The `useReactToolScheduler` hook (`packages/cli/src/ui/hooks/useReactToolScheduler.ts`) receives the tool call request and uses the `CoreToolScheduler` (`packages/core/src/core/coreToolScheduler.ts`) to manage it.
    *   The `CoreToolScheduler` finds the requested tool (e.g., `ShellTool`) in the `ToolRegistry`.
    *   It calls the tool's `shouldConfirmExecute` method. This is a critical security step. For dangerous tools like `shell` or `edit`, this triggers a confirmation dialog in the UI.
    *   Assuming the user confirms, the tool's `execute` method is called. The tool performs its side effect (e.g., runs a shell command, reads a file).
    *   The tool returns a `ToolResult` object.

6.  **Returning Tool Output to the Model:**
    *   The `CoreToolScheduler` converts the `ToolResult` into a `functionResponse` part, a specific data structure the Gemini API understands.
    *   This `functionResponse` is passed back up to the `useGeminiStream` hook.
    *   The hook calls `submitQuery` **again**, but this time the "prompt" is the `functionResponse` part. This closes the loop, sending the tool's output back to the model for the next step of its reasoning process.

7.  **Final Text Response:** The flow repeats from Step 3. The Gemini API receives the tool's output, processes it, and generates a final textual response, which is streamed back to the user via `Content` events.

#### **2. Core Data Models**

This data flow relies on a set of well-defined data structures. Understanding these is key to modeling the application in Haskell's type system.

*   **`Content` (`@google/genai`):** The fundamental unit of conversation history. It's a record containing a `role` (`'user'` or `'model'`) and an array of `Part`s. This directly maps to the Gemini API's representation.

*   **`Part` (`@google/genai`):** A segment of a `Content` object. It's a discriminated union that can be text, a `functionCall`, a `functionResponse`, or other data like `inlineData`.

*   **`HistoryItem` (`packages/cli/src/ui/types.ts`):** The UI's representation of a single item in the displayed history. It's a rich discriminated union that includes types like `user`, `gemini`, `tool_group`, `error`, etc. This is distinct from the API's `Content` model and is tailored for rendering.

*   **`Turn` (`packages/core/src/core/turn.ts`):** An object that encapsulates the state of a single back-and-forth interaction. It manages the streaming API call and yields structured `ServerGeminiStreamEvent`s.

*   **`Tool` Abstractions (`packages/core/src/tools/tools.ts`):**
    *   **`DeclarativeTool`:** The base class for all tools. It defines the tool's `name`, `description`, and `parameterSchema`.
    *   **`ToolInvocation`:** An object created by a tool's `build` method. It represents a *specific, validated call* to that tool with a given set of arguments. It contains the `execute` logic. This separation of tool definition from its invocation is a key architectural pattern.

#### **3. Sequence Diagram: User Prompt with Tool Call**

This diagram illustrates the flow described above.

```mermaid
sequenceDiagram
    participant User
    participant CLI UI (React/Ink)
    participant useGeminiStream (Hook)
    participant GeminiClient (Core)
    participant Turn (Core)
    participant ToolScheduler (Core)
    participant ShellTool (Core)
    participant GeminiAPI

    User->>+CLI UI (React/Ink): Types "list the files" and hits Enter
    CLI UI (React/Ink)->>+useGeminiStream (Hook): submitQuery("list the files")
    useGeminiStream (Hook)->>+GeminiClient (Core): sendMessageStream("list the files")
    GeminiClient (Core)->>+Turn (Core): new Turn()
    Turn (Core)->>+GeminiAPI: sendMessageStream (Streaming Request)
    activate GeminiAPI
    GeminiAPI-->>-Turn (Core): Stream Chunk 1 (ToolCallRequest: list_directory)
    Turn (Core)-->>-useGeminiStream (Hook): Yields ToolCallRequest Event
    useGeminiStream (Hook)->>+ToolScheduler (Core): scheduleToolCalls(["list_directory"])
    ToolScheduler (Core)->>+ShellTool (Core): build({path: "."}) -> invocation
    ShellTool (Core)-->>-ToolScheduler (Core): Returns Invocation
    ToolScheduler (Core)->>ShellTool (Core): invocation.shouldConfirmExecute()
    ShellTool (Core)-->>-ToolScheduler (Core): Returns `false` (no confirmation needed)
    ToolScheduler (Core)->>ShellTool (Core): invocation.execute()
    activate ShellTool (Core)
    ShellTool (Core)->>ShellTool (Core): Runs `ls` command
    ShellTool (Core)-->>-ToolScheduler (Core): Returns ToolResult (file list)
    deactivate ShellTool (Core)
    ToolScheduler (Core)-->>-useGeminiStream (Hook): onComplete(ToolResult)
    deactivate ToolScheduler (Core)
    useGeminiStream (Hook)->>useGeminiStream (Hook): Converts ToolResult to functionResponse
    useGeminiStream (Hook)->>+GeminiClient (Core): submitQuery(functionResponse)
    GeminiClient (Core)->>+Turn (Core): new Turn()
    Turn (Core)->>+GeminiAPI: sendMessageStream (with functionResponse)
    GeminiAPI-->>-Turn (Core): Stream Chunk 2 (Content: "Here are the files: ...")
    Turn (Core)-->>-useGeminiStream (Hook): Yields Content Event
    deactivate Turn (Core)
    useGeminiStream (Hook)-->>-CLI UI (React/Ink): Updates UI with streaming text
    deactivate useGeminiStream (Hook)
    GeminiAPI-->>-GeminiClient (Core): Stream End
    deactivate GeminiAPI
    deactivate GeminiClient (Core)
    CLI UI (React/Ink)-->>User: Displays final file list
    deactivate CLI UI (React/Ink)
```

#### **Conclusion of Phase 2**

The Gemini CLI's core is a well-defined, event-driven state machine orchestrated by the `useGeminiStream` hook. It clearly separates concerns: the `core` package handles the "what" (API logic, tool definitions), while the `cli` package handles the "how" (UI rendering, state management, user interaction). The use of async generators for streaming and a dedicated scheduler for tool calls are robust patterns for managing the asynchronous nature of an agentic application.

The data models are clear and provide a strong basis for defining ADTs in Haskell. The next step is to dissect the individual components in more detail.

I am now ready to proceed with **Phase 3: Deep Dive into Architectural Components**. Please provide the go-ahead.
### **Phase 3 Artifact: Architectural Component Deep Dive**

#### **1. Configuration Management**

*   **Objective:** Understand how application settings are loaded, merged from different sources, and made available to the rest of the application.
*   **Key Files Inspected:** `packages/cli/src/config/config.ts`, `settings.ts`, `settingsSchema.ts`, `auth.ts`, `extension.ts`, `trustedFolders.ts`.

*   **Architectural Analysis:**
    *   **Layered Configuration:** The application employs a clear hierarchical configuration system. Settings are loaded from up to three sources and merged with a specific precedence: **System-wide > User-level (`~/.gemini/`) > Workspace-level (`./.gemini/`)**. The `loadSettings` function in `settings.ts` is the orchestrator for this process. This layered approach allows for global defaults, user preferences, and project-specific overrides, which is a robust pattern for a developer tool.
    *   **Schema-Driven:** The `settingsSchema.ts` file acts as a single source of truth for all possible configuration options. It defines the type, label, category, default value, and whether a setting requires an application restart. This is a strong design choice that enables features like automatic settings validation and could be used to auto-generate a settings UI.
    *   **Authentication State:** Authentication is managed as a distinct part of the configuration. `auth.ts` contains logic to validate the selected authentication method (`AuthType`) against the presence of necessary environment variables (e.g., `GEMINI_API_KEY`). The `Config` object in `packages/core` holds the final, resolved authentication details which are used to instantiate the `GeminiClient`.
    *   **Extensibility:** The configuration system is extensible. It merges `mcpServers` and `customThemes` from different scopes, and the `extension.ts` loader contributes its own settings, demonstrating a modular design.

*   **Haskell Rewrite Implications:**
    *   This layered configuration model maps perfectly to a **`Monoid`** or **`Semigroup`** in Haskell. We can define a `Settings` record type and a `mappend` (`<>`) operation where `workspace <> user` ensures that fields present in `workspace` take precedence over `user`.
    *   The entire configuration can be managed within an `AppConfig` record that is passed through the application, likely using a `ReaderT AppConfig IO` monad transformer to provide read-only access to configuration throughout the call stack.
    *   The `settingsSchema.ts` provides a clear blueprint for the Haskell `Settings` data type. Libraries like `aeson` can be used to derive `FromJSON` instances for parsing the settings files.
    *   Authentication state can be represented by an Algebraic Data Type (ADT), such as `data AuthProvider = ApiKey Text | GoogleAuth OAuthToken | VertexAI`, which would be a field within the `AppConfig`.

#### **2. UI Layer**

*   **Objective:** Deconstruct the UI into its logical components, state management patterns, and side-effect handling mechanisms.
*   **Key Files Inspected:** `packages/cli/src/ui/App.tsx`, the `hooks/` directory, the `components/` directory, and the `contexts/` directory.

*   **Architectural Analysis:**
    *   **Declarative Component Model:** The UI is built with React and the Ink library, using a declarative, component-based architecture. `App.tsx` serves as the root component, composing various sub-components (`HistoryItemDisplay`, `InputPrompt`, `Footer`, etc.).
    *   **Centralized Logic in Hooks:** The core of the application's interactive logic is encapsulated within custom React hooks. This is a key architectural pattern:
        *   `useGeminiStream`: This is the application's "main loop." It orchestrates the entire lifecycle of a user prompt, from submission to handling the streaming response, processing tool calls, and managing the `StreamingState`.
        *   `useHistoryManager`: Manages the array of `HistoryItem`s, which is the state for the rendered conversation history.
        *   `useKeypress` & `useInputHistory`: Handle low-level terminal input events and provide command-line history (up/down arrows).
        *   `useSlashCommandProcessor` & `useAtCommandProcessor`: These hooks are specialized state machines that are activated when the user input matches their respective patterns (`/` or `@`), managing the command-specific UI and logic, like providing completion suggestions.
    *   **Global State Propagation:** The `contexts/` directory reveals the use of React Context for propagating global state without "prop drilling." `SessionContext` holds session-wide statistics, while `SettingsContext` makes application settings available to all components.

*   **Haskell Rewrite Implications:**
    *   The largest paradigm shift will be here. We cannot use React hooks. The stateful, interactive nature of the UI will be managed within a state monad, likely a stack like **`StateT AppState IO ()`**.
    *   The logic contained within the various React hooks must be extracted into more portable functions. For example, the core logic of `useGeminiStream` would become a function like `processPrompt :: Text -> StateT AppState IO ()` that modifies the `AppState` and performs `IO` actions.
    *   UI rendering would be handled by a dedicated Haskell TUI library like `brick`. The application's main loop would be responsible for catching input events, passing them to our stateful logic function, and then re-rendering the UI based on the new `AppState`.
    *   The `AppState` record in Haskell will contain the fields managed by hooks and contexts in the TypeScript version (e.g., `history :: [HistoryItem]`, `currentInput :: Text`, `streamingState :: StreamingState`).

#### **3. Command & Service Architecture**

*   **Objective:** Understand how slash commands are discovered, loaded, and dispatched.
*   **Key Files Inspected:** `packages/cli/src/services/CommandService.ts`, `BuiltinCommandLoader.ts`, `FileCommandLoader.ts`, `McpPromptLoader.ts`.

*   **Architectural Analysis:**
    *   **Strategy/Provider Pattern:** `CommandService` is the central orchestrator that uses multiple "loader" strategies, each conforming to the `ICommandLoader` interface. This is a clean and extensible design.
    *   **Asynchronous Discovery:** `CommandService.create` uses `Promise.allSettled` to load commands from all sources (built-in, files, MCP) in parallel. This is efficient, especially if file or network I/O is involved.
    *   **Conflict Resolution:** The service intentionally loads commands in a specific order (`McpPromptLoader`, then `BuiltinCommandLoader`, then `FileCommandLoader`). It then merges them into a `Map`, where later additions with the same name overwrite earlier ones. This provides a clear precedence rule: **File > Built-in > MCP**. For extension commands, it has special logic to rename them to avoid conflicts.

*   **Haskell Rewrite Implications:**
    *   This architecture translates very cleanly to Haskell. We can define a type alias for a loader: `type CommandLoader = IO [Command]`.
    *   The `CommandService` can be a function `loadAllCommands :: [CommandLoader] -> IO (Map Text Command)` that executes the loaders and folds the results into a `Map`, applying the same precedence rules.
    *   Slash commands can be modeled with an ADT: `data Command = Help | Clear | Quit | Custom { ... }`. This allows the dispatcher to be implemented with type-safe pattern matching.

#### **4. Tooling Abstraction**

*   **Objective:** Define the interface for tools and understand how they are registered, validated, and executed.
*   **Key Files Inspected:** `packages/core/src/tools/tools.ts`, `tool-registry.ts`, and specific tool implementations like `shell.ts` and `edit.ts`.

*   **Architectural Analysis:**
    *   **Separation of Definition and Invocation:** The architecture makes a powerful distinction between a `DeclarativeTool` and a `ToolInvocation`. The `DeclarativeTool` is a factory that defines the tool's schema and description. The `build()` method of this factory takes user-provided arguments, validates them, and produces a `ToolInvocation` object. This invocation object encapsulates the specific, validated parameters and the `execute` logic for that single call. This is an excellent pattern that promotes safety and clarity.
    *   **Multi-Level Validation:** Parameters are validated first against a JSON schema (`validateToolParams`) and then with custom business logic (`validateToolParamValues`), providing a robust two-tier validation system.
    *   **Confirmation Hooks:** The `shouldConfirmExecute` method on the `ToolInvocation` is a critical security and UX feature. It decouples the decision to execute from the execution itself, allowing the UI layer to intercept and ask for user permission. This is essential for tools with significant side effects.
    *   **Tool Registry:** The `ToolRegistry` acts as a central repository for all available tools, whether they are built-in, discovered from a command, or provided by an MCP server.

*   **Haskell Rewrite Implications:**
    *   This pattern can be modeled elegantly using Haskell's type system. We can use a **type class** or, more simply, a **record of functions** to define the `Tool` interface.
    *   A possible definition:
        ```haskell
        data Tool = Tool
          { tName        :: Text
          , tDescription :: Text
          , tSchema      :: Aeson.Value
          , tBuild       :: Aeson.Value -> Either ToolError ToolInvocation
          }

        data ToolInvocation = ToolInvocation
          { tiExecute      :: IO ToolResult
          , tiShouldConfirm :: IO (Maybe ConfirmationDetails)
          }
        ```
    *   All side effects (like file I/O or running shell commands) are naturally contained within the `IO` monad in the `tiExecute` and `tiShouldConfirm` functions, cleanly separating pure and impure logic.
    *   The `ToolRegistry` simply becomes a `Map Text Tool`.

---

#### **Conclusion of Phase 3**

The deep dive reveals a mature and robust architecture. Key patterns include layered configuration, a declarative UI with logic centralized in hooks, a provider-based service model for commands, and a clear separation between tool definition and tool invocation.

These patterns, while implemented in TypeScript, have clear analogues in a functional paradigm. The next step is to synthesize these findings into a concrete blueprint for the Haskell implementation.
### **Phase 4 Artifact: Haskell Architectural Blueprint**

#### **1. Core Data Types (Algebraic Data Types - ADTs)**

The foundation of the Haskell application will be a set of strong, descriptive types that model the application's domain.

```haskell
-- File: src/Gemini/CLI/Types.hs

module Gemini.CLI.Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Aeson as Aeson

-- Represents the overall application state, managed by a State monad.
data AppState = AppState
  { appHistory         :: [HistoryItem]
  , appCurrentInput    :: T.Text
  , appStreamingState  :: StreamingState
  , appSessionStats    :: SessionStats
  , appConfig          :: AppConfig
  , appIsQuitting      :: Bool
  -- ... other UI-specific state fields
  }

-- Represents a single item displayed in the UI history.
data HistoryItem
  = UserMessage T.Text
  | GeminiMessage T.Text
  | ToolCallGroup [ToolCallDisplay]
  | ErrorMessage T.Text
  | InfoMessage T.Text
  -- ... other constructors for /help, /about, etc.
  deriving (Show, Eq)

-- Represents the state of the model's response stream.
data StreamingState = Idle | Responding | WaitingForConfirmation
  deriving (Show, Eq)

-- A display-focused representation of a tool call.
data ToolCallDisplay = ToolCallDisplay
  { tcName   :: T.Text
  , tcArgs   :: Aeson.Value
  , tcStatus :: ToolCallStatus
  , tcResult :: Maybe T.Text
  } deriving (Show, Eq)

data ToolCallStatus = TExecuting | TSuccess | TError
  deriving (Show, Eq)

-- The application's read-only configuration.
data AppConfig = AppConfig
  { cfgAuthDetails :: AuthDetails
  , cfgModel       :: T.Text
  , cfgTools       :: ToolRegistry
  , cfgSettings    :: MergedSettings
  -- ... other configuration fields
  }

-- Represents the different ways the application can be authenticated.
data AuthDetails
  = AuthApiKey T.Text
  | AuthGoogleOAuth -- (Potentially holding a token)
  | AuthVertexAI    -- (Potentially holding project info)
  | AuthNotSet

-- The central repository for all available tools.
type ToolRegistry = M.Map T.Text Tool

-- Represents a tool's definition and its execution logic.
data Tool = Tool
  { toolName        :: T.Text
  , toolDescription :: T.Text
  , toolSchema      :: Aeson.Value
  , toolBuild       :: Aeson.Value -> Either ToolError ToolInvocation
  }

-- Represents a specific, validated invocation of a tool.
data ToolInvocation = ToolInvocation
  { tiExecute       :: IO ToolResult -- All side effects are in IO
  , tiShouldConfirm :: IO (Maybe ConfirmationDetails) -- Side effects for checking fs, etc.
  }

-- The result of a tool's execution.
data ToolResult = ToolResult
  { trLlmContent    :: T.Text
  , trDisplayOutput :: T.Text
  }

-- Represents a user-facing error from a tool.
newtype ToolError = ToolError T.Text

-- Represents the different events that can drive state changes in the app.
data AppEvent
  = EventUserInput T.Text
  | EventApiResponseChunk T.Text
  | EventToolCallRequest ToolCall
  | EventToolCallCompleted ToolResult
  | EventKeyPress Key -- From the TUI library
  | EventError T.Text
```

#### **2. Proposed Module Structure**

A modular structure will separate concerns, mirroring the original project's package-based design but adapted for Haskell conventions.

```
gemini-cli-hs/
├── src/
│   ├── Main.hs                       -- Application entry point
│   └── Gemini/
│       └── CLI/
│           ├── Types.hs              -- Core ADTs (defined above)
│           ├── App.hs                -- The main application monad, state, and loop logic
│           ├── Config.hs             -- Loading and merging of settings.json
│           ├── UI.hs                 -- TUI rendering logic (using Brick)
│           │
│           ├── Core/
│           │   ├── Client.hs         -- Gemini API interaction, streaming
│           │   └── Turn.hs           -- Logic for a single conversation turn
│           │
│           ├── Commands/
│           │   ├── Parser.hs         -- Parsing of slash commands from user input
│           │   └── Builtin.hs        -- Implementations of /help, /clear, etc.
│           │
│           └── Tools/
│               ├── Types.hs          -- Tool-specific types (Tool, ToolInvocation, etc.)
│               ├── Registry.hs       -- ToolRegistry loading and management
│               ├── Shell.hs          -- Implementation of the shell tool
│               ├── Edit.hs           -- Implementation of the edit tool
│               └── ...               -- Other built-in tools
│
├── test/
│   └── Spec.hs
│
├── gemini-cli-hs.cabal
└── package.yaml
```

#### **3. Modeling Side Effects (The `IO` Boundary)**

All interactions with the external world will be explicitly contained within the `IO` monad. This forces a clean separation between pure, testable business logic and impure actions.

**List of Primary `IO` Actions:**

*   **Network:**
    *   `Gemini.CLI.Core.Client.sendMessageStream :: AppConfig -> [Content] -> IO (Stream (Either ApiError ApiResponseChunk))`
    *   `Gemini.CLI.Config.performAuth :: AuthProvider -> IO AuthDetails`
*   **File System:**
    *   `Gemini.CLI.Config.loadSettings :: FilePath -> IO MergedSettings`
    *   `Gemini.CLI.Tools.Shell.executeShellCommand :: Text -> IO Text`
    *   `Gemini.CLI.Tools.Edit.applyFileEdit :: FilePath -> Text -> IO ()`
    *   `Gemini.CLI.Tools.ReadFile.readFileContent :: FilePath -> IO Text`
*   **Terminal I/O (Handled by the TUI library like `brick`):**
    *   Reading keypress events.
    *   Rendering the UI to the screen.

#### **4. The Main Application Loop**

The core of the application will be a recursive function that represents the main event loop. It will be built using a monad transformer stack to manage state, configuration, and side effects cleanly.

**Proposed Monad Stack:**

```haskell
-- File: src/Gemini/CLI/App.hs

-- The AppM monad provides read-only access to config, stateful access to AppState,
-- and the ability to perform IO actions.
type AppM = ReaderT AppConfig (StateT AppState IO)
```

**Main Loop Signature & Logic:**

```haskell
-- File: src/Gemini/CLI/App.hs

-- The main loop function. It waits for an event, processes it, updates the state,
-- performs any necessary IO, and then calls itself with the new state.
appLoop :: AppM ()
appLoop = do
  -- 1. Get Event (Handled by the TUI library, e.g., brick's event handler)
  event <- liftIO getNextTuiEvent

  -- 2. Process Event (A pure function that pattern matches on the event)
  let (newState, nextAction) = handleEvent event (currentState)

  -- 3. Update State
  put newState

  -- 4. Perform IO Action (Side effects are executed here)
  case nextAction of
    ActionNone -> return ()
    ActionQuit -> -- (Set a flag to break the loop)
    ActionSubmitPrompt prompt -> liftIO $ runGeminiStream prompt
    ActionRunTool toolInvocation -> liftIO $ runToolInvocation toolInvocation
    -- ... other actions

  -- 5. Render UI (The TUI library's draw function will use the new state)
  -- This step is typically handled implicitly by the TUI library's main loop.

  -- 6. Recurse (unless quitting)
  unless (appIsQuitting newState) appLoop

-- The event handler is a pure function.
handleEvent :: AppEvent -> AppState -> (AppState, AppAction)
handleEvent event state =
  case event of
    EventUserInput txt -> -- (parse for slash command or create prompt)
    EventApiResponseChunk chunk -> -- (append chunk to current Gemini message)
    -- ... other event handlers
```

---

#### **Conclusion of Phase 4**

This blueprint provides a solid, functional-first architectural foundation for the Gemini CLI rewrite. By defining the core data structures as ADTs, isolating side effects in the `IO` monad, and structuring the application around a stateful event loop, we can build a system that is robust, type-safe, and highly testable.

The proposed module structure mirrors the logical separation of concerns found in the original TypeScript project, ensuring that the rewrite will be organized and maintainable. This completes the analysis phase. We now have a comprehensive plan and a clear architectural vision to proceed with the implementation in Haskell when ready.
