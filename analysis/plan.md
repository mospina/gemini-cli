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
