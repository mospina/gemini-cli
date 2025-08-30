Excellent. Phase 3 is now underway.

This deep dive analyzes the key architectural components of the Gemini CLI, focusing on their design, responsibilities, and how their patterns can be translated into a functional paradigm for the Haskell rewrite.

---

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

I am now ready to proceed with **Phase 4: Synthesis & Haskell Rewrite Blueprint**. Please provide the go-ahead.
