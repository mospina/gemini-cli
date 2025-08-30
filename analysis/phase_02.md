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
