Of course. You've correctly identified that the `runNonInteractive` function is for a different purpose (autonomous agents). The main interactive CLI loop that the user sees is fundamentally different.

Instead of a traditional `while (true)` loop, it's an **event-driven loop** managed by React's state and lifecycle, hosted within the Ink terminal UI framework.

### **Where is the Main Interactive Loop?**

The "loop" is not in a single function but is orchestrated primarily by the **`useGeminiStream`** custom React hook, located at:

*   **File:** `packages/cli/src/ui/hooks/useGeminiStream.ts`

This hook is the heart of the interactive application. It is used by the main `<App>` component (`packages/cli/src/ui/App.tsx`), which acts as the host that renders the UI and ties all the stateful logic from the hooks together.

The flow is not "do this, then do that," but rather "when this event happens, update the state, which causes a re-render."

---

### **Flow of Data in the Interactive Loop**

Here is the step-by-step flow of data, starting from a user keypress and ending with the UI updating on the screen.

#### **Stage 1: User Input and Pre-Processing**

1.  **Keypress Event:** The user presses a key in their terminal.
    *   **Data Type:** Raw keypress data from `stdin`.
    *   **Location:** The `useKeypress` hook (`packages/cli/src/ui/hooks/useKeypress.ts`) captures this raw data and transforms it into a structured `Key` object (e.g., `{ name: 'a', ctrl: false, ... }`).

2.  **Input Buffer Management:** The `Key` object is handled by the `InputPrompt.tsx` component.
    *   **Data Type:** The `Key` object is processed by the `useTextBuffer` hook (`packages/cli/src/ui/components/shared/text-buffer.ts`), which manages the user's input line.
    *   **Process:** If it's a printable character, it's appended to the buffer. If it's a control key (like backspace or arrow keys), the buffer and cursor position are updated accordingly. The state of this buffer is a simple `string`.

3.  **Submission:** The user hits "Enter".
    *   **Process:** The `useKeypress` and `useTextBuffer` hooks identify this as a submit action. The `InputPrompt` component calls the `submitQuery` function, which it received as a prop from the `useGeminiStream` hook.
    *   **Data Type:** The final `string` from the text buffer is passed to `submitQuery`.

#### **Stage 2: Command Dispatch and API Request**

4.  **Central Orchestration (`useGeminiStream`):** The `submitQuery` function is the main entry point for processing a completed user input.
    *   **Data In:** `query: string`
    *   **Process:**
        *   The query is first added to the UI history as a `user` message using the `useHistoryManager` hook. This immediately renders the user's prompt on the screen.
        *   The query is then inspected. If it starts with `/` or `@`, it's handed off to the `useSlashCommandProcessor` or `useAtCommandProcessor` respectively.
            *   **Slash Command Path:** The slash command's `action` is executed. Most of these actions are **local**—they modify the UI state directly (e.g., `/clear` calls `ui.clear()`, `/help` adds a `help` item to the history) and the flow **stops here**. The return type is `{ type: 'handled' }`.
            *   **Natural Language Path:** If it's not a slash command, the flow continues to the Gemini API.
    *   **Data Out:** The `string` query is passed to `geminiClient.sendMessageStream`.

#### **Stage 3: Core Logic and Streaming Response (`packages/core`)**

5.  **API Abstraction Layer:** The request moves from the `cli` package to the `core` package.
    *   **Data In:** `query: string`
    *   **Process:** `GeminiClient.sendMessageStream` converts the string into the API-required `Content[]` format (`[{ role: 'user', parts: [{ text: query }] }]`). It creates a `Turn` object to manage the API call.
    *   **Data Type:** The `Turn` object's `run()` method returns an `AsyncGenerator<ServerGeminiStreamEvent>`.

6.  **Consuming the Stream:** The `useGeminiStream` hook iterates over the stream from the `Turn` object.
    *   **Data In:** `ServerGeminiStreamEvent` (e.g., `{ type: 'content', value: 'Hello' }` or `{ type: 'tool_call_request', ... }`)
    *   **Process:**
        *   **If `content` event:** The text chunk is appended to a *pending* `HistoryItem` of type `gemini`. Updating this pending item causes React to re-render only the last line of the response, creating the streaming text effect.
        *   **If `tool_call_request` event:** The request is sent to the `useReactToolScheduler` hook. The scheduler displays a confirmation prompt to the user (if necessary) and then executes the tool via the `CoreToolScheduler`.
    *   **Data Out:** The UI state is updated via `setPendingHistoryItem` or by scheduling a tool call.

#### **Stage 4: Rendering the Output**

7.  **React Re-renders:** Every single time a state-setting function is called (e.g., `addItem`, `setPendingHistoryItem`), React triggers a re-render of the `<App>` component.
    *   The `History` component iterates through the `history` array (from `useHistoryManager`) and the `pendingHistoryItems`.
    *   For each `HistoryItem`, it renders the appropriate component (e.g., `<UserMessage>`, `<GeminiMessage>`, `<ToolGroupMessage>`). This is how the user sees the conversation history, streaming responses, and tool call statuses update in real-time.

### **Data Type Transformation Summary**

The data transforms as it moves through the interactive loop:

`Raw Buffer` (stdin) -> `Key` (object) -> `string` (in text buffer) -> `HistoryItem` (for UI) AND `Content[]` (for API) -> `ServerGeminiStreamEvent` (from API stream) -> `HistoryItem` (for UI) -> **Rendered Output**

### **Data Flow Diagram: Interactive Loop**

```mermaid
sequenceDiagram
    participant User
    participant InputPrompt (React Component)
    participant useGeminiStream (Hook)
    participant useHistoryManager (Hook)
    participant GeminiClient (Core)
    participant GeminiAPI

    User->>+InputPrompt (React Component): Types "list files"
    InputPrompt (React Component)->>+useGeminiStream (Hook): submitQuery("list files")
    useGeminiStream (Hook)->>+useHistoryManager (Hook): addItem({type: 'user', text: 'list files'})
    useHistoryManager (Hook)-->>-CLI UI: State Update -> Re-render
    
    useGeminiStream (Hook)->>+GeminiClient (Core): sendMessageStream("list files")
    GeminiClient (Core)->>+GeminiAPI: Streaming Request
    activate GeminiAPI
    
    GeminiAPI-->>-GeminiClient (Core): Stream Chunk (ToolCall: list_directory)
    GeminiClient (Core)-->>-useGeminiStream (Hook): Yields ToolCallRequest Event
    useGeminiStream (Hook)->>useGeminiStream (Hook): Schedules tool, shows confirmation
    
    User->>+CLI UI: Confirms tool execution
    useGeminiStream (Hook)-->>+ToolScheduler (Core): Executes tool
    ToolScheduler (Core)-->>-useGeminiStream (Hook): Returns ToolResult
    
    useGeminiStream (Hook)->>useGeminiStream (Hook): Converts ToolResult to functionResponse
    useGeminiStream (Hook)->>+GeminiClient (Core): submitQuery(functionResponse)
    GeminiClient (Core)->>+GeminiAPI: Streaming Request (with tool result)
    
    GeminiAPI-->>-GeminiClient (Core): Stream Chunk (Content: "file1.txt...")
    GeminiClient (Core)-->>-useGeminiStream (Hook): Yields Content Event
    useGeminiStream (Hook)->>+useHistoryManager (Hook): setPendingItem({type: 'gemini', text: "file1.txt..."})
    useHistoryManager (Hook)-->>-CLI UI: State Update -> Re-render with streaming text
    
    GeminiAPI-->>-GeminiClient (Core): Stream End
    deactivate GeminiAPI
    useGeminiStream (Hook)->>+useHistoryManager (Hook): addItem(final pending item)
    useHistoryManager (Hook)-->>-CLI UI: State Update -> Re-render final message
    deactivate useHistoryManager (Hook)
    deactivate GeminiClient (Core)
    deactivate useGeminiStream (Hook)
    deactivate InputPrompt (React Component)
```
