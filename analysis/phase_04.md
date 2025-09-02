Excellent. Phase 4 is now underway.

This final phase synthesizes the architectural understanding gained from the previous phases into a concrete blueprint for rewriting the Gemini CLI in Haskell. This blueprint outlines the core data types, module structure, and the functional design of the main application loop, providing a clear roadmap for implementation.

---

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
