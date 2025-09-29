# Tool Lifecycle in Gemini CLI

This document traces the complete lifecycle of a tool in the Gemini CLI, from registration to execution.

## 1. Tool Registration

### Files involved:
- `packages/core/src/tools/tool-registry.ts`
- `packages/core/src/config/config.ts`

### Process:
1. Tools are registered in the `ToolRegistry` via the `registerTool()` method
2. Core tools are registered during application initialization in `Config.createToolRegistry()`
3. Tools can also be discovered dynamically from:
   - Project commands (via `discoverAndRegisterToolsFromCommand()`)
   - MCP servers (via `mcpClientManager.discoverAllMcpTools()`)

### Data Types:
- `ToolRegistry` - A class that maintains a `Map<string, AnyDeclarativeTool>`
- `AnyDeclarativeTool` - A type alias for `DeclarativeTool<object, ToolResult>`

## 2. Tool Definition and Validation

### Files involved:
- `packages/core/src/tools/tools.ts`
- `packages/core/src/tools/ls.ts` (example implementation)

### Process:
1. Tools extend `BaseDeclarativeTool<TParams, TResult>` which implements `DeclarativeTool<TParams, TResult>`
2. Each tool defines:
   - `name` - Internal tool name
   - `displayName` - User-friendly name
   - `description` - What the tool does
   - `kind` - Categorization (Read, Edit, Execute, etc.)
   - `parameterSchema` - JSON schema for parameter validation
3. Tools implement:
   - `validateToolParamValues()` - Custom validation logic
   - `createInvocation()` - Creates a `ToolInvocation` instance with validated parameters

### Data Types:
- `DeclarativeTool<TParams, TResult>` - Interface defining tool structure
- `BaseDeclarativeTool<TParams, TResult>` - Abstract base class with common functionality
- `ToolBuilder<TParams, TResult>` - Interface for building tool invocations
- `ToolInvocation<TParams, TResult>` - Interface for executing validated tool calls

## 3. Tool Invocation Creation

### Process:
1. When a tool is requested, `BaseDeclarativeTool.build()` is called with raw parameters
2. Parameters are validated using:
   - JSON schema validation via `SchemaValidator`
   - Custom validation via `validateToolParamValues()`
3. If validation passes, `createInvocation()` is called to create a `ToolInvocation` instance
4. The `ToolInvocation` contains the validated parameters and execution logic

### Data Types:
- `ToolInvocation<TParams, TResult>` - Contains validated parameters and execution methods
- `BaseToolInvocation<TParams, TResult>` - Abstract base class for invocations

## 4. Tool Scheduling and Execution

### Files involved:
- `packages/core/src/core/coreToolScheduler.ts`
- `packages/cli/src/ui/hooks/useReactToolScheduler.ts`

### Process:
1. UI calls `useReactToolScheduler.schedule()` with tool request info
2. This delegates to `CoreToolScheduler.schedule()` which:
   - Looks up the tool in the registry
   - Validates parameters using `tool.build()`
   - Checks for confirmation requirements with `invocation.shouldConfirmExecute()`
   - Either waits for user confirmation or schedules for immediate execution
3. When scheduled, `invocation.execute()` is called with:
   - `AbortSignal` for cancellation
   - Optional output update callback for streaming

### Data Types:
- `ToolCallRequestInfo` - Request containing tool name and parameters
- `ToolCall` - Union type representing different states of a tool call
- `CoreToolScheduler` - Manages tool execution lifecycle
- `ToolResult` - Contains the result of tool execution

## 5. Tool Result Processing

### Process:
1. Tool execution returns a `ToolResult` with:
   - `llmContent` - Content for LLM consumption
   - `returnDisplay` - User-friendly display content
   - Optional `error` - Error information if execution failed
2. `CoreToolScheduler` converts this to a Gemini API `functionResponse`
3. Result is passed back to the UI via callbacks

### Data Types:
- `ToolResult` - Standard result structure
- `ToolResultDisplay` - Union of string or FileDiff for display
- `PartListUnion` - Gemini API content format

## Architecture Benefits

This architecture cleanly separates concerns:
- Tool definition and validation are handled by the tool classes themselves
- Tool registration is managed by the ToolRegistry
- Tool scheduling and execution are handled by the CoreToolScheduler
- UI integration is provided by useReactToolScheduler

The validation happens at two levels:
1. JSON schema validation for basic parameter structure
2. Custom validation logic in `validateToolParamValues()` for business rules

This design ensures that tools are well-defined, validated before execution, and can be safely executed with proper error handling and user confirmation when needed.