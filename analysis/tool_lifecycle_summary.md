# Tool Lifecycle - Key Files and Data Types

## Core Files

1. **Tool Definition**: `packages/core/src/tools/tools.ts`
   - Contains base classes and interfaces for tools
   - Defines `DeclarativeTool`, `BaseDeclarativeTool`, `ToolInvocation`

2. **Tool Registry**: `packages/core/src/tools/tool-registry.ts`
   - Manages tool registration and discovery
   - Handles both built-in and dynamically discovered tools

3. **Tool Scheduler**: `packages/core/src/core/coreToolScheduler.ts`
   - Orchestrates tool execution lifecycle
   - Manages validation, confirmation, and execution

4. **UI Integration**: `packages/cli/src/ui/hooks/useReactToolScheduler.ts`
   - Bridges React UI with core tool scheduler
   - Manages UI state for tool calls

5. **Configuration**: `packages/core/src/config/config.ts`
   - Registers core tools during initialization
   - Manages tool enablement and discovery

## Key Data Types

### Tool Definition
- `DeclarativeTool<TParams, TResult>` - Interface for all tools
- `BaseDeclarativeTool<TParams, TResult>` - Abstract base class
- `ToolBuilder<TParams, TResult>` - Interface for building invocations
- `ToolInvocation<TParams, TResult>` - Interface for executing validated calls
- `BaseToolInvocation<TParams, TResult>` - Abstract base class for invocations

### Tool Registration
- `ToolRegistry` - Class managing tool collection
- `AnyDeclarativeTool` - Type alias for generic tool

### Tool Execution
- `ToolCallRequestInfo` - Request containing tool name and parameters
- `ToolCall` - Union type for all tool call states
- `ToolResult` - Standard result structure with LLM and display content
- `ToolResultDisplay` - Union of string or FileDiff for display

### Scheduling
- `CoreToolScheduler` - Core execution orchestrator
- `TrackedToolCall` - UI-tracked tool call with submission status