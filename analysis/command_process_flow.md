# `/tools` Command Process Flow

This document outlines the step-by-step process of how the Gemini CLI TUI handles the user input `/tools`, including the data types involved at each stage.

### 1. Command Registration & Invocation

*   **Registration**: A `SlashCommand` object named `toolsCommand` is defined in `packages/cli/src/ui/commands/toolsCommand.ts`. This object contains:
    *   `name`: `'tools'` (`string`)
    *   `description`: `'list available Gemini CLI tools'` (`string`)
    *   `kind`: `CommandKind.BUILT_IN` (enum member)
    *   `action`: A function of type `(context: CommandContext, args?: string) => Promise<void>`
*   **Invocation**: When the user enters `/tools` in the TUI, the command handler identifies the input and invokes the `action` function associated with the `toolsCommand`.

### 2. Action Function Execution

*   The `action` function is called with two arguments:
    *   `context`: An object of type `CommandContext`. This object acts as a container for application-wide services and the UI instance.
    *   `args`: An optional `string` that captures any text following the command (e.g., `desc` for `/tools desc`).

### 3. Argument Handling

*   A local `string` variable, `subCommand`, is assigned the trimmed value of the `args` parameter.
*   A `boolean` flag, `useShowDescriptions`, is initialized to `false`. It is set to `true` only if `subCommand` is equal to `"desc"` or `"descriptions"`.

### 4. Retrieving the Tool Registry

*   The code calls `context.services.config?.getToolRegistry()`.
*   **Data Type**: This method is expected to return an instance of the `ToolRegistry` class.
*   If the `ToolRegistry` instance is not available (`undefined`), an error message is created and displayed:
    *   **Data Type**: An object conforming to the `UIAddItem` interface: `{ type: MessageType.ERROR, text: 'Could not retrieve tool registry.' }`.

### 5. Fetching and Filtering Tools

*   `toolRegistry.getAllTools()` is called, which returns an array of `AnyDeclarativeTool` objects. `AnyDeclarativeTool` is a TypeScript union type representing any possible tool definition.
*   This array is then filtered using the `Array.prototype.filter()` method. The filter condition `!('serverName' in tool)` checks for the absence of the `serverName` property on each `tool` object.
*   **Data Type**: The result of the filter is a new array, `geminiTools`, of type `AnyDeclarativeTool[]`, containing only the built-in tools.

### 6. Formatting the Output Message

*   A `string` variable named `message` is initialized to build the final output.
*   The code iterates through the `geminiTools` array. For each `tool` (`AnyDeclarativeTool`) in the array:
    *   If `useShowDescriptions` is `true`, the tool's `displayName` (`string`), `name` (`string`), and `description` (`string`) are appended to the `message`.
    *   Otherwise, only the `displayName` is appended.
*   **Data Type**: ANSI escape codes (`string`) are embedded within the `message` string to add color for terminal display.

### 7. Displaying the Result

*   The fully formatted `message` string is wrapped in an object.
*   **Data Type**: An object conforming to the `UIAddItem` interface: `{ type: MessageType.INFO, text: message }`.
*   This object is passed to the `context.ui.addItem()` method, which renders the content in the TUI, displaying the list of available tools to the user.
