# Tracing the Process of Handling `gemini -p <prompt>`

When a user runs `gemini -p <prompt>`, the following process occurs:

## 1. Entry Point and Argument Parsing

1. **CLI Entry Point**: The command starts at `packages/cli/index.ts`, which imports and calls the `main()` function from `packages/cli/src/gemini.tsx`.

2. **Argument Parsing**: In `main()`, the CLI arguments are parsed using `yargs` through the `parseArguments()` function in `packages/cli/src/config/config.ts`. 
   - The `-p` flag is defined as an alias for `--prompt` with the description "Prompt. Appended to input on stdin (if any)."
   - The parsed arguments are stored in a `CliArgs` object, with the prompt value accessible as `argv.prompt`.

## 2. Configuration Loading

3. **Configuration Setup**: After parsing arguments, `loadCliConfig()` is called to create a `Config` object that encapsulates all the CLI settings.
   - The prompt from `argv.prompt` is used to set the initial question in the config.
   - The config determines if the CLI should run in interactive mode based on whether there's a prompt and if stdin is a TTY.

## 3. Determining Execution Mode

4. **Mode Detection**: The CLI determines if it should run in interactive or non-interactive mode:
   - Since a prompt is provided with `-p`, and there's no `-i` flag, it will run in non-interactive mode.
   - This is determined by the line: `const interactive = !!argv.promptInteractive || (process.stdin.isTTY && question.length === 0);`

## 4. Handling Stdin (if applicable)

5. **Stdin Reading**: If stdin is not a TTY (meaning data is being piped in), the CLI reads from stdin:
   - The `readStdin()` function in `packages/cli/src/utils/readStdin.ts` is used to read up to 8MB of data from stdin.
   - If both stdin data and a prompt are provided, they are concatenated with the prompt appended to the stdin data.

## 5. Non-Interactive Execution

6. **Non-Interactive Processing**: Since we're using `-p`, the CLI enters non-interactive mode:
   - The `runNonInteractive()` function in `packages/cli/src/nonInteractiveCli.ts` is called.
   - This function handles the communication with the Gemini API without user interaction.

## 6. API Communication

7. **Gemini API Interaction**: In `runNonInteractive()`:
   - A Gemini client is obtained from the config.
   - The prompt is sent to the Gemini API as a user message.
   - The response is streamed to stdout as it's received.
   - If the model requests tool calls, they are executed and the results are sent back to the model.

## 7. Output and Exit

8. **Output and Termination**: 
   - The model's response is written directly to stdout.
   - When the conversation is complete (no more tool calls), a final newline is added.
   - The process exits with code 0.

## Key Code Paths

- **Entry Point**: `packages/cli/index.ts` → `main()` in `packages/cli/src/gemini.tsx`
- **Argument Parsing**: `parseArguments()` in `packages/cli/src/config/config.ts`
- **Configuration**: `loadCliConfig()` in `packages/cli/src/config/config.ts`
- **Stdin Handling**: `readStdin()` in `packages/cli/src/utils/readStdin.ts`
- **Non-Interactive Execution**: `runNonInteractive()` in `packages/cli/src/nonInteractiveCli.ts`

This process allows users to quickly get responses from Gemini without entering an interactive session, making it suitable for scripting and automation.