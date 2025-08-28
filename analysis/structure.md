packages/
├── cli
│   ├── index.ts
│   ├── package.json
│   ├── src
│   │   ├── commands
│   │   │   ├── mcp
│   │   │   │   ├── add.test.ts
│   │   │   │   ├── add.ts
│   │   │   │   ├── list.test.ts
│   │   │   │   ├── list.ts
│   │   │   │   ├── remove.test.ts
│   │   │   │   └── remove.ts
│   │   │   ├── mcp.test.ts
│   │   │   └── mcp.ts
│   │   ├── config
│   │   │   ├── auth.test.ts
│   │   │   ├── auth.ts
│   │   │   ├── config.integration.test.ts
│   │   │   ├── config.test.ts
│   │   │   ├── config.ts
│   │   │   ├── extension.test.ts
│   │   │   ├── extension.ts
│   │   │   ├── keyBindings.test.ts
│   │   │   ├── keyBindings.ts
│   │   │   ├── sandboxConfig.ts
│   │   │   ├── settingsSchema.test.ts
│   │   │   ├── settingsSchema.ts
│   │   │   ├── settings.test.ts
│   │   │   ├── settings.ts
│   │   │   ├── trustedFolders.test.ts
│   │   │   └── trustedFolders.ts
│   │   ├── gemini.test.tsx
│   │   ├── gemini.tsx
│   │   ├── nonInteractiveCli.test.ts
│   │   ├── nonInteractiveCli.ts
│   │   ├── patches
│   │   │   └── is-in-ci.ts
│   │   ├── services
│   │   │   ├── BuiltinCommandLoader.test.ts
│   │   │   ├── BuiltinCommandLoader.ts
│   │   │   ├── CommandService.test.ts
│   │   │   ├── CommandService.ts
│   │   │   ├── FileCommandLoader.test.ts
│   │   │   ├── FileCommandLoader.ts
│   │   │   ├── McpPromptLoader.ts
│   │   │   ├── prompt-processors
│   │   │   │   ├── argumentProcessor.test.ts
│   │   │   │   ├── argumentProcessor.ts
│   │   │   │   ├── shellProcessor.test.ts
│   │   │   │   ├── shellProcessor.ts
│   │   │   │   └── types.ts
│   │   │   └── types.ts
│   │   ├── test-utils
│   │   │   ├── customMatchers.ts
│   │   │   ├── mockCommandContext.test.ts
│   │   │   ├── mockCommandContext.ts
│   │   │   └── render.tsx
│   │   ├── ui
│   │   │   ├── App.test.tsx
│   │   │   ├── App.tsx
│   │   │   ├── colors.ts
│   │   │   ├── commands
│   │   │   │   ├── aboutCommand.test.ts
│   │   │   │   ├── aboutCommand.ts
│   │   │   │   ├── authCommand.test.ts
│   │   │   │   ├── authCommand.ts
│   │   │   │   ├── bugCommand.test.ts
│   │   │   │   ├── bugCommand.ts
│   │   │   │   ├── chatCommand.test.ts
│   │   │   │   ├── chatCommand.ts
│   │   │   │   ├── clearCommand.test.ts
│   │   │   │   ├── clearCommand.ts
│   │   │   │   ├── compressCommand.test.ts
│   │   │   │   ├── compressCommand.ts
│   │   │   │   ├── copyCommand.test.ts
│   │   │   │   ├── copyCommand.ts
│   │   │   │   ├── corgiCommand.test.ts
│   │   │   │   ├── corgiCommand.ts
│   │   │   │   ├── directoryCommand.test.tsx
│   │   │   │   ├── directoryCommand.tsx
│   │   │   │   ├── docsCommand.test.ts
│   │   │   │   ├── docsCommand.ts
│   │   │   │   ├── editorCommand.test.ts
│   │   │   │   ├── editorCommand.ts
│   │   │   │   ├── extensionsCommand.test.ts
│   │   │   │   ├── extensionsCommand.ts
│   │   │   │   ├── helpCommand.test.ts
│   │   │   │   ├── helpCommand.ts
│   │   │   │   ├── ideCommand.test.ts
│   │   │   │   ├── ideCommand.ts
│   │   │   │   ├── initCommand.test.ts
│   │   │   │   ├── initCommand.ts
│   │   │   │   ├── mcpCommand.test.ts
│   │   │   │   ├── mcpCommand.ts
│   │   │   │   ├── memoryCommand.test.ts
│   │   │   │   ├── memoryCommand.ts
│   │   │   │   ├── privacyCommand.test.ts
│   │   │   │   ├── privacyCommand.ts
│   │   │   │   ├── quitCommand.test.ts
│   │   │   │   ├── quitCommand.ts
│   │   │   │   ├── restoreCommand.test.ts
│   │   │   │   ├── restoreCommand.ts
│   │   │   │   ├── settingsCommand.test.ts
│   │   │   │   ├── settingsCommand.ts
│   │   │   │   ├── setupGithubCommand.test.ts
│   │   │   │   ├── setupGithubCommand.ts
│   │   │   │   ├── statsCommand.test.ts
│   │   │   │   ├── statsCommand.ts
│   │   │   │   ├── terminalSetupCommand.test.ts
│   │   │   │   ├── terminalSetupCommand.ts
│   │   │   │   ├── themeCommand.test.ts
│   │   │   │   ├── themeCommand.ts
│   │   │   │   ├── toolsCommand.test.ts
│   │   │   │   ├── toolsCommand.ts
│   │   │   │   ├── types.ts
│   │   │   │   └── vimCommand.ts
│   │   │   ├── components
│   │   │   │   ├── AboutBox.tsx
│   │   │   │   ├── AsciiArt.ts
│   │   │   │   ├── AuthDialog.test.tsx
│   │   │   │   ├── AuthDialog.tsx
│   │   │   │   ├── AuthInProgress.tsx
│   │   │   │   ├── AutoAcceptIndicator.tsx
│   │   │   │   ├── ConsoleSummaryDisplay.tsx
│   │   │   │   ├── ContextSummaryDisplay.test.tsx
│   │   │   │   ├── ContextSummaryDisplay.tsx
│   │   │   │   ├── ContextUsageDisplay.tsx
│   │   │   │   ├── DebugProfiler.tsx
│   │   │   │   ├── DetailedMessagesDisplay.tsx
│   │   │   │   ├── EditorSettingsDialog.tsx
│   │   │   │   ├── FolderTrustDialog.test.tsx
│   │   │   │   ├── FolderTrustDialog.tsx
│   │   │   │   ├── Footer.test.tsx
│   │   │   │   ├── Footer.tsx
│   │   │   │   ├── GeminiRespondingSpinner.tsx
│   │   │   │   ├── Header.test.tsx
│   │   │   │   ├── Header.tsx
│   │   │   │   ├── Help.tsx
│   │   │   │   ├── HistoryItemDisplay.test.tsx
│   │   │   │   ├── HistoryItemDisplay.tsx
│   │   │   │   ├── InputPrompt.test.tsx
│   │   │   │   ├── InputPrompt.tsx
│   │   │   │   ├── LoadingIndicator.test.tsx
│   │   │   │   ├── LoadingIndicator.tsx
│   │   │   │   ├── MemoryUsageDisplay.tsx
│   │   │   │   ├── messages
│   │   │   │   │   ├── CompressionMessage.tsx
│   │   │   │   │   ├── DiffRenderer.test.tsx
│   │   │   │   │   ├── DiffRenderer.tsx
│   │   │   │   │   ├── ErrorMessage.tsx
│   │   │   │   │   ├── GeminiMessageContent.tsx
│   │   │   │   │   ├── GeminiMessage.tsx
│   │   │   │   │   ├── InfoMessage.tsx
│   │   │   │   │   ├── ToolConfirmationMessage.test.tsx
│   │   │   │   │   ├── ToolConfirmationMessage.tsx
│   │   │   │   │   ├── ToolGroupMessage.tsx
│   │   │   │   │   ├── ToolMessage.test.tsx
│   │   │   │   │   ├── ToolMessage.tsx
│   │   │   │   │   ├── UserMessage.tsx
│   │   │   │   │   └── UserShellMessage.tsx
│   │   │   │   ├── ModelStatsDisplay.test.tsx
│   │   │   │   ├── ModelStatsDisplay.tsx
│   │   │   │   ├── PrepareLabel.tsx
│   │   │   │   ├── SessionSummaryDisplay.test.tsx
│   │   │   │   ├── SessionSummaryDisplay.tsx
│   │   │   │   ├── SettingsDialog.test.tsx
│   │   │   │   ├── SettingsDialog.tsx
│   │   │   │   ├── shared
│   │   │   │   │   ├── MaxSizedBox.test.tsx
│   │   │   │   │   ├── MaxSizedBox.tsx
│   │   │   │   │   ├── RadioButtonSelect.test.tsx
│   │   │   │   │   ├── RadioButtonSelect.tsx
│   │   │   │   │   ├── __snapshots__
│   │   │   │   │   │   └── RadioButtonSelect.test.tsx.snap
│   │   │   │   │   ├── text-buffer.test.ts
│   │   │   │   │   ├── text-buffer.ts
│   │   │   │   │   ├── vim-buffer-actions.test.ts
│   │   │   │   │   └── vim-buffer-actions.ts
│   │   │   │   ├── ShellConfirmationDialog.test.tsx
│   │   │   │   ├── ShellConfirmationDialog.tsx
│   │   │   │   ├── ShellModeIndicator.tsx
│   │   │   │   ├── ShowMoreLines.tsx
│   │   │   │   ├── __snapshots__
│   │   │   │   │   ├── IDEContextDetailDisplay.test.tsx.snap
│   │   │   │   │   ├── ModelStatsDisplay.test.tsx.snap
│   │   │   │   │   ├── SessionSummaryDisplay.test.tsx.snap
│   │   │   │   │   ├── ShellConfirmationDialog.test.tsx.snap
│   │   │   │   │   ├── StatsDisplay.test.tsx.snap
│   │   │   │   │   └── ToolStatsDisplay.test.tsx.snap
│   │   │   │   ├── StatsDisplay.test.tsx
│   │   │   │   ├── StatsDisplay.tsx
│   │   │   │   ├── SuggestionsDisplay.tsx
│   │   │   │   ├── ThemeDialog.tsx
│   │   │   │   ├── Tips.tsx
│   │   │   │   ├── ToolStatsDisplay.test.tsx
│   │   │   │   ├── ToolStatsDisplay.tsx
│   │   │   │   └── UpdateNotification.tsx
│   │   │   ├── constants.ts
│   │   │   ├── contexts
│   │   │   │   ├── KeypressContext.test.tsx
│   │   │   │   ├── KeypressContext.tsx
│   │   │   │   ├── OverflowContext.tsx
│   │   │   │   ├── SessionContext.test.tsx
│   │   │   │   ├── SessionContext.tsx
│   │   │   │   ├── SettingsContext.tsx
│   │   │   │   ├── StreamingContext.tsx
│   │   │   │   └── VimModeContext.tsx
│   │   │   ├── editors
│   │   │   │   └── editorSettingsManager.ts
│   │   │   ├── hooks
│   │   │   │   ├── atCommandProcessor.test.ts
│   │   │   │   ├── atCommandProcessor.ts
│   │   │   │   ├── shellCommandProcessor.test.ts
│   │   │   │   ├── shellCommandProcessor.ts
│   │   │   │   ├── slashCommandProcessor.test.ts
│   │   │   │   ├── slashCommandProcessor.ts
│   │   │   │   ├── useAtCompletion.test.ts
│   │   │   │   ├── useAtCompletion.ts
│   │   │   │   ├── useAuthCommand.ts
│   │   │   │   ├── useAutoAcceptIndicator.test.ts
│   │   │   │   ├── useAutoAcceptIndicator.ts
│   │   │   │   ├── useBracketedPaste.ts
│   │   │   │   ├── useCommandCompletion.test.ts
│   │   │   │   ├── useCommandCompletion.tsx
│   │   │   │   ├── useCompletion.ts
│   │   │   │   ├── useConsoleMessages.test.ts
│   │   │   │   ├── useConsoleMessages.ts
│   │   │   │   ├── useEditorSettings.test.ts
│   │   │   │   ├── useEditorSettings.ts
│   │   │   │   ├── useFocus.test.ts
│   │   │   │   ├── useFocus.ts
│   │   │   │   ├── useFolderTrust.test.ts
│   │   │   │   ├── useFolderTrust.ts
│   │   │   │   ├── useGeminiStream.test.tsx
│   │   │   │   ├── useGeminiStream.ts
│   │   │   │   ├── useGitBranchName.test.ts
│   │   │   │   ├── useGitBranchName.ts
│   │   │   │   ├── useHistoryManager.test.ts
│   │   │   │   ├── useHistoryManager.ts
│   │   │   │   ├── useInputHistory.test.ts
│   │   │   │   ├── useInputHistory.ts
│   │   │   │   ├── useKeypress.test.ts
│   │   │   │   ├── useKeypress.ts
│   │   │   │   ├── useKittyKeyboardProtocol.ts
│   │   │   │   ├── useLoadingIndicator.test.ts
│   │   │   │   ├── useLoadingIndicator.ts
│   │   │   │   ├── useLogger.ts
│   │   │   │   ├── useMessageQueue.test.ts
│   │   │   │   ├── useMessageQueue.ts
│   │   │   │   ├── usePhraseCycler.test.ts
│   │   │   │   ├── usePhraseCycler.ts
│   │   │   │   ├── usePrivacySettings.test.ts
│   │   │   │   ├── usePrivacySettings.ts
│   │   │   │   ├── useReactToolScheduler.ts
│   │   │   │   ├── useRefreshMemoryCommand.ts
│   │   │   │   ├── useReverseSearchCompletion.test.tsx
│   │   │   │   ├── useReverseSearchCompletion.tsx
│   │   │   │   ├── useSettingsCommand.ts
│   │   │   │   ├── useShellHistory.test.ts
│   │   │   │   ├── useShellHistory.ts
│   │   │   │   ├── useShowMemoryCommand.ts
│   │   │   │   ├── useSlashCompletion.test.ts
│   │   │   │   ├── useSlashCompletion.ts
│   │   │   │   ├── useStateAndRef.ts
│   │   │   │   ├── useTerminalSize.ts
│   │   │   │   ├── useThemeCommand.ts
│   │   │   │   ├── useTimer.test.ts
│   │   │   │   ├── useTimer.ts
│   │   │   │   ├── useToolScheduler.test.ts
│   │   │   │   ├── vim.test.ts
│   │   │   │   └── vim.ts
│   │   │   ├── IdeIntegrationNudge.tsx
│   │   │   ├── keyMatchers.test.ts
│   │   │   ├── keyMatchers.ts
│   │   │   ├── privacy
│   │   │   │   ├── CloudFreePrivacyNotice.tsx
│   │   │   │   ├── CloudPaidPrivacyNotice.tsx
│   │   │   │   ├── GeminiPrivacyNotice.tsx
│   │   │   │   └── PrivacyNotice.tsx
│   │   │   ├── semantic-colors.ts
│   │   │   ├── __snapshots__
│   │   │   │   └── App.test.tsx.snap
│   │   │   ├── themes
│   │   │   │   ├── ansi-light.ts
│   │   │   │   ├── ansi.ts
│   │   │   │   ├── atom-one-dark.ts
│   │   │   │   ├── ayu-light.ts
│   │   │   │   ├── ayu.ts
│   │   │   │   ├── color-utils.test.ts
│   │   │   │   ├── color-utils.ts
│   │   │   │   ├── default-light.ts
│   │   │   │   ├── default.ts
│   │   │   │   ├── dracula.ts
│   │   │   │   ├── github-dark.ts
│   │   │   │   ├── github-light.ts
│   │   │   │   ├── googlecode.ts
│   │   │   │   ├── no-color.ts
│   │   │   │   ├── semantic-tokens.ts
│   │   │   │   ├── shades-of-purple.ts
│   │   │   │   ├── theme-manager.test.ts
│   │   │   │   ├── theme-manager.ts
│   │   │   │   ├── theme.test.ts
│   │   │   │   ├── theme.ts
│   │   │   │   └── xcode.ts
│   │   │   ├── types.ts
│   │   │   └── utils
│   │   │       ├── clipboardUtils.test.ts
│   │   │       ├── clipboardUtils.ts
│   │   │       ├── CodeColorizer.tsx
│   │   │       ├── commandUtils.test.ts
│   │   │       ├── commandUtils.ts
│   │   │       ├── computeStats.test.ts
│   │   │       ├── computeStats.ts
│   │   │       ├── ConsolePatcher.ts
│   │   │       ├── displayUtils.test.ts
│   │   │       ├── displayUtils.ts
│   │   │       ├── formatters.test.ts
│   │   │       ├── formatters.ts
│   │   │       ├── InlineMarkdownRenderer.tsx
│   │   │       ├── isNarrowWidth.ts
│   │   │       ├── kittyProtocolDetector.ts
│   │   │       ├── MarkdownDisplay.test.tsx
│   │   │       ├── MarkdownDisplay.tsx
│   │   │       ├── markdownUtilities.test.ts
│   │   │       ├── markdownUtilities.ts
│   │   │       ├── platformConstants.ts
│   │   │       ├── __snapshots__
│   │   │       │   └── MarkdownDisplay.test.tsx.snap
│   │   │       ├── TableRenderer.tsx
│   │   │       ├── terminalSetup.ts
│   │   │       ├── textUtils.ts
│   │   │       ├── updateCheck.test.ts
│   │   │       └── updateCheck.ts
│   │   ├── utils
│   │   │   ├── checks.ts
│   │   │   ├── cleanup.test.ts
│   │   │   ├── cleanup.ts
│   │   │   ├── dialogScopeUtils.ts
│   │   │   ├── events.ts
│   │   │   ├── gitUtils.test.ts
│   │   │   ├── gitUtils.ts
│   │   │   ├── handleAutoUpdate.test.ts
│   │   │   ├── handleAutoUpdate.ts
│   │   │   ├── installationInfo.test.ts
│   │   │   ├── installationInfo.ts
│   │   │   ├── package.ts
│   │   │   ├── readStdin.ts
│   │   │   ├── resolvePath.ts
│   │   │   ├── sandbox-macos-permissive-closed.sb
│   │   │   ├── sandbox-macos-permissive-open.sb
│   │   │   ├── sandbox-macos-permissive-proxied.sb
│   │   │   ├── sandbox-macos-restrictive-closed.sb
│   │   │   ├── sandbox-macos-restrictive-open.sb
│   │   │   ├── sandbox-macos-restrictive-proxied.sb
│   │   │   ├── sandbox.ts
│   │   │   ├── settingsUtils.test.ts
│   │   │   ├── settingsUtils.ts
│   │   │   ├── spawnWrapper.ts
│   │   │   ├── startupWarnings.test.ts
│   │   │   ├── startupWarnings.ts
│   │   │   ├── updateEventEmitter.ts
│   │   │   ├── userStartupWarnings.test.ts
│   │   │   ├── userStartupWarnings.ts
│   │   │   └── version.ts
│   │   ├── validateNonInterActiveAuth.test.ts
│   │   ├── validateNonInterActiveAuth.ts
│   │   └── zed-integration
│   │       ├── acp.ts
│   │       ├── fileSystemService.ts
│   │       ├── schema.ts
│   │       └── zedIntegration.ts
│   ├── test-setup.ts
│   ├── tsconfig.json
│   └── vitest.config.ts
├── core
│   ├── index.ts
│   ├── package.json
│   ├── src
│   │   ├── code_assist
│   │   │   ├── codeAssist.ts
│   │   │   ├── converter.test.ts
│   │   │   ├── converter.ts
│   │   │   ├── oauth2.test.ts
│   │   │   ├── oauth2.ts
│   │   │   ├── server.test.ts
│   │   │   ├── server.ts
│   │   │   ├── setup.test.ts
│   │   │   ├── setup.ts
│   │   │   └── types.ts
│   │   ├── config
│   │   │   ├── config.test.ts
│   │   │   ├── config.ts
│   │   │   ├── flashFallback.test.ts
│   │   │   └── models.ts
│   │   ├── core
│   │   │   ├── client.test.ts
│   │   │   ├── client.ts
│   │   │   ├── contentGenerator.test.ts
│   │   │   ├── contentGenerator.ts
│   │   │   ├── coreToolScheduler.test.ts
│   │   │   ├── coreToolScheduler.ts
│   │   │   ├── geminiChat.test.ts
│   │   │   ├── geminiChat.ts
│   │   │   ├── geminiRequest.ts
│   │   │   ├── logger.test.ts
│   │   │   ├── logger.ts
│   │   │   ├── loggingContentGenerator.ts
│   │   │   ├── nonInteractiveToolExecutor.test.ts
│   │   │   ├── nonInteractiveToolExecutor.ts
│   │   │   ├── prompts.test.ts
│   │   │   ├── prompts.ts
│   │   │   ├── __snapshots__
│   │   │   │   └── prompts.test.ts.snap
│   │   │   ├── subagent.test.ts
│   │   │   ├── subagent.ts
│   │   │   ├── tokenLimits.ts
│   │   │   ├── turn.test.ts
│   │   │   └── turn.ts
│   │   ├── ide
│   │   │   ├── constants.ts
│   │   │   ├── detect-ide.test.ts
│   │   │   ├── detect-ide.ts
│   │   │   ├── ide-client.test.ts
│   │   │   ├── ide-client.ts
│   │   │   ├── ideContext.test.ts
│   │   │   ├── ideContext.ts
│   │   │   ├── ide-installer.test.ts
│   │   │   ├── ide-installer.ts
│   │   │   └── process-utils.ts
│   │   ├── index.test.ts
│   │   ├── index.ts
│   │   ├── mcp
│   │   │   ├── google-auth-provider.test.ts
│   │   │   ├── google-auth-provider.ts
│   │   │   ├── oauth-provider.test.ts
│   │   │   ├── oauth-provider.ts
│   │   │   ├── oauth-token-storage.test.ts
│   │   │   ├── oauth-token-storage.ts
│   │   │   ├── oauth-utils.test.ts
│   │   │   └── oauth-utils.ts
│   │   ├── __mocks__
│   │   │   └── fs
│   │   │       └── promises.ts
│   │   ├── mocks
│   │   │   └── msw.ts
│   │   ├── prompts
│   │   │   ├── mcp-prompts.ts
│   │   │   └── prompt-registry.ts
│   │   ├── services
│   │   │   ├── chatRecordingService.test.ts
│   │   │   ├── chatRecordingService.ts
│   │   │   ├── fileDiscoveryService.test.ts
│   │   │   ├── fileDiscoveryService.ts
│   │   │   ├── fileSystemService.test.ts
│   │   │   ├── fileSystemService.ts
│   │   │   ├── gitService.test.ts
│   │   │   ├── gitService.ts
│   │   │   ├── loopDetectionService.test.ts
│   │   │   ├── loopDetectionService.ts
│   │   │   ├── shellExecutionService.test.ts
│   │   │   └── shellExecutionService.ts
│   │   ├── telemetry
│   │   │   ├── clearcut-logger
│   │   │   │   ├── clearcut-logger.test.ts
│   │   │   │   ├── clearcut-logger.ts
│   │   │   │   └── event-metadata-key.ts
│   │   │   ├── constants.ts
│   │   │   ├── file-exporters.ts
│   │   │   ├── index.ts
│   │   │   ├── integration.test.circular.ts
│   │   │   ├── loggers.test.circular.ts
│   │   │   ├── loggers.test.ts
│   │   │   ├── loggers.ts
│   │   │   ├── metrics.test.ts
│   │   │   ├── metrics.ts
│   │   │   ├── sdk.test.ts
│   │   │   ├── sdk.ts
│   │   │   ├── telemetry.test.ts
│   │   │   ├── tool-call-decision.ts
│   │   │   ├── types.ts
│   │   │   ├── uiTelemetry.test.ts
│   │   │   └── uiTelemetry.ts
│   │   ├── test-utils
│   │   │   ├── config.ts
│   │   │   ├── mockWorkspaceContext.ts
│   │   │   └── tools.ts
│   │   ├── tools
│   │   │   ├── diffOptions.test.ts
│   │   │   ├── diffOptions.ts
│   │   │   ├── edit.test.ts
│   │   │   ├── edit.ts
│   │   │   ├── glob.test.ts
│   │   │   ├── glob.ts
│   │   │   ├── grep.test.ts
│   │   │   ├── grep.ts
│   │   │   ├── ls.test.ts
│   │   │   ├── ls.ts
│   │   │   ├── mcp-client-manager.test.ts
│   │   │   ├── mcp-client-manager.ts
│   │   │   ├── mcp-client.test.ts
│   │   │   ├── mcp-client.ts
│   │   │   ├── mcp-tool.test.ts
│   │   │   ├── mcp-tool.ts
│   │   │   ├── memoryTool.test.ts
│   │   │   ├── memoryTool.ts
│   │   │   ├── modifiable-tool.test.ts
│   │   │   ├── modifiable-tool.ts
│   │   │   ├── read-file.test.ts
│   │   │   ├── read-file.ts
│   │   │   ├── read-many-files.test.ts
│   │   │   ├── read-many-files.ts
│   │   │   ├── shell.test.ts
│   │   │   ├── shell.ts
│   │   │   ├── __snapshots__
│   │   │   │   └── shell.test.ts.snap
│   │   │   ├── tool-error.ts
│   │   │   ├── tool-registry.test.ts
│   │   │   ├── tool-registry.ts
│   │   │   ├── tools.test.ts
│   │   │   ├── tools.ts
│   │   │   ├── web-fetch.test.ts
│   │   │   ├── web-fetch.ts
│   │   │   ├── web-search.test.ts
│   │   │   ├── web-search.ts
│   │   │   ├── write-file.test.ts
│   │   │   └── write-file.ts
│   │   └── utils
│   │       ├── bfsFileSearch.test.ts
│   │       ├── bfsFileSearch.ts
│   │       ├── browser.ts
│   │       ├── editCorrector.test.ts
│   │       ├── editCorrector.ts
│   │       ├── editor.test.ts
│   │       ├── editor.ts
│   │       ├── environmentContext.test.ts
│   │       ├── environmentContext.ts
│   │       ├── errorParsing.test.ts
│   │       ├── errorParsing.ts
│   │       ├── errorReporting.test.ts
│   │       ├── errorReporting.ts
│   │       ├── errors.ts
│   │       ├── fetch.ts
│   │       ├── filesearch
│   │       │   ├── crawlCache.test.ts
│   │       │   ├── crawlCache.ts
│   │       │   ├── crawler.test.ts
│   │       │   ├── crawler.ts
│   │       │   ├── fileSearch.test.ts
│   │       │   ├── fileSearch.ts
│   │       │   ├── ignore.test.ts
│   │       │   ├── ignore.ts
│   │       │   ├── result-cache.test.ts
│   │       │   └── result-cache.ts
│   │       ├── fileUtils.test.ts
│   │       ├── fileUtils.ts
│   │       ├── flashFallback.integration.test.ts
│   │       ├── formatters.ts
│   │       ├── generateContentResponseUtilities.test.ts
│   │       ├── generateContentResponseUtilities.ts
│   │       ├── getFolderStructure.test.ts
│   │       ├── getFolderStructure.ts
│   │       ├── getPty.ts
│   │       ├── gitIgnoreParser.test.ts
│   │       ├── gitIgnoreParser.ts
│   │       ├── gitUtils.ts
│   │       ├── LruCache.ts
│   │       ├── memoryDiscovery.test.ts
│   │       ├── memoryDiscovery.ts
│   │       ├── memoryImportProcessor.test.ts
│   │       ├── memoryImportProcessor.ts
│   │       ├── messageInspectors.ts
│   │       ├── nextSpeakerChecker.test.ts
│   │       ├── nextSpeakerChecker.ts
│   │       ├── partUtils.test.ts
│   │       ├── partUtils.ts
│   │       ├── paths.test.ts
│   │       ├── paths.ts
│   │       ├── quotaErrorDetection.ts
│   │       ├── retry.test.ts
│   │       ├── retry.ts
│   │       ├── safeJsonStringify.test.ts
│   │       ├── safeJsonStringify.ts
│   │       ├── schemaValidator.ts
│   │       ├── secure-browser-launcher.test.ts
│   │       ├── secure-browser-launcher.ts
│   │       ├── session.ts
│   │       ├── shell-utils.test.ts
│   │       ├── shell-utils.ts
│   │       ├── summarizer.test.ts
│   │       ├── summarizer.ts
│   │       ├── systemEncoding.test.ts
│   │       ├── systemEncoding.ts
│   │       ├── testUtils.ts
│   │       ├── textUtils.ts
│   │       ├── user_account.test.ts
│   │       ├── user_account.ts
│   │       ├── user_id.test.ts
│   │       ├── user_id.ts
│   │       ├── workspaceContext.test.ts
│   │       └── workspaceContext.ts
│   ├── test-setup.ts
│   ├── tsconfig.json
│   └── vitest.config.ts
├── test-utils
│   ├── index.ts
│   ├── package.json
│   ├── src
│   │   ├── file-system-test-helpers.ts
│   │   └── index.ts
│   └── tsconfig.json
└── vscode-ide-companion
    ├── assets
    │   └── icon.png
    ├── development.md
    ├── esbuild.js
    ├── eslint.config.mjs
    ├── LICENSE
    ├── NOTICES.txt
    ├── package.json
    ├── README.md
    ├── scripts
    │   └── generate-notices.js
    ├── src
    │   ├── diff-manager.ts
    │   ├── extension-multi-folder.test.ts
    │   ├── extension.test.ts
    │   ├── extension.ts
    │   ├── ide-server.ts
    │   ├── open-files-manager.test.ts
    │   ├── open-files-manager.ts
    │   └── utils
    │       └── logger.ts
    └── tsconfig.json

54 directories, 580 files
