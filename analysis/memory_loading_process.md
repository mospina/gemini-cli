# Memory Loading Process in gemini-cli

This document describes how gemini-cli reads and loads GEMINI.md memory files.

## Overview

The gemini-cli implements a hierarchical memory system that loads context files (GEMINI.md by default) from multiple locations to provide instructional context to the Gemini model. This system allows for both global and project-specific configurations.

## Memory Loading Process

### 1. Initialization

When the CLI starts, the `loadCliConfig` function in `packages/cli/src/config/config.ts` calls `loadHierarchicalGeminiMemory`, which is a wrapper around the server's `loadServerHierarchicalMemory` function.

### 2. File Discovery

The `loadServerHierarchicalMemory` function in `packages/core/src/utils/memoryDiscovery.ts` discovers GEMINI.md files through a hierarchical search:

- **Global memory file**: Located in `~/.gemini/GEMINI.md`
- **Upward search**: From current directory up to project root (identified by .git directory)
- **Downward search**: In subdirectories using BFS (breadth-first search)
- **Extension context files**: Any extension-specific context files if configured

### 3. File Reading

The `readGeminiMdFiles` function reads each discovered file and processes any import statements using the `processImports` function in `packages/core/src/utils/memoryImportProcessor.ts`.

### 4. Import Processing

The `processImports` function handles `@file/path` syntax in GEMINI.md files, allowing one file to import content from another. It supports two import formats:
- **Flat format**: All imported files are concatenated in order of first encounter
- **Tree format**: Imports are processed with nested comments indicating source files

### 5. Content Concatenation

The `concatenateInstructions` function combines all file contents with markers indicating the source file path:
```
--- Context from: path/to/file.md ---
[File content]
--- End of Context from: path/to/file.md ---
```

### 6. Configuration Storage

The resulting memory content and file count are stored in the Config object via:
- `setUserMemory()` - stores the concatenated content
- `setGeminiMdFileCount()` - tracks the number of files loaded

### 7. Memory Refresh

The `/memory refresh` command can reload all GEMINI.md files using the same process, allowing for dynamic updates without restarting the CLI.

## Configuration Options

The memory loading system supports several configuration options:

- **Multiple context file names**: Through the `contextFileName` setting, users can specify alternative filenames
- **Include directories**: The `loadMemoryFromIncludeDirectories` setting enables loading files from additional directories
- **Import format**: Controlled by the `memoryImportFormat` setting (flat or tree)
- **File filtering**: Options to respect .gitignore and .geminiignore files

## Security and Validation

The system includes several security measures:
- Path validation to prevent directory traversal attacks
- Circular import detection to prevent infinite loops
- Maximum import depth limits (default: 5 levels)
- Validation that imported files are within allowed directories