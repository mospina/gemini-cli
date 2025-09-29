#!/bin/bash

# Script to create an EPUB book from markdown files in the analysis directory
# Usage: ./create_epub.sh [output_filename]

set -e  # Exit on any error

# Default output filename
OUTPUT_FILE="gemini-cli-analysis.epub"

# Use first argument as output filename if provided
if [ $# -gt 0 ]; then
    OUTPUT_FILE="$1"
fi

# Check if pandoc is installed
if ! command -v pandoc &> /dev/null; then
    echo "Error: pandoc is not installed. Please install pandoc to use this script."
    exit 1
fi

echo "Creating EPUB book: $OUTPUT_FILE"

# Create a temporary directory for our work
TEMP_DIR=$(mktemp -d)
echo "Using temporary directory: $TEMP_DIR"

# Copy markdown files to temp directory with a specific order
# We'll rename them with numbers to control the order in the book
cp README.md "$TEMP_DIR/00-README.md"
cp plan.md "$TEMP_DIR/01-plan.md"
cp structure.md "$TEMP_DIR/02-structure.md"
cp system_prompt.md "$TEMP_DIR/03-system_prompt.md"
cp main_loop_flow.md "$TEMP_DIR/04-main_loop_flow.md"
cp command_process_flow.md "$TEMP_DIR/05-command_process_flow.md"
cp cli_process_trace.md "$TEMP_DIR/06-cli_process_trace.md"
cp run_non_interactive_flow.md "$TEMP_DIR/07-run_non_interactive_flow.md"
cp phase_01.md "$TEMP_DIR/08-phase_01.md"
cp phase_02.md "$TEMP_DIR/09-phase_02.md"
cp phase_03.md "$TEMP_DIR/10-phase_03.md"
cp phase_04.md "$TEMP_DIR/11-phase_04.md"
cp docs.md "$TEMP_DIR/12-docs.md"
cp analysis.md "$TEMP_DIR/13-analysis.md"

# Copy papers directory if it exists
if [ -d "papers" ]; then
    cp -r papers "$TEMP_DIR/"
fi

# Create a title page markdown file
cat > "$TEMP_DIR/title.md" << 'EOF'
---
title: Qwen Code Analysis
author: Generated from Analysis Documents
date: $(date +'%Y-%m-%d')
---

# Qwen Code Analysis

This book contains the analysis documents for the Qwen Code project.
EOF

# Create the EPUB using pandoc
# We put the title page first, then all other markdown files
echo "Generating EPUB..."
pandoc \
    --from=markdown \
    --to=epub \
    --output="$OUTPUT_FILE" \
    --metadata title="Qwen Code Analysis" \
    --metadata author="Generated from Analysis Documents" \
    --metadata date="$(date +'%Y-%m-%d')" \
    --toc \
    --toc-depth=2 \
    --epub-chapter-level=1 \
    "$TEMP_DIR/title.md" \
    "$TEMP_DIR"/[0-9]*.md

# Check if the EPUB was created successfully
if [ $? -eq 0 ] && [ -f "$OUTPUT_FILE" ]; then
    echo "Successfully created EPUB: $OUTPUT_FILE"
    echo "File size: $(du -h "$OUTPUT_FILE" | cut -f1)"
else
    echo "Error: Failed to create EPUB"
    exit 1
fi

# Clean up temporary directory
rm -rf "$TEMP_DIR"
echo "Temporary files cleaned up"

echo "Done!"