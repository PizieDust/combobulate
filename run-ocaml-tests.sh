#!/bin/bash
# Script to run OCaml-specific tests for Combobulate
# Usage: ./run-ocaml-tests.sh [options] [test-name-pattern]
#
# Options:
#   --imenu          Run only imenu tests (default)
#   --generated      Run generated tests (navigation, drag, etc.)
#   --all            Run all OCaml tests (imenu + generated)
#   --tag TAG        Run tests with specific tag (e.g., ocaml, imenu)
#   --verbose        Show verbose output
#
# Examples:
#   ./run-ocaml-tests.sh                              # Run imenu tests
#   ./run-ocaml-tests.sh --all                        # Run all OCaml tests
#   ./run-ocaml-tests.sh --tag ocaml                  # Run tests tagged 'ocaml'
#   ./run-ocaml-tests.sh combobulate-test-ocaml-imenu-ml-file  # Run specific test

set -e

cd "$(dirname "$0")"

# Default options
TEST_TYPE="imenu"
VERBOSE=""
SELECTOR=""

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --imenu)
            TEST_TYPE="imenu"
            shift
            ;;
        --generated)
            TEST_TYPE="generated"
            shift
            ;;
        --all)
            TEST_TYPE="all"
            shift
            ;;
        --tag)
            SELECTOR="(tag $2)"
            shift 2
            ;;
        --verbose)
            VERBOSE="--eval '(setq ert-batch-backtrace-right-margin 200)'"
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            # Assume it's a test name pattern
            SELECTOR="\"$1\""
            shift
            ;;
    esac
done

# Build the command
BASE_CMD="emacs --batch --chdir ./tests/ -L .. -L . -L tuareg -l tuareg-treesit.el -l ert -l ../combobulate"

# Load appropriate test files based on test type
case $TEST_TYPE in
    imenu)
        echo "Running OCaml imenu tests..."
        TEST_FILES="-l test-ocaml-imenu.el"
        ;;
    generated)
        echo "Running generated OCaml tests..."
        echo "Note: Generated tests must be created first by running the test generator"
        TEST_FILES="-l test-combobulate-navigate-next.gen.el -l test-combobulate-navigate-previous.gen.el -l test-combobulate-navigate-down.gen.el -l test-combobulate-drag-up.gen.el -l test-combobulate-drag-down.gen.el"
        if [ -z "$SELECTOR" ]; then
            SELECTOR='"ocaml"'
        fi
        ;;
    all)
        echo "Running all OCaml tests..."
        TEST_FILES="-l test-ocaml-imenu.el -l test-ocaml-implementation-navigation.el -l test-ocaml-interface-navigation.el"
        # Add generated test files if they exist
        for gentest in test-combobulate-navigate-next.gen.el test-combobulate-navigate-previous.gen.el test-combobulate-navigate-down.gen.el test-combobulate-drag-up.gen.el test-combobulate-drag-down.gen.el; do
            if [ -f "tests/$gentest" ]; then
                TEST_FILES="$TEST_FILES -l $gentest"
            fi
        done
        if [ -z "$SELECTOR" ]; then
            SELECTOR='"ocaml"'
        fi
        ;;
esac

# Build final command
if [ -z "$SELECTOR" ]; then
    CMD="$BASE_CMD $TEST_FILES -f ert-run-tests-batch-and-exit"
else
    CMD="$BASE_CMD $TEST_FILES --eval '(ert-run-tests-batch-and-exit $SELECTOR)'"
fi

# Add verbose flag if requested
if [ -n "$VERBOSE" ]; then
    CMD="$CMD $VERBOSE"
fi

# Execute
eval $CMD
