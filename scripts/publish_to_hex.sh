#!/usr/bin/env bash
#
# Publish bc_gitops to hex.pm
#
# Usage: ./scripts/publish_to_hex.sh
#
# Prerequisites:
#   - rebar3 installed
#   - hex authentication configured (rebar3 hex user auth)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_DIR"

echo "==> Checking prerequisites..."

# Check rebar3
if ! command -v rebar3 &> /dev/null; then
    echo "ERROR: rebar3 not found. Please install rebar3."
    exit 1
fi

# Check for uncommitted changes
if [[ -n $(git status --porcelain) ]]; then
    echo "ERROR: Uncommitted changes detected. Please commit or stash them first."
    git status --short
    exit 1
fi

echo "==> Running tests..."
rebar3 eunit

echo "==> Running dialyzer..."
rebar3 dialyzer || echo "WARNING: Dialyzer found issues (continuing anyway)"

echo "==> Building documentation..."
rebar3 ex_doc

echo "==> Building hex package..."
rebar3 hex build

echo ""
echo "==> Package built successfully!"
echo ""
echo "To publish, run:"
echo "  rebar3 hex publish"
echo ""
echo "To publish docs only:"
echo "  rebar3 hex publish docs"
echo ""
echo "NOTE: You must be authenticated with hex.pm"
echo "      Run 'rebar3 hex user auth' if needed"
