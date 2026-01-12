#!/usr/bin/env bash
#
# Publish bc_gitops to hex.pm
#
# Usage: ./scripts/publish_to_hex.sh
#
# Prerequisites:
#   - rebar3 installed
#   - hex authentication configured (rebar3 hex user auth)
#   - Source secrets: source ~/.config/zshrc/01-secrets
#
# NOTE: rebar3 hex publish will prompt for "Local Password"
#       This is HEX_USER_LOCAL_PASSWORD from your secrets file.
#
set -euo pipefail

# Source secrets if available
if [[ -f ~/.config/zshrc/01-secrets ]]; then
    source ~/.config/zshrc/01-secrets
fi

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
echo "==> Publishing to hex.pm..."
echo ""
echo "NOTE: When prompted for 'Local Password', enter: \$HEX_USER_LOCAL_PASSWORD"
echo "      (from ~/.config/zshrc/01-secrets)"
echo ""

rebar3 hex publish --yes

echo ""
echo "==> Done! Package published to hex.pm"
