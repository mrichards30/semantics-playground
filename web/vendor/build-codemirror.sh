#!/bin/sh
# Regenerates codemirror.js: the CodeMirror 6 packages the program editor
# uses, bundled into one script that defines a global `CM`.
# Run from anywhere; needs npm. The output is committed.
set -e
here=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
cd "$tmp"
npm init -y >/dev/null
npm install --silent --no-audit --no-fund \
  @codemirror/state@6.5.2 @codemirror/view@6.38.6 @codemirror/commands@6.10.0 \
  @codemirror/autocomplete@6.19.1 @codemirror/lint@6.9.1 esbuild@0.25.11
cat > entry.js <<'JS'
export * as state from "@codemirror/state";
export * as view from "@codemirror/view";
export * as commands from "@codemirror/commands";
export * as autocomplete from "@codemirror/autocomplete";
export * as lint from "@codemirror/lint";
JS
{
  echo "/* CodeMirror 6 (MIT licence, https://codemirror.net), bundled by build-codemirror.sh */"
  npx esbuild entry.js --bundle --minify --format=iife --global-name=CM --log-level=warning
} > "$here/codemirror.js"
rm -rf "$tmp"
