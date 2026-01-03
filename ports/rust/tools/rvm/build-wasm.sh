#!/bin/bash
# Build RosettaVM for WebAssembly

set -e

# Check for wasm-pack
if ! command -v wasm-pack &> /dev/null; then
    echo "Installing wasm-pack..."
    cargo install wasm-pack
fi

# Build WASM package
echo "Building RosettaVM for WebAssembly..."
wasm-pack build --target web --features wasm --no-default-features --release

# Output location
echo ""
echo "✅ Build complete!"
echo "Output in: pkg/"
echo ""
echo "Files:"
ls -la pkg/

echo ""
echo "To use in the playground, copy pkg/ to docs/wasm/"
echo "Then import in JavaScript:"
echo ""
echo "  import init, { evaluate, compile_to } from './wasm/rosettavm.js';"
echo "  await init();"
echo "  const result = evaluate('main = 1 + 2');"
