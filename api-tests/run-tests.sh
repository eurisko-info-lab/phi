#!/bin/bash
# Run Phi API tests with Newman
# Prerequisites: npm install -g newman

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
COLLECTION="$SCRIPT_DIR/phi-api.postman_collection.json"

# Check if newman is installed
if ! command -v newman &> /dev/null; then
    echo "Newman not found. Install with: npm install -g newman"
    exit 1
fi

# Check if server is running
if ! curl -s http://localhost:8080/health > /dev/null; then
    echo "Error: Phi server is not running on localhost:8080"
    echo "Start it with: sbt 'runMain phi.PhiServer'"
    exit 1
fi

echo "Running Phi API tests..."
newman run "$COLLECTION" \
    --env-var "baseUrl=http://localhost:8080" \
    --reporters cli,json \
    --reporter-json-export "$SCRIPT_DIR/test-results.json"

echo ""
echo "Test results saved to: $SCRIPT_DIR/test-results.json"
