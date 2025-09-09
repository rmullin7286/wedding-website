#!/bin/bash

# Hot reload development server for the wedding website
# Watches for changes in app/ directory and automatically rebuilds and restarts the server

export WEDDING_DATABASE="$HOME/.local/share/wedding/wedding.db"
export WEDDING_PASSWORD="password"

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
log() {
    local color=$1
    shift
    echo -e "${color}[$(date '+%H:%M:%S')] $*${NC}"
}

# Check if fswatch is installed
if ! command -v fswatch &> /dev/null; then
    log $RED "fswatch is not installed. Please install it before running."
    exit 1
fi

# Kill any existing server process
cleanup() {
    if [ ! -z "$SERVER_PID" ]; then
        log $YELLOW "Stopping server (PID: $SERVER_PID)..."
        kill $SERVER_PID 2>/dev/null || true
        wait $SERVER_PID 2>/dev/null || true
    fi
}

# Set up cleanup on script exit
trap cleanup EXIT INT TERM

# Function to build and run the server
build_and_run() {
    log $BLUE "Building project..."
    
    if cabal build; then
        log $GREEN "Build successful!"
        
        # Kill existing server if running
        if [ ! -z "$SERVER_PID" ]; then
            log $YELLOW "Restarting server..."
            kill $SERVER_PID 2>/dev/null || true
            wait $SERVER_PID 2>/dev/null || true
        else
            log $BLUE "Starting server..."
        fi
        
        # Start the new server in background
        cabal run wedding -- --config-file server-local.yaml &
        SERVER_PID=$!
        
        log $GREEN "Server started with PID: $SERVER_PID"
        log $BLUE "Server running at http://localhost:8080"
        
    else
        log $RED "Build failed!"
    fi
}

log $BLUE "Starting hot reload development server..."
log $BLUE "Watching for changes in app/ directory..."

# Initial build and run
build_and_run

# Watch for file changes and rebuild
fswatch -o app/ | while read f; do
    log $YELLOW "Files changed, rebuilding..."
    build_and_run
done
