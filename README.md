# Wedding Website

A beautiful wedding website built with HTMX and Haskell, combining modern web interactions with functional programming elegance.

## Tech Stack

- **Frontend**: HTMX + HTML5 + CSS3
- **Backend**: Haskell with Servant framework
- **HTML Generation**: Lucid (type-safe HTML)
- **Database**: PostgreSQL
- **Styling**: Tailwind CSS

## Development Environment

This project was developed with the following tool versions (installed via ghcup):

- **GHC**: 9.12.2 (latest)
- **Cabal**: 3.12.1.0 (recommended)
- **HLS**: 2.10.0.0 (recommended)
- **ghcup**: 0.1.50.2

## Features

- Beautiful responsive design for all devices
- Hero section with wedding details
- Our Story timeline
- Wedding party showcase
- Venue information and schedule
- Dynamic RSVP system with HTMX
- Photo gallery
- Travel and accommodation info
- FAQ section
- Admin interface for managing RSVPs

## Getting Started

### Prerequisites

You'll need to install the Haskell toolchain using ghcup. This project was developed with specific versions for compatibility:

1. **Install ghcup** (if not already installed):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

2. **Install the recommended tool versions**:
   ```bash
   # Install GHC (Glasgow Haskell Compiler)
   ghcup install ghc 9.12.2
   ghcup set ghc 9.12.2
   
   # Install Cabal (build tool)
   ghcup install cabal 3.12.1.0
   ghcup set cabal 3.12.1.0
   
   # Install HLS (Haskell Language Server) for IDE support
   ghcup install hls 2.10.0.0
   ghcup set hls 2.10.0.0
   ```

3. **Verify your installation**:
   ```bash
   ghc --version    # Should show 9.12.2
   cabal --version  # Should show 3.12.1.0
   ```

### Building the Project

1. **Clone and navigate to the project**:
   ```bash
   cd /path/to/wedding-website
   ```

2. **Update package list**:
   ```bash
   cabal update
   ```

3. **Build the project**:
   ```bash
   cabal build
   ```
   
   This will:
   - Download and install all Haskell dependencies
   - Compile the wedding website library and executable
   - Report any compilation errors

4. **Build in watch mode** (optional, for development):
   ```bash
   # Install ghcid for automatic rebuilding
   cabal install ghcid
   
   # Run with auto-rebuild on file changes
   ghcid --command="cabal repl wedding-website"
   ```

### Running the Application

1. **Start the web server**:
   ```bash
   cabal run wedding-website
   ```
   
   You should see output like:
   ```
   Wedding website starting on http://localhost:8080
   ```

2. **Visit the website**: Open http://localhost:8080 in your browser

3. **Stop the server**: Press `Ctrl+C` in the terminal

### Development Workflow

1. **Make code changes** in the `src/` directory
2. **Rebuild** with `cabal build` to check for errors
3. **Restart the server** with `cabal run wedding-website`
4. **Refresh your browser** to see changes

### Troubleshooting

**Build Issues:**
- If you get dependency resolution errors, try `cabal clean` then `cabal build`
- Make sure you're using the correct GHC version: `ghcup set ghc 9.12.2`
- Update cabal package list: `cabal update`

**Runtime Issues:**
- Port 8080 already in use? The server will show an error. Kill other processes using port 8080
- Static assets not loading? Make sure the `static/` directory exists and contains `css/style.css`

**IDE Setup:**
- VS Code: Install the Haskell extension
- Emacs: Use haskell-mode with lsp-haskell
- Vim/Neovim: Use coc-haskell or built-in LSP

### Project Commands Reference

```bash
# Build the project
cabal build

# Run the application
cabal run wedding-website

# Run tests (when implemented)
cabal test

# Generate documentation
cabal haddock

# Clean build artifacts
cabal clean

# Start a REPL for development
cabal repl wedding-website

# Check code with GHC warnings
cabal build --ghc-options="-Wall -Werror"

# Profile the application (for performance)
cabal run wedding-website --enable-profiling

# Check dependencies
cabal freeze
```

## Project Structure

```
wedding-website/
├── app/
│   └── Main.hs              # Application entry point
├── src/                     # Source code (to be created)
│   ├── Wedding/
│   │   ├── Server.hs        # Servant server configuration
│   │   ├── Pages/           # HTML page generators
│   │   ├── Database/        # Database models and queries
│   │   └── Types.hs         # Data types
├── static/                  # Static assets (CSS, JS, images)
├── wedding-website.cabal    # Cabal project file
└── README.md               # This file
```

## Development

Always run `cabal build` after making changes to ensure code compiles correctly.

## License

BSD-3-Clause (see LICENSE file)