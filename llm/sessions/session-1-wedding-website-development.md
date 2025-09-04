# Wedding Website Development Session 1

## Project Overview
Building a beautiful wedding website for Ryan & Shae's wedding on **May 30, 2026** using:
- **Backend**: Haskell, Warp/Servant web server, Lucid for HTML generation
- **Frontend**: Bootstrap 5, elegant typography, potential HTMX for future dynamic content
- **Database**: SQLite with sqlite-simple
- **Venue**: Trillium Nursery, 8335 196th Ave NE, Redmond, WA 98053

## Wedding Color Palette
CSS variables defined in `static/style.css`:
- **Primary (Periwinkle)**: `rgba(119,129,171,255)`
- **Secondary (Wine Red)**: `rgba(122,39,61,255)` 
- **Neutral (Sage)**: `rgba(133,150,127,255)`
- **Light Text**: `rgba(255,255,255,0.95)`
- **Light Text Muted**: `rgba(255,255,255,0.7)`

## Typography
- **Headings**: Playfair Display (elegant serif for romantic feel)
- **Body Text**: Crimson Text (readable serif that pairs well)
- Both imported from Google Fonts

## Project Structure

### Core Modules
```
app/
├── Main.hs                           # Entry point with CLI parsing
├── Wedding/
│   ├── CLI.hs                        # Command line argument parsing
│   ├── Server.hs                     # Servant API and server
│   ├── DB.hs                         # Database types (Attendee, Group)
│   ├── Component/
│   │   ├── BasePage.hs              # Base HTML template
│   │   └── NavBar.hs                # Bootstrap navbar component
│   ├── Html/
│   │   └── Attributes.hs            # Custom HTML attributes for Bootstrap
│   └── Page/
│       ├── Home.hs                  # Homepage with hero, when/where, RSVP sections
│       └── RSVP.hs                  # RSVP form page
└── static/
    ├── style.css                    # Wedding-themed CSS
    └── image/
        └── cover.jpg                # Hero background image
```

### Dependencies (wedding.cabal)
```haskell
build-depends:
  base ^>=4.18.3.0,
  http-api-data,           # Form parsing
  http-types,              # HTTP utilities  
  lucid,                   # HTML DSL
  optparse-applicative,    # CLI parsing
  servant,                 # Web framework
  servant-lucid,           # Lucid integration
  servant-server,          # Server implementation
  sqlite-simple,           # SQLite interface
  text,                    # Text handling
  warp                     # HTTP server
```

## Implemented Features

### 1. Hot Reload Development (`dev-run.sh`)
- Watches `app/` for file changes using `fswatch`
- Automatically rebuilds and restarts server
- Colored logging for build status
- Usage: `./dev-run.sh`

### 2. Responsive Navbar
- **Location**: `Wedding.Component.NavBar`
- **Features**: Bootstrap 5 responsive navbar with wedding colors
- **Styling**: Periwinkle background, white text, wine red hover effects
- **Links**: Home, When & Where (anchor), RSVP
- **Mobile**: Hamburger menu with custom white icon

### 3. Homepage Sections

#### Hero Section
- Full viewport height with cover photo background
- Couple names "Ryan & Shae" in Playfair Display
- Wedding date "May 30, 2026" 
- Text positioned in upper third to avoid covering faces
- Dark overlay for text readability

#### When & Where Section  
- **Background**: Periwinkle with white text
- **Layout**: Two-column on desktop, stacked on mobile
- **When**: Saturday, May 30th, 2026 (times TBD)
- **Where**: Trillium Nursery with Google Maps integration
- **Button**: Wine red "View on Google Maps" with hover effects

#### RSVP Section
- **Background**: Wine red with elegant messaging
- **Call-to-Action**: Periwinkle RSVP button
- **Text**: "We hope you'll join us on this special day..."

### 4. RSVP Functionality
- **Landing Page**: Clean form for guest name input
- **POST Endpoint**: Handles form submission with redirect
- **Pattern**: POST-Redirect-GET for proper form handling
- **Form Data**: `RSVPFormData` with `FromForm` instance
- **Future**: Will lookup guests and redirect to personalized RSVP

### 5. Custom HTML Attributes (`Wedding.Html.Attributes`)
- Bootstrap data attributes: `dataBsToggle_`, `dataBsTarget_`
- ARIA attributes: `ariaControls_`, `ariaExpanded_`, `ariaLabel_`
- Uses Lucid's `makeAttribute` function

### 6. Command Line Interface (`Wedding.CLI`)
- **Library**: optparse-applicative
- **Required Arg**: `--config-file FILE` for YAML configuration
- **Help Text**: Comprehensive program description
- **Usage**: `cabal run wedding -- --config-file config.yaml`

## Database Design (Planned)

### Tables
```sql
-- Groups for families/couples  
CREATE TABLE groups (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL
);

-- Individual attendees
CREATE TABLE attendees (
    id INTEGER PRIMARY KEY,
    group_id INTEGER REFERENCES groups(id),
    name TEXT NOT NULL,
    attending BOOLEAN,
    dietary_restrictions TEXT
);
```

### SQLite Configuration
- **Connection Pattern**: Single long-lived connection (safe for low-traffic wedding site)
- **WAL Mode**: `PRAGMA journal_mode=WAL` for better concurrency
- **Foreign Keys**: `PRAGMA foreign_keys=ON` for referential integrity
- **Thread Safety**: SQLite handles synchronization in serialized mode

## Git Repository
- **Initialized**: Clean .gitignore for Haskell, macOS, editor files
- **Commits**: Well-structured commit history with descriptive messages
- **Branches**: Working on master branch

## API Routes

### Current Routes
```haskell
type WeddingAPI =
  Get '[HTML] (Html ())                                          -- GET /     (Home)
  :<|> "rsvp" :> Get '[HTML] (Html ())                          -- GET /rsvp (RSVP form)
  :<|> "rsvp" :> ReqBody '[FormUrlEncoded] RSVPFormData         -- POST /rsvp (Form submission)
             :> Post '[PlainText] NoContent
  :<|> "static" :> Raw                                          -- Static assets
```

### Planned Routes
- `GET /rsvp/guest/:name` - Personalized RSVP page after name lookup
- Database integration for guest management
- Admin interface for viewing RSVPs

## Development Workflow

### Running the Server
```bash
# Development with hot reload
./dev-run.sh

# Manual run
cabal run wedding -- --config-file server.yaml

# Build only
cabal build
```

### Key URLs
- **Homepage**: http://localhost:8080
- **RSVP Form**: http://localhost:8080/rsvp
- **Static Assets**: http://localhost:8080/static/

## Next Steps (TODO)
1. **YAML Configuration**: Load server settings from config file
2. **Database Integration**: SQLite connection and guest data management  
3. **Guest Lookup**: Implement name-based guest lookup for personalized RSVP
4. **Admin Interface**: View and manage RSVPs
5. **Email Integration**: RSVP confirmations
6. **Additional Pages**: Accommodations, Registry, Photos

## Design Inspiration
- **Reference Site**: https://rey-fluid-demo.squarespace.com/
- **Aesthetic**: Minimalist, elegant, romantic
- **Layout**: Clean typography, generous whitespace, responsive grid

## Technical Decisions

### Why Haskell/Servant?
- Type safety for web development
- Excellent HTML generation with Lucid
- Clean, functional approach to web APIs

### Why SQLite?
- Perfect for wedding website scale (low traffic, simple data)
- Single-file database, easy deployment
- Excellent for read-heavy workloads

### Why Bootstrap?
- Rapid development with consistent components
- Excellent responsive design out-of-box
- Easy customization with CSS variables

## Performance Considerations
- Single SQLite connection shared across threads (safe for low concurrency)
- Static asset serving via Warp
- Minimal JavaScript (Bootstrap only)
- Optimized CSS with wedding color variables

This session established the foundation for a beautiful, functional wedding website with elegant design, solid architecture, and room for growth.