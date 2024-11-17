## Core Components

### 1. Dev Directory Management
- Standard directory structure for project artifacts
- Location configurable (default: `dev/`)
```
dev/
  ├── ai/              # AI interaction records
  │   ├── threads/     # Conversation threads
  │   └── context/     # AI session context
  ├── adr/             # Architectural Decision Records
  └── context/         # Project context documents
      ├── standards/   # Project standards
      ├── overview/    # Project documentation
      └── templates/   # Project templates
```

### 2. AI Interaction Components
#### Claude API Wrapper (`dado.ai.claude`)
- Minimal HTTP wrapper over Claude API
- Uses hato for HTTP client
- Handles authentication, rate limiting, errors
- Separates API interaction from orchestration

#### Thread Management
- Conversation threading system
- Context preservation between iterations
- Thread persistence
- Multi-thread analysis for ADRs

### 3. Documentation Management
- ADR generation and management
- Doc string and comment maintenance
- Project context documents
- Documentation evolution tracking

### 4. Emacs Interface
- CIDER/REPL integration
- Dedicated interaction buffer
- Change preview and application UI
- Command system
