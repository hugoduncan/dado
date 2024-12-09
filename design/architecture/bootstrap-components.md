Looking at the current architecture, for the initial goal of enabling Claude interaction at the REPL, we would need to implement:

Core components (minimal functionality):

Project Configuration (for API keys and storage locations)
AI Session Manager (basic session state)
AI Message Thread Manager (simple message history)
AI Context Manager (basic system prompts)


Ports and Adapters:

Storage Port & File System Adapter (EDN storage)
AI Port & Claude Adapter (basic API interaction)
