# ADR: AI Refactoring Agent

## Status
Proposed

## Context
- Need a dedicated AI agent for code refactoring
- Must integrate with existing AI Agent component
- Need specialized prompts and context for refactoring
- Must support interactive refactoring conversations
- Should handle both high-level and detailed refactoring
- Must manage appropriate file context during refactoring
- Should use FOD format for code changes

## Decision
We will create a Refactoring Agent component that:
- Provides a single function to create a refactoring agent
- Includes specialized prompts for refactoring guidance
- Manages context loading for refactoring operations
- Uses the dado.ai.agents.refactoring namespace
- Stores its prompt template in resources

### Component Interface
