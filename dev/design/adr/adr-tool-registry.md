# ADR: Dado Tool Registry Component

## Status
Proposed

## Context
- Need a discoverable registry of tools for project maintenance actions
- Tools need to be easy to add without modifying core code
- Tools need to support different programming languages
- Tools should be clearly documented
- Tools should validate their inputs
- Tools should have consistent error handling
- Tools are different from AI Tools which are used with AI providers

## Decision
We will create a Tool Registry that:

### Tool Structure
A tool is defined by a map with the following schema:
