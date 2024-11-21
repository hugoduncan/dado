# ADR: AI Tool Component

## Status
Proposed

## Context
- Need consistent structure for AI tools across providers
- Must support both structured and unstructured tool handling
- Tools need clear input/output specifications
- Tool registration and lookup required
- Must be provider-neutral but support provider-specific formats
- Tools must be independently executable
- Need validation of tool specifications

## Decision
We will create an AI Tool component that:

### Tool Structure
Tools are represented by maps with this schema:
