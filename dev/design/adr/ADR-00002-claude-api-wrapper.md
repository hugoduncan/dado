# 2. Claude API Wrapper Design

Date: 2024-11-12
Status: Proposed

## Context

We need a simple, reliable way to interact with Claude's API
endpoints. This polylith component should be a minimal wrapper that
other parts of the system can build upon. We want to keep this layer
simple and focused on API interaction only, allowing for:

- Possible future support of other AI APIs
- Separation of concerns between API interaction and higher-level orchestration
- Clear error handling and rate limiting at the API level

## Decision

1. Create a minimal wrapper under the dado.ai.claude namespace prefix
   - Direct mapping to Claude API endpoints
   - No additional abstraction layers
   - Focus on HTTP interaction only
   - Stateless

2. Use hato as HTTP client
   - Modern, maintained library
   - Good performance characteristics
   - Clean API design
   - Built-in async support
   - Type hints and spec support

3. Component Responsibilities
   - API endpoint interaction only
   - Request/response handling
   - Error handling
   - Rate limiting
   - Authentication management

4. Non-Responsibilities
   - Conversation management
   - Context handling
   - Response interpretation
   - Project-specific logic

## Consequences

### Positive
- Clear separation of concerns
- Simple testing and maintenance
- Easy to adapt to API changes
- Could support multiple AI providers
- Straightforward error handling

### Negative
- Other components need to handle higher-level orchestration
- May need to replicate some structure across different AI providers
- Need to carefully manage API credentials

### Neutral
- Need to establish conventions for error handling
- Need to document API response structures

## Notes

Initial endpoints needed:

- Message creation via `POST /v1/messages`

This should take a `config` map, with an `:api-key` entry.

It should have a `messages` argument, with a sequence of messages.

Other options can be passed via keywords.


- Response streaming
