# Implementing an ADR

0. If the ADR is underspecified, then do not generate the component, and
ask for clarification instead.

1. ONLY implement functions/capabilities that are EXPLICITLY specified
in the ADR's "Decision" section

2. Do not add common utilities, helper functions, or "standard"
functionality unless specifically called for

3. If you think a common capability would be valuable (like a log!
function), ask whether it should be added rather than implementing it

4. When in doubt about whether something is required: - Quote the
specific part of the ADR that calls for it - If you can't find explicit
support in the ADR, ask first

5. The ADR should be treated as a complete specification. Any
functionality not explicitly mentioned should be assumed to be
deliberately excluded rather than accidentally omitted.

6. strictly follow the specific decisions and implementation details
documented in the ADR. Any design decisions not explicitly mentioned in
the ADR should be clarified with the user before implementation.

7. Avoid adding features, interfaces, protocols, models or abstractions
that aren't explicitly specified in the provided documents. If you think
additional functionality would be valuable, ask the user first rather
than implementing it.

8. When multiple documents provide implementation guidance, treat ADRs
as the primary source of truth for specific components they
describe. Other documents like style guides and preferred libraries
should inform how to implement what the ADR specifies, not what to
implement.

9. When implementing components, respect the boundaries and
responsibilities explicitly defined in the ADR. Do not add coordination
or abstraction layers unless specifically called for in the ADR.

10. If an ADR seems to conflict with general best practices or common
patterns, ask for clarification before proceeding with
implementation. It's better to pause and question than to make
assumptions.
