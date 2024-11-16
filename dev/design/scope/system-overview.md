# DADO - AI-Assisted Development Tool

Dado uses the 4 level module of system development.

System development is in no way a linear progression through the 3
levels.  It is intended to be an iterative process that spans across
them all.

At the beginning of the system the design is vague.  As the project
progresses the level of detail and completeness increases.  This means
that documents and code will change over time.

As decisions are made, they are recorded in an ADR, an Architectural
Decision Record.  Despite the name this will be used for scope,
features, architecture and implementation.

The ADR's are one of the main vehicles for interacting with the AI.
They can be used as both input and output.  Apart from design, they
should also be used to document project standards.

The changes over time are recorded in version control.

AI message threads will be used to generate and refine design documents.
Existing documents are selectively provided to a message thread.
Different system contexts will be used for different design tasks. At a
minimum, there will be different system prompts used for each of the
three design levels.  There should also be ways of selecting documents
for as context for the AI.

AI interactions will recorded, so that at any time they can be resumed
and changed.

AI will also be used to generate documentation.  ADRs are intended to be
definitive and completem, whereas documentation can be used to provide a
simplified view from a single perspective (eg. end user documentation
doesn't need to the pros and cons of a particular implementation library
choice)

Changes to documents proposed by AI should have a variety of ways of
applying them, eg. automatic, accepting the whole of a change, or
partial acceptance.

The developer should be able to specify templates that are used by AI
when generating and improving documents.

The aim is to be able to use dado from the command line, from a clojure
REPL, or from emacs.


# Optional Features

- Quality assessment capabilities

- Risk management capabilities

- Progress visibility capabilities

- Decision analysis capabilities
