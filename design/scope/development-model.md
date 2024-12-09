Dado imposes a conceptual design that is split into three levels of
specificity; scope, architecture and implementation.

Each document driving the development process clearly belongs to just
one of these levels.

# 3 Level specificity model

## Scope Level

The scope level describes the goals and features of the system without
mentioning the architecture or implementation of the system.

The goal of the scope level is to have a description of the scope and
features.  Explicitly limiting the scope is important, either by
explicitly excluding features or deferring them to later.

The interaction between features should be minimised.

## Architecture level

The architecture level describes the decomposition of the system into
sub-systems, components, libraries, tools, etc.

It can describe the tech stack to be used in terms of the technologies
(but not in terms of concrete products), e.g use a relational database
rather than use Postgres.

It does not describe implementation.

It does not change or refine the project scope and features.

## Implementation level

This is the actual implementation of the system as constrained by the
architecture and scope levels.


# Complementary development models

The three level model does not preclude the use of Domain Driven
Development (DDD) or Behavior-Driven Development (BDD).

DDD could be used at the architectural or implementation levels.

BDD could be used at all three levels.

The 4+1 Process view is arguably missing from the 3 level model, but
these concerns should be include in the scope level.  The Scenarios view
can become part of the tests at the the appropriate level of the 3 level
model.
