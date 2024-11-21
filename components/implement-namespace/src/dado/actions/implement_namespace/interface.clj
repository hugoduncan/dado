(ns dado.actions.implement-namespace.interface
  "Component for generating polylith component implementations from ADR specifications"
  (:require [dado.actions.implement-namespace.core :as core]))

(defn execute
  "Implements a polylith component from an ADR specification.
   Returns map of implementation results.

   Arguments:
   - config: Project configuration map
   - adr-name: Name of ADR file
   - options: Map of implementation options
     :mode - :interactive or :non-interactive (default :interactive)
     :allow-overwrite - Allow overwriting existing files (default false)

   Returns implementation results map containing:
   - :component-name - Name of created component
   - :component-path - Path to component directory
   - :files-created - List of created files
   - :namespaces-created - List of created namespaces
   - :dependencies - List of component dependencies
   - :interface-ns - Generated interface namespace

   Throws :error/implement-namespace for implementation failures"
  [config adr-name options]
  (core/execute config adr-name options))
