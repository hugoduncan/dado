(ns org.hugoduncan.dado.elisp-feature.core
  "Assistant functions for authoring elisp features."
  (:require
   [org.hugoduncan.dado.chat-gpt.interface :as chat-gpt]
   [org.hugoduncan.dado.elisp-target.interface :as elisp-target]
   [org.hugoduncan.dado.operation.interface :as operation]))

(def critique-feature-system
  "You will critique the elsip feature given by the user.

Compare the doc comments against the implementation of the feature, and comment
on any inconsistencies.

Comment on doc comments that could be improved.

Comment on style and elisp idioms.

Comment on feature naming.

Comment on function naming.

Comment on variable naming.

Comment on function and feature complexity.

Comment on function and feature size.

Suggest improvements.

Critique the elisp feature given by the user.")

(defn critique-feature
  "Critique the feature with the given source."
  [feature-source]
  (->> feature-source
       (chat-gpt/generate-completion critique-feature-system)
       elisp-target/format-parsed-completion-as-code))

(defmethod operation/op ["elisp" "critique-ns"]
  [_language _action args _ns]
  {"suggestion" (critique-feature (:feature-source args))})
