(ns dado.ai.tools.matching-file.config
  "Configuration constants for matching file tool."
  (:require [malli.core :as m]))

(def Config
  "Configuration schema for matching file tool"
  [:map])

(def config? (m/validator Config))
