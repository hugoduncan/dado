(ns dado.conversation-action.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.conversation-action.interface :as conversation-action]))

(deftest create-conversation-test
  (let [c-id (conversation-action/create-conversation!
              "refactoring"
              "claude")]
    (is (string? c-id))
    (let [resp (conversation-action/response! c-id "hello")]
      (is resp)
      (is (not resp)))))
