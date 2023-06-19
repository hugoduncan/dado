(ns org.hugoduncan.dado.chat-gpt.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [org.hugoduncan.dado.chat-gpt.core :as chat-gpt-core]))

(deftest parse-completion-string-test
  (let [completion-string "Here is some explanation.
  ```
  (inc 1)
  ```

  And some more explanation.
  ```clojure
  (inc 2)
  ```

  And some elisp.
  ```elisp
  (inc 3)
  ```

  "
        expected-result [{:text "Here is some explanation."}
                         {:code "(inc 1)"}
                         {:text "And some more explanation."}
                         {:code "(inc 2)"}
                         {:text "And some elisp."}
                         {:code "(inc 3)"}]]
    (is (= expected-result
           (chat-gpt-core/parse-completion-string completion-string))))
  (let [completion-string "```
  (inc 1)
  ```
  Code first.
  "
        expected-result   [{:code "(inc 1)"}
                           {:text "Code first."}]]
    (is (= expected-result
           (chat-gpt-core/parse-completion-string completion-string)))))
