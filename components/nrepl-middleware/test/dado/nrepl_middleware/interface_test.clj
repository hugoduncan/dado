(ns dado.nrepl-middleware.interface-test
  (:require
   [clojure.test :as test :refer :all]
   [dado.nrepl-middleware.interface :as nrepl-middleware]
   [nrepl.transport :as t]))

(deftest dado-chat-test
  (let [response  (volatile! nil)
        transport (reify nrepl.transport.Transport
                    (recv [_])
                    (send [_ msg]
                      (prn :send msg)
                      (vreset! response msg)))

        input           {:op           "dado/chat"
                         :message      "hello"
                         :agent-name   "test"
                         :ai-port-name "claude"
                         :transport    transport}
        unrecognised    (volatile! nil)
        unrecognised-fn (fn [msg] (vreset! unrecognised msg))
        completion      (nrepl-middleware/wrap-dado-chat unrecognised-fn)]
    (is (not @unrecognised))
    (is (not @response))
    (is  @response)
    (is (completion input))
    (is (not (completion input)))))
