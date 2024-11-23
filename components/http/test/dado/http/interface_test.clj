(ns dado.http.interface-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [dado.http.interface :as http]
   [hato.client :as hc]))

(deftest robust-request-test
  (when-not (str/blank? (System/getenv "LIVE-TESTS"))
    (let [request-fn (http/robust-request-fn hc/request)]
      (is (request-fn {:url "https://cnn.com"}))))

  (let [attempt-count (volatile! 0)
        mock-request  (fn [request]
                        (vswap! attempt-count inc)
                        {:uri          "https://cnn.com"
                         :status       529
                         :body         "Overloaded"
                         :headers      {"retry-after" "0"}
                         :version      :http-2
                         :request      request
                         :request-time 90})
        request-fn    (http/robust-request-fn mock-request)]
    (is (request-fn {:url "https://cnn.com" :method :get}))
    (is (< 1 @attempt-count)))

  (let [attempt-count (volatile! 0)
        mock-request  (fn [request]
                        (vswap! attempt-count inc)
                        {:uri          "https://cnn.com"
                         :status       529
                         :body         "Overloaded"
                         :headers      {"retry-after" "Wed, 21 Oct 2015 07:28:00 GMT"}
                         :version      :http-2
                         :request      request
                         :request-time 90})
        request-fn    (http/robust-request-fn mock-request)]
    (is (request-fn {:url "https://cnn.com" :method :get}))
    (is (< 1 @attempt-count))))
