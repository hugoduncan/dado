(ns dado.patch.core.common
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [taoensso.telemere :as t]))

(defn write-changes!
  "Write changes to filesystem, returns true on success"
  [op-info new-content]
  (t/trace!
   {:id   :dado.patch/write-file
    :data {:op op-info :content new-content}}
   (try
     (let [{:keys [op target-path source-path]} op-info]
       (case op
         (:edit :create)
         (do
           (when (= op :create)
             (fs/create-dirs (fs/parent target-path)))
           (let [temp-path (str target-path ".tmp")]
             (spit temp-path new-content)
             (fs/move temp-path target-path {:replace-existing true}))
           (t/event! (if (= op :create)
                       :patch/file-created
                       :patch/applied)
                     {:level :debug :path target-path}))

         :delete
         (do
           (fs/delete target-path)
           (t/event! :patch/file-deleted
                     {:level :debug :path target-path}))

         :move
         (do
           (fs/create-dirs (fs/parent target-path))
           (fs/move source-path target-path)
           (t/event! :patch/file-moved
                     {:level  :debug
                      :source source-path
                      :target target-path}))

         :copy
         (do
           (fs/create-dirs (fs/parent target-path))
           (fs/copy source-path target-path)
           (t/event! :patch/file-copied
                     {:level  :debug
                      :source source-path
                      :target target-path})))
       true)

     (catch Exception e
       (throw (ex-info "Failed to write changes"
                       {:type    :error/file-access
                        :context {:component "dado.patch"
                                  :op        op-info}
                        :cause   e}))))))
