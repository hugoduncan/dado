(ns org.hugoduncan.dado.clojure-target.core
  (:require
   [clojure.java.io :as io]
   [clojure.repl :as repl])
  (:import
   (java.io FileInputStream
            InputStreamReader
            LineNumberReader
            PushbackReader)))

(defn source-from-file-fn
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the filesystem.  Return nil if
  it can't find the source."
  [x]
  (when-let [v (resolve x)]
    (when-let [filepath (:file (meta v))]
      (when-let [strm (.exists (io/file  filepath))]
        (with-open [rdr (-> filepath
                            io/file
                            FileInputStream.
                            InputStreamReader.
                            LineNumberReader.)]
          (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
          (let [text      (StringBuilder.)
                pbr       (proxy [PushbackReader] [rdr]
                            (read [] (let [i (proxy-super read)]
                                       (.append text (char i))
                                       i)))
                read-opts (if (.endsWith ^String filepath "cljc") {:read-cond :allow} {})]
            (if (= :unknown *read-eval*)
              (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
              (read read-opts (PushbackReader. pbr)))
            (str text)))))))

(defn source-fn [x]
  (or (repl/source-fn x)
      (source-from-file-fn x)))
