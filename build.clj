(ns build
  "build script for dado.

   Targets:
   * jar :project PROJECT
     - creates an jar for the given project

   For help, run:
     clojure -A:deps -T:build help/doc

   Create jar for middleware:
     clojure -T:build jar :project middleware"
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]
   [clojure.tools.deps :as t]
   [clojure.tools.deps.util.dir :refer [with-dir]]
   [clojure.string :as str]))

(defn- get-project-aliases []
  (let [edn-fn (juxt :root-edn :project-edn)]
    (-> (t/find-edn-maps)
        (edn-fn)
        (t/merge-edns)
        :aliases)))

(defn- ensure-project-root
  "Given a task name and a project name, ensure the project
   exists and seems valid, and return the absolute path to it."
  [task project]
  (let [project-root (str (System/getProperty "user.dir") "/projects/" project)]
    (when-not (and project
                   (.exists (io/file project-root))
                   (.exists (io/file (str project-root "/deps.edn"))))
      (throw
       (ex-info
        (str task " task requires a valid :project option")
        {:project project})))
    project-root))

(def version (format "0.1.%s" (b/git-count-revs nil)))

(defn- project-info [project]
  (let [project-root (ensure-project-root "jar" project)
        aliases      (with-dir (io/file project-root) (get-project-aliases))
        lib          (-> aliases :jar :lib)]
    {:project-root project-root
     :aliasesa     aliases
     :lib          lib}))

(defn- ensure-lib! [project aliases lib]
  (when-not lib
    (throw
     (ex-info
      (str "the " project " project's deps.edn file"
           "does not specify the :lib name in its :jar alias")
      {:aliases aliases}))))


(defn default-target
  "Return the default target directory name."
  {:arglists '([])}
  ([] (default-target nil))
  ([target]
   (or target "target")))

(defn default-basis
  "Return the default basis."
  {:arglists '([])}
  ([] (default-basis nil))
  ([basis]
   (or basis (b/create-basis {}))))

(defn default-class-dir
  "Return the default `class-dir`.
  May be passed a non-default target directory name."
  {:arglists '([] [target])}
  ([] (default-class-dir nil nil))
  ([target] (default-class-dir nil target))
  ([class-dir target]
   (or class-dir (str (default-target target) "/classes"))))

(defn default-jar-file
  "Given the `lib` and `version`, return the default JAR
  filename.
  `lib` can be omitted and will default to `'application`
  (for uberjar usage).
  May be passed a non-default target directory name."
  ([version] (default-jar-file nil nil version))
  ([lib version] (default-jar-file nil lib version))
  ([target lib version]
   (format "%s/%s-%s.jar" (default-target target) (name (or lib 'application)) version)))

(defn- lifted-basis
  "This creates a basis where source deps have their primary
  external dependencies lifted to the top-level, such as is
  needed by Polylith and possibly other monorepo setups."
  []
  (let [default-libs (:libs (b/create-basis))
        source-dep?  #(not (:mvn/version (get default-libs %)))
        lifted-deps
        (reduce-kv (fn [deps lib {:keys [dependents] :as coords}]
                     (if (and (contains? coords :mvn/version) (some source-dep? dependents))
                       (assoc deps lib (select-keys coords [:mvn/version :exclusions]))
                       deps))
                   {}
                   default-libs)]
    (-> (b/create-basis {:extra {:deps lifted-deps}})
        (update :libs #(into {} (filter (comp :mvn/version val)) %)))))

(defn- jar-opts
  "Provide sane defaults for jar/uber tasks.
  :lib is required, :version is optional for uber, everything
  else is optional."
  [{:keys [basis class-dir conflict-handlers jar-file lib
           main ns-compile resource-dirs scm sort src-dirs tag
           target transitive uber-file version]
    :as   opts}]
  (when transitive
    (assert (nil? basis) ":transitive cannot be true when :basis is provided"))
  (let [basis       (if transitive
                      (lifted-basis)
                      (default-basis basis))
        directory?  #(let [f (java.io.File. %)]
                       (and (.exists f) (.isDirectory f)))
        scm-default (cond tag     {:tag tag}
                          version {:tag (str "v" version)})
        src-default (or src-dirs ["src"])
        version     (or version "standalone")
        xxx-file    (default-jar-file target lib version)]
    (assoc opts
           :basis      (default-basis basis)
           :class-dir  (default-class-dir class-dir target)
           :conflict-handlers conflict-handlers
           :jar-file   (or    jar-file    xxx-file)
           :ns-compile (or    ns-compile  (when (and main (not sort))
                                            [main]))
           :scm        (merge scm-default scm)
           :src-dirs   src-default
           :src+dirs   (if transitive
                         (filter directory? (:classpath-roots basis))
                         (into src-default
                               (or resource-dirs ["resources"])))
           :uber-file  (or    uber-file   xxx-file))))

(defn jar*
  "Build the library JAR file.
      Requires: :lib, :version
  Accepts any options that are accepted by:
  * tools.build/write-pom
  * tools.build/jar
  Writes pom.xml into META-INF in the :class-dir, then
  copies :src-dirs + :resource-dirs into :class-dir, then
  builds :jar-file into :target (directory).
  If you are building a JAR in a monorepo and rely on
  :local/root dependencies for the actual source components,
  such as in a Polylith project, pass :transitive true to
  use a 'lifted' basis and to ensure all source files are
  copied into the JAR."
  {:arglists '([{:keys [lib version
                        basis class-dir jar-file main manifest repos
                        resource-dirs scm src-dirs src-pom tag target
                        transitive]}])}
  [{:keys [lib version] :as opts}]
  (assert (and lib version) "lib and version are required for jar")
  (let [{:keys [class-dir jar-file src+dirs] :as opts}
        (jar-opts opts)
        current-dir (System/getProperty "user.dir")
        current-rel #(str/replace % (str current-dir "/") "")]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)
    (println "Copying" (str (str/join ", " (map current-rel src+dirs)) "..."))
    (b/copy-dir {:src-dirs   src+dirs
                 :target-dir class-dir})
    (println "Building jar" (str jar-file "..."))
    (b/jar opts)
    (b/delete {:path class-dir})
    (b/write-pom opts))
  opts)

(defn jar
  "Builds an jar for the specified project.

   Options:
   * :project - required, the name of the project to build,
   * :jar-file - optional, the path of the JAR file to build,
     relative to the project folder; can also be specified in
     the :jar alias in the project's deps.edn file; will
     default to target/PROJECT.jar if not specified.

   Returns:
   * the input opts with :class-dir, :compile-opts, :main, and :jar-file
     computed.

   The project's deps.edn file must contain a :jar alias."
  [{:keys [project jar-file] :as opts}]
  (let [{:keys [project-root aliases lib]} (project-info project)]
    (ensure-lib! project aliases lib)
    (binding [b/*project-root* project-root]
      (let [class-dir "target/classes"
            jar-file  (or jar-file
                          (-> aliases :jar :jar-file)
                          (str "target/" (name lib) "-" version ".jar"))
            opts      (merge opts
                             {:class-dir    class-dir
                              :compile-opts {:direct-linking true}
                              :jar-file     jar-file
                              :lib          lib
                              :version      version
                              :transitive   true})]
        (b/delete {:path class-dir})
        (jar* opts)
        (println "Jar is built.")
        opts))))


(defn install*
  "Install the JAR to the local Maven repo cache.
  Requires: :lib, :version
  Accepts any options that are accepted by:
  * `tools.build/install`"
  {:arglists '([{:keys [lib version
                        basis class-dir classifier jar-file target]}])}
  [{:keys [lib version basis class-dir classifier jar-file target] :as opts}]
  (assert (and lib version) ":lib and :version are required for install")
  (let [target (default-target target)]
    (b/install {:basis      (default-basis basis)
                :lib        lib
                :classifier classifier
                :version    version
                :jar-file   (or jar-file (default-jar-file target lib version))
                :class-dir  (default-class-dir class-dir target)})
    opts))

(defn install
  "Install the JAR locally."
  [{:keys [project jar-file] :as opts}]
  (let [{:keys [project-root aliases lib]} (project-info project)]
    (ensure-lib! project aliases lib)
    (binding [b/*project-root* project-root]
      (-> opts
          (assoc :lib lib :version version)
          (install*)))))

(defn deploy*
  "Deploy the JAR to Clojars.
  Requires: :lib, :version
  Accepts any options that are accepted by:
  * `deps-deploy/deploy`
  If :artifact is provided, it will be used for the deploy,
  else :jar-file will be used (making it easy to thread
  options through `jar` and `deploy`, specifying just :jar-file
  or relying on the default value computed for :jar-file)."
  {:arglists '([{:keys [lib version
                        artifact class-dir installer jar-file pom-file target]}])}
  [{:keys [lib version class-dir installer jar-file target] :as opts}]
  (assert (and lib version) ":lib and :version are required for deploy")
  (when (and installer (not= :remote installer))
    (println ":installer" installer "is deprecated -- use install task for local deployment"))
  (let [target    (default-target target)
        class-dir (default-class-dir class-dir target)
        jar-file  (or jar-file (default-jar-file target lib version))
        dd-deploy (try (requiring-resolve 'deps-deploy.deps-deploy/deploy) (catch Throwable _))]
    (if dd-deploy
      (dd-deploy (merge {:installer :remote :artifact (b/resolve-path jar-file)
                         :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}
                        opts))
      (throw (ex-info "deps-deploy is not available in the 'slim' build-clj" {}))))
  opts)

(defn deploy
  "Deploy the JAR to Clojars."
  [{:keys [project jar-file] :as opts}]
  (let [{:keys [project-root aliases lib]} (project-info project)]
    (ensure-lib! project aliases lib)
    (binding [b/*project-root* project-root]
      (-> opts
          (assoc :lib lib :version version)
          (deploy*)))))
