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
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]
   [clojure.tools.deps :as t]
   [clojure.tools.deps.util.dir :refer [with-dir]]
   [clojure.string :as str]))

(def version (format "0.1.%s" (b/git-count-revs nil)))

(defn- valid-project-root
  "Return the directory name for the given polylith project name.
  If the directory does not contain a deps.edn, return nil."
  [project]
  (assert project)
  (let [cwd          (System/getProperty "user.dir")
        project-root (io/file cwd "projects" (name project))]
    (when (and project
               (.exists project-root)
               (.exists (io/file project-root "deps.edn")))
      project-root)))

(defn- ensure-project-root-for-task!
  "Given a task name and a project name, ensure the polylith project
  exists and seems valid, and return the absolute path to it."
  [project task]
  (if-let [project-root (valid-project-root project)]
    project-root
    (throw
     (ex-info
      (str task " task requires a valid :project option")
      {:project project}))))

(defn- get-project-aliases []
  (let [all-maps (juxt :root-edn :project-edn)]
    (-> (t/find-edn-maps)
        all-maps
        (t/merge-edns)
        :aliases)))

(defn- project-info
  "Return info for the polylith `project` in the current repository."
  [project]
  (let [project-root (ensure-project-root-for-task! project "jar")
        aliases      (with-dir project-root (get-project-aliases))
        lib          (-> aliases :jar :lib)]
    {:project-root project-root
     :aliases      aliases
     :lib          lib}))

(defn- ensure-lib-in-project-deps-edn! [project aliases lib]
  (when-not lib
    (throw
     (ex-info
      (str "the " project " project's deps.edn file"
           "does not specify the :lib name in its :jar alias")
      {:aliases aliases}))))

(defn default-target
  "Return the default target directory name."
  []
  "target")

(defn default-basis
  "Return the default basis."
  []
  (b/create-basis {}))

(defn default-class-dir
  "Return the default `class-dir`."
  [target]
  (io/file target "classes"))

(defn jar-file-name
  "Given the `lib` and `version`, return the default JAR filename."
  [lib version]
  (format "%s-%s.jar" (name lib) version))

(defn- lifted-basis
  "Return a basis where source deps have their primary
  external dependencies lifted to the top-level, such as is
  needed by Polylith and possibly other monorepo setups."
  []
  (let [default-libs (:libs (b/create-basis))
        source-dep?  #(not (:mvn/version (get default-libs %)))
        lifted-deps
        (reduce-kv
         (fn [deps lib {:keys [dependents] :as coords}]
           (if (and (contains? coords :mvn/version)
                    (some source-dep? dependents))
             (assoc deps lib (select-keys coords [:mvn/version :exclusions]))
             deps))
         {}
         default-libs)]
    (-> (b/create-basis {:extra {:deps lifted-deps}})
        (update :libs #(into {} (filter (comp :mvn/version val)) %)))))

(defn- directory?
  [p]
  (let [f (io/file p)]
    (and (.exists f) (.isDirectory f))))

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
  (let [basis         (if transitive
                        (lifted-basis)
                        (or basis (default-basis)))
        src-dirs      (or src-dirs ["src"])
        resource-dirs (or resource-dirs ["resources"])
        target        (or target (default-target))
        class-dir     (or class-dir (default-class-dir target))
        xxx-file      (io/file target (jar-file-name lib version))
        jar-file      (or jar-file xxx-file)
        uber-file     (or uber-file xxx-file)
        scm-default   (cond tag     {:tag tag}
                            version {:tag (str "v" version)})
        scm           (merge scm-default scm)]
    (assoc opts
           :basis      basis
           :class-dir  class-dir
           :conflict-handlers conflict-handlers
           :jar-file   jar-file
           :ns-compile (or ns-compile (when (and main (not sort))
                                        [main]))
           :scm        scm
           :src-dirs   src-dirs
           :src+dirs   (if transitive
                         (filter directory? (:classpath-roots basis))
                         (into src-dirs resource-dirs))
           :uber-file  uber-file)))

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
    (ensure-lib-in-project-deps-edn! project aliases lib)
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
  (let [target (or target (default-target))]
    (b/install {:basis      (or basis (default-basis))
                :lib        lib
                :classifier classifier
                :version    version
                :jar-file   (or jar-file
                                (str (io/file target (jar-file-name lib version))))
                :class-dir  (or class-dir (str (default-class-dir target)))})
    opts))

(defn install
  "Install the JAR locally."
  [{:keys [project jar-file] :as opts}]
  (let [{:keys [project-root aliases lib]} (project-info project)]
    (ensure-lib-in-project-deps-edn! project aliases lib)
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
  (let [target    (or target (default-target))
        class-dir (or class-dir (default-class-dir target))
        jar-file  (or jar-file (str (io/file target (jar-file-name lib version))))
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
    (ensure-lib-in-project-deps-edn! project aliases lib)
    (binding [b/*project-root* project-root]
      (-> opts
          (assoc :lib lib :version version)
          (deploy*)))))
