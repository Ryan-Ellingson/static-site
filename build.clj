(ns build
  (:require
   [core :refer [build-site]]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]))

(defn clean
  "Deletes target directory"
  [] (when (.isDirectory (io/file
                "target"))
       (run! io/delete-file (reverse (rest (file-seq (io/file "target")))) )))


(defn build [& args]
  (clean)
  (.mkdir (io/file "target"))
  (.mkdir (io/file "target/blogs"))
  (b/copy-dir {:src-dirs ["resources"]
               :target-dir "target"})
  (build-site))
