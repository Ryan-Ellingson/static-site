(ns build
  (:require
   [core :refer [build-site]]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]))

(defn clean
  "Deletes target directory"
  [] (when (.isFile (io/file
                "target"))
    (run! io/delete-file (reverse (file-seq (io/file "target"))))))

(defn build [_]
  (clean)
  (.mkdir (io/file "target"))
  (b/copy-dir {:src-dirs ["resources"]
               :target-dir "target"})
  (build-site))
