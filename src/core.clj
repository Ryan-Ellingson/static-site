(ns core
  (:require
   [hiccup2.core :as h]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.edn :as edn])
  (:import org.apache.commons.io.FileUtils))

(defn page-wrapper
  "Wraps content so the site's consistent."
  [page & other]
  (h/html [:html
           [:head
            [:meta {:charset "UTF-8"}]
            [:meta {:name "viewport"
                    :content "width=device-width, initial-scale=1"}]
            [:meta {:name "description"
                    :content "Personal blog of Ryan Ellingson."}]
            [:link {:rel "alternate"
                    :type "application/rss+xml"
                    :href "/rss.xml"}]
            [:title "Ryan's Blog"]
            [:link {:rel "icon" :type "image/png" :sizes "96x96" :href "/favicon-96x96.png"}]
            [:link {:href "/styles.css" :rel "stylesheet" :type "text/css"}]]
           ;;Highlight.JS
           ;;Add code highlighting to code blocks
           [:link {:href "https://unpkg.com/@highlightjs/cdn-assets@11.4.0/styles/obsidian.min.css" :rel "stylesheet" :type "text/css"}]
           [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js" :type "text/javascript"}]
           [:script {:src "https://unpkg.com/@highlightjs/cdn-assets@11.4.0/languages/clojure.min.js" :type "text/javascript"}]
           [:script {:src "https://unpkg.com/@highlightjs/cdn-assets@11.4.0/languages/go.min.js" :type "text/javascript"}]
           [:script "hljs.highlightAll();"]
           [:header
            [:h1 [:a {:href "/"} "Ryan Ellingson"]]
            [:sub "Data Engineer"]]
           [:body page other [:div {:class "image-container"} [:img {:src "/images/doge.png"}]]]]))


(defn get-top-org-properties
  "Get the top :PROPERTIES: key, value pairs."
  [n]
  (->> (str/split n #"\n")
       (drop-while #(not= %  ":PROPERTIES:"))
       (rest)
       (take-while #(not= % ":END:"))
       (map #(re-matches #":(\w+):\s+(.*)" %))
       (map #(rest %))
       (flatten)
       (apply hash-map)))

(defn get-org-keywords
  "Get keywords, including title, filetags, etc."
  [n]
  (let [export-tags (->>
                     (str/split n #"\n")
                     (drop-while #(not= % ":END:"))
                     (rest)
                     (take-while #(re-matches #"#.*" %))
                     (mapcat #(vector (last (re-matches #"#\+(\w+).*" %)) (last (re-matches #"\#\+\w+: (.*)" %))))
                     (apply hash-map))]
    (assoc export-tags "filetags" (rest (str/split (get export-tags "filetags") #":")))))

(defn parse-heading
  "Parse a heading line into a hiccup form"
  [line]
  [(keyword (str "h" (count (take-while #(not= % \space) line))))
   (apply str (apply str (take-while #(not= % \:) (drop-while #(or (= % \*) (= % \space)) line))))])

(defn  parse-body
  "Parsing the body of a blog."
  [file-string]
  (let
   [lines (->>
           (str/split file-string #"\n")
           (drop-while #(not= % ":END:"))
           rest
           (drop-while #(re-matches #"#.*" %))
           rest)]
    (reduce (fn [accm, line]
              (cond
                (str/starts-with? line "[") (conj accm [:img {:src line}])
                (= (first line) \*) (conj accm (parse-heading line))
                (str/starts-with? line "#") accm ;;ignoring keywords of files for now
                :else (conj accm [:p line])))
            []
            lines)))

(defn blog-page [file-string]
  (let
   [keywords (get-org-keywords file-string)
    title (get keywords "title")
    tags (get keywords "tags")]
    (vec (concat
          [:main
           [:h1 title]]
          (parse-body file-string)))))

(page-wrapper (blog-page file-string))

(defn build-site
  "entry point to build the site"
  []
  (->> (blog-page file-string)
       page-wrapper
       str
       (spit "target/index.html")))
