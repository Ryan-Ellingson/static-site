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

(defn get-org-tags-from-file
  "Given a path to an org file, extract the file tags from it"
  [f]
  (with-open
   [r (io/reader f :encoding "ISO-8859-1")]
    (let [tag-line (->> (line-seq r)
                        (drop-while #(not (re-find #"\#\+filetags:" %)))
                        first)]
      (if tag-line
        (set (drop 1 (->
                      tag-line
                      (str/split #" ")
                      second
                      (str/split #":"))))
        #{}))))

(defn get-blog-files
  "Return a list of org roam files tagged with PYD"
  []
  (let [file-list (->> (.listFiles (io/file  (str (System/getProperty "user.home") "/org/roam")))
                       (map #(.getPath %))
                       (filter #(re-matches #".*\.org$" %)))]
    (->> file-list
         (map #(vector % (get-org-tags-from-file %)))
         (filter #(contains? (second %) "PYD"))
         (map first))))


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
  [(keyword (str "h" (inc (count (take-while #(not= % \space) line)))))
   (apply str (apply str (take-while #(not= % \:) (drop-while #(or (= % \*) (= % \space)) line))))])

(defn parse-image
  "Parse a file link into a hiccup form"
  [line]
  (let [image-file (io/file (str/replace-first (second (re-matches #"\[\[file:(.*)\]\]" line)) #"~" (System/getProperty "user.home")))
        new-filepath (str "images/" (.getName image-file))]
    (FileUtils/copyFileToDirectory
     image-file
     (io/file "target/images/"))
    [:img {:src new-filepath}]))

;;WARN: this results in an infinite loop if we have a string that donesn't start with a \[ character
(defn parse-bracket
  "Parse out the first instance of a bracketed item at the start of the string"
  [n]
  (apply str (loop
              [line (rest n)
               elem [(first n)]
               stack (list (first line))]
               (if (or (empty? line) (empty? stack))
                 elem
                 (let [c (first line)]
                   (case c
                     \[ (recur (rest line) (conj elem c) (conj stack \[))
                     \] (recur (rest line) (conj elem c) (pop stack))
                     (recur (rest line) (conj elem c) stack)))))))

(defn parse-paragraph
  "Parse a paragraph, including text, links, and footnotes into a hiccup form"
  [p-line]
  (loop
   [line p-line
    elem [:p]]
    (if (empty? line)
      elem
      (if (= \[ (first line)) ;; if this is a footnote, link, or image
        (let [run (parse-bracket line)
              rc (count run)]
          (cond
            (re-matches #"\[\[file:.*\]\]" run) (recur (drop rc line) (conj elem [:img {:src run}])) ;;Paragraph inline image
            (re-matches #"\[\[https:.*\]\]" run) (recur (drop rc line) (conj elem (let
                                                                                   [[_ link desc]  (re-find #"\[\[(.*)\]\[(.*)\]\]" run)]
                                                                                    [:a {:href link} desc]))) ;;link
            (re-matches #"\[fn:.*\]" run) (recur (drop rc line) (let [n (apply str (take-while #(not= \] %) (drop 4 run)))]
                                                                  (conj elem [:a {:name (str "back_" n) :href (str "#footnote_" n)} [:sub (str "[" n "]")]])))) ;;image
          )
        (let [run (apply str (take-while #(not= \[ %) line)) ;; if this is just text
              rc (count run)]
          (recur (drop rc line) (conj elem run)))))))

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
    (reduce (fn [accm line]
              (let [fc (first line)
                    fw (first (str/split line #" "))
                    last-element (last accm)
                    last-element-type (first last-element)]
                (case fc
                  \# (if (= fw "#+begin_src")
                       (conj accm [:pre :open [:code {:class "language-clojure"}]])
                       (conj (pop accm) [:pre (last last-element)])) ;; We ignore '#+ATTR_ORG' for images for now
                  \- (if (= last-element-type :ul)
                       (conj (pop accm) (conj last-element [:li (str/replace line #"^- " "")]))
                       (conj accm [:ul [:li (str/replace line #"^- " "")]])) ;; No nested lists for now.
                  \* (conj accm (parse-heading line))
                  \[ (if  (re-matches #"\[\[file:(.*)\]\]" line)
                       (conj accm (parse-image line))
                       (conj accm line)) ;;footnotes at end
                  (if (and (= last-element-type :pre) (= :open (second last-element)))
                    (conj (pop accm) [:pre :open (conj (last last-element) (str line \newline))])
                    (if (empty? line)
                      accm
                      (conj accm (parse-paragraph line)))))))

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

(defn build-site
  "entry point to build the site"
  []
  (->> (blog-page file-string)
       page-wrapper
       str
       (spit "target/index.html")))
