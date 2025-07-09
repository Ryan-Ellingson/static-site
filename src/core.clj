(ns core
  (:require
   [hiccup2.core :as h]
   [hiccup.util :refer [raw-string]]
   [clojure.string :as str]
   [clojure.java.io :as io])

  (:import org.apache.commons.io.FileUtils
           [java.time.format DateTimeFormatter]
           [java.time ZonedDateTime ZoneOffset]
           [java.util Locale]))

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
           [:link {:href "https://unpkg.com/highlightjs@9.16.2/styles/gruvbox-dark.css" :rel "stylesheet" :type "text/css"}]
           [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js" :type "text/javascript"}]
           [:script {:src "https://unpkg.com/@highlightjs/cdn-assets@11.4.0/languages/clojure.min.js" :type "text/javascript"}]
           [:script {:src "https://unpkg.com/@highlightjs/cdn-assets@11.4.0/languages/go.min.js" :type "text/javascript"}]
           [:script "hljs.highlightAll();"]
           [:header
            [:h1 [:a {:href "/"} "Ryan Ellingson"]]
            [:sub "Data Engineer"]
            [:nav
             (into [:ul {:class "menu-bar"}]
                   (mapv (fn [[link label]]
                           [:a {:href link}  [:li  label]])
                         [["/" "home"]
                          ["https://github.com/Ryan-Ellingson" "github"]
                          ["/feed.xml" "rss"]]))]]
           [:body page other [:div {:class "watermark-image"} [:img {:src "/images/turtle.webp"}]]]]))

(def ^:dynamic *local-links* nil)

(defn rfc->dmy
  "Convert an RFC 822/2822 date string to a dd-MM-yyyy format"
  [rfc-date]
  (-> (ZonedDateTime/parse rfc-date (DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss Z" Locale/US))
      (.format (DateTimeFormatter/ofPattern "dd-MM-yyyy"))))

(defn get-org-file-metadata
  "Given a path to an org file, parse the properties and tags"
  [org-file-path]
  (let [f (io/file org-file-path)]
    (assert (.exists f) (str "org file doesn't exist " f))
    (with-open
     [r (io/reader f :encoding "ISO-8859-1")]
      (let [top-block (take-while #(not (= % \n)) (line-seq r))]
        (letfn [(get-line-value [s pattern]
                  (->> s
                       (map #(re-find pattern %))
                       (filter #(not (nil? %)))
                       first
                       second))]
          {:roam-uid (get-line-value top-block #":ID:\ +(.*)")
           :title (get-line-value top-block #"\#\+title:\ +(.*)")
           :tags (let [t (get-line-value top-block #"\#\+filetags:\ +(.*)")]
                   (if (nil? t)
                     `()
                     (rest (str/split t #":"))))
           :slug (get-line-value top-block #"\#\+slug:\ +(.*)")
           :pubDate (let [d (get-line-value top-block #"\#\+pubdate:\ +(.*)")]
                      (if (nil? d)
                        ""
                        (rfc->dmy d)))
           :path org-file-path})))))

(defn get-blogs-from-roam
  "Get all the blog files marked with the PYD tag"
  []
  (->> (.listFiles
        (io/file
         (str (System/getProperty "user.home") "/org/roam")))
       (map #(.getPath %))
       (filter #(re-matches #".*\.org$" %))
       (map get-org-file-metadata)
       (filter #(some #{"PYD"} (:tags %)))))

(defn home-page [blog-info]
  [:main {:class "home-page"}
   [:h1 "👋 Hello!"]
   [:div "I'm Ryan. I write here about software engineering, data, and other adjacent topics. Thanks for stopping by!"]
   [:h1 "🗒 Blog list"]
   (map (fn [{:keys [title tags slug pubDate]}]
          [:div {:class "blog-listing"}
           [:a {:href (str "/blogs/" (str/replace title #" " "-"))} title] [:span {:class "date"} pubDate]
           [:p slug]])
        blog-info)])

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

(defn parse-link
  "Parse a string like [[https://example.com][a link]] into a anchor element"
  [s]
  (let
   [[_ link desc]  (re-find #"\[\[(.*)\]\[(.*)\]\]" s)]
    [:a {:href link} desc]))

(defn parse-footnote
  "Parse a string like [fn:n] into a footnote-sub element"
  [s]
  (let
   [n (apply str (take-while #(not= \] %) (drop 4 s)))]
    [:a {:class "footnote" :name (str "back_" n) :href (str "#footnote_" n)} [:sub (str "[" n "]")]]))

(defn parse-local-link
  "Parse a string like [[id:some-roam-unique-id][link name]] into a link element"
  [s]
  (let
   [[_ id remark]  (re-find #"\[\[id\:(.*)\]\[(.*)\]\]" s)
    link (:link (first (filter #(= id (:id %)) *local-links*)))]
    [:a {:href link} remark]))

(defn parse-paragraph
  [s]
  (loop [coll [:p]
         p-text s
         l-char nil
         f-char nil]
    (let [[next-char & text-left] p-text
          f-chars #{\/ \~ \- \* \_}
          b-chars  #{\. \, \- \) \space}
          prefix-chars #{\. \, \- \( \space}]
      (if (empty? p-text)
        (if f-char
          (conj (conj
                 (pop coll)
                 (assoc
                  (last coll) 1
                  (apply str (butlast (last (last coll))))))
                (str next-char))
          coll)

        (cond
                       ;; Beginning a formatting sequence
          (and (f-chars next-char)
               (or (prefix-chars l-char) (nil? l-char))
               (nil? f-char)
               text-left
               (re-find (re-pattern
                         (str \( \\ next-char \$ \|
                              \\ next-char "\\." \|
                              \\ next-char \, \|
                              \\ next-char "\\)" \|
                              \\ next-char \- \|
                              \\ next-char \space \))) (apply str text-left)))
          (recur
           (conj coll [(case next-char \/ :i \~ :code \- :s \* :b \_ :u) ""]) text-left next-char next-char)
                        ;; Ending a formatting sequence
          (and f-char
               (= l-char f-char)
               (or
                (= next-char \space)
                (empty? text-left)
                (and
                 (b-chars next-char)
                 (or (empty? text-left) (b-chars (second p-text))))))
          (recur
           (conj (conj
                  (pop coll)
                  (assoc
                   (last coll) 1
                   (apply str (butlast (last (last coll))))))
                 (str next-char)) text-left next-char nil)
                       ;; adding to an existing formatted element
          f-char
          (recur
           (conj (pop coll) (assoc (last coll) 1 (str (last (last coll)) next-char))) text-left next-char f-char)
                       ;; parse-link
          (= next-char \[)
          (let
           [run (parse-bracket p-text)
            rc (count run)
            new-elem (cond
                       (re-matches #"\[fn:.*\]" run) (parse-footnote run)
                       (re-matches #"\[\[https:.*\]\]" run) (parse-link run)
                       (re-matches #"\[\[id:.*\]\[.*\]\]" run) (parse-local-link run))]
            (recur
             (conj coll new-elem)
             (drop rc p-text)
             \]
             f-char))
          :else
          (recur
           (if (string? (last coll))
             (conj (pop coll) (str (last coll) next-char))
             (conj coll (str next-char))) text-left next-char f-char))))))

(defn parse-ending-footnote
  "Parse a footnote at the end of the file of form \"[fn:n] footnote content\" into a footnote element"
  [s]
  (let [footnote-number (apply str (take-while #(not= \] %) (drop 4 s)))
        footnote-element (assoc (parse-paragraph (drop 1 (drop-while #(not= \] %) s))) 0 :span)]
    [:div [:a {:href (str "#back_" footnote-number) :name (str "footnote_" footnote-number)} (str "^" footnote-number)] footnote-element]))

(defn add-element-to-list-in-order
  "Adds an item to the end of a ul element at the specified level."
  ([elem line]
   (add-element-to-list-in-order elem [:li (parse-paragraph (str/replace line #"^\ *- " ""))] (/ (count (take-while #(= \space %) line)) 2)))
  ([elem li level]
   (assert (= (first elem) :ul) (str "Tried to add list item" li "to non-list element: " elem))
   (if (= level 0)
     ;; If we're at the correct indentation level, add the item to the list
     (conj elem li)
     (let [last-li (last elem)]
       (if (= (first (last last-li)) :ul)
         ;; list exists at the indentation level Descend into it
         (conj (pop elem) (assoc last-li 2
                                 (add-element-to-list-in-order (last last-li) li (dec level))))
         ;; sub-list does not exist, create it and add the item as the first element
         (conj (pop elem) (conj last-li [:ul li])))))))

(defn blog-page [blog-metadata]
  [:main
   [:h1 (:title blog-metadata)]
   (let [org-file-text (slurp (:path blog-metadata))
         body (->> (str/split org-file-text #"\n")
                   (drop-while #(not= "" %))
                   (take-while #(not= "* Footnotes" %))
                   (filter #(not= "" %)))
         footnotes (->> (str/split org-file-text #"\n")
                        (drop-while #(not= "* Footnotes" %))
                        rest
                        (filter seq)
                        vec)]
     (->>
      ;; Group into block elements and Handle one-line elements
      (reduce (fn [accm, line]
                (let [first-token (first (str/split (str/trim line) #"\ "))
                      last-element (last accm)
                      last-element-type (if (vector? last-element) (first last-element) nil)]
                  (cond
                    ;; Start of a code block
                    (re-matches #"\#\+begin_src" first-token) (conj accm (let [lang (second (re-matches #"\#\+begin_src\ (\w*).*" line))]
                                                                           [:pre :open [:code {:class lang}]]))
                    ;; End of a code block
                    (re-matches #"\#\+end_src" first-token) (conj (pop accm) [:pre (last last-element)]) ;; remove the :open tag
                    ;; Headings
                    (re-matches #"\*+" first-token) (conj accm [(keyword (str "h" (count first-token))) (str/trim (apply str (drop-while #(= \* %) line)))])
                    ;; Lists
                    (re-matches #"\-" first-token) (if (= last-element-type :ul)
                                                     (conj (pop accm) (conj last-element line))
                                                     (conj accm  [:ul line]))
                    ;; Images
                    (re-matches #"\[\[file:.*\]\]" first-token) (conj accm (let [image-file
                                                                                 (io/file
                                                                                  (str/replace-first
                                                                                   (second (re-matches #"\[\[file:(.*)\]\]" line))
                                                                                   #"~" (System/getProperty "user.home")))
                                                                                 new-filepath (str "/images/" (.getName image-file))]
                                                                             (FileUtils/copyFileToDirectory
                                                                              image-file
                                                                              (io/file "target/images/"))
                                                                             [:img {:src new-filepath}]))
                    ;; Code in a code block
                    (and (= :pre last-element-type) (= :open (second last-element))) (conj (pop accm) (conj (pop last-element) (conj (last last-element) (str line \newline))))
                    ;; Paragraph
                    :else (conj accm [:p line]))))

              []
              body)
      ;; Convert list
      (map (fn [elem]
             (if (= (first elem) :ul)
               (reduce
                add-element-to-list-in-order
                [:ul]
                (rest elem))
               elem)))
      ;; Parse Paragraphs
      (map (fn [elem]
             (if (= (first elem) :p)
               (parse-paragraph (second elem))
               elem)))
      vec
      (#(conj % (vec (concat [:div] (map parse-ending-footnote footnotes)))))
      (concat [:div])
      vec
      ))])

(defn create-blog-page [blog-metadata]
  (->> blog-metadata
       blog-page
       page-wrapper
       (spit (str "target/blogs/" (str/replace (:title blog-metadata) #" " "-") ".html"))))

(defn build-site
  "entry point to build the site"
  []
  ;; Copy style files
  (FileUtils/copyFileToDirectory
   (io/file "resources/styles.css")
   (io/file "target"))
  ;; parse all blog files
  (let [blog-metadata (get-blogs-from-roam)]
    (binding [*local-links* (map #(hash-map :id (:id %) :link (str "/blogs/" (str/replace (:title %) #" " "-"))) blog-metadata)]
      (->> (home-page blog-metadata)
           page-wrapper
           (spit "target/index.html"))
      (doall (map #(create-blog-page %) blog-metadata)))))

(build-site)


;; (defn generate-rss-feed
;;   "Generate an RSS feed from a collection of posts"
;;   [blog-info local-links]
;;   (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
;;        (h/html
;;         {:mode :xml}
;;         [:rss {:version "2.0"
;;                :xmlns:atom "http://www.w3.org/2005/Atom"
;;                :xmlns:content "http://purl.org/rss/1.0/modules/content/"}
;;          [:channel
;;           [:title "Ryan Ellingson"]
;;           [:description]
;;           [:link "https://www.ryanellingson.dev"]
;;           [:atom:link {:href "https://www.ryanellingson.dev/feed.xml"
;;                        :rel "self"
;;                        :type "application/rss+xml"}]
;;           [:pubDate (-> blog-info first :pubDate)]
;;           [:lastBuildDate  (current-rfc822-timestamp)]
;;           [:language "en-US"]

;;      ;; Generate an item for each post
;;           (for [{:keys [title slug pubDate] :as blog-metadata} (take 20 blog-info)]
;;             (let [link (get-blog-link-from-title title)]
;;               [:item
;;                [:title title]
;;                [:link link]
;;                [:description slug]
;;                [:pubDate pubDate]
;;                [:author "Ryan Ellingson"]
;;                [:guid {:isPermaLink "true"} (str "https://www.ryanellingson.dev" link)]
;;                [:content:encoded (raw-string (str "<![CDATA["  (h/html (blog-page local-links blog-metadata)) "]]>"))]]))]])))

;; (defn build-site
;;   "entry point to build the site"
;;   []
;;   (FileUtils/copyFileToDirectory
;;    (io/file "resources/styles.css")
;;    (io/file "target"))
;;   (let [blog-info (get-blog-metadata)
;;         local-links (map #(hash-map :id (:id %) :link (get-blog-link-from-title (:title %))) blog-info)]
;;     (->> (home-page blog-info)
;;          page-wrapper
;;          (spit "target/index.html"))
;;     (doall (map #(create-blog-page % local-links) blog-info))
;;     (spit "target/feed.xml" (generate-rss-feed blog-info local-links))))
