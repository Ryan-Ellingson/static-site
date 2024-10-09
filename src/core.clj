(ns core
  (:require
   [hiccup2.core :as h]))

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
           [:body page other [:div {:class "image-container"}[:img {:src "/images/doge.png"}]]]
;; <div class="image-container">
;;   <img src="path_to_your_image.jpg" alt="Description of Image">
;; </div>

           ]))

(defn build-site
  "entry point to build the site"
  []
  (spit "target/index.html" (str (page-wrapper [:span {:class "foo"} "bar"]))))
