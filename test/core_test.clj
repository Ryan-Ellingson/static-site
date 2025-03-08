(ns core-test
  (:require [clojure.test :as t]
            [core :as c]))

(t/deftest test-parsing-heading
  (t/is (= [:h2 "a first heading"] (c/parse-heading "* a first heading")))
  (t/is (= [:h3 "a second heading"] (c/parse-heading "** a second heading"))))

(t/deftest test-parsing-paragraph
  (t/is (=
         (c/parse-paragraph "A line with a *bold* word")
         [:p "A line with a " [:b "bold"] " word"])
        "Mis-parse of bold word")
  (t/is (=
         (c/parse-paragraph "A line with a /italic/ word")
         [:p "A line with a " [:i "italic"] " word"])
        "Mis-parse of italic word")
  (t/is (=
         (c/parse-paragraph "A line with a _underline_ word")
         [:p "A line with a " [:u "underline"] " word"])
        "Mis-parse of underline word")

  (t/is (=
         (c/parse-paragraph "A line with a ~code~ word")
         [:p "A line with a " [:code "code"] " word"])
        "Mis-parse of code word")
  (t/is (=
         (c/parse-paragraph "A line with a footnote[fn:1] in the middle")
         [:p "A line with a footnote" [:a {:class "footnote"
                                           :name "back_1"
                                           :href "#footnote_1"}
                                       [:sub "[1]"]]
          " in the middle"])
        "Mis-parse of footnote")
  (t/is (=
         (c/parse-paragraph "A line with a [[https://example.com][link]] in the middle")
         [:p "A line with a " [:a {:href "https://example.com"} "link"] " in the middle"])
        "Mis-parse of a link")
  (t/is (=
         (c/parse-paragraph "Ignore ~/target~ the backslash in the code block")
         [:p "Ignore " [:code "/target"] " the backslash in the code block"])
        "Didn't ignore backslash in code block."))
