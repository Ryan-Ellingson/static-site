(ns core-test
  (:require [clojure.test :as t]
            [core :as c]))

(t/deftest test-parsing-heading
  (t/is (= [:h2 {:id "a_first_heading"} "a first heading"] (c/parse-heading "* a first heading")))
  (t/is (= [:h3 {:id "a_second_heading"} "a second heading"] (c/parse-heading "** a second heading"))))

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
        "Didn't ignore backslash in code block.")
  (t/is (=
         (c/parse-paragraph "we're going to be [[id:6545bad5-06a5-413f-b472-a5f7d6a18291][Tracking Goals with Clojure and Sqlite]]")
         [:p "we're going to be " [:a {:href "/blogs/Tracking-Goals-with-Clojure-and-Sqlite"} "Tracking Goals with Clojure and Sqlite"]])
        "Didn't parse local link correctly"))

(t/deftest test-parsing-body
  (t/is (=
         (c/parse-body ":END:\n#+foo\n- line\n  - indent 1\n    - indent 2\n  - indent 1\n- line\n  - indent 1")
         [[:ul [:li [:p "line"]
                [:ul [:li [:p "indent 1"]
                      [:ul [:li [:p "indent 2"]]]]]
                [:ul [:li [:p "indent 1"]]]]
           [:li [:p "line"]
            [:ul [:li [:p "indent 1"]]]]]])
        "Incorrectly parsed nested lines"))
