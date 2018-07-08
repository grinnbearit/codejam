(ns codejam.saving-the-universe-test
  (:require [midje.sweet :refer :all]
            [codejam.saving-the-universe :refer :all]
            [clojure.string :as str])
  (:import java.io.StringReader
           java.io.StringWriter))



(facts
 "find remaining queries"

 (find-remaining-queries #{:a :b}
                         [])
 => []

 (find-remaining-queries #{:a :b}
                         [:a :a])
 => []

 (find-remaining-queries #{:a :b}
                         [:a :a :b :a :a])
 => [:b :a :a])


(facts
 "count switches"

 (count-switches #{:a :b}
                 [])
 => 0

 (count-switches #{:a :b}
                 [:a :a])
 => 0

 (count-switches #{:a :b}
                 [:a :a :b :a :a])
 => 2)


(facts
 "parse chunk"

 (parse-chunk ["2" "apple" "banana" "0"])
 => [["apple" "banana"] ["0"]]

 (parse-chunk ["0"])
 => [[] []])


(facts
 "read input"

 (read-input (StringReader. (str/join "\n"
                                      ["2"
                                       "2"
                                       "apple" "banana"
                                       "0"
                                       "1"
                                       "apple"
                                       "1"
                                       "banana"])))
 => [[#{"apple" "banana"} []]
     [#{"apple"} ["banana"]]])


(facts
 "solve"

 (solve [[#{:a :b}
          [:a :a]]
         [#{:a :b}
          [:a :a :b :a :a]]])
 => [0 2])


(facts
 "write output"

 (let [w (java.io.StringWriter.)]
   (write-output w [0 2])
   (str w))
 => (str "Case #1: 0\n"
         "Case #2: 2"))
