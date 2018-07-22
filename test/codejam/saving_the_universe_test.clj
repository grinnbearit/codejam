(ns codejam.saving-the-universe-test
  (:require [midje.sweet :refer :all]
            [codejam.saving-the-universe :refer :all]
            [clojure.string :as str]
            [orchestra.spec.test :as st])
  (:import java.io.StringReader
           java.io.StringWriter))


(st/instrument)


(facts
 "find remaining queries"

 (find-remaining-queries #{"Yeehaw" "NSM" "Dont Ask" "B9" "Googol"}
                         ["Yeehaw" "Yeehaw" "Googol" "B9" "Googol" "NSM" "B9" "NSM" "Dont Ask" "Googol"])
 => ["Dont Ask" "Googol"]

 (find-remaining-queries #{"Yeehaw" "NSM" "B9" "Googol"}
                         ["Dont Ask" "Googol"])
 => []

 (find-remaining-queries #{"Yeehaw" "NSM" "Dont Ask" "B9" "Googol"}
                         ["Googol" "Dont Ask" "NSM" "NSM" "Yeehaw" "Yeehaw" "Googol"])
 => [])


(facts
 "count switches"

 (count-switches #{"Yeehaw" "NSM" "Dont Ask" "B9" "Googol"}
                 ["Yeehaw" "Yeehaw" "Googol" "B9" "Googol" "NSM" "B9" "NSM" "Dont Ask" "Googol"])
 => 1

 (count-switches #{"Yeehaw" "NSM" "Dont Ask" "B9" "Googol"}
                 ["Googol" "Dont Ask" "NSM" "NSM" "Yeehaw" "Yeehaw" "Googol"])
 => 0)


(facts
 "parse chunks"

 (parse-chunks ["5" "Yeehaw" "NSM" "Dont Ask" "B9" "Googol"
                "10" "Yeehaw" "Yeehaw" "Googol" "B9" "Googol" "NSM" "B9" "NSM" "Dont Ask" "Googol"
                "5" "Yeehaw" "NSM" "Dont Ask" "B9" "Googol"
                "7" "Googol" "Dont Ask" "NSM" "NSM" "Yeehaw" "Yeehaw" "Googol"])
 => [["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
     ["Yeehaw" "Yeehaw" "Googol" "B9" "Googol" "NSM" "B9" "NSM" "Dont Ask" "Googol"]
     ["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
     ["Googol" "Dont Ask" "NSM" "NSM" "Yeehaw" "Yeehaw" "Googol"]])


(facts
 "read input"

 (read-input (StringReader. (->> ["2"
                                  "5" "Yeehaw" "NSM" "Dont Ask" "B9" "Googol"
                                  "10" "Yeehaw" "Yeehaw" "Googol" "B9" "Googol" "NSM" "B9" "NSM" "Dont Ask" "Googol"
                                  "5" "Yeehaw" "NSM" "Dont Ask" "B9" "Googol"
                                  "7" "Googol" "Dont Ask" "NSM" "NSM" "Yeehaw" "Yeehaw" "Googol"]
                                 (str/join "\n"))))
 => [[["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
      ["Yeehaw" "Yeehaw" "Googol" "B9" "Googol" "NSM" "B9" "NSM" "Dont Ask" "Googol"]]
     [["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
      ["Googol" "Dont Ask" "NSM" "NSM" "Yeehaw" "Yeehaw" "Googol"]]])


(facts
 "solve"

 (solve [[["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
          ["Yeehaw" "Yeehaw" "Googol" "B9" "Googol" "NSM" "B9" "NSM" "Dont Ask" "Googol"]]
         [["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
          ["Googol" "Dont Ask" "NSM" "NSM" "Yeehaw" "Yeehaw" "Googol"]]])
 => [1 0])


(facts
 "write output"

 (let [w (java.io.StringWriter.)]
   (write-output w [1 0])
   (str w))
 => (str "Case #1: 1\n"
         "Case #2: 0"))
