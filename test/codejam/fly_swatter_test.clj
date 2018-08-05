(ns codejam.fly-swatter-test
  (:require [midje.sweet :refer :all]
            [codejam.fly-swatter :refer :all :as core]
            [clojure.string :as str]
            [orchestra.spec.test :as st])
  (:import java.io.StringReader
           java.io.StringWriter))


(st/instrument)


(facts
 "parse line"

 (parse-line "0.25 1.0 0.1 0.01 0.5")
 => [0.25 1.0 0.1 0.01 0.5]

 (parse-line "0.25 1.0 0.1 0.01 0.9")
 => [0.25 1.0 0.1 0.01 0.9]

 (parse-line "0.00001 10000 0.00001 0.00001 1000")
 => [0.00001 10000.0 0.00001 0.00001 1000.0]

 (parse-line "0.4 10000 0.00001 0.00001 700")
 => [0.4 10000.0 0.00001 0.00001 700.0]

 (parse-line "1 100 1 1 10")
 => [1.0 100.0 1.0 1.0 10.0])


(facts
 "read input"

 (read-input (StringReader. (->> ["5"
                                  "0.25 1.0 0.1 0.01 0.5"
                                  "0.25 1.0 0.1 0.01 0.9"
                                  "0.00001 10000 0.00001 0.00001 1000"
                                  "0.4 10000 0.00001 0.00001 700"
                                  "1 100 1 1 10"]
                                 (str/join "\n"))))
 => [[0.25 1.0 0.1 0.01 0.5]
     [0.25 1.0 0.1 0.01 0.9]
     [1.0E-5 10000.0 1.0E-5 1.0E-5 1000.0]
     [0.4 10000.0 1.0E-5 1.0E-5 700.0]
     [1.0 100.0 1.0 1.0 10.0]])
