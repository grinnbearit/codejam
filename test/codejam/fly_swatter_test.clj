(ns codejam.fly-swatter-test
  (:require [midje.sweet :refer :all]
            [codejam.fly-swatter :refer :all :as core]
            [clojure.string :as str]
            [orchestra.spec.test :as st])
  (:import java.io.StringReader
           java.io.StringWriter))


(st/instrument)


(facts
 "within racquet?"

 (within-racquet? 5.0 [3.0 5.0])
 => false

 (within-racquet? 5.0 [3.0 4.0])
 => true

 (within-racquet? 5.0 [3.0 3.0])
 => true)


(facts
 "Hit by rim?"

 (hit-by-rim? 6.0 0.5 0.4 [3.0 4.0])
 => false

 (hit-by-rim? 6.0 0.5 0.5 [3.0 4.0])
 => true

 (hit-by-rim? 6.0 0.5 0.6 [3.0 4.0])
 => true)


(facts
 "Hit by chord?"

 (hit-by-chord? 1.0 2.0 0.1 [0.5 0.5])
 => true

 (hit-by-chord? 1.0 2.0 0.1 [1.1 1.1])
 => true

 (hit-by-chord? 1.0 2.0 0.1 [5.1 5.1])
 => true

 (hit-by-chord? 1.0 2.0 0.1 [6.9 6.9])
 => true

 (hit-by-chord? 1.0 2.0 0.1 [6.0 6.0])
 => false)


(facts
 "Hit by racquet"

 (hit-by-racquet? 6.0 0.5 1.0 1.0 0.5 [3.0 4.0])
 => true)


(facts
 "generate position!"

 (generate-position! 2.0)
 => [-0.5 1.0]

 (provided
  (rand 2.0) =streams=> [0.5 1.0]

  (rand-nth [-1 1]) =streams=> [-1 1]))


(facts
 "probability"

 (probability 5 0.1 6.0 0.5 1.0 1.0)
 => 0.75

 (provided
  (generate-position! 6.0) =streams=> [[5.9 5.9]
                                       [0.0 0.0]
                                       [1.0 1.0]
                                       [2.0 2.0]
                                       [1.5 1.5]]))


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


(facts
 "solve"

 (solve [[0.25 1.0 0.1 0.01 0.5]
         [0.25 1.0 0.1 0.01 0.9]
         [0.00001 10000 0.00001 0.00001 1000]
         [0.4 10000 0.00001 0.00001 700]
         [1 100 1 1 10]])
 => [1.000 0.910 0.000 0.002 0.574]

 (provided
  (probability 1000000 0.25 1.0 0.1 0.01 0.5) => 1.000
  (probability 1000000 0.25 1.0 0.1 0.01 0.9) => 0.910
  (probability 1000000 0.00001 10000 0.00001 0.00001 1000) => 0.000
  (probability 1000000 0.4 10000 0.00001 0.00001 700) => 0.002
  (probability 1000000 1 100 1 1 10) => 0.574))
