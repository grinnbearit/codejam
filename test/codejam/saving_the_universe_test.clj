(ns codejam.saving-the-universe-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [codejam.saving-the-universe :refer :all]
            [clojure.string :as str]))

;;; ----------------------------------------------------------------------------
;;; De-privatize the functions we want to test
;;; ----------------------------------------------------------------------------
(def parse-int #'codejam.saving-the-universe/parse-int)
(def parse-single-case #'codejam.saving-the-universe/parse-single-case)
(def format-case-result #'codejam.saving-the-universe/format-case-result)


(facts
 "about utility functions"
 (fact
  "parse-int converts string to int"
  (parse-int "123") => 123
  (parse-int "0") => 0))


(facts
 "about parsing functions"
 (fact
  "parse-single-case correctly parses one case and returns remaining lines"
  (let [lines ["5"
               "Yeehaw" "NSM" "Dont Ask" "B9" "Googol"
               "3"
               "Googol" "B9" "Googol"
               "---remaining-line-1---"
               "---remaining-line-2---"]]
    (parse-single-case lines)
    => [{:engines ["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
         :queries ["Googol" "B9" "Googol"]}
        ["---remaining-line-1---" "---remaining-line-2---"]]))

 (fact
  "parse-input-string parses all cases from a full string"
  (let [input-str "2\n5\nA\nB\nC\nD\nE\n2\nA\nB\n2\nX\nY\n1\nX"]
    (parse-input-string input-str)
    => [{:engines ["A" "B" "C" "D" "E"]
         :queries ["A" "B"]}
        {:engines ["X" "Y"]
         :queries ["X"]}])))


(facts
 "about main solving function: solve-case (greedy)"
 (fact
  "solves sample case #1 correctly"
  (let [case-data {:engines ["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
                   :queries ["Yeehaw" "Yeehaw" "Googol" "B9" "Googol"
                             "NSM" "B9" "NSM" "Dont Ask" "Googol"]}]
    (solve-case case-data) => 1))

 (fact
  "solves sample case #2 correctly"
  (let [case-data {:engines ["Yeehaw" "NSM" "Dont Ask" "B9" "Googol"]
                   :queries ["Googol" "Dont Ask" "NSM" "NSM"
                             "Yeehaw" "Yeehaw" "Googol"]}]
    (solve-case case-data) => 0))

 (fact
  "solves a case where a switch is required"
  ;; Engines: A, B. Queries: A, B.
  ;; 1. q="A", seg={"A"}, sw=0
  ;; 2. q="B", seg is size 1 (S-1). Must switch.
  ;;    sw=1, seg={"B"}
  ;; Final: 1 switch
  (let [case-data {:engines ["A" "B"]
                   :queries ["A" "B"]}]
    (solve-case case-data) => 1))

 (fact
  "solves a case where no switch is required (has 3rd option)"
  ;; Engines: A, B, C. (S=3). Queries: A, B.
  ;; 1. q="A", seg={"A"}, sw=0
  ;; 2. q="B", seg is size 1 (!= S-1). No switch.
  ;;    sw=0, seg={"A", "B"}
  ;; Final: 0 switches
  (let [case-data {:engines ["A" "B" "C"]
                   :queries ["A" "B"]}]
    (solve-case case-data) => 0))

 (fact
  "solves case with zero queries"
  (let [case-data {:engines ["A" "B" "C"]
                   :queries []}]
    (solve-case case-data) => 0)))

(facts
 "about formatting and main entry point"
 (fact "format-case-result produces correct string"
       (format-case-result 1 123) => "Case #1: 123\n"
       (format-case-result 99 0) => "Case #99: 0\n")

 (fact
  "solve-from-file integrates everything correctly"
  (let [sample-input (str/join "\n"
                               ["2"
                                "5"
                                "Yeehaw" "NSM" "Dont Ask" "B9" "Googol"
                                "10"
                                "Yeehaw" "Yeehaw" "Googol" "B9" "Googol"
                                "NSM" "B9" "NSM" "Dont Ask" "Googol"
                                "5"
                                "Yeehaw" "NSM" "Dont Ask" "B9" "Googol"
                                "7"
                                "Googol" "Dont Ask" "NSM" "NSM"
                                "Yeehaw" "Yeehaw" "Googol"])
        expected-output (apply str
                               ["Case #1: 1\n"
                                "Case #2: 0\n"])]
    (solve-from-file "dummy-filename.txt") => expected-output
    (provided
     (slurp "dummy-filename.txt") => sample-input))))


;; Test Cases
;; https://zibada.guru/gcj/2008qr/problems/#A

;; (facts
;;  (fact
;;   "Test Set 1"
;;   (solve-from-file "data/saving_the_universe/test_set_1/ts1_input.txt")
;;   => (slurp "data/saving_the_universe/test_set_1/ts1_output.txt")

;;   (fact
;;    "Test Set 2"
;;    (solve-from-file "data/saving_the_universe/test_set_2/ts2_input.txt")
;;    => (slurp "data/saving_the_universe/test_set_2/ts2_output.txt"))))
