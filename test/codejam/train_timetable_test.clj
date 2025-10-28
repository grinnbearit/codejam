(ns codejam.train-timetable-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [codejam.train-timetable :refer :all]
            [clojure.string :as str]))

;; ----------------------------------------------------------------------------
;; De-privatize the functions we want to test
;; ----------------------------------------------------------------------------
(def parse-int #'codejam.train-timetable/parse-int)
(def time-to-int #'codejam.train-timetable/time-to-int)
(def parse-trips #'codejam.train-timetable/parse-trips)
(def parse-single-case #'codejam.train-timetable/parse-single-case)
(def calculate-trains #'codejam.train-timetable/calculate-trains)
(def format-case-result #'codejam.train-timetable/format-case-result)

(facts
 "about utility functions"
 (fact
  "parse-int converts string to int"
  (parse-int "123") => 123)

 (fact
  "time-to-int converts 'HH:MM' to minutes"
  (time-to-int "00:00") => 0
  (time-to-int "01:00") => 60
  (time-to-int "09:30") => 570
  (time-to-int "23:59") => 1439))


(facts
 "about parsing functions"
 (fact
  "parse-trips reads N lines"
  (let [lines ["09:00 10:00" "11:00 12:00" "13:00 14:00"]]
    (parse-trips 2 lines)
    => [{:dep 540 :arr 600}
        {:dep 660 :arr 720}]

    (count (parse-trips 2 lines)) => 2))

 (fact "parse-single-case reads one full case"
       (let [lines ["5"
                    "3 2"
                    "09:00 12:00"
                    "10:00 13:00"
                    "11:00 12:30"
                    "12:02 15:00"
                    "09:00 10:30"
                    "---next case---"]]
         (parse-single-case lines)
         => [{:T 5
              :trips-A-B [{:dep 540 :arr 720}
                          {:dep 600 :arr 780}
                          {:dep 660 :arr 750}]
              :trips-B-A [{:dep 722 :arr 900}
                          {:dep 540 :arr 630}]}
             ["---next case---"]])))


(facts
 "about core logic: calculate-trains"
 (fact
  "needs 1 train if one departure and no arrivals"
  (calculate-trains [100] [] 5) => 1)

 (fact
  "needs 0 trains if no departures"
  (calculate-trains [] [100] 5) => 0)

 (fact
  "needs 0 trains if arrival is ready in time"
  ;; Dep: 100. Arr: 90, Ready: 95. 95 <= 100. OK.
  (calculate-trains [100] [90] 5) => 0)

 (fact
  "needs 1 train if arrival is too late"
  ;; Dep: 100. Arr: 90, Ready: 90 + 15 (T=15). Ready: 105. 105 > 100. New train.
  (calculate-trains [100] [90] 15) => 1)

 (fact
  "correctly reuses multiple trains"
  ;; Dep: [100, 200]. Ready: [95, 195].
  ;; D(100) uses R(95).
  ;; D(200) uses R(195).
  ;; Result: 0 new trains.
  (calculate-trains [100 200] [90 190] 5) => 0)

 (fact
  "correctly handles shortage of trains"
  ;; Dep: [100, 110]. Ready: [95].
  ;; D(100) uses R(95).
  ;; D(110) has no ready trains. New train.
  ;; Result: 1 new train.
  (calculate-trains [100 110] [90] 5) => 1)

 (fact
  "correctly handles ready-time being too late"
  ;; Dep: [100, 110]. Ready: [110].
  ;; D(100): R(110) is > 100. New train.
  ;; D(110): R(110) is <= 110. Reuse.
  ;; Result: 1 new train.
  (calculate-trains [100 110] [105] 5) => 1))


(facts
 "about main solving function: solve-case"
 (fact
  "solves sample case #1"
  (let [case-data {:T 5,
                   :trips-A-B [{:dep 540 :arr 720}
                               {:dep 600 :arr 780}
                               {:dep 660 :arr 750}],
                   :trips-B-A [{:dep 722 :arr 900}
                               {:dep 540 :arr 630}]}]
    (solve-case case-data) => [2 2]))

 (fact
  "solves sample case #2"
  (let [case-data {:T 2,
                   :trips-A-B [{:dep 540 :arr 541}
                               {:dep 720 :arr 722}],
                   :trips-B-A []}]
    ;; Dep-A: [540, 720]. Arr-A: []. Ready-A: [].
    ;; D(540) needs new train.
    ;; D(720) needs new train.
    ;; trains-A = 2.
    ;; Dep-B: []. trains-B = 0.
    (solve-case case-data) => [2 0])))


(facts
 "about formatting and main entry point"
 (fact
  "format-case-result produces correct string"
  (format-case-result 1 [2 3]) => "Case #1: 2 3\n"
  (format-case-result 10 [0 5]) => "Case #10: 0 5\n")

 (fact
  "solve-from-file integrates everything correctly"
  (let [sample-input (str/join "\n"
                               ["2"
                                "5"
                                "3 2"
                                "09:00 12:00"
                                "10:00 13:00"
                                "11:00 12:30"
                                "12:02 15:00"
                                "09:00 10:30"
                                "2"
                                "2 0"
                                "09:00 09:01"
                                "12:00 12:02"])
        expected-output (apply str ["Case #1: 2 2\n"
                                    "Case #2: 2 0\n"])]
    (solve-from-file "dummy-filename.txt") => expected-output
    (provided
     (slurp "dummy-filename.txt") => sample-input))))


;; Test Cases
;; https://zibada.guru/gcj/2008qr/problems/#B

;; (facts
;;  (fact
;;   "Test Set 1"
;;   (solve-from-file "data/train_timetable/test_set_1/ts1_input.txt")
;;   => (slurp "data/train_timetable/test_set_1/ts1_output.txt")

;;   (fact
;;    "Test Set 2"
;;    (solve-from-file "data/train_timetable/test_set_2/ts2_input.txt")
;;    => (slurp "data/train_timetable/test_set_2/ts2_output.txt"))))
