(ns codejam.train-timetable-test
  (:require [midje.sweet :refer :all]
            [codejam.train-timetable :refer :all :as core]
            [clojure.string :as str]
            [orchestra.spec.test :as st]
            [swissknife.collections :refer [priority-queue]])
  (:import java.io.StringReader
           java.io.StringWriter))


(st/instrument)


(facts
 "schedule train"

 ;; Case 1
 (schedule-train 5 [(priority-queue)
                    (priority-queue)
                    0 0]
                 [540 630 ::core/b])
 => [[635] [] 0 1]


 (schedule-train 5 [(conj (priority-queue) 635)
                    (priority-queue)
                    0 1]
                 [540 720 ::core/a])
 => [[635] [725] 1 1]


 (schedule-train 5 [(conj (priority-queue) 635)
                    (conj (priority-queue) 725)
                    1 1]
                 [600 780 ::core/a])
 => [[635] [725 785] 2 1]


 (schedule-train 5 [(conj (priority-queue) 635)
                    (conj (priority-queue) 725 785)
                    2 1]
                 [660 750 ::core/a])
 => [[] [725 755 785] 2 1]


 (schedule-train 5 [(priority-queue)
                    (conj (priority-queue) 725 755 785)
                    2 1]
                 [722 900 ::core/b])
 => [[905] [725 755 785] 2 2]


 ;;  Case 2
 (schedule-train 2 [(priority-queue)
                    (priority-queue)
                    0 0]
                 [540 541 ::core/a])
 => [[] [543] 1 0]


 (schedule-train 2 [(priority-queue)
                    (conj (priority-queue) 543)
                    1 0]
                 [720 722 ::core/a])
 => [[] [543 724] 2 0])


(facts
 "count trains"

 (count-trains 5
               [[540 630 ::core/b]
                [540 720 ::core/a]
                [600 780 ::core/a]
                [660 750 ::core/a]
                [722 900 ::core/b]])
 => [2 2]


 (count-trains 2
               [[540 541 ::core/a]
                [720 722 ::core/a]])
 => [2 0])


(facts
 "timestamp -> minutes"

 (timestamp->minutes "00:00") => 0
 (timestamp->minutes "12:30") => 750)


(facts
 "interval -> minutes"

 (interval->minutes "00:00 12:30") => [0 750])


(facts
 "parse chunks"

 (parse-chunks ["5"
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
 => [{::core/turnaround-time 5
      ::core/schedule [[540 630 ::core/b]
                       [540 720 ::core/a]
                       [600 780 ::core/a]
                       [660 750 ::core/a]
                       [722 900 ::core/b]]}
     {::core/turnaround-time 2
      ::core/schedule [[540 541 ::core/a]
                       [720 722 ::core/a]]}])


(facts
 "read input"
 (read-input (StringReader. (->> ["2"
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
                                  "12:00 12:02"]
                                 (str/join "\n"))))
 => [{::core/turnaround-time 5
      ::core/schedule [[540 630 ::core/b]
                       [540 720 ::core/a]
                       [600 780 ::core/a]
                       [660 750 ::core/a]
                       [722 900 ::core/b]]}
     {::core/turnaround-time 2
      ::core/schedule [[540 541 ::core/a]
                       [720 722 ::core/a]]}])


(facts
 "solve"

 (solve [{::core/turnaround-time 5
          ::core/schedule [[540 630 ::core/b]
                           [540 720 ::core/a]
                           [600 780 ::core/a]
                           [660 750 ::core/a]
                           [722 900 ::core/b]]}
         {::core/turnaround-time 2
          ::core/schedule [[540 541 ::core/a]
                           [720 722 ::core/a]]}])
 => [[2 2]
     [2 0]])


(facts
 "write output"

 (let [w (java.io.StringWriter.)]
   (write-output w [[2 2]
                    [2 0]])
   (str w))
 => (str "Case #1: 2 2\n"
         "Case #2: 2 0"))
