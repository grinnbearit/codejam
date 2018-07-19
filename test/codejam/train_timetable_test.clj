(ns codejam.train-timetable-test
  (:require [midje.sweet :refer :all]
            [codejam.train-timetable :refer :all]
            [clojure.string :as str])
  (:import java.io.StringReader
           java.io.StringWriter))


(facts
 "count trains"

 (count-trains [[540 630 :b]
                [540 720 :a]
                [600 780 :a]
                [660 750 :a]
                [722 900 :b]]
               5)
 => [2 2]


 (count-trains [[540 541 :a]
                [720 722 :a]]
               2)
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
 => [{:turnaround-time 5
      :schedule [[540 630 :b]
                 [540 720 :a]
                 [600 780 :a]
                 [660 750 :a]
                 [722 900 :b]]}
     {:turnaround-time 2
      :schedule [[540 541 :a]
                 [720 722 :a]]}])


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
 => [{:turnaround-time 5
      :schedule [[540 630 :b]
                 [540 720 :a]
                 [600 780 :a]
                 [660 750 :a]
                 [722 900 :b]]}
     {:turnaround-time 2
      :schedule [[540 541 :a]
                 [720 722 :a]]}])


(facts
 "solve"

 (solve [{:turnaround-time 5
          :schedule [[540 630 :b]
                     [540 720 :a]
                     [600 780 :a]
                     [660 750 :a]
                     [722 900 :b]]}
         {:turnaround-time 2
          :schedule [[540 541 :a]
                     [720 722 :a]]}])
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
