(ns codejam.train-timetable
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [swissknife.collections :refer [priority-queue]]))


(defn count-trains
  [train-info]
  (loop [num-a 0
         num-b 0
         schedule (:schedule train-info)
         station-a (priority-queue first)
         station-b (priority-queue first)]
    (if (empty? schedule)
      [num-a num-b]
      (let [[start end train] (first schedule)]
        (if (= train :a)
          (if (or (empty? station-a)
                  (< start (first (peek station-a))))
            (recur (inc num-a)
                   num-b
                   (rest schedule)
                   station-a
                   (conj station-b [(+ end (:turnaround-time train-info))]))
            (recur num-a
                   num-b
                   (rest schedule)
                   (pop station-a)
                   (conj station-b [(+ end (:turnaround-time train-info))])))
          (if (or (empty? station-b)
                  (< start (first (peek station-b))))
            (recur num-a
                   (inc num-b)
                   (rest schedule)
                   (conj station-a [(+ end (:turnaround-time train-info))])
                   station-b)
            (recur num-a
                   num-b
                   (rest schedule)
                   (conj station-a [(+ end (:turnaround-time train-info))])
                   (pop station-b))))))))


(defn timestamp->minutes
  "Takes a string timestamp and returns the
  number of minutes from midnight that morning"
  [timestamp-str]
  (-> (t/interval (t/date-time 1970)
                  (tf/parse timestamp-str))
      (t/in-minutes)))


(defn interval->minutes
  "Takes a string train interval and returns a
  pair of minutes from midnight that morning"
  [interval-str]
  (->> (str/split interval-str #" ")
       (mapv timestamp->minutes)))


(defn parse-chunks
  [lines]
  (lazy-seq
   (when-let [s (seq lines)]
     (let [turnaround-time (Integer/parseInt (first lines))
           [na nb] (map #(Integer/parseInt %)
                        (str/split (second lines) #" "))
           a-trains (->> (drop 2 lines)
                         (take na)
                         (map interval->minutes)
                         (map #(conj % :a)))
           b-trains (->> (drop (+ 2 na) lines)
                         (take  nb)
                         (map interval->minutes)
                         (map #(conj % :b)))]
       (cons {:turnaround-time turnaround-time
              :schedule (sort (concat a-trains b-trains))}
             (parse-chunks (drop (+ 2 na nb) lines)))))))


(defn read-input
  [f]
  (->> (line-seq (io/reader f))
       (drop 1)
       (parse-chunks)))


(defn write-output
  [f solutions]
  (letfn [(formatter [idx [num-a num-b]]
            (format "Case #%d: %d %d" idx num-a num-b))]
    (->> (map formatter (drop 1 (range)) solutions)
         (str/join "\n")
         (spit f))))
