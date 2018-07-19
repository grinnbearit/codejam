(ns codejam.train-timetable
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [swissknife.collections :refer [priority-queue]]))


(defn count-trains
  [schedule turnaround-time]
  (letfn [(reducer [[station-a station-b num-a num-b]
                    [start end train]]
            (if (= :a train)
              (if (or (empty? station-a)
                      (< start (peek station-a)))
                [station-a
                 (conj station-b (+ end turnaround-time))
                 (inc num-a)
                 num-b]
                [(pop station-a)
                 (conj station-b (+ end turnaround-time))
                 num-a
                 num-b])
              (if (or (empty? station-b)
                      (< start (peek station-b)))
                [(conj station-a (+ end turnaround-time))
                 station-b
                 num-a
                 (inc num-b)]
                [(conj station-a (+ end turnaround-time))
                 (pop station-b)
                 num-a
                 num-b])))]

    (->> (reduce reducer [(priority-queue)
                          (priority-queue)
                          0 0]
                 schedule)
         (drop 2))))


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


(defn solve
  [problems]
  (letfn [(mapper [{:keys [schedule turnaround-time]}]
            (count-trains schedule turnaround-time))]

    (map mapper problems)))


(defn write-output
  [f solutions]
  (letfn [(formatter [idx [num-a num-b]]
            (format "Case #%d: %d %d" idx num-a num-b))]

    (->> (map formatter (drop 1 (range)) solutions)
         (str/join "\n")
         (spit f))))
