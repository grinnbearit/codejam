(ns codejam.train-timetable
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clojure.spec.alpha :as s]
            [swissknife.collections :refer [priority-queue]]))

;;; https://code.google.com/codejam/contest/32013/dashboard#s=p1


(defn schedule-train
  "With a `railway` and a `departure` returns the next state of the railway
  adding trains when needed"
  [turnaround-time railway departure]
  (let [[station-a station-b num-trains-a num-trains-b] railway
        [start end train] departure]
    (if (= ::a train)
      (if (or (empty? station-a)
              (< start (peek station-a)))
        [station-a
         (conj station-b (+ end turnaround-time))
         (inc num-trains-a)
         num-trains-b]
        [(pop station-a)
         (conj station-b (+ end turnaround-time))
         num-trains-a
         num-trains-b])
      (if (or (empty? station-b)
              (< start (peek station-b)))
        [(conj station-a (+ end turnaround-time))
         station-b
         num-trains-a
         (inc num-trains-b)]
        [(conj station-a (+ end turnaround-time))
         (pop station-b)
         num-trains-a
         num-trains-b]))))


(s/def ::minutes nat-int?)
(s/def ::turnaround-time ::minutes)
(s/def ::station (s/coll-of ::minutes))
(s/def ::railway (s/tuple ::station ::station nat-int? nat-int?))
(s/def ::train #{::a ::b})
(s/def ::departure (s/and (s/tuple ::minutes ::minutes ::train)
                          (fn [[start end _]] (<= start end))))

(s/fdef schedule-train
        :args (s/cat ::turnaround-time ::turnaround-time
                     ::railway-state ::railway
                     ::next-departure ::departure)
        :ret ::railway)


(defn count-trains
  "Counts the number of trains, at both stations, needed to
  satisfy the schedule"
  [turnaround-time schedule]
  (let [railway [(priority-queue) (priority-queue) 0 0]]
    (-> (reduce (partial schedule-train turnaround-time)
                railway schedule)
        (subvec 2))))


(s/def ::schedule (s/coll-of ::departure))
(s/def ::train-count (s/tuple nat-int? nat-int?))

(s/fdef count-trains
        :args (s/cat ::turnaround-time ::turnaround-time
                     ::schedule ::schedule)
        :ret ::train-count)


(defn timestamp->minutes
  "Takes a string timestamp and returns the
  number of minutes from midnight that morning"
  [timestamp-str]
  (-> (t/interval (t/date-time 1970)
                  (tf/parse timestamp-str))
      (t/in-minutes)))


(s/fdef timestamp->minutes
        :args (s/cat ::timestamp-str string?)
        :ret ::minutes)


(defn interval->minutes
  "Takes a string train interval and returns a
  pair of minutes from midnight that morning"
  [interval-str]
  (->> (str/split interval-str #" ")
       (mapv timestamp->minutes)))


(s/fdef interval->minutes
        :args (s/cat ::interval-str string?)
        :ret (s/tuple ::minutes ::minutes))


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
                         (map #(conj % ::a)))
           b-trains (->> (drop (+ 2 na) lines)
                         (take  nb)
                         (map interval->minutes)
                         (map #(conj % ::b)))]
       (cons {::turnaround-time turnaround-time
              ::schedule (sort (concat a-trains b-trains))}
             (parse-chunks (drop (+ 2 na nb) lines)))))))


(s/def ::chunk
  (s/keys :req [::turnaround-time ::schedule]))

(s/fdef parse-chunks
        :args (s/cat ::lines (s/coll-of string?))
        :ret (s/coll-of ::chunk)
        :fn (fn [spec]
              (let [num-lines (count (-> spec :args ::lines))
                    num-chunks (count (-> spec :ret))
                    num-trains (apply + (map (comp count ::schedule) (-> spec :ret)))]
                (= num-lines
                   (+ (* num-chunks 2) num-trains)))))


(defn read-input
  [f]
  (->> (line-seq (io/reader f))
       (drop 1)
       (parse-chunks)))


(s/fdef read-input
        :args (s/cat ::f io/reader)
        :ret (s/coll-of ::chunk))


(defn solve
  [problems]
  (letfn [(mapper [problem]
            (count-trains (::turnaround-time problem)
                          (::schedule problem)))]

    (map mapper problems)))


(s/fdef solve
        :args (s/cat ::problems (s/coll-of ::chunk))
        :ret (s/coll-of ::train-count))


(defn write-output!
  [f solutions]
  (letfn [(formatter [idx [num-a num-b]]
            (format "Case #%d: %d %d" idx num-a num-b))]

    (->> (map formatter (drop 1 (range)) solutions)
         (str/join "\n")
         (spit f))))
