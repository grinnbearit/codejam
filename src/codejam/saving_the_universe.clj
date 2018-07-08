(ns codejam.saving-the-universe
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn find-remaining-queries
  "returns `remaining-queries`, the queries left after the longest possible run without switching.
  Returns an empty list if no queries are left to execute."
  [engines queries]
  (loop [unseen-engines engines
         remaining-queries queries]
    (if (empty? remaining-queries)
      []
      (let [next-query (first remaining-queries)]
        (cond (not (unseen-engines next-query))
              (recur unseen-engines
                     (rest remaining-queries))

              (empty? (rest unseen-engines))
              remaining-queries

              :else
              (recur (disj unseen-engines next-query)
                     (rest remaining-queries)))))))


(defn count-switches
  "returns the number of switches required to execute all queries"
  [engines queries]
  (loop [unseen-engines engines
         remaining-queries queries
         num-switches 0]
    (let [remaining-queries (find-remaining-queries unseen-engines remaining-queries)]
      (if (empty? remaining-queries)
        num-switches
        (recur (disj engines (first remaining-queries))
               remaining-queries
               (inc num-switches))))))


(defn parse-chunk
  "returns a parsed list of strings and the remaining seq of lines"
  [lines]
  (let [num-strings (Integer/parseInt (first lines))]
    [(take num-strings (rest lines))
     (drop num-strings (rest lines))]))


(defn read-input
  [f]
  (loop [problems-acc []
         remaining-lines (rest (line-seq (io/reader f)))]
    (if (empty? remaining-lines)
      problems-acc
      (let [[engines we-lines] (parse-chunk remaining-lines)
            [queries wq-lines] (parse-chunk we-lines)]
        (recur (conj problems-acc [(set engines) queries])
               wq-lines)))))


(defn solve
  [problems]
  (letfn [(reducer [acc [engines queries]]
            (conj acc (count-switches engines queries)))]
    (reduce reducer [] problems)))


(defn write-output
  [f solutions]
  (letfn [(reducer [acc [idx sol]]
            (conj acc (format "Case #%d: %d" idx sol)))]
    (->> (str/join "\n" (reduce reducer [] (map list (iterate inc 1) solutions)))
         (spit f))))
