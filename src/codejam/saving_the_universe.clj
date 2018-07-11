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


(defn parse-chunks
  "returns a lazy-seq of vecs of strings representing a chunk"
  [lines]
  (lazy-seq
   (when-let [s (seq lines)]
     (let [num-strings (Integer/parseInt (first lines))]
       (cons (take num-strings (rest lines))
             (parse-chunks (drop num-strings (rest lines))))))))


(defn read-input
  [f]
  (->> (line-seq (io/reader f))
       (drop 1)
       (parse-chunks)
       (partition 2)))


(defn solve
  [problems]
  (letfn [(mapper [[engines queries]]
            (count-switches (set engines) queries))]
    (map mapper problems)))


(defn write-output
  [f solutions]
  (letfn [(formatter [idx sol]
            (format "Case #%d: %d" idx sol))]
    (->> (map formatter (drop 1 (range)) solutions)
         (str/join "\n")
         (spit f))))
