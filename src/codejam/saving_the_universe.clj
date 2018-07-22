(ns codejam.saving-the-universe
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;;; https://code.google.com/codejam/contest/32013/dashboard#s=p0


(defn find-remaining-queries
  "returns `remaining-queries`, the queries left after the longest possible run without switching.
  Returns an empty list if no queries are left to execute."
  [engines queries]
  (let [unswitched (->> (reductions disj engines queries)
                        (take-while seq)
                        (count))]
    (drop (dec unswitched) queries)))


(s/def ::engine string?)
(s/def ::query string?)

(s/fdef find-remaining-queries
        :args (s/cat ::engines (s/coll-of ::engine)
                     ::queries (s/coll-of ::query))
        :ret (s/coll-of ::query))


(defn count-switches
  "returns the number of switches required to execute all queries"
  [engines queries]
  (loop [remaining-queries (find-remaining-queries engines queries)
         num-switches 0]
    (if (empty? remaining-queries)
      num-switches
      (recur (find-remaining-queries (disj engines (first remaining-queries))
                                     remaining-queries)
             (inc num-switches)))))


(s/fdef count-switches
        :args (s/cat ::engines (s/coll-of ::engine)
                     ::queries (s/coll-of ::query))
        :ret nat-int?)


(defn parse-chunks
  "returns a lazy-seq of vecs of strings representing a chunk"
  [lines]
  (lazy-seq
   (when-let [s (seq lines)]
     (let [num-strings (Integer/parseInt (first lines))]
       (cons (take num-strings (rest lines))
             (parse-chunks (drop num-strings (rest lines))))))))


(s/def ::chunk (s/coll-of string?))

(s/fdef parse-chunks
        :args (s/cat ::lines (s/coll-of string?))
        :ret (s/coll-of ::chunk)
        :fn (fn [spec]
              (let [num-lines (count (-> spec :args ::lines))
                    num-chunks (count (-> spec :ret))
                    num-chunk-lines (apply + (map count (-> spec :ret)))]
                (= num-lines
                   (+ num-chunks num-chunk-lines)))))


(defn read-input
  [f]
  (with-open [rdr (io/reader f)]
    (->> (line-seq rdr)
         (drop 1)
         (parse-chunks)
         (partition 2)
         (map vec))))


(s/def ::problem (s/tuple (s/coll-of ::engine)
                          (s/coll-of ::query)))

(s/fdef read-input
        :args (s/cat ::f io/reader)
        :ret (s/coll-of ::problem))


(defn solve
  [problems]
  (letfn [(mapper [[engines queries]]
            (count-switches (set engines) queries))]
    (map mapper problems)))


(s/fdef solve
        :args (s/cat ::problems (s/coll-of ::problem))
        :ret (s/coll-of nat-int?))


(defn write-output!
  [f solutions]
  (letfn [(formatter [idx sol]
            (format "Case #%d: %d" idx sol))]
    (->> (map formatter (drop 1 (range)) solutions)
         (str/join "\n")
         (spit f))))


(s/fdef write-output!
        :args (s/cat ::f string?
                     ::solutions (s/coll-of nat-int?))
        :ret nil?)
