(ns codejam.fly-swatter
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))


(defn parse-line
  [line]
  (let [[f r t c g] (str/split line #" ")]
    [(Double/parseDouble f)
     (Double/parseDouble r)
     (Double/parseDouble t)
     (Double/parseDouble c)
     (Double/parseDouble g)]))


(s/def ::fly pos?)
(s/def ::raquet pos?)
(s/def ::thickness pos?)
(s/def ::chord pos?)
(s/def ::gap pos?)
(s/def ::problem (s/tuple ::fly
                          ::raquet
                          ::thickness
                          ::chord
                          ::gap))

(s/fdef parse-line
        :args (s/cat ::line string?)
        :ret ::problem)


(defn read-input
  [f]
  (with-open [rdr (io/reader f)]
    (->> (line-seq rdr)
         (drop 1)
         (map parse-line)
         (doall))))

(s/fdef read-input
        :args (s/cat ::f io/reader)
        :ret (s/coll-of ::problem))
