(ns codejam.fly-swatter
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [swissknife.core :refer [map-values]]))


(s/def ::fly pos?)
(s/def ::racquet pos?)
(s/def ::thickness pos?)
(s/def ::chord pos?)
(s/def ::gap pos?)
(s/def ::position (s/tuple double? double?))
(s/def ::problem (s/tuple ::fly ::racquet ::thickness ::chord ::gap))
(s/def ::probability (fn [x] (<= 0 x 1)))


(defn within-racquet?
  "The fly's centre is inside the outside racquet radius"
  [racquet [x y]]
  (<= (+ (Math/pow x 2)
         (Math/pow y 2))
      (Math/pow racquet 2)))


(s/fdef within-racquet?
        :args (s/cat ::racquet ::racquet
                     ::position ::position)
        :ret boolean?)


(defn hit-by-rim?
  "Given a position within the racquet, returns if the fly
  was hit by the rim"
  [racquet thickness fly [x y]]
  (<= (- racquet thickness)
      (+ (Math/sqrt (+ (Math/pow x 2)
                       (Math/pow y 2)))
         fly)))


(s/fdef hit-by-rim?
        :args (s/cat ::racquet ::racquet
                     ::thickness ::thickness
                     ::fly ::fly
                     ::position ::position)
        :ret boolean?)


(defn hit-by-chord?
  "Given a position within the racquet, returns if the fly
  was hit by a chord"
  [chord gap fly [x y]]
  (let [closest-x (- (Math/abs x) fly)
        closest-y (- (Math/abs y) fly)
        shifted-x (- closest-x chord)
        shifted-y (- closest-y chord)
        space-x (rem shifted-x (+ gap (* 2 chord)))
        space-y (rem shifted-y (+ gap (* 2 chord)))]
    (or (<= closest-x chord)
        (<= closest-y chord)
        (zero? space-x)
        (zero? space-y)
        (< gap (+ (* 2 fly) space-x))
        (< gap (+ (* 2 fly) space-y)))))


(s/fdef hit-by-chord?
        :args (s/cat ::chord ::chord
                     ::gap ::gap
                     ::fly ::fly
                     ::position ::position)
        :ret boolean?)


(defn hit-by-racquet?
  "combines `hit-by-rim?` and `hit-by-chord?`"
  [racquet thickness chord gap fly position]
  (or (hit-by-rim? racquet thickness fly position)
      (hit-by-chord? chord gap fly position)))


(s/fdef hit-by-racquet?
        :args (s/cat ::racquet ::racquet
                     ::thickness ::thickness
                     ::chord ::chord
                     ::gap ::gap
                     ::fly ::fly
                     ::position ::position))


(defn generate-position!
  "returns a randomly generated position [x y] within the
  bounding box of the racquet"
  [racquet]
  [(* (rand racquet)
      (rand-nth [-1 1]))
   (* (rand racquet)
      (rand-nth [-1 1]))])


(s/fdef generate-position!
        :args (s/cat ::racquet ::racquet)
        :ret ::position)


(defn probability
  "returns the probability of the fly being hit by
  the racquet, `num-samples` is the number of samples simulated"
  [num-samples fly racquet thickness chord gap]
  (let [tally (->> (repeatedly num-samples (partial generate-position! racquet))
                   (filter (partial within-racquet? racquet))
                   (map (partial hit-by-racquet? racquet thickness chord gap fly))
                   (frequencies))]
    (double (/ (get tally true 0)
               (+ (get tally true 0) (get tally false 0))))))


(s/fdef probability
        :args (s/cat ::num-samples pos-int?
                     ::fly ::fly
                     ::racquet ::racquet
                     ::thickness ::thickness
                     ::chord ::chord
                     ::gap ::gap))


(defn parse-line
  [line]
  (let [[f r t c g] (str/split line #" ")]
    [(Double/parseDouble f)
     (Double/parseDouble r)
     (Double/parseDouble t)
     (Double/parseDouble c)
     (Double/parseDouble g)]))


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


(defn solve
  [problems]
  (letfn [(mapper [problem]
            (apply probability 1000000 problem))]

    (map mapper problems)))


(s/fdef solve
        :args (s/cat ::problems (s/coll-of ::problem))
        :ret (s/coll-of ::probability))


(defn write-output!
  [f solutions]
  (letfn [(formatter [idx sol]
            (format "Case #%d: %f" idx sol))]
    (->> (map formatter (drop 1 (range)) solutions)
         (str/join "\n")
         (spit f))))
