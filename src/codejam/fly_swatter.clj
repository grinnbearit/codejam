(ns codejam.fly-swatter
  "Solves the 'Fly Swatter' Code Jam problem."
  (:require [clojure.string :as str]))

;;; =============================================================================
;;; --- Utility Functions ---
;;; =============================================================================

(defn- parse-double [s]
  (Double/parseDouble s))

;; Use Java's Math/pow for speed
(defn- sq [x]
  (Math/pow x 2))

;;; =============================================================================
;;; --- Parsing Functions ---
;;; =============================================================================

(defn- parse-single-case
  "Parses one case from the given line.
  Returns a map of the 5 parameters."
  [line]
  (let [[f R t r g] (map parse-double (str/split line #" "))]
    {:f f :R R :t t :r r :g g}))


(defn parse-input-string
  "Parses the full input string into a lazy sequence of case-maps."
  [input-string]
  (let [lines (str/split-lines input-string)
        num-cases (Integer/parseInt (first lines))]
    (map parse-single-case (take num-cases (rest lines)))))


;;; =============================================================================
;;; --- Solving Function ---
;;; =============================================================================

(defn solve-case
  "Solves a single parsed case."
  [{:keys [f R t r g]}]

  (let [;; 1. Calculate the 'safe' radius (misses the ring)
        ;; R_prime = R - t - f
        R-prime (max 0.0 (- R t f))

        ;; 2. Calculate the 'safe' gap side length (misses the strings)
        ;; s = g - 2f
        s (max 0.0 (- g (* 2.0 f)))

        ;; 3. Calculate the pitch (string-center to string-center)
        ;; p = g + 2r
        p (+ g (* 2.0 r))]

    (if (or (zero? R-prime) (zero? s))
      ;; If either safe radius or safe gap is zero, we must hit.
      1.0

      ;; 4. Calculate P(Miss)
      ;; P(Miss) = (Area_safe_circle / Area_total) * (Area_safe_gap / Area_unit_cell)
      ;; P(Miss) = (pi * R_prime^2 / pi * R^2) * (s^2 / p^2)
      (let [ratio-R (sq (/ R-prime R))
            ratio-g (sq (/ s p))
            prob-miss (* ratio-R ratio-g)]

        ;; 5. P(Hit) = 1 - P(Miss)
        (- 1.0 prob-miss)))))


;;; =============================================================================
;;; --- Formatting Functions ---
;;; =============================================================================

(defn- format-case-result
  "Formats a single result into 'Case #X: P' string."
  [case-num prob]
  ;; We must format to at least 6 decimal places, 8 is safer.
  (format "Case #%d: %.8f\n" case-num prob))


;;; =============================================================================
;;; --- Main Entry Point ---
;;; =============================================================================

(defn solve-from-file
  "Main function: Reads file, solves all cases, returns formatted string."
  [filename]
  (->> (slurp filename)
       (parse-input-string)
       (map solve-case)
       (map-indexed (fn [idx result] (format-case-result (inc idx) result)))
       (reduce str)))
