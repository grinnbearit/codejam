(ns codejam.train-timetable
  "Solves the 'Train Timetable' Code Jam problem."
  (:require [clojure.string :as str]))

;;; =============================================================================
;;; --- Utility Functions ---
;;; =============================================================================

(defn- parse-int [s]
  (Integer/parseInt s))


(defn- time-to-int
  "Converts an 'HH:MM' string to total minutes since midnight."
  [time-str]
  (let [[hours minutes] (->> (str/split time-str #":")
                             (map parse-int))]
    (+ (* hours 60) minutes)))

;;; =============================================================================
;;; --- Parsing Functions ---
;;; =============================================================================

(defn- parse-trips
  "Parses N trip lines, returning a list of maps."
  [n lines]
  (let [trip-lines (take n lines)]
    (map (fn [line]
           (let [[dep-str arr-str] (str/split line #" ")]
             {:dep (time-to-int dep-str)
              :arr (time-to-int arr-str)}))
         trip-lines)))


(defn- parse-single-case
  "Parses one case from the given lines.
  Returns a vector: [parsed-case-map, remaining-lines]"
  [lines]
  (let [T (parse-int (first lines))
        [na-str nb-str] (str/split (second lines) #" ")
        NA (parse-int na-str)
        NB (parse-int nb-str)

        lines-after-header (drop 2 lines)
        trips-A-B (parse-trips NA lines-after-header)

        lines-after-A (drop NA lines-after-header)
        trips-B-A (parse-trips NB lines-after-A)

        lines-after-B (drop NB lines-after-A)]

    [{:T T, :trips-A-B trips-A-B, :trips-B-A trips-B-A}
     lines-after-B]))


(defn parse-input-string
  "Parses the full input string into a lazy sequence of case-maps."
  [input-string]
  (let [lines (str/split-lines input-string)
        num-cases (parse-int (first lines))]
    (loop [remaining-lines (rest lines)
           n num-cases
           cases []]
      (if (zero? n)
        cases
        (let [[case-data next-lines] (parse-single-case remaining-lines)]
          (recur next-lines (dec n) (conj cases case-data)))))))

;;; =============================================================================
;;; --- Solving Functions ---
;;; =============================================================================

(defn- calculate-trains
  "Calculates the minimum trains needed at one station.
  - 'departures' is a list of departure times.
  - 'arrivals' is a list of arrival times at this station.
  - 'turnaround-T' is the time to add to arrivals."
  [departures arrivals turnaround-T]

  (let [;; Sort all the 'demand' events
        sorted-departures (sort departures)

        ;; Sort all the 'supply' events (when trains become ready)
        sorted-ready-times (->> arrivals
                                (map (fn [arr] (+ arr turnaround-T)))
                                (sort))]

    ;; Loop through departures, consuming ready trains when possible
    (loop [ds sorted-departures
           rs sorted-ready-times
           trains-needed 0]

      (if (empty? ds)
        ;; Base case: No more departures to serve.
        trains-needed

        ;; Recursive step:
        (let [d1 (first ds)             ; Next departure
              r1 (first rs)             ; Earliest ready train
              remaining-ds (rest ds)]

          (cond
            (empty? rs)
            ;; No ready trains left, must use a new train.
            (recur remaining-ds rs (inc trains-needed))

            (<= r1 d1)
            ;; A train is ready! (r1 <= d1).
            ;; We can reuse this train. Consume *both* the departure and the ready train.
            (recur remaining-ds (rest rs) trains-needed)

            (> r1 d1)
            ;; The earliest ready train (r1) is too late for this departure (d1).
            ;; We must use a new train for d1.
            ;; The ready train r1 remains available for a *later* departure.
            (recur remaining-ds rs (inc trains-needed))))))))


(defn solve-case
  "Solves a single parsed case."
  [{:keys [T trips-A-B trips-B-A]}]

  ;; Get the 4 lists of events
  (let [departures-A (map :dep trips-A-B)
        arrivals-B   (map :arr trips-A-B)

        departures-B (map :dep trips-B-A)
        arrivals-A   (map :arr trips-B-A)

        ;; Calculate trains needed *at station A*
        trains-A (calculate-trains departures-A arrivals-A T)

        ;; Calculate trains needed *at station B*
        trains-B (calculate-trains departures-B arrivals-B T)]

    [trains-A trains-B])) ; Return the pair


;;; =============================================================================
;;; --- Formatting Functions ---
;;; =============================================================================

(defn- format-case-result
  "Formats a single result into 'Case #X: Y Z' string."
  [case-num [trains-A trains-B]]
  (str "Case #" case-num ": " trains-A " " trains-B "\n"))


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
