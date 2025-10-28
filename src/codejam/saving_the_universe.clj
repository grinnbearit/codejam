(ns codejam.saving-the-universe
  "Solves the 'Saving the Universe' Code Jam problem using a greedy algorithm."
  (:require [clojure.string :as str]))

;;; =============================================================================
;;; --- Utility Functions ---
;;; =============================================================================

(defn- parse-int [s]
  (Integer/parseInt s))

;;; =============================================================================
;;; --- Parsing Functions ---
;;; =============================================================================

(defn- parse-single-case
  "Parses one case from the given lines.
  Returns a vector: [parsed-case-map, remaining-lines]"
  [lines]
  (let [S (parse-int (first lines))
        engines (take S (rest lines))
        lines-after-S (drop (inc S) lines)

        Q (parse-int (first lines-after-S))
        queries (take Q (rest lines-after-S))
        lines-after-Q (drop (inc Q) lines-after-S)]

    [{:engines engines :queries queries} lines-after-Q]))


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
;;; --- Solving Function
;;; =============================================================================

(defn solve-case
  "Solves a single parsed case using the greedy algorithm."
  [{:keys [engines queries]}]
  (let [S (count engines)] ; We only need the *number* of engines

    ;; We use reduce to process all queries one by one.
    ;; The accumulator 'state' is a map holding the switch count
    ;; and the set of unique engines seen in the *current* segment.
    (let [final-state (reduce
                        (fn [state query]
                          (let [{:keys [switches segment-engines]} state]

                            (if (contains? segment-engines query)
                              ;; Case 1: Query already seen in this segment.
                              ;; No change to state, just continue.
                              state

                              ;; Case 2: This is a new, unique query for this segment.
                              (if (= (count segment-engines) (dec S))
                                ;; Case 2a: The segment is "full" (has S-1 unique engines).
                                ;; This new query forces a mandatory switch.
                                {:switches (inc switches)        ; Increment switch count
                                 :segment-engines #{query}}      ; Start a new segment

                                ;; Case 2b: The segment is not full.
                                ;; Just add this query to the current segment.
                                {:switches switches
                                 :segment-engines (conj segment-engines query)}))))

                        ;; Initial state for the reduce:
                        {:switches 0, :segment-engines #{}}

                        ;; The collection to process:
                        queries)]

      ;; The final result is the total number of switches.
      (:switches final-state))))

;;; =============================================================================
;;; --- Formatting Functions ---
;;; =============================================================================

(defn- format-case-result
  "Formats a single result into 'Case #X: Y' string."
  [case-num result]
  (str "Case #" case-num ": " result "\n"))

;;; =============================================================================
;;; --- Main Entry Point ---
;;; =============================================================================

(defn solve-from-file
  "Main function: Reads file, solves all cases, returns formatted string."
  [filename]
  (->> (slurp filename)
       (parse-input-string) ; string -> [{:engines... :queries...}, ...]
       (map solve-case)     ; [case-data, ...] -> [result1, result2, ...]
       (map-indexed (fn [idx result] (format-case-result (inc idx) result))) ; [0, 1, ...] -> ["Case #1: ...", ...]
       (reduce str)))
