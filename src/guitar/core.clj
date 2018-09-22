(ns guitar.core)

(def all-the-notes
  [[:A]
   [:A# :Bb]
   [:B]
   [:C]
   [:C# :Db]
   [:D]
   [:D# :Eb]
   [:E]
   [:F]
   [:F# :Gb]
   [:G]
   [:G# :Ab]])

(defn containing-note
  [note within-notes]
  (some #{note} within-notes))

(defn not-containing-note
  [note within-notes]
  (not (containing-note note within-notes)))

(defn from
  [note]
  (drop-while (partial not-containing-note note)
              (cycle all-the-notes)))

(defn steps
  [note half-steps]
  (-> (from note)
      (nth half-steps)))
;; (steps :A 13)

(def strings
  [:E :A :D :G :B :E])

(defn frets-of-note
  [string note]
  (let [max-fret 22]
    (->> (map-indexed vector (from string))
         (filter (fn [[idx notes]]
                   (if (containing-note note notes)
                     idx)))
         (take-while (fn [[idx notes]]
                       (<= idx max-fret)))
         (map first))))

;; (frets-of-note :B (first (steps :C# 0)))
;; => 2, 14

(defn notes-of-fret
  [string fret]
  (nth (from string)
       fret))

(notes-of-fret :E 1)

(defn random-notes
  []
  (->> all-the-notes shuffle cycle))

(defn random-string-notes
  [string]
  (map (fn [note]
         (let [note (rand-nth note)]
           [note (frets-of-note string note)]))
       (random-notes)))

(take 5 )
;; (= (fret :E :A) [5 17])

(first (random-string-notes :E))

(defn prompt [string note]
  (println "String: " (name string)
           " Note: " (name note)
           " Fret?")
  (clojure.string/trim (read-line)))

(defn game
  []
  (let [new-string (fn []
                     (let [string (rand-nth strings)]
                       [string (random-string-notes string)]))]
    (loop [[string notes] (new-string)]
      (let [[[note frets] & more] notes
            frets (set frets)
            input (prompt string note)]
        (when (seq input)
          (if (frets (read-string input))
            (println "Correct!")
            (println "Incorrect! It was " frets))
          (recur (new-string)))))))

;; (game)

