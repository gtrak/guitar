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

(def same-notes
  "Normalize sharps to flats"
  {:A :A
   :A# :Bb
   :Bb :Bb
   :B :B
   :C :C
   :C# :Db
   :Db :Db
   :D :D
   :D# :Eb
   :Eb :Eb
   :E :E
   :F :F
   :F# :Gb
   :Gb :Gb
   :G :G
   :G# :Ab
   :Ab :Ab})

(defn note-str
  [a]
  (clojure.string/lower-case (name a)))

(def same-notes-strtable
  (reduce-kv (fn [acc k v]
               (conj acc [(note-str k)
                          (note-str v)]))
             {}
             same-notes))

(defn same-note?
  [a b]
  (= (same-notes-strtable (note-str a))
     (same-notes-strtable (note-str b))))

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

;; (notes-of-fret :E 1)

(defn prompt [string note]
  (println "String: " (name string)
           " Note: " (name note)
           " Fret?")
  (if-let [l (read-line)]
    (clojure.string/trim l)))

(defn prompt-fret [string fret]
  (println "String: " (name string)
           " Fret: " fret
           " Note?")
  (if-let [l (read-line)]
    (clojure.string/trim l)))

(defrecord GuitarNote [string note frets])

(defn random-notes
  []
  (->> all-the-notes
       shuffle
       cycle))

(defn string-note
  [string note]
  (GuitarNote. string
               note
               (frets-of-note string note)))

(defn random-string-notes
  [string]
  (map (fn [note-names]
         (string-note string (rand-nth note-names)))
       (random-notes)))

;; (first (random-string-notes :E))

(defn new-string
  []
  (let [string (rand-nth strings)]
    (random-string-notes string)))

(defn one-string
  [string]
  (random-string-notes string))

(defn between-frets
  [a b]
  (let [[min max] [(min a b)
                   (max a b)]]
    (mapcat
     (fn [note-names]
       (for [string strings
             :let [note (string-note string (rand-nth note-names))]
             :when (some (fn [fret]
                           (<= min fret max))
                         (:frets note))]
         note))
     (random-notes))))

;; (take 5 (between-frets 8 12))



(defn game
  []
  (let [gen (partial between-frets 5 9)]
    (loop [notes (gen)]
      (let [[{:keys [string note frets]} & more] notes
            ;;frets (set frets)
            ;;input (prompt string note)
            input (prompt-fret string (rand-nth frets))]
        (when (seq input)
          (println input)
          ;; TODO: d#/eb are same note
          (if (same-note? note input)
            ;;(frets (read-string input))
            (println "Correct!")
            ;;(println "Incorrect! It was " frets)
            (println "Incorrect! It was " (name note))
            )
          (recur more))))))

;; (game)

