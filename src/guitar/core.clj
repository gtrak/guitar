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
  (into {}
        (for [[alias :as note-names] all-the-notes
              note note-names]
          [note alias])))

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

(defn prompt-string
  [string note]
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

(defprotocol GameMode
  (gen [this])
  ;; Given a generated note, prompt for input
  (prompt [this note])
  ;; Given a generated note and input, return truthy if they're equivalent
  (check [this note input])
  ;; Display the the right answer?
  (correct-answer [this note]))

(def fretmode
  (reify GameMode
    (gen [this]
      (between-frets 5 9))
    (prompt [this note]
      (let [{:keys [string note frets]} note]
        (prompt-fret string (rand-nth frets))))
    (check [this note input]
      (same-note? (:note note) input))
    (correct-answer [this note]
      (name (:note note)))))

(def notemode
  (reify GameMode
    (gen [this]
      (between-frets 5 9))
    (prompt [this note]
      (let [{:keys [string note frets]} note]
        (prompt-string string note)))
    (check [this note input]
      ((set (:frets note))
       (read-string input)))
    (correct-answer [this note]
      (:frets note))))

(defn game
  [mode]
  (loop [notes (gen mode)]
    (let [[note & more] notes
          input (prompt mode note)]
      (when (seq input)
        (println input)
        (if (check mode note input)
          (println "Correct!")
          (println "Incorrect! It was "
                   (correct-answer mode note)))
        (recur more)))))

;; (game)

