(ns guitar.core)

(defn notes
  []
  (cycle [[:A]
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
          [:G# :Ab]]))

(defn containing-note
  [note within-notes]
  (some #{note} within-notes))

(defn not-containing-note
  [note within-notes]
  (not (containing-note note within-notes)))

(defn from
  [note]
  (drop-while (partial not-containing-note note)
              (notes)))

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

;; (= (fret :E :A) [5 17])
