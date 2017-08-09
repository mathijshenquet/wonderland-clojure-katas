(ns tiny-maze.solver)

(def start :S)
(def end :E)

(defn start-index
  [row]
  (first
    (first (filter #(= start (second %))
                   (map-indexed vector row)))))

(defn find-start
  [maze]
  (first (filter second
                 (map-indexed vector (map start-index maze)))))

(defn adj [pos]
  (let [[x y] pos]
    [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn next-steps [maze prevs pos]
  (filter #(and (#{0 end} (get-in maze %))
                (not (prevs %))) (adj pos)))

(defn find-path [maze prevs pos]
  (if (= end (get-in maze pos))
    (into prevs [pos])
    (first (map #(find-path maze (into prevs [pos]) %)
            (next-steps maze prevs pos)))))

(defn render-solution
  [maze path]
  (reduce (fn [acc nxt]
            (update-in acc nxt (constantly :x)))
          maze path))

(defn solve-maze [maze]
  (let [start (find-start maze)]
    (render-solution maze
                     (find-path maze #{start} start))))