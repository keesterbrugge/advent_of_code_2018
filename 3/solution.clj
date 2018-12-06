(defn destructure-line [[_ id x-start y-start x-len y-len]]
  (let [f #(Integer/parseInt %)
        x-start (f x-start)
        y-start (f y-start)
        x-len (f x-len)
        y-len (f y-len)]
    {:x-start x-start
     :y-start y-start
     :x-end (+ x-start x-len)
     :y-end (+ y-start y-len)
     :x-len x-len
     :y-len y-len
     :id (f id)}))

(defn process-line [line]
  (->> line
       (re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)$")
       destructure-line))

(defn file->blocks [my-file-name]
  (map process-line (clojure.string/split-lines (slurp my-file-name))))

(file->blocks "input3.1.txt")



(+ 2 2)
