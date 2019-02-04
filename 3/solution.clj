(defn create-edges [[id x-start y-start x-len y-len]]
  [{:value x-start :id id :type :start :dim :x}
   {:value (+ x-start x-len) :id id :type :end :dim :x}
   {:value y-start :id id :type :start :dim :y}
   {:value (+ y-start y-len) :id id :type :end :dim :y}])

(defn process-line2 [line]
  (->> line
       (re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)$")
       rest ; first group is whole matching string
       (map #(Integer/parseInt %))
       create-edges))

;; (defn file->blocks [my-file-name]
;;   (->> my-file-name
;;        slurp
;;        clojure.string/split-lines
;;        (map process-line2)))

;; (def blocks (file->blocks "smalltest.txt"))

(defn edges [filename] (->> filename
                            slurp
                            clojure.string/split-lines
                            (mapcat process-line2)))

(def edges-small (edges "smalltest.txt"))

(def edges-x-1 (filter #( and (= :y (:dim %)) (not= 3 (:id %))) edges-small))

(sort '(:start :end))
;; => (:end :start)

(sort-by (juxt :value :type) edges-x-1) 
;; => ({:value 0, :id 1, :type :start, :dim :y} {:value 1, :id 2, :type :start, :dim :y} {:value 2, :id 1, :type :end, :dim :y} {:value 6, :id 2, :type :end, :dim :y})

(seq '())
;; => nil

(first '() )
;; => nil

(defn myf
  ;; "walk over collection of edges. return the sum of the length of overlapping intervals. must be sorted already by value then type"
  ([coll] (myf 0 0 nil coll))
  ([sum-overlap overlappers start [f & r :as coll]]
   (if-not (seq coll)
     sum-overlap
     (let [end? (= :end (:type f))
           new-overlappers (if end? (dec overlappers) (inc overlappers))
           end-overlap? (and (= 1 new-overlappers) end?)
           start-overlap? (and (= 2 new-overlappers ) (not end?))
           ;; _ (println f)
           ]
       (cond
         end-overlap? (recur (+ sum-overlap (- (:value f) start)) new-overlappers nil r)
         start-overlap? (recur sum-overlap new-overlappers (:value f) r)
         :else (recur sum-overlap new-overlappers start r)))))) 


(myf (sort-by (juxt :value :type) edges-x-1) )
;; => 1


;; function that takes as input a set of ids and collection of edges and returns the subset of edges with id in set of ids
(defn select-edges
  [ids edges]
  (filter (fn [e] (contains? ids (:id e))) edges))

(def edges-at-xis1 (select-edges #{1 2} edges-small) )
;; => ({:value 0, :id 1, :type :start, :dim :x} {:value 4, :id 1, :type :end, :dim :x} {:value 0, :id 1, :type :start, :dim :y} {:value 2, :id 1, :type :end, :dim :y} {:value 1, :id 2, :type :start, :dim :x} {:value 6, :id 2, :type :end, :dim :x} {:value 1, :id 2, :type :start, :dim :y} {:value 6, :id 2, :type :end, :dim :y})



;; function that takes as input collection of edges and sorts them for traversal
(defn sort-edges
  [edges]
  (sort-by (juxt :value :type) edges))

(->> edges-small (filter #(= :y (:dim %))) (select-edges #{1 2}) sort-edges ) 
;; => ({:value 0, :id 1, :type :start, :dim :y} {:value 1, :id 2, :type :start, :dim :y} {:value 2, :id 1, :type :end, :dim :y} {:value 6, :id 2, :type :end, :dim :y})

(defn overlap-y [ids-present edges]
  (->> edges
     (filter #(= :y (:dim %)))
     (select-edges ids-present)
     sort-edges
     myf)) 

(overlap-y #{1 2 } edges-small)
;; => 1

(overlap-y #{3    2 } edges-small)
;; => 2

(loop [coll  edges-small]
  (println (first coll))
  (recur (rest coll))) 





;; (defn destructure-line [[_ id x-start y-start x-len y-len]]
;;   (let [f #(Integer/parseInt %)
;;         x-start (f x-start)
;;         y-start (f y-start)
;;         x-len (f x-len)
;;         y-len (f y-len)]
;;     {:x-start x-start
;;      :y-start y-start
;;      :x-end (+ x-start x-len)
;;      :y-end (+ y-start y-len)
;;      :x-len x-len
;;      :y-len y-len
;;      :id (f id)}))

;; (defn process-line [line]
;;   (->> line
;;        (re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)$")
;;        destructure-line))

