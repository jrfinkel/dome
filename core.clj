(ns dome.core)

;;; helpers

(defn truncate [f] (double (/ (int (* f 1e7)) 1e7)))

(defn distinct-by
  "Returns elements of xs which return unique
   values according to f. If multiple elements of xs return the same
   value under f, the first is returned"
  [f xs]
  (let [s (java.util.HashSet.)]
    (for [x xs
          :let [id (f x)]
          :when (not (.contains s id))]
      (do (.add s id)
          x))))

;;; math

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))
(defn atan [x] (Math/atan x))
(defn sq [x] (Math/pow x 2))

(defn ->cartesian [point]
  {:z (* (:radius point) (sin (:theta point)) (cos (:phi point)))
   :y (* (:radius point) (sin (:theta point)) (sin (:phi point)))
   :x (* (:radius point) (cos (:theta point)))})

;;; geometry

(defn distance [point1 point2]
  (Math/sqrt
   (- (+ (sq (:radius point1)) (sq (:radius point2)))
      (* 2 (:radius point1) (:radius point2)
         (+ (* (cos (:theta point1))
               (cos (:theta point2)))
            (* (sin (:theta point1))
               (sin (:theta point2))
               (cos (- (:phi point1) (:phi point2)))))))))

;; radius of an ellipse at theta, with radii r1 r2
(defn ->radius [r1 r2 theta]
  (let [e2 (sq (/ r1 r2))]
    (* r2 (Math/sqrt (/ e2 (+ (* e2 (sq (sin theta))) (sq (cos theta))))))))

;; from the table
(defn ->icosa-triangle2-angles [[a b] freq]
  (let [[p t]
        (case [freq [a b]]
          ([2 [3 1]] [4 [6 2]]) [18. 90.]
          ([2 [3 2]] [4 [6 4]]) [54. 90.]
          ([2 [4 2]] [4 [8 4]]) [36. 116.5650512]
          [4 [5 1]] [8.7723551 75.4545635]
          [4 [5 2]] [26.2676986 73.9549430]
          [4 [5 3]] [45.7323015 73.9549430]
          [4 [5 4]] [63.2276449 75.4545635]
          [4 [6 3]] [36. 90.]
          [4 [7 3]] [27.2276449 104.5454366]
          [4 [7 4]] [44.7723551 104.5454366]
          [5 [6 1]] [6.9684505 72.7969533]
          [5 [6 2]] [20.4898913 71.1600132]
          [5 [6 3]] [36. 70.5008455]
          [5 [6 4]] [51.5101087 71.1600132]
          [5 [6 5]] [65.0315495 72.7969533]
          [5 [7 2]] [14.2819224 84.0101625]
          [5 [7 3]] [28.4369994 83.6117970]
          [5 [7 4]] [43.5630006 83.6117970]
          [5 [7 5]] [57.7180776 84.0101625]
          [5 [8 3]] [21.7180776 95.9898376]
          [5 [8 4]] [36. 96.1794105]
          [5 [8 5]] [50.2819224 95.9898376]
          [5 [9 4]] [29.0315495 107.2030468]
          [5 [9 5]] [42.9484505 107.2030468]
          [5 [10 5]] [36. 116.5650512])]
    {:phi (Math/toRadians p) :theta (Math/toRadians t)}))

;;; domes!

(defn points-2d [freq & [icosa-ellipse?]]
  (for [x (range 0 (inc (* (if icosa-ellipse? 2 1) freq)))
        y (range 0 (inc freq))
        :when (and (>= x y) (<= (- x y) freq))]
    [x y]))

(defn neighbors-2d [point points]
  (for [p1 [point]
        p2 points
        :when (and (not= [p1 p2] (sort [p1 p2]))
                   (every? #(<= 0 (- (nth p1 %) (nth p2 %)) 1) [0 1]))]
    p2))

(defn ->3d [[a b] freq]
  (let [x b
        y (- a b)
        z (- freq x y)]
    [x y z]))

(defn ->angles-2d [[a b] freq & [icosa-ellipse?]]
  (if (> a freq)
    (do (assert icosa-ellipse?)
        (->icosa-triangle2-angles [a b] freq))
    (let [[x y z] (->3d [a b] freq)
          x1 (if icosa-ellipse? (* x (sin (Math/toRadians 72))) x)
          y1 (if icosa-ellipse? (+ y (* x (cos (Math/toRadians 72)))) y)
          z1 (if icosa-ellipse? (+ (/ freq 2) (/ z (* 2 (cos (Math/toRadians 36))))) z)]
      {:phi (cond (zero? x1) 0 (zero? y1) (Math/toRadians (if icosa-ellipse? 72 90)) :else (atan (/ x1 y1)))
       :theta (if (zero? z1) (Math/toRadians (if icosa-ellipse? 72 90)) (atan (/ (Math/sqrt (+ (sq x1) (sq y1))) z1)))})))

(defn correction [angles E & [correction-type]]
  (if-not correction-type
    angles
    (do (assert (#{:E :E2 :E3 :E4} correction-type))
        (update-in angles [:theta] (fn [theta]
                                     (atan (/ (tan theta)
                                              (case correction-type
                                                :E2 (Math/sqrt E)
                                                :E3 (Math/pow E (/ 1 3))
                                                :E4 (Math/pow E (/ 1 20))
                                                :E E))))))))

(defn add-radius [angles r1 r2]
  (assoc angles :radius (->radius r1 r2 (:theta angles))))

(defn project-point [point freq r1 r2 & [correction-type icosa-ellipse?]]
  (-> (->angles-2d point freq icosa-ellipse?)
      (correction (/ r1 r2) correction-type)
      (add-radius r1 r2)
      (assoc :point-2d point :point-3d (->3d point freq))))

(defn struts [freq r1 r2 & [correction-type icosa-ellipse?]]
  (let [points (points-2d freq icosa-ellipse?)]
    (for [point points
          neighbor (neighbors-2d point points)
          :let [p1 (project-point point freq r1 r2 correction-type icosa-ellipse?)
                p2 (project-point neighbor freq r1 r2 correction-type icosa-ellipse?)]]
      [p1 p2 (distance p1 p2)])))

(defn ->display-format [struts icosa? outfile]
  (spit outfile
        (clojure.string/join "\n"
                             (concat
                              ["model = Sketchup.active_model" "entities = model.active_entities"]
                              (for [[p1 p2] struts
                                    [delta-phi m] (if icosa?
                                                    [[0 1] [72 1] [144 1] [216 1] [288 1] [0 -1] [72 -1] [144 -1] [216 -1] [288 -1]]
                                                    [[-90 1] [0 1] [90 -1] [180 -1]])
                                    :let [c1 (->cartesian (update-in p1 [:phi] + delta-phi))
                                          c2 (->cartesian (update-in p2 [:phi] + delta-phi))]]
                                (format "entities.add_line [%s, %s, %s], [%s, %s, %s]"
                                        (* m (:x c1)) (:y c1) (* m (:z c1))
                                        (* m (:x c2)) (:y c2) (* m (:z c2))))))))

(defn ->strut-counts [struts]
  (->> struts (map (comp truncate last)) frequencies (sort-by first)))

(defn display [struts]
  (pprint
   (map (fn [[p1 p2 dist]]
          [(:point-2d p2) (:point-2d p1) dist])
        struts)))
