(ns dome.core-test
  (:use dome.core clojure.test))

(deftest distinct-by-test
  (is (= [0 1] (distinct-by odd? (range 10)))))

(deftest round-test
  (is (= 0.3333333 (round (double (/ 1 3)))))
  (is (= 0.6666667 (round (double (/ 2 3))))))

(deftest mean-test
  (is (= (double (mean [1 2 3 4 5 6 7 8 9 10])) 5.5))
  (is (= (double (mean [1])) 1.0))
  (is (= (double (mean [])) 0.0)))

(defn approx-= [x y]
  (boolean (< (Math/abs (- x y)) 1e-4)))

(deftest ->cartesian-test
  (doseq [[[r t p] [z y x]] [[[1.0 0.0 0.0] [0.0 0.0 1.0]]
                             [[1.0 Math/PI 0.0] [0.0 0.0 -1.0]]
                             [[1.0 (/ Math/PI 2) 0.0] [1.0 0.0 0.0]]
                             [[1.0 0.0 (/ Math/PI 2)] [0.0 0.0 1.0]]
                             [[1.0 (/ Math/PI 2) (/ Math/PI 2)] [0.0 1.0 0.0]]
                             [[1.0 (/ Math/PI -2) (/ Math/PI 2)] [0.0 -1.0 0.0]]
                             [[1.0 (/ Math/PI 2) (/ Math/PI -2)] [0.0 -1.0 0.0]]
                             [[1.0 (/ Math/PI -2) (/ Math/PI -2)] [0.0 1.0 0.0]]
                             [[1.0 (/ Math/PI 2) (/ Math/PI 4)] [(/ (Math/sqrt 2.0) 2.0) (/ (Math/sqrt 2.0) 2.0) 0.0]]]]
    (let [{x1 :x y1 :y z1 :z} (->cartesian {:radius r :theta t :phi p})]
      (doseq [[coord a b] [[:x x x1] [:y y y1] [:z z z1]]]
        (testing (format "%s r=%s t=%s p=%s" coord r t p)
          (is (approx-= a b))))))
  (dotimes [_ 10]
    (is (approx-= 0.0 (:x (->cartesian {:radius 1.0 :theta (/ Math/PI 2) :phi (rand (* 2 Math/PI))}))))
    (is (approx-= 0.0 (:z (->cartesian {:radius 1.0 :theta (rand (* 2 Math/PI)) :phi (/ Math/PI 2)}))))
    (is (approx-= 0.0 (:y (->cartesian {:radius 1.0 :theta (rand (* 2 Math/PI)) :phi Math/PI}))))))

;; TODO: update after cartesian update
(deftest distance-test
  (is (= 2.0
         (distance {:radius 1.0 :theta 0.0 :phi 0.0}
                   {:radius 1.0 :theta Math/PI :phi 0.0})))
  (is (approx-= (Math/sqrt 2.0)
                (distance {:radius 1.0 :theta 0.0 :phi 0.0}
                          {:radius 1.0 :theta (/ Math/PI 2) :phi 0.0}))))

(deftest ->radius-test
  (is (= 1.0 (->radius 1.0 1.0 Math/PI)))
  (is (= 1.0 (->radius 1.0 2.0 0)))
  (is (= 1.0 (->radius 1.0 2.0 Math/PI)))
  (is (= 2.0 (->radius 1.0 2.0 (/ Math/PI 2))))
  (is (= 2.0 (->radius 1.0 2.0 (/ Math/PI -2)))))

(deftest points-2d-test
  (is (= [[0 0] [1 0] [1 1] [2 0] [2 1] [2 2]]
         (points-2d 2 false)))
  (is (= [[0 0] [1 0] [1 1] [2 0] [2 1] [2 2] [3 0] [3 1] [3 2] [3 3] [4 1] [4 2] [4 3] [5 2] [5 3] [6 3]]
         (points-2d 3 true))))

(deftest neighbords-2d-test
  (is (= [[0 0] [1 0]]
         (neighbors-2d [1 1] (points-2d 2 false))))
  (is (= [[1 1] [2 1]]
         (neighbors-2d [2 2] (points-2d 2 false))))
  (is (= [[1 1] [2 1]]
         (neighbors-2d [2 2] (points-2d 2 true))))
  (is (= [[2 0] [2 1]]
         (neighbors-2d [3 1] (points-2d 2 true))))
  (is (= []
         (neighbors-2d [4 1] (points-2d 2 false)))))

(deftest ->3d-test
  (doseq [[d2 d3] [[[0 0] [0 0 2]]
                   [[1 0] [0 1 1]]
                   [[1 1] [1 0 1]]
                   [[2 0] [0 2 0]]
                   [[2 1] [1 1 0]]
                   [[2 2] [2 0 0]]]]
    (is (= d3 (->3d d2 2)))))

(deftest ->angles-2d-test
  ;; #s are from the book
  (doseq [[freq points] [[6 [[[0 0] 0.0 0.0]
                             [[1 0] 0.0 9.3247035]
                             [[1 1] 72.0 9.3247035]
                             [[2 0] 0.0 20.0767513]
                             [[2 1] 36.0 16.4722107]
                             [[3 0] 0.0 31.7174744]
                             [[3 1] 22.3861776 27.2237351]
                             [[3 2] 49.6138225 27.2237351]
                             [[4 1] 16.0353713 39.1034177]
                             [[4 2] 36 37.3773682]]]
                         [2 [[[0 0] 0.0 0.0]
                             [[1 0] 0.0 31.7174743]
                             [[1 1] 72.0 31.7174743]
                             [[2 0] 0.0 63.4349488]
                             [[2 1] 36.0 58.2825255]
                             [[3 1] 18.0 90.0]
                             [[3 2] 54.0 90.0]]]]
          [pt phi theta] points]
    (let [{p :phi t :theta} (map-vals #(Math/toDegrees %) (->angles-2d pt freq true))]
      (testing (str freq " " pt " phi" p " " phi)
        (is (approx-= p phi)))
      (testing (str freq " " pt " theta [" t "] [" theta "]")
        (is (approx-= t theta))))))

(deftest add-radius-test
  (doseq [[pt radius] [[[0 0] 0.66666667]
                       [[1 0] 0.72461704]
                       [[1 1] 0.72461704]
                       [[2 0] 0.89442719]
                       [[2 1] 0.86210372]
                       [[3 1] 1.0]
                       [[3 2] 1.0]]]
    (is (approx-= radius
                  (:radius (add-radius (->angles-2d pt 2 true) (/ 1.0 1.5) 1.0))))))

(deftest within-distance-test
  (is (within-difference? 0.1 [0.01 0.02 0.03 0.04 0.07]))
  (is (not (within-difference? 0.1 [0.01 0.02 0.03 0.04 0.11]))))

(deftest consolidate-struts-mapping-test
  (is (= {0.2 0.21, 0.21 0.21, 0.22 0.21, 0.1 0.11, 0.11 0.11, 0.12 0.11, 0.23 0.23}
         (consolidate-struts-mapping 0.03 [0.10 0.11 0.12 0.20 0.21 0.22 0.23]))))

(deftest find-best-cuts-test

  )

(deftest struts-test
  (let [freq 2
        the-struts (fn [freq & [margin filter? r1 r2]]
                     (->> (struts freq (or r1 20) (or r2 20) true)
                          (keep (fn [[{pt1 :point-2d} {pt2 :point-2d} length]]
                                  (when (or (not filter?)
                                            (and (<= (first pt1) freq)  (<= (first pt2) freq)))
                                    length)))
                          (map #(round % (or margin 1e2)))
                          frequencies))]
    (testing "2V"
      (is (= {10.93 6, 12.36 3}
             (the-struts 2 1e2 true)))
      (is (= {10.93 10, 12.36 6}
             (the-struts 2 1e2 false))))

    (testing "4V"
      (is (= {5.064 6 5.905 3 5.891 6 6.257 6 6.498 3 5.972 6}
             (the-struts 4 1e3 true)))
      (is (= {5.064 10 5.905 6 5.891 12 6.257 12 6.498 6 5.972 10}
             (the-struts 4 1e3 false))))

    (testing "5V"
      (is (= {4.514 6, 4.636 3, 4.945 6, 4.902 6, 5.103 6, 5.232 3, 4.907 3, 4.632 6, 3.963 6}
             (the-struts 5 1e3 true))))

    (println (->> (the-struts 5 1e3 false 15 25) keys sort))
    ))

(deftest consolidate-struts-test
  (let [freq 5
        the-struts (struts freq 20 20 true)
        consolidated-strut-mapping (consolidate-struts-mapping 0.004 (map last the-struts))
        consolidated-struts (consolidate-struts the-struts consolidated-strut-mapping freq)]
    #_(->> consolidated-struts
           (map (juxt :length :count))
           (map-keys #(round % 1e3))
           clojure.pprint/pprint)
    (->> consolidated-struts
         (map :count)
         (apply +)
         println)
    )
  )
