(ns p-p-p-pokerface)

(defn rank [card]
  (let [
    [val _] card
    valmap {\T 10 \J 11 \Q 12 \K 13 \A 14}]
  (cond
    (Character/isDigit val) (Integer/valueOf (str val))
    :else (get valmap val))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn getfreq [clos hand]
  (vals (frequencies (map clos hand))))

(defn pair? [hand]
  (<= 2 (apply max (getfreq rank hand))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (getfreq rank hand))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (getfreq rank hand))))

(defn flush? [hand]
  (= 5 (apply max (getfreq suit hand))))

(defn full-house? [hand]
  (= [2 3] (sort (getfreq rank hand))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (getfreq rank hand))))

(defn straight? [hand]
  (let [
    values (sort (map rank hand))
    smallest (apply min values)
    expected (range smallest (+ smallest 5))
    ace-as-one (sort (replace {14 1} values))]
  (cond
    (=  expected values) true
    (= (range 1 6) ace-as-one) true
    :else false)))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                 [two-pairs? 2] [three-of-a-kind? 3]
                 [straight? 4] [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        possible-hand (fn [checker] ((first checker) hand))]
        (apply max (map second (filter possible-hand checkers)))))
