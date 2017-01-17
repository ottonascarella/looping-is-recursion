(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (cond
                   (= 0 exp) 1
                   (= 1 exp) acc
                   :else (recur (* base acc) base (dec exp))))]
    (if-not (neg? exp)
      (helper base base exp)
      (/ 1.0 (helper base base (Math/abs exp))))))

(defn last-element [a-seq]
    (cond
      (= 0 (count a-seq)) nil
      (= 1 (count a-seq)) (first a-seq)
      :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                (cond
                  (and (empty? seq1) (empty seq2)) true
                  (not= (first seq1) (first seq2)) false
                  :else (recur (rest seq1) (rest seq2))))]
    (if (not= (count seq1) (count seq2)) false
      (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (if-not (empty? a-seq)
    (loop [acc 0, a-seq a-seq]
      (let [f (first a-seq)]
        (cond
          (nil? f) f
          (pred f) acc
          :else (recur (inc acc) (rest a-seq)))))
    nil))

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [sum 0 index 0 a-seq a-seq]
      (if (empty? a-seq)
        (/ sum index)
        (recur (+ sum (first a-seq)) (inc index) (rest a-seq))))))

(defn toggle [a-set what]
  (if (contains? a-set what)
    (disj a-set what)
    (conj a-set what)))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (loop [acc #{} a-seq a-seq]
      (if (empty? a-seq)
        acc
        (recur (toggle acc (first a-seq)) (rest a-seq))))))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (loop [i 2
                 fib-minus-one 1
                 fib-minus-two 0]
            (let [fib-sum (+ fib-minus-one fib-minus-two)]
              (if (= n i)
                fib-sum
                (recur (inc i) fib-sum fib-minus-one))))))

(defn cut-at-repetition [a-seq]
  (loop [final-seq []
         set-helper #{}
         a-seq a-seq]
    (let [f (first a-seq)]
      (cond
        (empty? a-seq) final-seq
        (contains? set-helper f) final-seq
        :else (recur (conj final-seq f)
                     (conj set-helper f)
                     (rest a-seq))))))
