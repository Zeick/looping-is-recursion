(ns looping-is-recursion)

(defn singleton? [coll]
  (if (empty? coll) 
    false
    (empty? (rest coll))))

(defn power [base exp]
  (let [helper (fn [acc base exp]
    (if (zero? exp)
      acc
      (recur (* acc base) base (dec exp))))]
  (if (zero? base)
    0
  (helper 1 base exp))))


(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
    (if (singleton? a-seq)
      (first a-seq)
      (recur (dec acc) (rest a-seq))
    ))]
    (if (empty? a-seq)
      nil
      (helper (count a-seq) a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1-tail seq2-tail]
      (if (or (empty? seq1-tail) (empty? seq2-tail))
          acc
        (if (not= (first seq1-tail) (first seq2-tail))
          acc
          (recur (inc acc) (rest seq1-tail) (rest seq2-tail))
        )))]
      (if (= (count seq1) (count seq2))
        (= (count seq1) (helper 0 seq1 seq2))
        false)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         newseq a-seq]
    (if (empty? newseq)
      nil
      (if (pred (first newseq))
        acc
        (recur (inc acc) (rest newseq))))))


(defn avg [a-seq]
  (loop [sum 0
         len 0
         the-seq a-seq]
    (if (empty? the-seq)
      (/ sum len)
      (recur (+ sum (first the-seq)) (inc len) (rest the-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
)

(defn parity [a-seq]
  (loop [my-set #{}
         my-seq a-seq]
    (if (empty? my-seq)
      my-set
      (recur (toggle my-set (first my-seq)) (rest my-seq)))))

(defn fast-fibo [n]
  (loop [first 0
         second 1
         index n]
    (cond
      (= index 0) first
      :else (recur second (+ first second) (dec index) ))))

(defn cut-at-repetition [a-seq]
  (loop [my-set #{}
         my-seq a-seq
         cut-seq []]
    (if (or (contains? my-set (first my-seq)) (empty? my-seq))
      cut-seq
      (recur (toggle my-set (first my-seq)) (rest my-seq) (conj cut-seq (first my-seq))))))

