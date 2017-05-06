(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-set]
                 (if (empty? (rest a-set))
                   (first a-set)
                   (recur (rest a-set))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [sq1 sq2]
        (if (some empty? [sq1 sq2])
          (every? empty? [sq1 sq2])
          (if (== (first sq1) (first sq2))
            (recur (rest sq1) (rest sq2))
            false)))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [n 0
         a-seq a-seq]
    (if (empty? a-seq)
      nil
      (if (pred (first a-seq))
        n
        (recur (inc n) (rest a-seq))))))

(defn avg [a-seq]
  (loop [len 0
         sum 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum len)
      (recur (inc len) (+ sum (first a-seq)) (rest a-seq)))))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-seq a-seq
         odd-set #{}]
    (if (empty? a-seq)
      odd-set
      (recur (rest a-seq) (toggle odd-set (first a-seq))))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [f1 0
           f2 1
           iter 1]
      (if (== iter n)
        f2
        (recur f2 (+ f1 f2) (inc iter))))))

(defn cut-at-repetition [a-seq]
  (loop [rep-elems #{}
         head []
         tail a-seq]
    (cond
      (empty? tail) head
      (contains? rep-elems (first tail)) head
      :else (recur (conj rep-elems (first tail)) (conj head (first tail)) (rest tail)))))

