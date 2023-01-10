(ns leet12-intToRoman)

(def divisors {1000 "M"
               900 "CM"
               500 "D"
               400 "CD"
               100 "C"
               90 "XC"
               50 "L"
               40 "XL"
               10 "X"
               9 "IX"
               5 "V"
               4 "IV"
               1 "I"})

(defn largestDivisor [n]
  (let [divs (sort > (keys divisors))]
    (some #(if (> (quot n %) 0) %) divs)))


(defn intToR [in]
  (loop [n in
         result ""]
    (if (= n 0)
      result
      (let [div (largestDivisor n)
            rem (- n div)]
        (recur rem (str result (divisors div)))))))
