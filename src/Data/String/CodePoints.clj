(ns Data.String.CodePoints._foreign)

(defn _unsafeCodePointAt0 [^String s]
  (.codePointAt s 0))

(defn _codePointAt [just]
  (fn [nothing]
    (fn [index]
      (fn [^String s]
        (let [cps (.. s (codePoints) (toArray))]
          (if (or (< index 0)
                  (>= index (count cps)))
            nothing
            (just (aget cps index))))))))

(defn _countPrefix [pred]
  (fn [^String s]
    (reduce (fn [i cp]
              (if (pred cp)
                (inc i)
                (reduced i)))
            0
            (.. s (codePoints) (toArray)))))

(defn _singleton [cp]
  (let [#^int cps (int-array 1 cp)]
    (String. cps 0 1)))

(defn _fromCodePointArray [cps]
  (let [#^int cps-arr (int-array cps)]
    (String. cps-arr 0 (count cps))))

(defn _toCodePointArray [^String s]
  (vec (.. s (codePoints) (toArray))))

(defn _take [n]
  (fn [^String s]
    (_fromCodePointArray (take n (.. s (codePoints) (toArray))))))
