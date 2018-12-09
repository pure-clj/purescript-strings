(ns Data.String.Unsafe._foreign
  (:refer-clojure :exclude [char]))

(defn charAt [i]
  (fn [s]
    (if (< -1 i (count s))
      (nth s i)
      (throw (ex-info "Data.String.Unsafe.charAt: Invalid index." {})))))

(defn char [s]
  (if (= (count s) 1)
    (first s)
    (throw (ex-info "Data.String.Unsafe.char: Expected string of length 1." {}))))
