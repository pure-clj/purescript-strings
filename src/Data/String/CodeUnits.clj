(ns Data.String.CodeUnits._foreign
  (:refer-clojure :exclude [take drop])
  (:require [clojure.string :as s]))

(defn fromCharArray [a]
  (apply str a))

(def toCharArray vec)

(def singleton str)

(defn _charAt [just]
  (fn [nothing]
    (fn [^long i]
      (fn [^String s]
        (if (< -1 i (count s))
          (just (.charAt s i))
          nothing)))))

(defn _toChar [just]
  (fn [nothing]
    (fn [s]
      (if (= 1 (count s))
        (just (first s))
        nothing))))

(def length count)

(defn countPrefix [p]
  (fn [s]
    (reduce (fn [i ch]
              (if (p ch)
                (inc i)
                (reduced i)))
            0 s)))

(defn _indexOf [just]
  (fn [nothing]
    (fn [x]
      (fn [s]
        (if-let [res (s/index-of s x)]
          (just res)
          nothing)))))

(defn _indexOf' [just]
  (fn [nothing]
    (fn [x]
      (fn [start-at]
        (fn [s]
          (if (or (neg? start-at)
                  (> start-at (count s)))
            nothing
            (if-let [res (s/index-of s x start-at)]
              (just res)
              nothing)))))))

(defn _lastIndexOf [just]
  (fn [nothing]
    (fn [x]
      (fn [s]
        (if-let [res (s/last-index-of s x)]
          (just res)
          nothing)))))

(defn _lastIndexOf' [just]
  (fn [nothing]
    (fn [x]
      (fn [start-at]
        (fn [s]
          (if (or (neg? start-at)
                  (> start-at (count s)))
            nothing
            (if-let [res (s/last-index-of s x start-at)]
              (just res)
              nothing)))))))

(defn take [n]
  (fn [s]
    (apply str (clojure.core/take n s))))

(defn drop [n]
  (fn [s]
    (apply str (clojure.core/drop n s))))

(defn _slice [b]
  (fn [e]
    (fn [s]
      (let [len (count s)
            start (cond (neg? b) (+ len b)
                        (> b len) (dec len)
                        :else b)
            end (cond (neg? e) (+ len e)
                      (> e len) len
                      :else e)]
        (subs s start end)))))

(defn splitAt [i]
  (fn [s]
    (let [i (-> i
                (max 0)
                (min (count s)))]
      {"before" (subs s 0 i)
       "after" (subs s i)})))
