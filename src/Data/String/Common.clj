(ns Data.String.Common._foreign
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :as s])
  (:import java.text.Collator
           java.util.regex.Pattern))

(defn _localeCompare [lt]
  (fn [eq]
    (fn [gt]
      (fn [^String s1]
        (fn [^String s2]
          (let [^Collator collator (Collator/getInstance)
                res (.compare collator s1 s2)]
            (case res
              -1 lt
              0 eq
              gt)))))))

(defn replace [s1]
  (fn [s2]
    (fn [s3]
      (s/replace-first s3 s1 s2))))

(defn replaceAll [s1]
  (fn [s2]
    (fn [s3]
      (s/replace s3 s1 s2))))

(defn split [^String s1]
  (fn [^String s2]
    (let [res (vec (.split s2 (Pattern/quote s1)))]
      (if (= [""] res)
        []
        res))))

(def toLower s/lower-case)

(def toUpper s/upper-case)

(def trim s/trim)

(defn joinWith [s]
  (fn [xs]
    (s/join s xs)))
