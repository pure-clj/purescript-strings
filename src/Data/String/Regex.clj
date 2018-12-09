(ns Data.String.Regex._foreign
  (:refer-clojure :exclude [replace test])
  (:require [clojure.string :as s])
  (:import [java.util.regex Matcher Pattern PatternSyntaxException]))

(defrecord Regexp [source flags pattern])

;; exports

(defn showRegex' [r]
  (format "/%s/%s" (:source r) (:flags r)))

(defn regex' [left]
  (fn [right]
    (fn [s1]
      (fn [s2]
        (try
          (letfn [(valid-flags [flags]
                    (->> flags
                         (filter #(some #{%} [\i \u \m \x \s \d]))
                         (apply str)))
                  (to-regex [r flags]
                    (if (= "" flags)
                      (re-pattern r)
                      (re-pattern (format "(?%s)%s" (valid-flags flags) r))))]
            (right (->Regexp s1 s2 (to-regex s1 s2))))
          (catch PatternSyntaxException e
            (left (.getMessage e))))))))

(defn source [r]
  (:source r))

(defn flags' [r]
  (let [f (:flags r)]
    {"multiline" (s/includes? f "m")
     "ignoreCase" (s/includes? f "i")
     "global" (s/includes? f "g")
     "sticky" false
     "unicode" (s/includes? f "u")}))

(defn test [r]
  (fn [s]
    (boolean (re-find (:pattern r) s))))

(defn _match [just]
  (fn [nothing]
    (fn [{:keys [flags pattern]}]
      (fn [s]
        (let [mapper (fn [match]
                       (if match
                         (just match)
                         nothing))
              res (if (s/includes? flags "g")
                    (->> (re-seq pattern s)
                         (mapv first))
                    (re-find pattern s))]
          (cond
            (or (nil? res) (= [] res)) nothing
            (instance? String res) (just [(just res)])
            :else (just (mapv mapper res))))))))

(defn replace [{:keys [flags pattern]}]
  (fn [replacement]
    (fn [s]
      (if (s/includes? flags "g")
        (s/replace s pattern replacement)
        (s/replace-first s pattern replacement)))))

(defn replace' [{:keys [flags pattern]}]
  (fn [f]
    (fn [s]
      (letfn [(matcher [matches]
                (let [[match & pars] (if (vector? matches) matches [matches])]
                  ((f match) (or pars []))))]
        (if (s/includes? flags "g")
          (s/replace s pattern matcher)
          (s/replace-first s pattern matcher))))))

(defn _search [just]
  (fn [nothing]
    (fn [{:keys [pattern]}]
      (fn [s]
        (let [^Pattern p pattern
              ^Matcher matcher (.matcher p s)]
          (if (.find matcher)
            (just (.start matcher))
            nothing))))))

(defn split [{:keys [pattern source]}]
  (fn [s]
    (if (and (= source "")
             (= s ""))
      []
      (s/split s pattern))))
