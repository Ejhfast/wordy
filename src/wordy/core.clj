(ns wordy.core
  (:gen-class)
  (:use net.cgrand.enlive-html)
  (:require [clojure.contrib [seq-utils :as seqs]])
  (:use (incanter core stats charts io))
  (:import java.net.URL)
  (:import java.util.regex.Matcher)
  (:import java.lang.String)
  (:import java.lang.Integer))

(defn url [name]
  (html-resource (URL. name)))

(defn match [thing lst]
  (reduce #(or %1 %2) (map #(= thing %1) lst)))

(defn rec-map [lst]
  (if (sequential? lst)
    (map #(if (map? %1)
            (let [mytag (str (%1 :tag))
                  con (%1 :content)
                  domatch (match mytag [":div" ":html" ":p" ":body"])
                  newc (if domatch con "")]
              (trampoline rec-map newc))
            (if (sequential? %1)
              (trampoline rec-map %1)
              (.toLowerCase %1)))
         lst)
    lst))

(defn re-replace [regex strin]
  (.replaceAll
   (re-matcher regex strin) " "))

(defn plus-map [map key]
  (if (nil? (map key))
    (assoc map key 1)
    (assoc map key (+ (map key) 1))))

(defn plus-list-map [mymap keylist]
  (if (empty? keylist)
    mymap
    (recur (plus-map mymap (first keylist)) (rest keylist))))

(defn split [str how]
  (seq (.split str how)))

(defn sortmap [mymap]
  (let [mykeys (keys mymap)
        keyorder (sort-by #(mymap %1) > mykeys)
        keymap (map (fn [key]
                      [key (mymap key)]) keyorder)]
    keymap))

(defn recursive-content [name]
  (let [site (url name)
        junk #"(\n|\t|\.|\?|\(|\)|\,|;|:|http:\/\/.*\.(com|org))+"
        rep " "]
    (re-replace junk (reduce str (seqs/flatten (rec-map site))))))

(defn count-words [url]
  (let [counter {}
        site (recursive-content url)
        one (plus-list-map counter (split site " "))]
    one))

(defn long-words [lst num]
  (filter #(if (> (length (or (seq (first %1)) [])) num) true false) lst))

(defn interesting [data wdsize totake]
  (take totake (long-words data wdsize)))

(defn graph-words [url nums nume step]
  (let [raw (sortmap (count-words url))
        rng (range nums (+ nume 1) step)
        totake (/ 10 (length rng))
        data (map #(interesting raw %1 totake) rng)
        words (map #(map (fn [x] (first x)) %1) data)
        numbers (map #(map (fn [x] (second x)) %1) data)
        bigwords (reduce concat words)
        prin (print bigwords "\n")
        bignums (reduce concat numbers)
        prin (print bignums "\n")
        size (length (first words))
        groups (reduce concat (map #(repeat size %1) rng))
        prin (print groups "\n")]
    (view (bar-chart bigwords bignums
                     :x-label "Words"
                     :y-label "Frequency"
                     :title url
                     :group-by groups))))

(defn -main [url nums nume step]
  (let [s (Integer/parseInt nums)
        e (Integer/parseInt nume)
        stp (Integer/parseInt step)]
		(graph-words url s e stp)))
