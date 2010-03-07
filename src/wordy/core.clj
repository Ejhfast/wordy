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

(defn rec-map [lst]
  (if (sequential? lst)
    (map #(if (map? %1)
            (let [mytag (%1 :tag)
                  newc (if (or (= mytag "script") (= mytag "link") (= mytag "a")) "" (%1 :content))]
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

(defn interesting [url num]
  (take 10 (long-words (sortmap (count-words url)) num)))

(defn graph-words [url num]
  (let [data (interesting url num)
        words (map #(first %1) data)
        numbers (map #(second %1) data)]
    (view (bar-chart words numbers))))

(defn -main [url num]
  (let [n (Integer/parseInt num)]
		(graph-words url n)))
