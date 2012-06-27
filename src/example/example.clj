(ns example.example
  (:require [vert.core :as core]))


(def funcs {})

(def context {:foo "bar" :hello "world"})

(defn ex1 []
  (core/render funcs "/Users/thomas/code/vert/src/example/templates/" context "ex1.html"))


(defn -main []
  (println "* Running examples...")
  (seq (ex1)))
