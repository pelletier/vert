(ns vert.utils
  (:use [clojure.string :as string :only [blank?]]))


;; A Clojure version of Python's re.split when using capuring parenthesis:
;; split a string using a regular expression and keep the delimiters in the
;; resulting list.
(defn re-tokenize [re text]
  (let [matcher (re-matcher re text)]
    (defn inner [last-index result]
      (if (.find matcher)
        (let [start-index (.start matcher)
              end-index (.end matcher)
              match (.group matcher)
              insert (subs text last-index start-index)]
          (if (string/blank? insert)
            (recur end-index (conj result match))
            (recur end-index (conj result insert match))))
        (conj result (subs text last-index))))
    (inner 0 [])))
