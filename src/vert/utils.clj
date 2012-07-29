(ns vert.utils
  (:use [clojure.string :as string :only [blank? trim]]))


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


;; Count the number of occurrences of a char in a string.
(defn count-occurences [string character]
  (count (filter #(= % character) string)))


;; Starts with for condp.
;; true if string starts with value.
(defn startswith? [value string]
  (.startsWith string value))

;; Strip markers: {% %} {{}}
(defn strip-markers [string]
  (string/trim (subs string 2 (- (count string) 2))))

;; Check if a string contains another one.
(defn string-contains? [string value]
  (not= (.indexOf string value) -1))
