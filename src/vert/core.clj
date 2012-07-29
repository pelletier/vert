(ns vert.core
  (:use [clojure.string :as string :only [split]]
        [vert.utils :as utils]))

(def block-tag-start "{%")
(def block-tag-end "%}")
(def variable-tag-start "{{")
(def variable-tag-end "}}")
(def comment-tag-start "{#")
(def comment-tag-end "#}")

(def tag-re (let [patterns [block-tag-start block-tag-end
                            variable-tag-start variable-tag-end
                            comment-tag-start comment-tag-end]
                  escaped-patterns (map #(java.util.regex.Pattern/quote %) patterns)]
              (re-pattern (apply format "(%s.*?%s|%s.*?%s|%s.*?%s)" escaped-patterns))))

(defn lexer [template-string]
  (let [res (utils/re-tokenize tag-re template-string)]
    res))

(defn read-file [templates-root template-name]
  (slurp (.getPath (clojure.java.io/file templates-root template-name))))

(defn render [funcs templates-root, context, template-name]
  (lexer (read-file templates-root template-name)))
