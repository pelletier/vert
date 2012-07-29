(ns vert.core
  (:use [clojure.string :as string :only [split]]
        [vert.utils :as utils]))

(def block-tag-start "{%")
(def block-tag-end "%}")
(def variable-tag-start "{{")
(def variable-tag-end "}}")
(def comment-tag-start "{#")
(def comment-tag-end "#}")
(def translator-comment-mark "Translators")

(def tag-re (let [patterns [block-tag-start block-tag-end
                            variable-tag-start variable-tag-end
                            comment-tag-start comment-tag-end]
                  escaped-patterns (map #(java.util.regex.Pattern/quote %) patterns)]
              (re-pattern (apply format "(%s.*?%s|%s.*?%s|%s.*?%s)" escaped-patterns))))


(deftype Token [token-type contents lineno])


(defn new-token [token in-tag new-lineno]
  (if in-tag
    (condp utils/startswith? token
      variable-tag-start (Token. :variable (utils/strip-markers token) new-lineno)
      block-tag-start (Token. :block (utils/strip-markers token) new-lineno)
      comment-tag-start  (let [content (if (utils/string-contains? token translator-comment-mark)
                                          (utils/strip-markers token)
                                          "")]
                            (Token. :comment content new-lineno)))
    (Token. :text token new-lineno)))

(defn lexer-create-tokens
  "Given a vector of the source of tokens, returns a vector of Token objects."
  [raw-tokens]
  (defn create-token [remaining-tokens accumulator in-tag lineno]
    (if (empty? remaining-tokens)
      accumulator
      (let [token (first remaining-tokens)
            new-lineno (+ lineno (utils/count-occurences token \newline))
            parsed-token (new-token token in-tag new-lineno)]
        (recur (rest remaining-tokens) (conj accumulator parsed-token) (not in-tag) new-lineno))))
  (create-token raw-tokens [] false 0))


(defn lexer
  "Given the source of a template, returns a vector of tokens."
  [template-string]
  (let [raw-tokens (utils/re-tokenize tag-re template-string)]
    (lexer-create-tokens raw-tokens)))


(defn read-file
  "Read the content of a template file within tempaltes-root."
  [templates-root template-name]
  (slurp (.getPath (clojure.java.io/file templates-root template-name))))


(defn render
  "Render a template file using the given context.
   funcs is a map of available tags and filters.
   templates-root is the path to a directory containing the templates.
   context is a map of available variables.
   template-name is the path of your template file, relative to tempalates-root."
  [funcs templates-root, context, template-name]
  (lexer (read-file templates-root template-name)))
