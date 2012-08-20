(ns vert.core
  (:require [clojure.zip :as zip])
  (:use [clojure.string :as string :only [split]]
        [vert.utils :as utils]))


(defn unbalanced-block [] (Exception. "Unable to find a closing block tag."))


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
(deftype Node [node-type token children])


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
  (loop [remaining-tokens raw-tokens
         accumulator []
         in-tag false
         lineno 0]
    (if (empty? remaining-tokens)
      accumulator
      (let [token (first remaining-tokens)
            new-lineno (+ lineno (utils/count-occurences token \newline))
            parsed-token (new-token token in-tag new-lineno)]
        (recur (rest remaining-tokens) (conj accumulator parsed-token) (not in-tag) new-lineno)))))


(defn lexer
  "Given the source of a template, returns a vector of tokens."
  [template-string]
  (let [raw-tokens (utils/re-tokenize tag-re template-string)]
    (lexer-create-tokens raw-tokens)))


(defn read-file
  "Read the content of a template file within tempaltes-root."
  [templates-root template-name]
  (slurp (.getPath (clojure.java.io/file templates-root template-name))))


(defn extract-block-name [token]
  (first (string/split (.contents token) #" ")))

(defn tree-branch? [node]
  (not (nil? (.children node))))


(defn tree-children [node]
  (.children node))


(defn tree-make-node [node children]
  (Node. (.node-type node) (.token node) children))


(defn create-tree []
  (zip/zipper tree-branch? tree-children tree-make-node (Node. :root nil [])))


; assume the first token in the tokens list is the first token inside the block.
(defn get-block-tokens [block-name tokens]
  (let [end-block-name (str "end" block-name)]
    (loop [rem-tokens tokens
            counter 1
            index 0]
      (let [tokens-rest (rest rem-tokens)]
        (cond
          (= counter 0) (subs tokens index)
          (empty? rem-tokens) (throw (unbalanced-block))
          :else (let [tok-name (extract-block-name (first rem-tokens))
                next-count (cond
                             (contains? {end-block-name "end"} tok-name) (dec counter)
                             (= block-name tok-name) (inc counter)
                             :else counter)]
            (recur tokens-rest next-count (inc index))))))))


;; Yeah those are not the most useful functions ever wrote, but I think it makes
;; the code a little easier to read.
(defn skip-tokens [tokens n]
  (subs tokens n))


;; TODO implement me!
(defn extract-block-tokens [tokens]
  ())

;; TODO implement me!
(defn create-block-tree [tokens]
  ())


(defn parser [templates-root funcs tokens-vector]
  (loop [remaining-tokens tokens-vector
         tree (create-tree)]
    (if (empty? remaining-tokens)
      tree
      (let [token (first remaining-tokens)
            tokens-rest (rest remaining-tokens)]
        (condp = (.token-type token)
          :text (recur tokens-rest (zip/append-child tree (Node. :text token nil)))
          :variable  (recur tokens-rest (zip/append-child tree (Node. :variable token nil)))
          :comment (recur tokens-rest (zip/append-child tree (Node. :comment token nil)))
          :block (let [block-tokens (extract-block-tokens remaining-tokens)
                       skipped-tokens (count block-tokens)]
                   (recur (skip-tokens remaining-tokens skipped-tokens)
                          (zip/append-child tree (create-block-tree block-tokens)))))))))


(defn render
  "Render a template file using the given context.
   funcs is a map of available tags and filters.
   templates-root is the path to a directory containing the templates.
   context is a map of available variables.
   template-name is the path of your template file, relative to tempalates-root."
  [funcs templates-root, context, template-name]
  (parser templates-root funcs (lexer (read-file templates-root template-name))))
