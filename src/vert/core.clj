(ns vert.core)


; Exceptions

(defn exception-eof []
  (throw (Exception. "Unexpected EOF.")))


; Lexical analysis
; Create a tree from the given file path.

(defn fread [n string]
  (if (>= n (count string))
      (exception-eof)
      (nth string n)))

(defn sread [n string]
  (if (>= n (count string))
    ""
    (nth string n)))

(defn strip-at-end [string suffix]
  (subs string 0 (- (count string) (count suffix))))

(defn endswith? [string suffix]
  (let [length-string (count string)
        length-suffix (count suffix)]
    (if (> length-suffix length-string)
      false
      (= (subs string (- length-string length-suffix) length-string) suffix))))

(defn lex-var-output [init-n string]
  (defn lex-var-output-inner [n value filters]
    (if (endswith? value "}}")
      (let [correct-value (strip-at-end value "}}")]
        (list n (list :var correct-value filters)))
      (recur (inc n) (let [v (str value (fread n string))]
                       (if (endswith? v  " ") value v)))))
  (lex-var-output-inner init-n "" []))

(def lex-syms {
  "{{" lex-var-output
})


(defn lex-token-start [content]
  (filter (partial endswith? content) (keys lex-syms)))


(defn lex [string]
  (let [length (count string)]
    (defn lex-inner [n current tree]
      (if (= n length)
        (if (empty? current)
          tree
          (conj tree (list :string current)))
        (let [token-handler (lex-token-start current)]
          (if (empty? token-handler)
            (recur (inc n) (str current (sread n string)) tree)
            (let [handler (first token-handler)
                  result ((get lex-syms handler) n string)
                  new-n (first result)
                  node (second result)
                  cleaned-current (strip-at-end current handler)
                  updated-tree (conj tree (list :string cleaned-current) node)]
              (recur (inc new-n) (str (sread new-n string)) updated-tree))))))
    (lex-inner 1 (str (sread 0 string)) '()))) ; Asssume the file is not empty.


(defn lex-file [path]
  (lex (slurp path)))

; Wrapper


(defn render
  "* functions is an hash of available functions (for filters and blocks).
   * root is a string representation of the root path of the templates structures.
   * context is an hash for identifiers binding (variables).
   * name is the name of the template, i.e. the file path in root."
  [functions root context name]
  (lex-file (.getPath (clojure.java.io/file root name))))
