(ns vert.core)


; Exceptions

(defn exception-eof []
  (throw (Exception. "Unexpected EOF.")))

(defn exception-expect [e g]
  (throw (Exception. (str "Expected " e " but got " g "."))))

(defn exception-regex [r v]
  (throw (Exception. (str v " should match the regex " r))))

; Lexical analysis
; Create a token stream from the given file path.

(defn fread [n string]
  (if (>= n (count string))
      (exception-eof)
      (nth string n)))

(defn sread [n string]
  (if (>= n (count string))
    ""
    (nth string n)))

(defn strip-at-end [string number]
  (subs string 0 (- (count string) number)))

(defn endswith? [string suffix]
  (let [length-string (count string)
        length-suffix (count suffix)]
    (if (> length-suffix length-string)
      false
      (= (subs string (- length-string length-suffix) length-string) suffix))))

(def lex-syms
  {
   "{{" :begin_var
   "}}" :end_var
   "<%" :begin_block
   "%>" :end_block
   "<#" :begin_comment
   "#>" :end_comment
  })


(defn lex-detect-token [content]
  (let [match (first (filter (partial endswith? content) (keys lex-syms)))]
    (if (not (nil? match))
      (get lex-syms match))))


(defn lex [string]
  (let [length (count string)]
    (defn lex-inner [n current queue]
      (if (>= n length)
        (if (empty? current)
          queue
          (conj queue (list :string current)))
        (let [token-handler (lex-detect-token current)]
          (if (nil? token-handler)
            (recur (inc n) (str current (sread n string)) queue)
            (let [node (list token-handler)
                  cleaned-current (strip-at-end current 2)
                  updated-queue (conj queue (list :string cleaned-current) node)]
              (recur (inc n) (str (sread n string)) updated-queue))))))
    (lex-inner 1 (str (sread 0 string)) (clojure.lang.PersistentQueue/EMPTY)))) ; Asssume the file is not empty.


(defn lex-file [path]
  (lex (slurp path)))


; Parser

;; Node: (next, label, meta)

(defstruct ast-node :next, :label, :meta)

(defn parse-ast-create []
  (struct-map ast-node :next nil :label :root :meta {}))

(defn parse-ast-append [tree label args]
  (cons (list label args) ast))

(defn queue-skip [queue n]
  (if (= n 0)
    queue
    (recur (pop queue) (dec n))))

(defn parse-expect
  ([queue type] (let [node (peek queue)]
                  (if (= (first node) type)
                    node
                    (exception-expect type (first node)))))
  ([queue type regex] (let [node (parse-expect queue type)
                            value (second node)]
                        (if (re-matches regex value)
                          node
                          (exception-regex regex value)))))

; Variable string
;  *([\w\-_]+)(\|[\w\-_]+(?:\(.+?\))?)* *

;; Should return an AST node. and the number of nodes to skip.
(defn parse-handle-var [queue]
  (let [string (parse-expect queue :string)]
    (second string)))

(def parse-token-handlers
  {
   :begin_var parse-handle-var
   :begin_block parse-handle-block
   :begin_comment parse-handle-comment
  })

(defn parse [token-queue]
  (defn parse-inner [queue ast]
    (if (empty? queue)
      ast
      (let [current-token (peek queue)])))
  (parse-inner token-queue (parse-ast-create)))

; Wrapper


(defn render
  "* functions is an hash of available functions (for filters and blocks).
   * root is a string representation of the root path of the templates structures.
   * context is an hash for identifiers binding (variables).
   * name is the name of the template, i.e. the file path in root."
  [functions root context name]
  (parse (lex-file (.getPath (clojure.java.io/file root name)))))
