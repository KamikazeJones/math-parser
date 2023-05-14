(ns math-parser.core
    (:gen-class))
; ------------------------------------------------
; LEXER
; ------------------------------------------------
(defn get-char [expr]
    (if (= 0 (count expr))
        :EOS
        (subs expr 0 1)))


(defn get-token-ws [expr]
    (let [ch (get-char expr)]
        (case ch
            (" " "\t" "\r" "\n")
            (get-token-ws (subs expr 1))

            ;default
            {:type :ws, :value " ", :rest expr})))

(defn get-token-number [expr token-str]
    (let [ch (get-char expr)]
        (case ch
            ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ".")
            (get-token-number (subs expr 1) (str token-str ch))

            ;default
            {:type :number, :value (Double/parseDouble token-str), :rest expr})))

(defn get-token [expr]
    (if (= 0 (count expr))

        {:type :EOS, :rest ""}

        ;else
        (let [ch (subs expr 0 1)]
            (case ch
                (" " "\t" "\n" "\r")
                (get-token ((get-token-ws (subs expr 1)) :rest))
                "-" {:type :minus, :rest (subs expr 1)}
                "+" {:type :plus, :rest (subs expr 1)}
                "*" {:type :star, :rest (subs expr 1)}
                "/" {:type :slash, :rest (subs expr 1)}
                ("(") {:type :left-p :rest (subs expr 1)}
                (")") {:type :right-p :rest (subs expr 1)}
                ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ".")
                (get-token-number (subs expr 1) ch)
                ;default
                {:type :error, :value ch, :rest (subs expr 1)}))))

(defn get-next-token [token]
    (get-token (token :rest)))

; ------------------------------------------------
; PARSER
; ------------------------------------------------

(defn parse-accu [parser parse-func-list parse-trees]
    (if (= [] parse-func-list)
        {:trees parse-trees :token (parser :token)}
        (let [parse-func (first parse-func-list)
              result (parse-func parser)]
            (if result
                (parse-accu
                    result
                    (rest parse-func-list)
                    (conj parse-trees (result :tree)))
                ;else
                false))))

(defn parse [parser parse-func-list]
    (parse-accu parser parse-func-list []))

; grammar

; Expression ::= E $ --$ marks the end of the Expression
; E    ::= T [ OP-T ]
; OP-T ::= ( '+' | '–' ) T [ OP-T ]
; T    ::= F [ OP-F ]
; OP-F ::= ( '*' | '/' ) F [ OP-F ]
; F    ::= Number | '–' F | '(' E ')'

(declare F)
(declare E)

(defn in? [xs el] (= true (some #(= % el) xs)))

(defn NUMBER [parser]
    (let [token (parser :token)]
        (if (= :number (token :type))
            {:tree  {:type   :float,
                     :symbol :number
                     :value  (token :value)},
             :token (get-next-token token)}
            ;else
            false)))

(defn terminal [parser terminal-key]
    (let [token (parser :token)]
        (if (= terminal-key (token :type))
            {:terminal-key terminal-key
             :token        (get-next-token token)}
            ;else
            false)))

(defn minus-F [parser]
    (let [r (parse parser [#(terminal % :minus) F])]
        (if r
            (let [t ((r :trees) 0) f ((r :trees) 1)]
                {:tree {:type :neg, :symbol :terminal :child f} :token (r :token)})
        ;else
            false)))

(defn lp-E-rp [parser]
    (let [r (parse parser [#(terminal % :left-p) E #(terminal % :right-p)])]
        (if r
            {:tree ((r :trees) 1), :token (r :token)}
        ;else
            false)))

(defn F [parser]
    (let [r (or (NUMBER parser)
                (minus-F parser)
                (lp-E-rp parser)
                )]
        (if r
            {:tree  {:type   :subexpr,
                     :symbol "F",
                     :child  (r :tree)}
             :token (r :token)}
        ;else
            false)
        ))

(defn OP-F [parser]
    (let [tok-type ((parser :token) :type)]
        (if (in? [:star :slash] tok-type)
            (let [result (parse parser [#(terminal % tok-type) F])]
                (if result
                    (OP-F
                        {:tree  {:symbol "OP-F",
                                 :type   (if (= tok-type :star) :mul :div),
                                 :left   (parser :tree),
                                 :right  ((result :trees) 1)}
                         :token (result :token)})
                ;else
                    false))
        ;else
            parser)))


(defn T [parser]
    (let [r (F parser)]
        (if r
            (let [result (OP-F r)]
                {:tree  {:type :subexpr, :symbol "T", :child (result :tree)}
                 :token (result :token)})
        ;else
            false)))

(defn OP-T [parser]
    (let [tok-type ((parser :token) :type)]
        (if (in? [:plus :minus] tok-type)
            (let [result (parse parser [#(terminal % tok-type) T])]
                (if result
                    (OP-T
                        {:tree  {:symbol "OP-T",
                                 :type   (if (= tok-type :plus) :add :sub),
                                 :left   (parser :tree),
                                 :right  ((result :trees) 1)}
                         :token (result :token)})
                ;else
                    false))
        ;else
            parser)))


(defn E [parser]
    (let [r (T parser)]
        (if r
            (let [result (OP-T r)]
                {:tree  {:type :subexpr, :symbol "E", :child (result :tree)}
                 :token (result :token)})
        ;else
            false)))


(defn new-parser [expr]
    (let [token (get-token expr)]
        {:tree {:type :empty}, :token token}))


(defn parse-expr [expr]
    (let [parser (new-parser expr)]
        (if (= :EOS ((parser :token) :type))
            {:tree {:type :float, :value 0.0, :symbol "E"} :token (parser :token)}
        ;else
            (parse parser [E]))))

; ---------------------------------------------------
; EVALUATE
; ---------------------------------------------------
(defn evaluate [tree]
    (case (tree :type)
        :float (tree :value)
        :subexpr (evaluate (tree :child))
        :mul (* (evaluate (tree :left)) (evaluate (tree :right)))
        :div (/ (evaluate (tree :left)) (evaluate (tree :right)))
        :add (+ (evaluate (tree :left)) (evaluate (tree :right)))
        :sub (- (evaluate (tree :left)) (evaluate (tree :right)))
        :neg (- (evaluate (tree :child)))))

(defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!"))

(defn calc [expression]
    ; (evaluate {:type :number, :value 7})
    (evaluate ((E (new-parser expression)) :tree)))

(defn traverse [tree name]
    (let [label (str (if (contains? tree :symbol) (tree :symbol) "")
               (if (contains? tree :type) (str (tree :type)) "")
               (if (contains? tree :value) (str ":" (tree :value)) ""))
          node-label (str name " [label=" "\"" label "\"" " shape=box]\n")]

        (if (contains? tree :child)
            (let
                [g (traverse (tree :child) (str name "C"))]
                (str node-label name " -> " name "C\n" g))
        ;else
            (if (contains? tree :left)
                (let [gl (traverse (tree :left) (str name "L"))
                      left-graph (str name " -> " name "L" "\n")]
                    (if (contains? tree :right)
                        (let [gr (traverse (tree :right) (str name "R"))
                              right-graph (str name " -> " name "R" "\n")]
                            (str node-label left-graph gl right-graph gr))
                        ;else
                        (str node-label left-graph gl)))
            ;else
                (str node-label)))))

(defn make-graph [tree]
    (traverse tree "r")                                     ; returns a graph
)

(defn generate-graph [expression]

    (str "digraph parse_tree {\n"
         "start [label=\"" expression "\" shape=box color=lightyellow style=filled]" "\n"
         (make-graph ((E (new-parser expression)) :tree))
         "}"))