(ns ruby-syntax.core
  (:use [clojure.string :only [split join]]))

(def INFIX (set '(+ - * / % ** & | ^ << >> && || and or
                  == != > < >= <= <=> ===)))

(defn join-seq [delim seqs]
  (apply concat (interpose [delim] seqs)))

(defn syntax-error [message]
  (throw (RuntimeException. message)))

(declare translate-form translate-forms)

(defn translate-method-call [target method args]
  (if (list? method)
    (apply translate-method-call target method)
    (concat (translate-form target)
            ["." (name method)]
            (when (seq args)
              (concat ["("]
                      (join-seq ", "
                                (map translate-form args))
                      [")"])))))

(defn translate-private-call [method args]
  (concat [(name method)]
          (concat ["("]
                  (join-seq ", "
                            (map translate-form args))
                  [")"])))

(defn translate-infix [op args]
  (concat ["("]
          (join-seq (str " " op " ")
                    (map translate-form args))
          [")"]))

(defn translate-array-literal [elements]
  (concat ["["]
          (join-seq ", " (map translate-form elements))
          ["]"]))

(defn translate-hash-literal [pairs]
  (concat ["{"]
          (join-seq ", " (map #(concat (translate-form (key %))
                                       [" => "]
                                       (translate-form (val %)))
                              pairs))
          ["}"]))

(defn translate-identifier [identifier]
  [(join "::" (split (if (namespace identifier)
                       (str (namespace identifier)
                            "."
                            (name identifier))
                       (name identifier))
                     #"\."))])

(defn translate-array-ref [target args]
  (concat (translate-form target)
          ["["]
          (join-seq ", " (map translate-form args))
          ["]"]))

(defn translate-do [forms]
  (concat ["("]
          (translate-forms forms)
          [")"]))

(defn translate-if
  ([predicate then-clause]
    (concat ["(if "]
            (translate-form predicate)
            ["; "]
            (translate-form then-clause)
            [" end)"]))
  ([predicate then-clause else-clause]
    (concat ["(if "]
            (translate-form predicate)
            ["; "]
            (translate-form then-clause)
            [" else "]
            (translate-form else-clause)
            [" end)"])))

(defn translate-arg-spec [args]
  (join-seq ", " (map (comp vector str) args)))

(defn translate-block-call
  ([call expr]
     (translate-form (concat call
                             [(list 'ruby-syntax.core/block-expr expr)])))
  ([call args body & more]
     (concat (translate-form call)
             [" { |"]
             (translate-arg-spec args)
             ["| "]
             (translate-forms (cons body more))
             [" }"])))

(defn translate-block-expr [form]
  (concat ["&"] (translate-form form)))

(defn translate-lambda [args & body]
  (apply translate-block-call 'lambda args body))

(defn translate-form [form]
  (cond
    (map? form)
      (translate-hash-literal form)
    (vector? form)
      (translate-array-literal form)
    (seq? form)
      (let [head (first form)
            args (rest form)]
        (case head
          . (translate-method-call (first args)
                                   (second args)
                                   (drop 2 args))
          new (translate-method-call (first args)
                                     'new
                                     (rest args))
          set! (translate-infix '= args)
          aref (translate-array-ref (first args) (rest args))
          do (translate-do args)
          if (apply translate-if args)
          fn (apply translate-lambda args)
          with-block (apply translate-block-call args)
          ruby-syntax.core/block-expr (translate-block-expr (first args))
          clojure.core/unquote [`(translate-form ~(first args))]
          ;else
            (cond
              (INFIX head)
                (translate-infix head args)
              (and (symbol? head)
                   (or (.startsWith (name head) ".")
                       (.endsWith (name head) ".")))
                (translate-form (macroexpand-1 form))
              :else
                (translate-private-call head args))))
    (or (number? form)
        (keyword? form))
      [(str form)]
    (symbol? form)
      (translate-identifier form)
    :else
      (syntax-error (str "Unexpected form type "
                         (type form)
                         " for "
                         form))))

(defn translate-forms [forms]
  (join-seq "; " (map translate-form forms)))

(defn coalesce-tokens [tokens]
  (when-not (empty? tokens)
    (for [token-group (partition-by string? tokens)]
      (if (= (count token-group) 1)
        (first token-group)
        (if (string? (first token-group))
          (apply str token-group)
          `(concat ~@token-group))))))
