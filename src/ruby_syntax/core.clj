(ns ruby-syntax.core
  (:use [clojure.string :only [split join]]))

(def INFIX (set '(+ - * / % ** & | ^ << >> && || and or
                  == != > < >= <= <=> ===)))

(def PREFIX (set '(+ - ! not)))

(defn join-seq [delim seqs]
  (apply concat (interpose [delim] seqs)))

(defn syntax-error [message]
  (throw (RuntimeException. message)))

(declare translate-form translate-forms)

(defn translate-method-call [target method args]
  (if (list? method)
    (translate-method-call target (first method) (rest method))
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

(defn translate-prefix [op arg]
  (concat [(str "(" op " ")]
          (translate-form arg)
          [")"]))

(defn translate-infix [op args]
  (cond
    (= (count args) 1)
      (translate-prefix op (first args))
    :else
      (concat ["("]
              (join-seq (str " " op " ")
                        (map translate-form args))
              [")"])))

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

(defn translate-lambda [defs]
  (when-not (= (count defs) 1)
    (throw (RuntimeException. "Multiple arities not supported")))
  (let [[args & body] (first defs)]
    (apply translate-block-call 'lambda args body)))

(defn translate-def [method args & body]
  (concat ["def "]
          (translate-form method)
          ["("]
          (translate-arg-spec args)
          ["); "]
          (translate-forms body)
          [" end"]))

(defn translate-module [module-name & body]
  (concat ["module "]
          (translate-form module-name)
          ["; "]
          (translate-forms body)
          [" end"]))

(defn translate-class [class-name & body]
  (concat ["class "]
          (if (vector? class-name)
            (concat (translate-form (first class-name))
                    [" < "]
                    (translate-form (second class-name)))
            (translate-form class-name))
          ["; "]
          (translate-forms body)
          [" end"]))

(defn translate-singleton-class [expr & body]
  (concat ["class << "]
          (translate-form expr)
          ["; "]
          (translate-forms body)
          [" end"]))

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
          def (apply translate-def args)
          module (apply translate-module args)
          class (apply translate-class args)
          singleton-class (apply translate-singleton-class args)
          fn* (translate-lambda args)
          with-block (apply translate-block-call args)
          ruby-syntax.core/block-expr (translate-block-expr (first args))
          clojure.core/unquote [`(translate-form ~(first args))]
          ;else
            (cond
              (INFIX head)
                (translate-infix head args)
              (PREFIX head)
                (translate-prefix head (first args))
              :else
                (let [expanded (macroexpand-1 form)]
                  (if (identical? expanded form)
                    (translate-private-call head args)
                    (translate-form expanded))))))
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
