(ns ruby-syntax.core
  (:use [clojure.string :only [split join]])
  (:require [ruby-syntax.ast :as ast]))

(def INFIX-ALIASES {'bitwise-and '&
                    'bitwise-or '|
                    'bitwise-xor (symbol "^")})
(def INFIX (set (concat '(+ - * / % ** & | ^ << >> && || and or
                          == != > < >= <= <=> ===)
                        (keys INFIX-ALIASES))))

(def PREFIX-ALIASES {'bitwise-not (symbol "~")})
(def PREFIX (set (concat '(+ - ! not)
                         (keys PREFIX-ALIASES))))

(defn join-seq [delim seqs]
  (apply concat (interpose [delim] seqs)))

(defn syntax-error [message]
  (throw (RuntimeException. message)))

(declare translate-form translate-statements)

(defn translate-literal [form]
  (ast/->Literal form))

(defn translate-string [form]
  (ast/->StringLiteral form))

(defn translate-arg-spec [args]
  (map translate-form args))

(defn translate-method-call [target method args]
  (if (list? method)
    (translate-method-call target
                           (first method)
                           (rest method))
    (ast/->MethodCall (translate-form target)
                      (translate-form method)
                      (when (seq args)
                        (ast/->ArgGroup (translate-arg-spec args))))))

(defn translate-private-call [method args]
  (ast/->FunCall (translate-form method)
                 (ast/->ArgGroup (translate-arg-spec args))))

(defn translate-prefix-op [op arg]
  (ast/->PrefixOp (ast/->Identifier (get PREFIX-ALIASES op op))
                  (translate-form arg)))

(defn translate-infix-op [op args]
  (ast/->InfixOp (ast/->Identifier (get INFIX-ALIASES op op))
                 (map translate-form args)))

(defn translate-array-literal [elements]
  (ast/->ArrayLiteral (map translate-form elements)))

(defn translate-hash-literal [pairs]
  (ast/->HashLiteral (for [[k v] pairs]
                       (ast/->HashPair (translate-form k)
                                       (translate-form v)))))

(defn translate-identifier [identifier]
  (let [qualified-name (if (namespace identifier)
                         (str (namespace identifier)
                              "."
                              (name identifier))
                         (name identifier))]
    (reduce ast/->ModuleLookup
            (map ast/->Identifier
                 (split qualified-name #"\.")))))

(defn translate-array-ref [target args]
  (ast/->ArrayRef (translate-form target)
                  (ast/->ArrayLiteral (map translate-form args))))

(defn translate-if
  ([predicate then-clause]
    (ast/->If (translate-form predicate)
              (translate-form then-clause)
              nil))
  ([predicate then-clause else-clause]
    (ast/->If (translate-form predicate)
              (translate-form then-clause)
              (translate-form else-clause))))

(defn translate-with-block
  ([call expr]
    (translate-form (concat call
                            [(list 'ruby-syntax.core/block-expr expr)])))
  ([call args body & more]
    (ast/->Block (translate-form call)
                 (ast/->BlockArgList (translate-arg-spec args))
                 (translate-statements (cons body more)))))

(defn translate-block-expr [form]
  (ast/->BlockArg (translate-form form)))

(defn translate-lambda [defs]
  (when-not (= (count defs) 1)
    (throw (RuntimeException. "Multiple arities not supported")))
  (let [[arg-spec & body] (first defs)]
    (ast/->Block (ast/->FunCall (ast/->Identifier 'lambda) nil)
                 (ast/->BlockArgList (translate-arg-spec arg-spec))
                 (translate-statements body))))

(defn translate-def [target arg-spec & body]
  (ast/->Def (translate-form target)
             (ast/->ArgGroup (translate-arg-spec arg-spec))
             (translate-statements body)))

(defn translate-module [module-expr & body]
  (ast/->ModuleDef (translate-form module-expr)
                   (translate-statements body)))

(defn translate-class [class-expr & body]
  (if (vector? class-expr)
    (ast/->ClassDef (translate-form (first class-expr))
                    (translate-form (second class-expr))
                    (translate-statements body))
    (ast/->ClassDef (translate-form class-expr)
                    nil
                    (translate-statements body))))

(defn translate-singleton-class [expr & body]
  (ast/->SingletonClassDef (translate-form expr)
                           (translate-statements body)))

(defn translate-do [& body]
  (ast/->StatementGroup (map translate-form body)))

(defn translate-doto [expr & body]
  (let [placeholder (gensym "doto__")]
    (ast/->Block (ast/->MethodCall (translate-form expr)
                                   (ast/->Identifier 'tap)
                                   nil)
                 (ast/->BlockArgList [(ast/->Identifier placeholder)])
                 (translate-statements
                   (for [form body]
                     (if (seq? form)
                       (apply list (first form) placeholder (rest form))
                       (list form placeholder)))))))

(defn translate-clojure-expr [expr]
  (ast/->ClojureExpr `[(apply str
                              (ast/emit-string-forms
                                (translate-form ~expr)))]))

(defn translate-list [[head & args :as form]]
  (case head
    . (translate-method-call (first args)
                             (second args)
                             (drop 2 args))
    new (translate-method-call (first args)
                               'new
                               (rest args))
    set! (translate-infix-op '= args)
    aref (translate-array-ref (first args) (rest args))
    do (apply translate-do args)
    if (apply translate-if args)
    def (apply translate-def args)
    module (apply translate-module args)
    class (apply translate-class args)
    singleton-class (apply translate-singleton-class args)
    doto (apply translate-doto args)
    fn* (translate-lambda args)
    with-block (apply translate-with-block args)
    ruby-syntax.core/block-expr (translate-block-expr (first args))
    clojure.core/unquote (apply translate-clojure-expr args)
    ;else
      (cond
        (and (> (count args) 1) (INFIX head))
          (translate-infix-op head args)
        (and (= (count args) 1) (PREFIX head))
          (translate-prefix-op head (first args))
        :else
          (let [expanded (macroexpand-1 form)]
            (if (identical? expanded form)
              (translate-private-call head args)
              (translate-form expanded))))))

(defn translate-form [form]
  (cond
    (map? form)
      (translate-hash-literal form)
    (vector? form)
      (translate-array-literal form)
    (seq? form)
      (translate-list form)
    (or (number? form)
        (keyword? form))
      (translate-literal form)
    (symbol? form)
      (translate-identifier form)
    (string? form)
      (translate-string form)
    :else
      (syntax-error (str "Unexpected form type "
                         (type form)
                         " for "
                         form))))

(defn translate-statements [forms]
  (ast/->Statements (map translate-form forms)))
