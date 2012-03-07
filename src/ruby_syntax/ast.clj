(ns ruby-syntax.ast)

(defprotocol Node
  (emit-string-forms [this]))

(defn get-names-in [rule]
  (cond
    (vector? rule)
      (mapcat get-names-in rule)
    (and (seq? rule)
         (not= (first rule) 'clojure.core/unquote))
      (mapcat get-names-in (rest rule))
    (symbol? rule)
      [rule]
    :else
      []))

(defn expand-rule [rule]
  (cond
    (vector? rule)
      `(when (and ~@(get-names-in rule))
         (concat ~@(map expand-rule rule)))
    (seq? rule)
      (let [[head & args] rule]
        (case head
          :seq
            `(apply concat (interpose ~(expand-rule (first args))
                                      (map emit-string-forms ~(second args))))
          clojure.core/unquote
            (first args)
          ;else
            nil))
    (string? rule)
      [rule]
    (symbol? rule)
      `(when ~rule (emit-string-forms ~rule))))

(defmacro defnode 
  ([type-name rule]
    `(defnode ~type-name
              ~(get-names-in rule)
              ~rule))
  ([type-name fields rule]
    `(defrecord ~type-name ~fields
       Node
         (emit-string-forms [this]
           ~(if (vector? rule)
             `(concat ~@(map expand-rule rule))
              (expand-rule rule))))))

(defn quote-ruby-string [value]
  (pr-str value))

(defnode ClojureExpr [expr] ~expr)

(defnode Literal [value] ~[(str value)])

(defnode StringLiteral [value] ~[(quote-ruby-string value)])

(defnode Identifier [identifier] ~[(name identifier)])

(defnode ModuleLookup [target "::" identifier])

(defnode MethodCall [target "." method args])

(defnode FunCall [method args])

(defnode PrefixOp ["(" op arg ")"])

(defnode InfixOp ["("
                  (:seq [" " op " "] args)
                  ")"])

(defnode ArrayLiteral ["["
                       (:seq ", " elements)
                       "]"])

(defnode HashPair [k " => " v])

(defnode HashLiteral ["{"
                      (:seq ", " pairs)
                      "}"])

(defnode ArrayRef [target index])

(defnode Statements [(:seq "; " statements)])

(defnode If ["if " pred
             "; " then-clause
             [" else " else-clause]
             " end"])

(defnode BlockArgList ["|"
                       (:seq ", " params)
                       "|"])

(defnode StatementGroup ["("
                         (:seq "; " statements)
                         ")"])

(defnode ArgGroup ["("
                   (:seq ", " params)
                   ")"])

(defnode SplatArg ["*" expr])

(defnode BlockArg ["&" expr])

(defnode Block [call " { " args " " body " }"])

(defnode Def ["def " method args "; "
              body
              " end"])

(defnode ModuleDef ["module " module-expr "; "
                    body
                    " end"])

(defnode ClassDef ["class " class-name [" < " super-name] "; "
                   body
                   " end"])

(defnode SingletonClassDef ["class << " expr "; "
                            body
                            " end"])

(defn coalesce-strings [forms]
  (apply concat (when-not (empty? forms)
                  (for [group (partition-by string? forms)]
                    (if (and (next group)
                             (string? (first group)))
                      [(apply str group)]
                      group)))))

(defn serialize-to-form [node]
  (let [forms (coalesce-strings (emit-string-forms node))]
    (cond
      (empty? forms)
        ""
      (not (next forms))
        (first forms)
      :else
        `(str ~@forms))))
