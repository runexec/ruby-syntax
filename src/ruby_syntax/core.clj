(ns ruby-syntax.core)

(def INFIX (set '(+ - * / % ** & | ^ << >> && || and or
                  == != > < >= <= <=> ===)))

(defn join-seq [delim seqs]
  (apply concat (interpose [delim] seqs)))

(defn syntax-error [message]
  (throw (RuntimeException. message)))

(declare translate-form)

(defn translate-list [start delim end args]
  (concat [start]
          (join-seq delim (map translate-form args))
          [end]))

(defn translate-method-call [target method args]
  (if (list? method)
    (apply translate-method-call target method)
    (concat (translate-form target)
            ["." (str method)]
            (translate-list "("
                            ", "
                            ")"
                            args))))

(defn translate-infix [op args]
  (translate-list "("
                  (str " " op " ")
                  ")"
                  args))

(defn translate-array-literal [args]
  (translate-list "["
                  ", "
                  "]"
                  args))

(defn translate-form [form]
  (cond
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
          ;else
            (cond
              (INFIX head)
                (translate-infix head args)
              (and (symbol? head)
                   (or (.startsWith (str head) ".")
                       (.endsWith (str head) ".")))
                (translate-form (macroexpand-1 form))
              :else
                (syntax-error (str "Unknown head " head)))))
    (or (number? form)
        (symbol? form))
      (str form)
    :else
      (syntax-error (str "Unexpected form type "
                         (type form)
                         " for "
                         form))))

(defn translate-forms [forms]
  (join-seq ";" (map translate-form forms)))

(defn forms-to-string [forms]
  (apply str (translate-forms forms)))
