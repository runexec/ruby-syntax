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
            ["." (str method)]
            (concat ["("]
                    (join-seq ", "
                              (map translate-form args))
                    [")"]))))

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
          (join-seq ", " (map #(str (translate-form (key %))
                                    " => "
                                    (translate-form (val %)))
                              pairs))
          ["}"]))

(defn translate-identifier [identifier]
  (join "::" (split (if (namespace identifier)
                      (str (namespace identifier)
                           "."
                           (name identifier))
                      (name identifier))
                    #"\.")))

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
        (keyword? form))
      (str form)
    (symbol? form)
      (translate-identifier form)
    :else
      (syntax-error (str "Unexpected form type "
                         (type form)
                         " for "
                         form))))

(defn translate-forms [forms]
  (join-seq ";" (map translate-form forms)))

(defn forms-to-string [forms]
  (apply str (translate-forms forms)))
