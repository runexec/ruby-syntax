(ns ruby-syntax
  (:require ruby-syntax.core))

(defmacro ruby-syntax [& forms]
  (ruby-syntax.core/forms-to-string forms))
