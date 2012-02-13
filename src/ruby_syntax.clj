(ns ruby-syntax
  (:use [ruby-syntax.core :only [translate-forms]]))

(defmacro ruby-syntax [& forms]
  `(str ~@(translate-forms forms)))
