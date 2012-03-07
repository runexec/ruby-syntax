(ns ruby-syntax
  (:use [ruby-syntax.ast :only [serialize-to-form]]
        [ruby-syntax.core :only [translate-statements]]))

(defmacro ruby-syntax [& forms]
  (serialize-to-form (translate-statements forms)))
