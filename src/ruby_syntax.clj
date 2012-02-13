(ns ruby-syntax
  (:use [ruby-syntax.core :only [translate-forms coalesce-tokens]]))

(defmacro ruby-syntax [& forms]
  `(str ~@(for [token (coalesce-tokens (translate-forms forms))]
            (if (string? token)
              token
              `(apply str ~token)))))
