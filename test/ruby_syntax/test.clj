(ns ruby-syntax.test
  (:use [ruby-syntax])
  (:use [clojure.test]))

(deftest literal-integer
  (is (= "3" (ruby-syntax 3)))
  (is (= "-3" (ruby-syntax -3))))

(deftest infix-operators
  (doseq [op '(+ - * / % ** & | ^ << >> && || and or
               == != > < >= <= <=> ===)]
    (is (= (str "(1 " op " 2)")
           (eval (list 'ruby-syntax/ruby-syntax (list op 1 2)))))))

(deftest method-call
  (is (= "foo.bar(1, 2)" (ruby-syntax (.bar foo 1 2))))
  (is (= "foo.bar(1, 2)" (ruby-syntax (. foo bar 1 2)))))

(deftest new-syntax
  (is (= "Foo.new(1, 2)" (ruby-syntax (Foo. 1 2))))
  (is (= "Foo.new(1, 2)" (ruby-syntax (new Foo 1 2)))))
