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

(deftest qualified-name
  (is (= "Foo::Bar::Baz" (ruby-syntax Foo.Bar.Baz)))
  (is (= "Foo::Bar::Baz::HOGE" (ruby-syntax Foo.Bar.Baz/HOGE))))

(deftest symbol-literal
  (is (= ":foobar" (ruby-syntax :foobar))))

(deftest array-literal
  (is (= "[1, 2, 3]" (ruby-syntax [1 2 3]))))

(deftest hash-literal
  (is (= "{1 => 2, 3 => 4}" (ruby-syntax {1 2 3 4}))))

(deftest assignment
  (is (= "(foo.bar = baz)" (ruby-syntax (set! (.bar foo) baz)))))

(deftest private-call
  (is (= "foo()" (ruby-syntax (foo))))
  (is (= "foo(1, 2)" (ruby-syntax (foo 1 2)))))

(deftest array-reference
  (is (= "foo[1, 2]" (ruby-syntax (aref foo 1 2)))))

(deftest multiple-statements
  (is (= "1; 2" (ruby-syntax 1 2)))
  (is (= "(1; 2)" (ruby-syntax (do 1 2)))))

(deftest if-statement
  (is (= "(if 1; 2 end)" (ruby-syntax (if 1 2))))
  (is (= "(if 1; 2 else 3 end)" (ruby-syntax (if 1 2 3)))))

(deftest call-with-block
  (is (= "foo(1, 2) { |a, b| c; d }"
         (ruby-syntax (with-block (foo 1 2) [a b]
                        c d))))
  (is (= "foo(1, 2, &:bar)"
         (ruby-syntax (with-block (foo 1 2) :bar)))))

(deftest lambda
  (is (= "lambda { |a, b| c; d }"
         (ruby-syntax (fn [a b] c d)))))

(deftest unquoted
  (let [expr '(+ 1 2)]
    (is (= "foo((1 + 2))"
           (ruby-syntax (foo ~expr))))))
