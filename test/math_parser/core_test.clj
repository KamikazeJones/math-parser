(ns math-parser.core-test
  (:require [clojure.test :refer :all]
            [math-parser.core :refer :all]))

(deftest F-test
  (is (= 0 1)))

(deftest NUMBER-test
  (let [parser (new-parser "1234.45 + 123")
        number (NUMBER parser)]

        (is (= ((number :tree) :value) 1234.45))
        (is (= ((number :token) :type) :plus))))

(deftest terminal-test
  (let [parser (new-parser "-123")
        t (terminal parser :minus)]
    (is (= t {:terminal-key :minus
              :token        {:rest  ""
                             :type  :number
                             :value 123.0}}))))

(deftest F-test
  (let [parser (new-parser "-123 + 5")
        f (F parser)]
    (is (= f {:token {:rest " 5"
                      :type :plus}
              :tree  {:child  {:child  {:child  {:symbol :number
                                                 :type   :float
                                                 :value  123.0}
                                        :symbol "F"
                                        :type   :subexpr}
                               :symbol :terminal
                               :type   :neg}
                      :symbol "F"
                      :type   :subexpr}}))
  )
)