(ns bank-ocr.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [bank-ocr.core :refer :all])
  (:import [java.io ByteArrayOutputStream]))

(def result "457508000\n664371485\n86110??36 ILL\n888888888 AMB ['888888988', '888886888', '888888880']\n123456789\n")  
  
(deftest test-task-4
  (let [out (ByteArrayOutputStream.)]
    (with-open [reader (io/reader (io/resource "bank_ocr/testParsingAccountsWithErrors.txt"))
	              writer (io/writer out)]
      (is reader)
      (is writer)
      (parse-to-writer reader writer))
	  (testing "test-task-4"
	    (is (= result (.toString out))))))
