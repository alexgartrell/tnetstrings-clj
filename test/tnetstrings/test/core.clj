(ns tnetstrings.test.core
  (:use tnetstrings.core clojure.test))

(deftest simple-loads
  (testing "with nil"
    (is (nil? (loads "0:~"))))
  (testing "with strings"
    (is (= "1337" (loads "4:1337,"))))
  (testing "with integers"
    (is (= 1337 (loads "4:1337#"))))
  (testing "with booleans"
    (is (= true (loads "4:true!")))
    (is (= false (loads "5:false!"))))
  (testing "with lists"
    (is (= (list) (loads "0:]")))
    (is (= (list 1 2 3 4 5) (loads "20:1:1#1:2#1:3#1:4#1:5#]"))))
  (testing "with maps"
    (is (= {} (loads "0:}")))
    (is (= {"foo" "bar" "biz" "bang"}
           (loads "25:3:foo,3:bar,3:biz,4:bang,}")))))

(deftest simple-dumps
  (testing "with nil"
    (is (= "0:~" (dumps nil))))
  (testing "with strings"
    (is (= "4:1337," (dumps "1337"))))
  (testing "with integers"
    (is (= "4:1337#" (dumps 1337))))
  (testing "with booleans"
    (is (= "4:true!" (dumps true)))
    (is (= "5:false!" (dumps false))))
  (testing "with lists"
    (is (= "0:]" (dumps (list))))
    (is (= "20:1:1#1:2#1:3#1:4#1:5#]" (dumps (list 1 2 3 4 5)))))
  (testing "with maps"
    (is (= "0:}" (dumps {})))
    (is (= "25:3:foo,3:bar,3:biz,4:bang,}"
           (dumps {"foo" "bar" "biz" "bang"})))))

;; This needs a lot of work, both here and in the actual core, we pretty much
;; puke whenever something has a bad prefix length
(deftest malformed-loads
  (is (= :NOTMATCHED (loads "3:foo!")))
  (is (= :NOTMATCHED (loads "6:3:foo!]")))
  (is (= :NOTMATCHED (loads "10:1:1#3:foo!]"))))

(defn roundtrip-item [item]
  (is (= item (loads (dumps item)))))

(deftest roundtrip
  (roundtrip-item {nil 10 "foo" (list "bar")})
  (roundtrip-item (list 1 2 nil "" nil "foo" {"bah" "bang"} (list 1 2))))
