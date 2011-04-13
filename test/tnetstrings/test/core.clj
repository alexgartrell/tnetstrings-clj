(ns tnetstrings.test.core
  (:use tnetstrings.core clojure.test))

(deftest simple-loads
  (is (= 1337 (loads "4:1337#")))
  (is (= "1337" (loads "4:1337,")))
  (is (nil? (loads "0:~")))
  (is (= (list) (loads "0:]")))
  (is (= (list 1 2 3 4 5) (loads "20:1:1#1:2#1:3#1:4#1:5#]")))
  (is (= {} (loads "0:}")))
  (is (= {"foo" "bar" "biz" "bang"} (loads "25:3:foo,3:bar,3:biz,4:bang,}"))))

(deftest simple-dumps
  (is (= "4:1337#" (dumps 1337)))
  (is (= "4:1337," (dumps "1337")))
  (is (= "0:~" (dumps nil)))
  (is (= "0:]" (dumps (list))))
  (is (= "20:1:1#1:2#1:3#1:4#1:5#]" (dumps (list 1 2 3 4 5))))
  (is (= "0:}" (dumps {})))
  (is (= "25:3:foo,3:bar,3:biz,4:bang,}" (dumps {"foo" "bar" "biz" "bang"}))))

(defn roundtrip-item [item]
  (is (= item (loads (dumps item)))))

(deftest roundtrip
  (roundtrip-item {nil 10 "foo" (list "bar")})
  (roundtrip-item (list 1 2 nil "" nil "foo" {"bah" "bang"} (list 1 2))))
