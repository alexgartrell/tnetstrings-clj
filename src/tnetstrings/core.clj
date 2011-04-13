(ns tnetstrings.core
  (:require [clojure.string :as str]))

(defn- lstrip [s]
  (.replaceAll s "^\\s*" ""))

(defn- explode-tnetstring [s]
  (let [[len-str data-plus] (.split (lstrip s) ":" 2)
        len (Integer. len-str)
        ; data-plus.length >= len + 1
        data (.substring data-plus 0 len)
        type (.charAt data-plus len)
        rest (.substring data-plus (+ len 1))]
    [data type rest]))

(defn- parse-list [data] nil)
(defn- parse-dict [data] nil)

(defn- parse [data type]
  (let [tis (fn [t] (= type t))] ; Abbreviating (= type t) to (tis t)
    (cond (tis \,) data              ; String
          (tis \#) (Integer. data)   ; Integer
          (tis \!) (= data "true")   ; Boolean
          (tis \~) nil               ; Null
          (tis \}) (parse-dict data) ; Dictionary
          (tis \]) (parse-list data) ; List
          :else :NOTMATCHED)))

(defn- parse-list [data]
  (loop [data data accum []]
    (if (str/blank? data)
      (reverse accum)
      (let [[data type rest] (explode-tnetstring data)
            item (parse data type)]
        (if (= item :NOTMATCHED)
          :NOTMATCHED
          (recur rest (cons (parse data type) accum)))))))

(defn- parse-dict [data]
  (loop [data data accum {}]
    (if (str/blank? data)
      accum
      (let [[kdata ktype val-plus] (explode-tnetstring data)
            [vdata vtype rest] (explode-tnetstring val-plus)
            key (parse kdata ktype)
            val (parse vdata vtype)]
        (if (or (= key :NOTMATCHED) (= val :NOTMATCHED))
          :NOTMATCHED
          (recur rest (assoc accum key val)))))))
    


(defn loads [s]
  (let [[data type rest] (explode-tnetstring s)]
    (parse data type)))

;prototype
(defn dump-item [item] nil)

(defn- dump-string [s]
  (str (.length s) \: s \,))

(defn- dump-integer [x]
  (let [s (str x)]
    (str (.length s) \: s \#)))

(defn- dump-list [lst]
  (loop [lst lst accum ""]
    (if (empty? lst) 
      (str (.length accum) \: accum \])
      (recur (rest lst) (str accum (dump-item (first lst)))))))

(defn- dump-map [dict]
  (loop [ks (keys dict) accum ""]
    (if (empty? ks)
      (str (.length accum) \: accum \})
      (let [k (first ks) v (get dict k)]
        (recur (rest ks)
               (str accum (dump-item k) (dump-item v)))))))

(defn- dump-item [item]
  (cond (nil? item) "0:~"
        (list? item) (dump-list item)
        (map? item) (dump-map item)
        (string? item) (dump-string item)
        (integer? item) (dump-integer item)
        :else :NOTMATCHED))

(defn dumps [data]
     (dump-item data))