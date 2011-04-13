(ns tnetstrings.core
  (:require [clojure.string :as str]))

;;;; Provides loads and dumps for conversion to and from typed netstrings
;;;; More on typed netstrings at http://www.tnetstrings.org

(defn- explode-tnetstring [s]
  (let [[len-str data-plus] (.split s ":" 2)
        len (Integer. len-str)
        ; data-plus.length >= len + 1
        data (.substring data-plus 0 len)
        type (.charAt data-plus len)
        remains (.substring data-plus (+ len 1))]
    [data type remains]))

;; prototyping for the benefit of load-list and load-map
(defn- load-item [data type] nil)

(defn- load-list [data]
  (loop [data data accum []]
    (if (str/blank? data)
      (reverse accum)
      (let [[data type remains] (explode-tnetstring data)
            item (load-item data type)]
        (if (= item :NOTMATCHED)
          :NOTMATCHED
          (recur remains (cons (load-item data type) accum)))))))

(defn- load-dict [data]
  (loop [data data accum {}]
    (if (str/blank? data)
      accum
      (let [[kdata ktype val-plus] (explode-tnetstring data)
            [vdata vtype remains] (explode-tnetstring val-plus)
            key (load-item kdata ktype)
            val (load-item vdata vtype)]
        (if (or (= key :NOTMATCHED) (= val :NOTMATCHED))
          :NOTMATCHED
          (recur remains (assoc accum key val)))))))
    
(defn- load-item [data type]
  (let [tis (fn [t] (= type t))] ; Abbreviating (= type t) to (tis t)
    (cond (tis \,) data              ; String
          (tis \#) (Integer. data)   ; Integer
          (tis \!) (= data "true")   ; Boolean
          (tis \~) nil               ; Null
          (tis \}) (load-dict data) ; Dictionary
          (tis \]) (load-list data) ; List
          :else :NOTMATCHED)))

(defn loads [s]
  (let [[data type remains] (explode-tnetstring s)]
    (load-item data type)))

;; prototyping for the benefit of dump-list and dump-map
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