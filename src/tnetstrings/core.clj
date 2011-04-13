(ns tnetstrings.core
  (:require [clojure.string :as str]))

;;;; Provides loads and dumps for conversion to and from typed netstrings
;;;; More on typed netstrings at http://www.tnetstrings.org

;;; Helper functions for loads
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

(defn- load-str [data]
  data)

(defn- load-int [data]
  (Integer. data))

(defn- load-bool [data]
  (cond (= data "true") true
        (= data "false") false
        :else :NOTMATCHED))

(defn- load-list [data]
  (loop [data data accum []]
    (if (str/blank? data)
      (reverse accum)
      (let [[data type remains] (explode-tnetstring data)
            item (load-item data type)]
        (if (= item :NOTMATCHED)
          :NOTMATCHED
          (recur remains (cons (load-item data type) accum)))))))

(defn- load-map [data]
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
    (cond (tis \~) nil               ; Null
          (tis \,) (load-str data)   ; String
          (tis \#) (load-int data)   ; Integer
          (tis \!) (load-bool data)  ; Boolean
          (tis \]) (load-list data)  ; List
          (tis \}) (load-map data)   ; Map
          :else :NOTMATCHED)))

;;; Helper functions for dumps

;; prototyping for the benefit of dump-list and dump-map
(defn dump-item [item] nil)

(defn- dump-str [s]
  (str (.length s) \: s \,))

(defn- dump-int [x]
  (let [s (str x)]
    (str (.length s) \: s \#)))

(defn- dump-bool [b]
  (if b "4:true!" "5:false!"))

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

(defn- boolean? [x] (or (= true x) (= false x)))

(defn- dump-item [item]
  (cond (nil? item) "0:~"                   ; Null
        (string? item) (dump-str item)      ; String
        (integer? item) (dump-int item)     ; Integer
        (boolean? item) (dump-bool item)    ; Boolean
        (list? item) (dump-list item)       ; List
        (map? item) (dump-map item)         ; Map
        :else :NOTMATCHED))


;;; Public Functions
(defn loads [s]
  (let [[data type remains] (explode-tnetstring s)]
    (load-item data type)))

(defn dumps [data]
     (dump-item data))