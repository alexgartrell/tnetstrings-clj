(ns tnetstrings.core
  (:require [clojure.string :as str]))

;;;; Provides loads and dumps for conversion to and from typed netstrings
;;;; More on typed netstrings at http://www.tnetstrings.org

;;; Helper functions for loads
(defn- explode-tnetstring [s]
  (if (or (neg? (.indexOf s (int \:))) (zero? (.length s)))
    [:SHORTCOUNT :SHORTCOUNT s]
    (try
      (let [[len-str data-plus] (.split s ":" 2)
            len (Integer. len-str)]
        (if (or (neg? len) (> (+ len 1) (.length data-plus)))
          [:SHORTCOUNT :SHORTCOUNT s]
          (let
              [data (.substring data-plus 0 len)
               type (.charAt data-plus len)
               remains (.substring data-plus (+ len 1))]
            [data type remains])))
      (catch java.lang.NumberFormatException e
        [:INVALID :INVALID s]))))

(defn- invalid-or-short? [ & rest ]
  (not (not-any? (fn [x] (or (= x :INVALID) (= x :SHORTCOUNT))) rest)))

;; prototyping for the benefit of load-list and load-map
(defn- load-item [data type] nil)

(defn- load-str [data]
  data)

(defn- load-int [data]
  (try
    (Integer. data)
    (catch java.lang.NumberFormatException e :INVALID)))

(defn- load-bool [data]
  (cond (= data "true") true
        (= data "false") false
        :else :INVALID))

(defn- load-list [data]
  (loop [data data accum []]
    (if (str/blank? data)
      (reverse accum)
      (let [[data type remains] (explode-tnetstring data)
            item (load-item data type)]
        (if (invalid-or-short? item)
          :INVALID ; shortcounts cannot happen inside of valid lists
          (recur remains (cons item accum)))))))

(defn- load-map [data]
  (loop [data data accum {}]
    (if (str/blank? data)
      accum
      (let [[kdata ktype val-plus] (explode-tnetstring data)
            [vdata vtype remains] (explode-tnetstring val-plus)
            key (load-item kdata ktype)
            val (load-item vdata vtype)]
        (if (invalid-or-short? key val)
          :INVALID ; shortcounts cannot happen inside of valid maps
          (recur remains (assoc accum key val)))))))
    
(defn- load-item [data type]
  (let [tis (fn [t] (= type t))] ; Abbreviating (= type t) to (tis t)
    (cond (tis \~) nil                  ; Null
          (tis \,) (load-str data)      ; String
          (tis \#) (load-int data)      ; Integer
          (tis \!) (load-bool data)     ; Boolean
          (tis \]) (load-list data)     ; List
          (tis \}) (load-map data)      ; Map
          (tis :SHORTCOUNT) :SHORTCOUNT ; Short Count
          :else :INVALID)))

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
  (cond (nil? item)     "0:~"                  ; Null
        (string? item)  (dump-str item)        ; String
        (integer? item) (dump-int item)        ; Integer
        (boolean? item) (dump-bool item)       ; Boolean
        (list? item)    (dump-list item)       ; List
        (vector? item)  (dump-list item)       ; Vector
        (set? item)     (dump-list item)       ; Set
        (map? item)     (dump-map item)        ; Map
        (keyword? item) (dump-str (name item)) ; Keyword
        :else :INVALID))


;;; Public Functions
(defn loads [s]
  (let [[data type remains] (explode-tnetstring s)]
    (load-item data type)))

(defn dumps [data]
     (dump-item data))