(ns contrib.ext.core
  (:require [clojure.string :as s]))


(defn starts-with?
  "Check to see if a string matches at least the initial
  part of sub. Unlike the clojure.string/starts-with?,
  accept lower case matches"
  [st sub]
  (s/starts-with?
   (-> st s/trim s/lower-case)
   (-> sub s/trim s/lower-case)))


(defn vector-map
  "Create a map of index of vectors to their values"
  [v]
  (let [indices (-> v count range)]
    (zipmap indices v)))


(defn except
  "Remove the given keys from the map"
  [coll & ks]
  (as-> ks i
    (set i)
    (complement i)
    (into {} (filter i) coll)))


(defn- except-instance?
  "For a given exception class ex, confirm that the error object e is
  an instance of it. If the error object is a clojure.lang.ExceptionInfo
  confirm that its :type property is the same as ex"
  [ex e]
  (if (instance? #?(:clj clojure.lang.ExceptionInfo
                    :cljs cljs.core.ExceptionInfo)
                 e)
    (-> e ex-data :type (= ex))
    :just))



(defmacro match [expr & forms]
  (assert (-> forms count even?) "Number of forms must be even")
  `(let [e# ~expr]
     (condp except-instance? e#
       ~@forms
       (throw e#))))

