(ns contrib.ext.strings
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


(defn match
  "Equivalent of rust's match but for exception handling. Note that
   the it only supports ExceptionInfo exceptions(this is intentional)."
  [expr])
