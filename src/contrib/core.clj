(ns contrib.core
  (:require [clojure.core.async :as a]
            [clojure.string :as string]
            [clojure.tools.reader.edn :as edn]))


(def ^{:private true} email#
  "A regex for emails"
  #"[a-z0-9!#$%&'*+/=^_`{|}~-]+(?:[a-z0-9!#$%&'*+/=^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)(?:[a-z0-9-]*[a-z0-9])?")

(def ^{:private true} phone#
  "A regex for nigerian phone numbers(without +234)"
  #"[789][01]\d{8}")

(def ^{:private true} decorators (atom {}))


(defn key-is?
  "Given a keyword/predicate, validate it is equal to val"
  [key val]
  #(-> % key (= val)))


(defn load-edn
  "Convert an edn file to clojure data structure"
  [path]
  (-> path slurp edn/read-string))


(defn data->bytes
  "Convert a clojure data structure to bytes"
  [data]
  (-> data prn-str .getBytes))


(defn uuid
  "Create a random UUID as a string"
  []
  (str (java.util.UUID/randomUUID)))


(defn <??
  "Generate an exception from a channel. i.e Given a channel for
  every object that it generates that is an instance of a `Throwable`
  throw the object.
   "
  [chan]
  (let [result (a/<!! chan)]
    (if (instance? Throwable result)
      (throw result)
      result)))


(defn slice
  "Given a sequence of length n such that start<end index, return
  another sequence between start(inclusive) and end(exclusive).
  Let start or end be i such that i<0, i will be interpreted
  as n-i. If end is not given take it to be n"
  ([sq start]
   (slice sq start (count sq)))
  ([sq start end]
   (let [remains #(if (neg? %1)
                    (+ %2 %1)
                    %1)
         s       (count sq)
         a       (remains start s)
         z       (remains end s)]
     (->> sq (take z) (drop a)))))


(defn slice-get
  "For every sequence of size m return the element at i if
  m=e and i to m-e if m>e."
  [coll i e]
  (let [s (count coll)]
    (cond
      (= s e) (nth coll i)
      (> s e) (slice coll i (+ 1 i (- s e)))
      :else   (throw
               (IllegalArgumentException.
                (str
                 "The length \"" s "\" of the collection "
                 "is less than the expected size \"" e "\""))))))


(defn drop-get
  "For every collection of size m return the element at i if
  m=e and m-e+i if m>e"
  [coll i e]
  (let [s (count coll)
        ni (+ i (- s e))]
    (cond
      (>= ni i) (nth coll ni)
      :else   (throw
               (IllegalArgumentException.
                (str
                 "The length \"" s "\" of the collection "
                 "is less than the expected size \"" e "\""))))))


(defn take-get
  "For every collection of size m return the element at i if
  m=e and i-e+m if m<e"
  [coll i e]
  (let [s        (count coll)
        ni       (- i (+ (- s) e))
        not-neg? (complement neg?)]
    (cond
      (and (<= ni i)
           (some? ni)) (nth coll ni)
      (neg? ni)           (throw
                           (IllegalArgumentException.
                            (str
                             "The length \"" s "\" of the collection "
                             "is way too small for the index  \"" i "\"")))
      :else               (throw
                           (IllegalArgumentException.
                            (str
                             "The length \"" s "\" of the collection "
                             "is more than the expected size \"" e "\""))))))



(defn pages
  "Split a full text into pages based on the pagebreak character(^L, \f)"
  [text]
  (string/split text #"\f"))


(defn clean
  "Remove empty strings from a list of strings"
  [strs]
  (filter seq strs))


(defn email?
  "Validate that the structure of a string s is similar to that of an email
  as given by `email#`"
  [s]
  (and (string? s)
       (->> s (re-matches email#) some?)))


(defn phone?
  "Validate that the structure of a string s is similar to that of a phone
  number as given by `phone#`"
  [s]
  (and (string? s)
       (->> s (re-matches phone#) some?)))


(defn merge-on
  "Given a vector of maps, use into to merge all the values of k
  in the maps onto the first"
  [maps k]
  (let [sample     (first maps)
        first-val  (k sample)
        other-vals (map k (rest maps))
        new-vals   (conj other-vals first-val)]
    (assoc sample k (into {} new-vals))))


(defn parse-date
  "Proxy to `java.text.SimpleDateFormat`'s parse method"
  [s format]
  (let [fmter (java.text.SimpleDateFormat. format)]
    (.parse fmter s)))


(defn now!
  "For any specific time(no pun intended) returns the current
  UTC time"
  []
  (java.util.Date.))


(defn clean-ns
  "Unbind all vars in a given namespace ns. Take ns
  to be the current namespace `*ns*` if it is not given"
  ([] (clean-ns *ns*))
  ([ns]
   (map #(ns-unmap ns %) (keys (ns-interns ns)))))


(defmacro retry
  "Retries the expressions the given number of times catching
   the given exception. The default number of retries is 10. Bubbles
   the error when the number of retries is exhausted"
  [except times & body]
  `(loop [left# (dec ~times)]
     (let [[result# passed#] (try
                               [(do ~@body) true]
                               (catch ~except e#
                                 (when (zero? left#)
                                   (throw e#))
                                 [nil false]))]
       (if passed#
         result#
         (recur (dec left#))))))


(defmacro ignore
  "For an expression and a set of form pairs taking the form
  ex-type result, where ex-type can be an exception type or :type
  value for ex-info exceptions, match any exception thrown by the
  expression with one of the ex-types and return it's result form. If
  none is matched rethrow the error"
  [expr & forms]
  (assert (-> forms count even?) "Number of forms must be even")
  `(try
     ~expr
     (catch Exception e#
       (condp except-instance? e#
         ~@forms
         (throw e#)))))


(defmacro what-now?
  "For a given expression expr, catch the exception except and print tell-me"
  [expr except tell-me]
  `(try
     ~expr
     (catch ~except e#
       (println ~tell-me)
       (throw e#))))


(defn except-instance?
  "For a given exception class ex, confirm that the error object e is
  an instance of it. If the error object is a clojure.lang.ExceptionInfo
  confirm that its :type property is the same as ex"
  [ex e]
  (if (instance? clojure.lang.ExceptionInfo e)
    (-> e ex-data :type (= ex))
    (instance? ex e)))


(defmacro except
  "This is a version of `ignore` for functions"
  [f binding & forms]
  (assert (-> forms count even?) "Number of forms must be even")
  `(fn [& args#]
     (try
       (apply ~f args#)
       (catch Exception e#
         (let [~binding (conj args# e#)]
           (condp except-instance? e#
             ~@forms
             (throw e#)))))))


(defmacro dochan
  "Wrap the expressions in a go-loop over the single binding to a channel
  till the chan closes"
  [binding & body]
  (let [[name channel] binding]
    `(a/go-loop [~name (a/<! ~channel)]
       (if (some? ~name)
         (do
           ~@body
           (recur (a/<! ~channel)))
         (a/close! ~channel)))))


(defmacro cond-let
  "Takes a symbol and a set of test/expr pairs. Tests may be
   expressions or binding vectors. If a test returns logical true,
   cond-let evaluates the corresponding, if a binding vector was
   provided, the expr will be evaluated within that context. The
   return value of the expr is returned and no more tests are
   evaluated. If no test/expr paris are present, nil is returned.
   An odd number of clauses will throw an exception. Thanks to
   https://gist.github.com/pyr/506fef9ba2d2ae3bbec6"
  [& clauses]
  (when clauses
    (list (if (vector? (first clauses)) 'if-let 'if)
          (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "cond-let requires an even number of forms.")))
          (cons `cond-let (next (next clauses))))))
