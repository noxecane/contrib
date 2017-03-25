(ns contrib.ext.async
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]))
  (:require #?(:cljs [cljs.core.async :as a])
            #?(:clj [clojure.core.async :as a :refer [go go-loop]])))


(defn fixed-chan
  "Create a channel to drops the oldest messages when its
   buffer is full."
  ([n]
   (-> n a/sliding-buffer a/chan))

  ([n xform]
   (-> n a/sliding-buffer (a/chan xform))))


(defn pipeline
  "Like core.async.pipeline-async, except the asynchronous
   function involved is a timeout for n milliseconds"
  [from to n]
  (letfn [(wait [v o]
            (go (a/<! (a/timeout n))
                (a/>! o v)
                (a/close! o)))]
    (a/pipeline-async 1 to wait from)
    to))


(defmacro loop-chan-js
  "Loop over the channel till it closes."
  {:style/indent 1}
  [binding & body]
  (let [[name expr] binding]
    `(let [chan# ~expr]
       (cljs.core.async.macros/go-loop [~name (cljs.core.async/<! chan#)]
         (when ~name
           ~@body
           (recur (cljs.core.async/<! chan#)))))))


(defmacro loop-chan
  "Loop over the channel till it closes."
  {:style/indent 1}
  [binding & body]
  (let [[name expr] binding]
    `(let [chan# ~expr]
       (clojure.core.async/go-loop [~name (clojure.core.async/<! chan#)]
         (when ~name
           ~@body
           (recur (clojure.core.async/<! chan#)))))))
