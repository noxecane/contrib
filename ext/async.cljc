(ns contrib.ext.async
  (:require [clojure.core.async]))


(defmacro dochan
  "Wrap the expressions in a go-loop over the single binding to a channel
  till the chan closes"
  [chan & body]
  (let [[name channel-expr] binding]
    `(let [channel# ~channel-expr]
       (go-loop [~name (a/<! channel#)]
         (if (some? ~name)
           (do
             ~@body
             (recur (a/<! channel#)))
           (a/close! channel#))))))

