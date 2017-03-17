(ns contrib.ui.signal
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :as a]))


(defprotocol IChannel
  (emit [s e]
    "Notifies all other systems of an event e")
  (on [s k] [s k f]
    "Listens for k change in an external system, optionally using f to transform the
     incoming event"))


(defn- key-filter
  "For a given key, create a transducer that filters event vectors"
  [k]
  (filter #(= (first %) k)))


(deftype CSPChannel [in out]
  IChannel
  (emit [_ event-v]
    (a/put! in event-v))

  (on [sock event-k]
    (let [sub (a/chan 10 (key-filter event-k))]
      (a/tap out sub)
      sub))

  (on [sock event-k xform]
    (let [xxform (comp (key-filter key) xform)
          sub    (a/chan 10 xxform)]
      (a/tap out sub)
      sub)))


(defn listen [sub$ f]
  (go-loop [e (a/<! sub$)]
    (f e)
    (recur (a/<! sub$))))


(defn channel []
  (let [in  (a/chan 10)
        out (a/mult in)]
    (->CSPChannel in out)))
