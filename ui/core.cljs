(ns contrib.ui.core
  (:require-macros [cljs.core.async.macros :as a :refer [go-loop]]
                   [contrib.ui.core :refer [dosub]])
  (:require [cljs.core.async :as a]
            [reagent.core :as r]))


(defn- by-id
  "Proxy to getElementById"
  [id]
  (js/document.getElementById id))


(defn embed
  "Mount the given react component on the HTML dom element
  with the given node-id"
  [main node-id]
  (r/render-component main (by-id node-id)))


(defprotocol INode
  (emit [s e]
    "Notifies all other node of an event e")
  (on [s k]
    "Passes events to k"))


(deftype AsyncNode [in out]
  INode
  (emit [_ event-v]
    (a/put! in event-v))

  (on [sock sub]
    (a/tap out sub)
    sub))


(defn node []
  (let [in  (a/chan 10)
        out (a/mult in)]
    (->AsyncNode in out)))


(defn box
  ([]
   (a/chan 10))

  ([xform]
   (a/chan 10 xform)))


(defn basket
  ([]
   (-> 10 a/sliding-buffer a/chan))

  ([xform]
   (-> 10 a/sliding-buffer (a/chan xform))))


(defn key-filter
  "For a given key, create a transducer that filters event vectors"
  [k]
  (filter #(= (first %) k)))


(defn key-sub
  "For a given key, create a channel to filter only events with such key"
  [sock k]
  (on sock (box (key-filter k))))


(defn wait
  "For a given amount of time, delay the consumption of the event"
  [from$ to$ n]
  (go-loop [e (a/<! from$)
            _ (a/<! (a/timeout n))]
    (a/put! to$ e)
    (recur (a/<! from$) (a/<! (a/timeout n))))
  to$)


(defn proxy! [from$ to]
  (dosub [e from$]
         (emit to e)))
